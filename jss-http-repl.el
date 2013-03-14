(define-derived-mode jss-http-repl-mode jss-super-mode "JSS HTTP REPL"
  "Major mode for manually creating, editing and submitting HTTP requests.

A jss-http-repl buffer can be quickly created from a JSS IO
 buffer; this allows for easily editing/debugging/replaying
 recent json requests from the browser.

This mode is designed for testing/debugging json/ajax requests
but it can be used with any kind of HTTP request."
  (setf buffer-file-coding-system 'utf-8-unix
        jss-http-repl-status :closed)
  ;; we do same string->byte conversions and assume that the buffer is
  ;; a multibyte buffer, this is already emacs' default, but let's
  ;; just be explicit about it.
  (set-buffer-multibyte t)
  t)

(easy-menu-define jss-http-repl-menu jss-http-repl-mode-map
  "Menu for JSS HTTP REPL buffers."
  '("JSS HTTP REPL"
    [ "Submit" jss-http-repl-send-request t]
    [ "Ensure Header" jss-http-repl-ensure-header t ]))

(define-key jss-http-repl-mode-map (kbd "C-c <return>") 'jss-http-repl-ensure-header)
(define-key jss-http-repl-mode-map (kbd "C-c C-c") 'jss-http-repl-send-request)

(defcustom jss-http-repl-track-cookies t
  "When T, and can be buffer local, automatically keep track of
  cookies by adding/removing headers from the request objects."
  :type 'boolean
  :group 'jss)

(make-variable-buffer-local 'jss-http-repl-track-cookies)

(defvar jss-http-repl-cookie-jar '())

(make-variable-buffer-local
 (defvar jss-http-repl-previous-header-string nil))

(make-variable-buffer-local
 (defvar jss-http-repl-previous-header-string nil))

(make-variable-buffer-local
 (defvar jss-http-repl-previous-data-string nil))

(make-variable-buffer-local
 (defvar jss-http-repl-status nil
   "The current status of the server connection (:opening, :sending, :receiving-headers, :receiving-data, :idle or :closed)"))

(make-variable-buffer-local
 (defvar jss-http-repl-keep-alive nil
   "When T the current server connection should be reused."))

(make-variable-buffer-local
 (defvar jss-http-repl-content-length nil
   "The number of bytes of body data we're expecting."))

(make-variable-buffer-local
 (defvar jss-http-repl-set-cookies '()
   "Any cookies that should be added to the following request's headers."))

(defface jss-http-repl-meta-data-face
  '((t :inherit font-lock-special-keyword-face))
  "Face used to mark 'meta' headers such as Endpoint:, --post
  data starts here-- etc.")

(defface jss-http-repl-submitted-face
  '((t :slant italic))
  "Face used to mark data that has been sent and is no longer editable.")

(defun* jss-http-repl-new (&rest insert-args)
  (interactive)
  (with-current-buffer (generate-new-buffer "*JSS HTTP REPL*")
    (jss-http-repl-mode)
    (apply 'jss-http-repl-insert-request insert-args)
    (jss-http-repl-goto-data-start)
    (switch-to-buffer-other-window
     (current-buffer))))

(defun* jss-http-repl-insert-request (&key headers data host port ssl)
  (unless (memq jss-http-repl-status (list :idle :closed))
    (error "Request in progress, can't insert new request."))

  (setf jss-http-repl-status :closed)

  (jss-wrap-with-text-properties (list 'jss-http-repl-endpoint t)
    (insert "Endpoint:"))
  (let (query-point)
    (let ((inhibit-read-only t))
      (insert "GET /")
      (setf query-point (point))
      (insert " HTTP/1.0\n"))
    (insert "Host: \n")
    (jss-wrap-with-text-properties (list 'read-only t 'face 'jss-http-repl-meta-data-face 'jss-header-end-marker t)
      (insert "--request data follows this line--"))
    (jss-wrap-with-text-properties (list 'jss-header-end-marker t 'rear-nonsticky t)
      (let ((inhibit-read-only t))
        (insert "\n")))

    (jss-http-repl-set-endpoint :host host :port port :ssl ssl)
    (when headers
      (if jss-http-repl-set-cookies
          (progn
            (jss-http-repl-set-headers headers :extra-headers (mapcar (lambda (cookie)
                                                                        (cons "Cookie" cookie))
                                                                      jss-http-repl-set-cookies))
            (setf jss-http-repl-set-cookies '()))
        (jss-http-repl-set-headers headers)))
    (when data
      (jss-http-repl-set-request-data data))
    
    (goto-char query-point)))

(defvar jss-http-repl-request-line-regexp
  "^\\(GET\\|POST\\|HEAD\\|PUT\\|DELETE\\|TRACE\\|CONNECT\\)\\s-+\\(.*\\)\\s-+\\(HTTP/1\\.[10]\\)\\s-*$")

(defun jss-http-repl-set-method (status)
  (interactive (list (jss-completing-read "Method: " (list "GET"
                                                           "POST"
                                                           "HEAD"
                                                           "OPTIONS"
                                                           "PUT"
                                                           "DELETE"
                                                           "TRACE"
                                                           "CONNECT"))))
  (save-excursion
    (save-match-data
      (block nil
        (jss-http-repl-goto-header-start)
        (while (not (looking-at jss-http-repl-request-line-regexp))
          (forward-line 1)
          (when (eobp)
            (error "Unable to find Request-Line")))
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 1))
        (insert status)))))

(defun jss-http-repl-goto-header-start ()
  (let ((endpoint (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint))
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun jss-http-repl-goto-header-end ()
  (let ((endpoint (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint))
    (while (not (get-text-property (point) 'jss-header-end-marker))
      (when (eobp)
        (error "Looking for header end but got to end of buffer."))
      (forward-char 1))
    (point)))

(defun jss-http-repl-goto-data-start ()
  (jss-http-repl-goto-header-end)
  (block nil
    (while (get-text-property (point) 'jss-header-end-marker)
      (when (eobp)
        (return))
      (forward-char 1)))
  (point))

(defun jss-http-repl-ensure-header (header-name)
  "If there already is a header named `header-name` then simple
moves to it, otherwise inserts it.

When called interactively will prompt, with completion, for the
name of the header to add."
  (interactive (list (jss-completing-read "Header: "
                                          (sort (mapcar (lambda (header)
                                                          (if (consp header) (car header) header))
                                                        jss-http-repl-request-header-editors)
                                                'string<)
                                          :require-match nil)))
  (if (jss-http-repl-goto-header header-name)
      t
    (insert header-name ": \n")
    (forward-line -1)
    (jss-http-repl-gopast-header header-name)))

(defun jss-http-repl-goto-header (header-name)
  "If there is a header named `header-name` in the current
request then moves point to after its ?: char and returns t,
otherwise leaves point at the last line of the headers (which a
simple insert is enough to insert a new header) and returns nil"
  (save-match-data
    (jss-http-repl-goto-header-start)
    (block nil
      (while (not (jss-http-repl-in-header-line header-name))
        (forward-line 1)
        (when (looking-at "--request data follows this line--")
          (return nil))
        (when (eobp)
          (error "Buffer ended before be could find the header. Is the --request data-- line missing?")
          (return)))
      ;; if we get here the while terminated we're 
      (beginning-of-line)
      (jss-http-repl-gopast-header header-name))))

(defun jss-http-repl-in-header-line (name)
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^" (regexp-quote name) ":"))))

(defun jss-http-repl-looking-at-header-p (header-name)
  (looking-at (concat "^" (regexp-quote name) ":\\s-*")))

(defun jss-http-repl-gopast-header (name)
  (beginning-of-line)
  (save-match-data
    (if (jss-http-repl-looking-at-header-p name)
        (goto-char (1- (match-end 0)))
      (error "Not looking at the header %s." name))))

(defvar jss-http-repl-request-header-editors
  '("Accept" "Accept-Charset" "Accept-Encoding" "Accept-Language" "Accept-Datetime"
    "Authorization"
    ("Cache-Control" . jss-http-repl-choose-cache-control)
    "Connection"
    "Cookie"
    ("Content-Length" . jss-http-repl-set-content-length) "Content-MD5" "Content-Type"
    "Date"
    "Expect"
    "From"
    "Host"
    "If-Match" "If-Modified-Since" "If-None-Match" "If-Range" "If-Unmodified-Since"
    "Max-Forwards"
    "Pragma"
    "Proxy-Authorization"
    "Range"
    "Referer"
    "TE"
    "Upgrade"
    ("User-Agent" . jss-http-repl-choose-user-agent)
    "Via"
    "Warning"
    "X-Requested-With"))

(defun jss-http-repl-request-header-editor (header-name)
  (dolist (h jss-http-repl-request-header-editors)
    (cond
     ((and (consp h)
           (string= header-name (car h)))
      (return (cdr h)))
     ((and (stringp h)
           (string= h header-name))
      (return t)))))

(defvar jss-http-repl-user-agents
  '(("IE 6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
    ("IE 7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)")
    ("IE 8" . "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1)")
    ("Googlebot" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
    ("Google Chrome 24/Mac OS X" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1309.0 Safari/537.17")
    ("Firefox" "Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1")
    ("Safari" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2")))

(defun jss-http-repl-choose-user-agent ()
  (let* ((user-agent-name (jss-completing-read "Browser: " (mapcar 'car jss-http-repl-user-agents)
                                               :require-match nil))
         (known-value (cl-assoc user-agent-name jss-http-repl-user-agents :test 'string=)))
    (if known-value
        (cdr known-value)
      ;; if the user typed in their own user agent string then we
      ;; won't find it in jss-http-repl-user-agents and we just
      ;; use whatever they sent.
      user-agent-name)))

(defun jss-http-repl-choose-cache-control ()
  (interactive)
  "no-cache")

(defun jss-http-repl-set-content-length ()
  (interactive)
  (save-excursion
    (format "%d" (string-bytes
                  (buffer-substring-no-properties (jss-http-repl-goto-data-start)
                                                  (point-max))))))

(defun jss-http-repl-delete-headers ()
  (jss-http-repl-goto-header-start)
  (delete-region (point) (jss-http-repl-goto-header-end)))

(defun jss-http-repl-looking-at-newline ()
  (and (char-after (point))
       (char-equal ?\r (char-after (point)))
       (char-after (1+ (point)))
       (char-equal ?\n (char-after (1+ (point))))))

(defun jss-chars-to-string (&rest chars)
  (apply 'concat (mapcar 'char-to-string chars)))

(defun* jss-http-repl-set-headers (header-string &key extra-headers)
  (delete-region (jss-http-repl-goto-header-start)
                 (jss-http-repl-goto-header-end))
  ;; the above delete-region leaves point at the beginning of the header block
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a #x0d #x0a ?\\ ?')
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a)
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (insert header-string)
  (loop
   for (name . value) in extra-headers
   do (insert name ": " value "\n"))
  (jss-http-repl-goto-header-start))

(defun jss-http-repl-request-data ()
  (save-excursion
    (jss-http-repl-goto-data-start)
    (buffer-substring-no-properties (point) (point-max))))

(defun jss-http-repl-set-request-data (data-string)
  (jss-http-repl-goto-data-start)
  (delete-region (point) (point-max))
  (when data-string
    (insert data-string)))

(defun* jss-http-repl-set-endpoint (&key host port ssl)
  (let ((endpoint-location (jss-find-property-block 'jss-http-repl-endpoint t)))
    (let ((inhibit-read-only t))
      (goto-char (car endpoint-location))
      (delete-region (car endpoint-location) (cdr endpoint-location))
      (jss-wrap-with-text-properties (list 'rear-nonsticky t
                                           'jss-http-repl-endpoint t
                                           'read-only t
                                           'face 'jss-http-repl-meta-data-face)
        (insert "Endpoint: "))
      (when host
        (insert host)
        (when port
          (insert ":" port)))
      (when ssl
        (insert " SSL"))
      (insert "\n"))))

(defun* jss-http-repl-get-endpoint ()
  (let ((endpoint-location (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint-location))
    (save-match-data
      (if (looking-at "Endpoint:\\s-*\\(.*?\\)\\(:\\([0-9]+\\)\\)?\\s-*\\(SSL\\)?\\s-*$")
          (list :host (match-string-no-properties 1)
                :port (match-string-no-properties 3)
                :ssl (if (match-string-no-properties 4)
                         t
                       nil))
        '()))))

(defun jss-http-repl-preflight ()
  (unless (memq jss-http-repl-status '(:idle :closed))
    (error "Can't prepared a new request unless we're in editing mode. current mode is %s." jss-http-repl-status))
  (let* ((headers (buffer-substring-no-properties (jss-http-repl-goto-header-start)
                                                  (jss-http-repl-goto-header-end)))
         (data (buffer-substring-no-properties (jss-http-repl-goto-data-start)
                                               (point-max)))
         (endpoint (jss-http-repl-get-endpoint))
         (request-start (car (jss-find-property-block 'jss-http-repl-endpoint t)))
         (submitted-overlay (make-overlay request-start (point-max))))
    (unless endpoint
      (error "Missing endpoint data, don't where to send request."))
    (unless (getf endpoint :host)
      (error "Missing host specifier from endpoint."))
    (unless (getf endpoint :port)
      (error "Missing port specifier from endpoint."))
    (overlay-put submitted-overlay 'face 'jss-http-repl-submitted-face)
    (let ((inhibit-read-only t))
      (remove-text-properties request-start (point-max)
                              (list 'jss-http-repl-endpoint t
                                    'jss-header-end-marker t
                                    'rear-nonsticky t))
      (add-text-properties request-start (point-max) (list 'read-only t)))
    (list* :header-string headers
           :data-string data
           endpoint)))

(defun jss-http-repl-send-request ()
  (interactive)
  (destructuring-bind (&key header-string data-string host port ssl)
      (jss-http-repl-preflight)
    (jss-request-submit host
                        (if port
                            (string-to-number port)
                          nil)
                        ssl
                        header-string
                        data-string)))

(defun jss-request-submit (host port ssl headers data)
  (setf jss-http-repl-previous-header-string headers
        jss-http-repl-previous-data-string data
        jss-http-repl-previous-host host
        jss-http-repl-previous-port port
        jss-http-repl-previous-ssl ssl
        jss-http-repl-status :opening)
  (message "Sending request to %s:%s." host port)
  (make-network-process :name "jss-http-repl-request"
                        :server nil
                        :host host
                        :service port
                        :nowait t
                        :coding '(binary . binary)
                        :filter 'jss-http-repl-process-filter
                        :sentinel 'jss-http-repl-process-sentinel
                        :buffer (current-buffer)))

(defun jss-http-repl-process-sentinel (proc status)
  (with-current-buffer (process-buffer proc)
    (cond
     ((string= "open\n" status)
      (jss-http-repl-process-send-data)
      (setf jss-http-repl-status :receiving-headers))
     ((string= "connection broken by remote peer\n" status)
      (setf jss-http-repl-status :closed
            jss-http-repl-keep-alive nil)
      (jss-http-repl-insert-next-request))
     (t
      (message "%s got unknown sentinel status %s." proc status)))))

(defun jss-http-repl-process-send-data ()
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (jss-wrap-with-text-properties (list'face 'jss-http-repl-meta-data-face
                                              'read-only t)
      (unless (= (point) (line-beginning-position))
        (insert "\n"))
      (insert "--response headers follow this line--\n")))
  (setf jss-http-repl-status :sending)
  (process-send-string proc (encode-coding-string jss-http-repl-previous-header-string 'us-ascii-dos))
  (when jss-http-repl-previous-data-string
    (process-send-string proc (jss-chars-to-string #x0d #x0a))
    (process-send-string proc (encode-coding-string jss-http-repl-previous-data-string 'utf-8-unix)))
  (setf jss-http-repl-status :receiving-headers))

(defun jss-http-repl-insert-next-request ()
  (let ((inhibit-read-only t))
    (insert "\n")
    (jss-http-repl-insert-request :headers jss-http-repl-previous-header-string
                                  :data jss-http-repl-previous-data-string
                                  :host jss-http-repl-previous-host
                                  :port (format "%d" jss-http-repl-previous-port)
                                  :ssl jss-http-repl-previous-ssl))
  (setf jss-http-repl-previous-header-string nil
        jss-http-repl-previous-data-string nil
        jss-http-repl-previous-host nil
        jss-http-repl-previous-port nil
        jss-http-repl-previous-ssl nil))

(defun jss-http-repl-process-filter (proc output)
  (unless (memq jss-http-repl-status '(:receiving-headers :receiving-data))
    (error "Process filter got unexpected data: %s" output))
  (with-current-buffer (process-buffer proc)
    (save-match-data
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (start (point)))
        (jss-wrap-with-text-properties (list 'read-only t)
          (insert output))
        (when (eql :receiving-headers jss-http-repl-status)
          (goto-char start)
          (beginning-of-line)
          (block nil
            (while (not (eobp))
              (when (looking-at "Connection:\\s-*\\(.*\\)\\s-*$")
                (setf jss-http-repl-keep-alive (string= "keep-alive" (downcase (match-string-no-properties 1)))))
              (when (looking-at "Content-Length:\\s-*\\([0-9]+\\)\\s-*$")
                (setf jss-http-repl-content-length (string-to-number (match-string-no-properties 1))))
              (when (and jss-http-repl-track-cookies
                         (looking-at "Set-Cookie:\\s-*\\(.*\\)$"))
                (push (match-string-no-properties 1) jss-http-repl-set-cookies))
              (when (looking-at (jss-chars-to-string #x0d #x0a))
                (delete-region (1- (point)) (+ 2 (point)))
                (jss-wrap-with-text-properties (list 'read-only t
                                                     'face 'jss-http-repl-meta-data-face)
                  (insert "\n--response data follows this line--\n"))
                (setf jss-http-repl-status :receiving-data
                      jss-http-repl-response-data-start (point))
                (forward-line 1)
                (return))
              (forward-line 1))))
        (when (eql :receiving-data jss-http-repl-status)
          (when (and jss-http-repl-response-data-start
                     jss-http-repl-content-length
                     (= jss-http-repl-content-length (- (point) jss-http-repl-response-data-start)))
            (setf jss-http-repl-status :idle)))
        (when (eql :idle jss-http-repl-status)
          (jss-http-repl-insert-next-request))))))

(provide 'jss-http-repl)
