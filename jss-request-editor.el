(define-derived-mode jss-request-editor-mode jss-super-mode "JSS Request Editor"
  "Major mode for manually creating, editing and submitting HTTP requests.

A jss-request-editor buffer can be quickly created from a JSS IO
 buffer; this allows for easily editing/debugging/replaying
 recent json requests from the browser.

This mode is designed for testing/debugging json/ajax requests
but it can be used with any kind of HTTP request."
  (setf buffer-file-coding-system 'utf8-unix
        jss-request-editor-status :closed)
  ;; we do same string->byte conversions and assume that the buffer is
  ;; a multibyte buffer, this is already emacs' default, but let's
  ;; just be explicit about it.
  (set-buffer-multibyte t)
  t)

(easy-menu-define jss-request-editor-menu jss-request-editor-mode-map
  "Menu for JSS Request Editor buffers."
  '("JSS Request Editor"
    [ "Submit" jss-request-editor-send-request t]
    [ "Ensure Header" jss-request-editor-ensure-header t ]))

(define-key jss-request-editor-mode-map (kbd "C-<return>") 'jss-request-editor-ensure-header)
(define-key jss-request-editor-mode-map (kbd "C-c C-c") 'jss-request-editor-send-request)

(make-variable-buffer-local (defvar jss-request-editor-previous-header-string nil))

(make-variable-buffer-local (defvar jss-request-editor-previous-data-string nil))

(make-variable-buffer-local (defvar jss-request-editor-status nil
                              "The current status of the server connection (:opening, :sending, :receiving-headers, :receiving-data, :idle or :closed)"))

(make-variable-buffer-local (defvar jss-request-editor-keep-alive nil
                              "When T the current server connection should be reused."))
(make-variable-buffer-local (defvar jss-request-editor-content-length nil
                              "The number of bytes of body data we're expecting."))

(defface jss-request-editor-meta-data-face
  '((t :inherit font-lock-special-keyword-face))
  "Face used to mark 'meta' headers such as Endpoint:, --post
  data starts here-- etc.")

(defface jss-request-editor-submitted-face
  '((t :slant italic))
  "Face used to mark data that has been sent and is no longer editable.")

(defun* jss-request-editor-new (&rest insert-args)
  (interactive)
  (with-current-buffer (generate-new-buffer "*JSS Request Editor*")
    (jss-request-editor-mode)
    (apply 'jss-request-editor-insert-request insert-args)
    (jss-request-editor-goto-data-start)
    (current-buffer)))

(defun* jss-request-editor-insert-request (&key headers data host port ssl)
  (unless (memq jss-request-editor-status (list :idle :closed))
    (error "Request in progress, can't insert new reuest."))

  (setf jss-request-editor-status :closed)

  (jss-wrap-with-text-properties (list 'jss-request-editor-endpoint t
                                       'face 'jss-request-editor-meta-data-face
                                       'rear-nonsticky t)
    (jss-wrap-with-text-properties (list 'read-only t)
      (insert "Endpoint:"))
    (let ((inhibit-read-only t))
      (insert " \n")))
  (let (query-point)
    (let ((inhibit-read-only t))
      (insert "GET /")
      (setf query-point (point))
      (insert " HTTP/1.0\n"))
    (insert "Host:")
    (insert "\n")
    (jss-wrap-with-text-properties (list 'read-only t 'face 'jss-request-editor-meta-data-face 'jss-header-end-marker t)
      (insert "--request data follows this line--"))
    (jss-wrap-with-text-properties (list 'jss-header-end-marker t 'rear-nonsticky t)
      (let ((inhibit-read-only t))
        (insert "\n")))

    (jss-request-editor-set-endpoint :host host :port port :ssl ssl)
    (jss-request-editor-set-headers headers)
    (jss-request-editor-set-request-data data)
    
    (goto-char query-point)))

(defvar jss-request-editor-request-line-regexp
  "^\\(GET\\|POST\\|HEAD\\|PUT\\|DELETE\\|TRACE\\|CONNECT\\)\\s-+\\(.*\\)\\s-+\\(HTTP/1\\.[10]\\)\\s-*$")

(defun jss-request-editor-set-method (status)
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
        (jss-request-editor-goto-header-start)
        (while (not (looking-at jss-request-editor-request-line-regexp))
          (forward-line 1)
          (when (eobp)
            (error "Unable to find Request-Line")))
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 1))
        (insert status)))))

(defun jss-request-editor-goto-header-start ()
  (let ((endpoint (jss-find-property-block 'jss-request-editor-endpoint t)))
    (goto-char (car endpoint))
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun jss-request-editor-goto-header-end ()
  (let ((endpoint (jss-find-property-block 'jss-request-editor-endpoint t)))
    (goto-char (car endpoint))
    (while (not (get-text-property (point) 'jss-header-end-marker))
      (when (eobp)
        (error "Looking for header end but got to end of buffer."))
      (forward-char 1))
    (point)))

(defun jss-request-editor-goto-data-start ()
  (jss-request-editor-goto-header-end)
  (block nil
    (while (get-text-property (point) 'jss-header-end-marker)
      (when (eobp)
        (return))
      (forward-char 1)))
  (point))

(defun jss-request-editor-ensure-header (header-name)
  "If there already is a header named `header-name` then simple
moves to it, otherwise inserts it.

When called interactively will prompt, with completion, for the
name of the header to add."
  (interactive (list (jss-completing-read "Header: "
                                          (sort (mapcar (lambda (header)
                                                          (if (consp header) (car header) header))
                                                        jss-request-editor-request-header-editors)
                                                'string<)
                                          :require-match nil)))
  (if (jss-request-editor-goto-header header-name)
      t
    (insert header-name ": \n")
    (forward-line -1)
    (jss-request-editor-gopast-header header-name)))

(defun jss-request-editor-goto-header (header-name)
  "If there is a header named `header-name` in the current
request then moves point to after its ?: char and returns t,
otherwise leaves point at the last line of the headers (which a
simple insert is enough to insert a new header) and returns nil"
  (save-match-data
    (jss-request-editor-goto-header-start)
    (block nil
      (while (not (jss-request-editor-in-header-line header-name))
        (forward-line 1)
        (when (looking-at "--request data follows this line--")
          (return nil))
        (when (eobp)
          (error "Buffer ended before be could find the header. Is the --request data-- line missing?")
          (return)))
      ;; if we get here the while terminated we're 
      (beginning-of-line)
      (jss-request-editor-gopast-header header-name))))

(defun jss-request-editor-in-header-line (name)
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^" (regexp-quote name) ":"))))

(defun jss-request-editor-looking-at-header-p (header-name)
  (looking-at (concat "^" (regexp-quote name) ":\\s-*")))

(defun jss-request-editor-gopast-header (name)
  (beginning-of-line)
  (save-match-data
    (if (jss-request-editor-looking-at-header-p name)
        (goto-char (1- (match-end 0)))
      (error "Not looking at the header %s." name))))

(defvar jss-request-editor-request-header-editors
  '("Accept" "Accept-Charset" "Accept-Encoding" "Accept-Language" "Accept-Datetime"
    "Authorization"
    ("Cache-Control" . jss-request-editor-choose-cache-control)
    "Connection"
    "Cookie"
    ("Content-Length" . jss-request-editor-set-content-length) "Content-MD5" "Content-Type"
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
    ("User-Agent" . jss-request-editor-choose-user-agent)
    "Via"
    "Warning"
    "X-Requested-With"))

(defun jss-request-editor-request-header-editor (header-name)
  (dolist (h jss-request-editor-request-header-editors)
    (cond
     ((and (consp h)
           (string= header-name (car h)))
      (return (cdr h)))
     ((and (stringp h)
           (string= h header-name))
      (return t)))))

(defvar jss-request-editor-user-agents
  '(("IE 6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
    ("IE 7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)")
    ("IE 8" . "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1)")
    ("Googlebot" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
    ("Google Chrome 24/Mac OS X" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1309.0 Safari/537.17")
    ("Firefox" "Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1")
    ("Safari" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2")))

(defun jss-request-editor-choose-user-agent ()
  (let* ((user-agent-name (jss-completing-read "Browser: " (mapcar 'car jss-request-editor-user-agents)
                                               :require-match nil))
         (known-value (cl-assoc user-agent-name jss-request-editor-user-agents :test 'string=)))
    (if known-value
        (cdr known-value)
      ;; if the user typed in their own user agent string then we
      ;; won't find it in jss-request-editor-user-agents and we just
      ;; use whatever they sent.
      user-agent-name)))

(defun jss-request-editor-choose-cache-control ()
  (interactive)
  "no-cache")

(defun jss-request-editor-set-content-length ()
  (interactive)
  (save-excursion
    (format "%d" (string-bytes
                  (buffer-substring-no-properties (jss-request-editor-goto-data-start)
                                                  (point-max))))))

(defun jss-request-editor-delete-headers ()
  (jss-request-editor-goto-header-start)
  (delete-region (point) (jss-request-editor-goto-header-end)))

(defun jss-request-editor-looking-at-newline ()
  (and (char-after (point))
       (char-equal ?\r (char-after (point)))
       (char-after (1+ (point)))
       (char-equal ?\n (char-after (1+ (point))))))

(defun jss-chars-to-string (&rest chars)
  (apply 'concat (mapcar 'char-to-string chars)))

(defun jss-request-editor-set-headers (header-string)
  (delete-region (jss-request-editor-goto-header-start)
                 (jss-request-editor-goto-header-end))
  ;; the above delete-region leaves point at the beginning of the header block
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a #x0d #x0a ?\\ ?')
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a)
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (insert header-string)
  (jss-request-editor-goto-header-start))

(defun jss-request-editor-request-data ()
  (save-excursion
    (jss-request-editor-goto-data-start)
    (buffer-substring-no-properties (point) (point-max))))

(defun jss-request-editor-set-request-data (data-string)
  (jss-request-editor-goto-data-start)
  (delete-region (point) (point-max))
  (when data-string
    (insert data-string)))

(defun* jss-request-editor-set-endpoint (&key host port ssl)
  (let ((endpoint-location (jss-find-property-block 'jss-request-editor-endpoint t)))
    (let ((inhibit-read-only t))
      (goto-char (car endpoint-location))
      (delete-region (car endpoint-location) (cdr endpoint-location))
      (jss-wrap-with-text-properties (list 'rear-nonsticky t
                                           'jss-request-editor-endpoint t)
        (jss-wrap-with-text-properties (list 'read-only t
                                             'face 'jss-request-editor-meta-data-face)
          (insert "Endpoint: "))
        (insert host ":" port)
        (when ssl
          (insert " SSL"))
        (insert "\n")))))

(defun* jss-request-editor-get-endpoint ()
  (let ((endpoint-location (jss-find-property-block 'jss-request-editor-endpoint t)))
    (goto-char (car endpoint-location))
    (save-match-data
      (if (looking-at "Endpoint:\\s-*\\(.*?\\)\\(:\\([0-9]+\\)\\)?\\s-*\\(SSL\\)?\\s-*$")
          (list :host (match-string-no-properties 1)
                :port (match-string-no-properties 3)
                :ssl (if (match-string-no-properties 4)
                         t
                       nil))
        '()))))

(defun jss-request-editor-preflight ()
  (unless (memq jss-request-editor-status '(:idle :closed))
    (error "Can't prepared a new request unless we're in editing mode. current mode is %s." jss-request-editor-status))
  (let* ((headers (buffer-substring-no-properties (jss-request-editor-goto-header-start)
                                                  (jss-request-editor-goto-header-end)))
         (data (buffer-substring-no-properties (jss-request-editor-goto-data-start)
                                               (point-max)))
         (endpoint (jss-request-editor-get-endpoint))
         (request-start (car (jss-find-property-block 'jss-request-editor-endpoint t)))
         (submitted-overlay (make-overlay request-start (point-max))))
    (unless endpoint
      (error "Missing endpoint data, don't where to send request."))
    (overlay-put submitted-overlay 'face 'jss-request-editor-submitted-face)
    (let ((inhibit-read-only t))
      (remove-text-properties request-start (point-max)
                              (list 'jss-request-editor-endpoint t
                                    'jss-header-end-marker t
                                    'rear-nonsticky t))
      (add-text-properties request-start (point-max) (list 'read-only t)))
    (list* :header-string headers
           :data-string data
           endpoint)))

(defun jss-request-editor-send-request ()
  (interactive)
  (destructuring-bind (&key header-string data-string host port ssl)
      (jss-request-editor-preflight)
    (jss-request-submit host
                        (if port
                            (string-to-number port)
                          nil)
                        ssl
                        header-string
                        data-string)))

(defun jss-request-submit (host port ssl headers data)
  (setf jss-request-editor-previous-header-string headers
        jss-request-editor-previous-data-string data
        jss-request-editor-previous-host host
        jss-request-editor-previous-port port
        jss-request-editor-previous-ssl ssl
        jss-request-editor-status :opening)
  (message "Sending request to %s:%s." host port)
  (make-network-process :name "jss-request-editor-request"
                        :server nil
                        :host host
                        :service port
                        :nowait t
                        :coding '(binary . binary)
                        :filter 'jss-request-editor-process-filter
                        :sentinel 'jss-request-editor-process-sentinel
                        :buffer (current-buffer)))

(defun jss-request-editor-process-sentinel (proc status)
  (with-current-buffer (process-buffer proc)
    (cond
     ((string= "open\n" status)
      (jss-request-editor-process-send-data)
      (setf jss-request-editor-status :receiving-headers))
     ((string= "connection broken by remote peer\n" status)
      (setf jss-request-editor-status :closed)))))

(defun jss-request-editor-process-send-data ()
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (jss-wrap-with-text-properties (list'face 'jss-request-editor-meta-data-face
                                              'read-only t)
      (insert "\n--response headers follow this line--\n")))
  (setf jss-request-editor-status :sending)
  (process-send-string proc (encode-coding-string jss-request-editor-previous-header-string 'us-ascii-dos))
  (when jss-request-editor-previous-data-string
    (process-send-string proc (jss-chars-to-string #x0d #x0a))
    (process-send-string proc (encode-coding-string jss-request-editor-previous-data-string 'utf-8-unix)))
  (setf jss-request-editor-status :receiving-headers))

(defun jss-request-editor-process-filter (proc output)
  (unless (memq jss-request-editor-status '(:receiving-headers :receiving-data))
    (error "Process filter got unexpected data: %s" output))
  (with-current-buffer (process-buffer proc)
    (save-match-data
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (start (point)))
        (jss-wrap-with-text-properties (list 'read-only t)
          (insert output))
        (when (eql :receiving-headers jss-request-editor-status)
          (goto-char start)
          (beginning-of-line)
          (while (not (eobp))
            (when (looking-at "Connection:\\s-*\\(.*\\)\\s-*$")
              (setf jss-request-editor-keep-alive (string= "keep-alive" (downcase (match-string-no-properties 1)))))
            (when (looking-at "Content-Length:\\s-*\\([0-9]+\\)\\s-*$")
              (setf jss-request-editor-content-length (string-to-number (match-string-no-properties 1))))
            (when (looking-at (jss-chars-to-string #x0d #x0a))
              (delete-region (1- (point)) (+ 2 (point)))
              (jss-wrap-with-text-properties (list 'read-only t
                                                   'face 'jss-request-editor-meta-data-face)
                (insert "\n--response data follows this line--\n"))
              (setf jss-request-editor-status :receiving-data
                    jss-request-editor-response-data-start (point)))
            (forward-line 1)))
        (when (eql :receiving-data jss-request-editor-status)
          (when (and jss-request-editor-response-data-start
                     jss-request-editor-content-length
                     (= jss-request-editor-content-length (- (point) jss-request-editor-response-data-start)))
            (setf jss-request-editor-status :idle)))
        (when (eql :idle jss-request-editor-status)
          (insert "\n")
          (jss-request-editor-insert-request :headers jss-request-editor-previous-header-string
                                             :data jss-request-editor-previous-data-string
                                             :host jss-request-editor-previous-host
                                             :port (format "%d" jss-request-editor-previous-port)
                                             :ssl jss-request-editor-previous-ssl))))))

(provide 'jss-request-editor)
