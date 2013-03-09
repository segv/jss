(require 'jss-browser-api)

(make-variable-buffer-local
 (defvar jss-current-io-object))

(defun jss-current-io () jss-current-io-object)

(define-derived-mode jss-io-mode jss-super-mode "JSS IO"
  ""
  (setf jss-current-io-object jss-io
        (jss-io-buffer jss-io) (current-buffer))

  (insert "Request: ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-method jss-io))
  (insert " ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-url jss-io))
  (insert "\n")

  (insert "Response: ")
  (if (jss-io-response-status jss-io)
      (insert (jss-io-response-status jss-io))
    (insert "None received."))
  (insert "\n")

  (jss-toggling-visibility
   (lambda ()
     (insert "Request Headers: ")
     (jss-insert-button "[view raw]" 'jss-toggle-request-headers-raw)
     (insert "\n"))
   (lambda ()
     (jss-wrap-with-text-properties `(jss-request-headers t)
       (jss-io-insert-header-table (jss-io-request-headers jss-io) :indent 2))))
    
  (insert "Request Data: ") (jss-insert-button "[view raw]" 'jss-toggle-request-data-raw) (insert "\n")
  
  (when (jss-io-response-status jss-io)
    (jss-toggling-visibility
     (lambda ()
       (insert "Response Headers: ") (jss-insert-button "[view raw]" 'jss-toggle-response-headers-raw) (insert "\n"))
     (lambda ()
       (jss-wrap-with-text-properties `(jss-response-headers t)
         (jss-io-insert-header-table (jss-io-response-headers jss-io) :indent 2))))

    (jss-toggling-visibility
     (lambda ()
       (insert "Response Data: ")
       (jss-insert-button "[view raw] " 'jss-toggle-response-data-raw)
       (when (jss-io-response-content-type jss-io)
         (insert "type: " (jss-io-response-content-type jss-io)))
       (when (jss-io-response-content-length jss-io)
         (insert "length: " (jss-io-response-content-length jss-io)))
       (insert "\n"))
     (lambda ()
       (let ((data (jss-io-response-data jss-io)))
         (if data
             (jss-io-insert-response-data jss-io)
           (insert "no data."))))))

  (read-only-mode 1)
  (goto-char (point-min)))

(define-key jss-io-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(defun* jss-io-insert-header-table (header-alist &key indent)
  (when header-alist
    (let* ((headers (mapcar (lambda (h)
                              (if (stringp (car h))
                                  (cons (car h) (cdr h))
                                (cons (format "%s" (car h)) (cdr h))))
                            header-alist))
           (longest-key (loop
                         for header in headers
                         for max = (max (or max 0) (length (car header)))
                         finally (return (+ 4 max)))))
      (dolist (header headers)
        (let ((start (point)))
          (when indent
            (insert (make-string indent ?\s)))
          (jss-insert-with-highlighted-whitespace (car header))
          (insert ": ")
          (insert (make-string (- longest-key (- (point) start)) ?\s))
          (insert (cdr header) "\n"))))))

(defun jss-console-switch-to-io-inspector (io)
  (interactive (list (jss-tab-get-io (jss-current-tab) (get-text-property (point) 'jss-io-id))))
  (unless io
    (error "io is nil. not good."))

  (if (and (jss-io-buffer io)
           (buffer-live-p (jss-io-buffer io)))
      (display-buffer (jss-io-buffer io))
    (setf (jss-io-buffer io)
          (get-buffer-create (generate-new-buffer-name (format "*JSS IO %s*" (jss-io-id io)))))
    (with-current-buffer (jss-io-buffer io)
      (let ((jss-io io))
        (jss-io-mode)))
    (display-buffer (jss-io-buffer io))))

(defmethod jss-io-insert-response-data ((io jss-generic-io))
  (let* ((content-type (jss-io-response-content-type io))
         (cleaner (gethash content-type jss-io-cleaners)))
    (if cleaner
        (let ((string (ignore-errors (funcall cleaner (jss-io-response-data io)))))
          (if string
              (insert string)
            (insert "Error pretty printing:\n")
            (insert (jss-io-response-data io))))
      (insert "Unrecognized content type: " (or  content-type "---") "\n")
      (insert (jss-io-response-data io)))))

(provide 'jss-io)
