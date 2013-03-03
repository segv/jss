(require 'jss-browser-api)

(make-variable-buffer-local
 (defvar jss-current-io-object))

(defun jss-current-io () jss-current-io-object)

(define-derived-mode jss-io-mode jss-super-mode "JSS IO"
  ""
  (add-hook 'kill-buffer-hook 'jss-io-kill-buffer nil t)
  (setf jss-current-io-object jss-io
        (jss-io-buffer jss-io) (current-buffer))
      
  (insert "Request:\n")
  (insert "  ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-method jss-io))
  (insert " ")
  (jss-insert-with-highlighted-whitespace (jss-io-request-url jss-io))
  (jss-section-marker)
  
  (insert "Request Headers:") (jss-insert-button "[view raw]" 'jss-toggle-request-headers-raw)
  (insert "\n")
  (jss-wrap-with-text-properties `(jss-request-headers t)
    (jss-io-insert-header-table (jss-io-request-headers jss-io) :indent 2))
  (jss-section-marker)
  
  (insert "Request Data:") (jss-insert-button "[view raw]" 'jss-toggle-request-data-raw) (insert "\n")
  (jss-section-marker)
  
  (when (jss-io-response-status jss-io)
    (insert "Response:\n")
    (insert (jss-io-response-status jss-io) "\n")
    (jss-section-marker)
    (insert "Response Headers: ") (jss-insert-button "[view raw]" 'jss-toggle-response-headers-raw) (insert "\n")
    (jss-wrap-with-text-properties `(jss-response-headers t)
      (jss-io-insert-header-table (jss-io-response-headers jss-io) :indent 2))
    (jss-section-marker)
        
    (insert "Response Data: ")
    (when (cdr (assoc 'Content-Type (jss-io-response-headers jss-io)))
      (insert "type: " (jss-io-response-content-type jss-io)))
    (when (cdr (assoc 'Content-Length (jss-io-response-headers jss-io)))
      (insert "length: " (jss-io-response-content-length jss-io)))
    (jss-insert-button "[view raw]" 'jss-toggle-response-data-raw) (insert "\n")
    (let ((data (jss-io-response-data jss-io)))
      (when data
        (insert (format "; char length: %d\n" (length data)))
        (insert (jss-io-response-data jss-io))))
    (insert "\n")
    (jss-section-marker))

  (read-only-mode 1)
  (goto-char (point-min))
  t)

(define-key jss-io-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(defun jss-io-kill-buffer ()
  (interactive)
  (jss-tab-unregister-io (jss-io-tab (jss-current-io)) (jss-current-io)))

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
            (insert make-string indent ?\s))
          (jss-insert-with-highlighted-whitespace (car header))
          (insert ": ")
          (insert (make-string (- longest-key (- (point) start)) ?\s))
          (insert (cdr header) "\n"))))))

(defun jss-console-switch-to-io-inspector (io)
  (interactive (list (jss-tab-get-io (jss-current-tab) (get-text-property (point) 'jss-io-id))))
  (unless io (error "io is nil. not good."))
  (if (jss-io-buffer io)
      (display-buffer (jss-io-buffer io))
    
    (with-current-buffer (get-buffer-create (jss-io-buffer-name io))
      (let ((jss-io io))
        (jss-io-mode)))
    (display-buffer (jss-io-buffer io))))

(provide 'jss-io)
