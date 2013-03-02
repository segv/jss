(require 'jss-browser-api)

(define-derived-mode jss-io-mode jss-super-mode "JSS IO"
  ""
  (add-hook 'kill-buffer-hook 'jss-io-kill-buffer nil t)

  t)

(make-variable-buffer-local
 (defvar jss-current-io-object))

(defun jss-current-io () jss-current-io-object)

(define-key jss-io-mode-map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))

(defun jss-io-kill-buffer ()
  (interactive)
  (jss-tab-unregister-io (jss-io-tab (jss-current-io)) (jss-current-io)))

(defun jss-io-insert-header-table (header-alist)
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
          (jss-insert-with-highlighted-whitespace (car header))
          (insert ": ")
          (insert (make-string (- longest-key (- (point) start)) ? ))
          (insert (cdr header) "\n"))))))

(defun jss-console-switch-to-io-inspector (io)
  (interactive (list (jss-tab-get-io (jss-current-tab) (get-text-property (point) 'jss-io-id))))
  (unless io (error "io is nil. not good."))
  (if (jss-io-buffer io)
      (display-buffer (jss-io-buffer io))
    
    (with-current-buffer (get-buffer-create (jss-io-buffer-name io))
      (jss-io-mode)

      (setf jss-current-io-object io
            (jss-io-buffer io) (current-buffer))
      
      (insert "Request:\n")
      (insert (jss-io-request-method io) " " (jss-io-request-url io))
      (jss-eol-mark)
      (jss-section-marker)
      (insert "Request Headers:") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
      (jss-io-insert-header-table (jss-io-request-headers io))
      (jss-section-marker)
      (insert "Request Data:\n")
      (jss-section-marker)
      (when (jss-io-response-status io)
        (insert "Response:\n")
        (insert (jss-io-response-status io) "\n")
        (jss-section-marker)
        (insert "Response Headers: ") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
        (jss-io-insert-header-table (jss-io-response-headers io))
        (jss-section-marker)
        
        (insert "Response Data: ")
        (when (cdr (assoc 'Content-Type (jss-io-response-headers io)))
          (insert "type: " (cdr (assoc 'Content-Type (jss-io-response-headers io)))))
        (when (cdr (assoc 'Content-Length (jss-io-response-headers io)))
          (insert "length: " (cdr (assoc 'Content-Length (jss-io-response-headers io)))))
        (let ((data (jss-io-response-data io)))
          (when data
            (insert (format "; char length: %d\n" (length data)))
            (insert (jss-io-response-data io))))
        (insert "\n")
        (jss-section-marker))

      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer (jss-io-buffer io))))

(provide 'jss-io)
