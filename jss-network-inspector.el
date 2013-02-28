(require 'jss-browser-api)

(define-derived-mode jss-network-inspector-mode jss-super-mode "JSS IO"
  ""
  t)

(defun jss-comment-char (string)
  (insert (propertize string
                      'face font-lock-comment-face
                      'font-lock-face font-lock-comment-face)))

(defun jss-eol-mark ()
  (when (member (preceding-char) (list ?  ?\n ?\t ?\r))
    (jss-comment-char "$"))
  (insert "\n"))

(defun jss-insert-with-highlighted-whitespace (string)
  (save-match-data
    (when (string-match "^[ \t\r\n\f]" string)
      (jss-comment-char "^"))
    (insert string)
    (when (string-match "[ \t\r\n\f]$" string)
      (jss-comment-char "$"))))

(defun jss-section-marker ()
  (insert "--------------------------------\n"))

(defun jss-network-inspector-insert-header-table (header-alist)
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
      (jss-network-inspector-mode)
      (setf jss-current-io-object io
            (jss-io-buffer io) (current-buffer))
      
      (insert "Request:\n")
      (insert (jss-io-request-method io) " " (jss-io-request-url io))
      (jss-eol-mark)
      (jss-section-marker)
      (insert "Request Headers:") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
      (jss-network-inspector-insert-header-table (jss-io-request-headers io))
      (jss-section-marker)
      (insert "Request Data: ") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
      (jss-section-marker)
      (when (jss-io-response-status io)
        (insert "Response:\n")
        (insert (jss-io-response-status io) "\n")
        (jss-section-marker)
        (insert "Response Headers: ") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
        
        (jss-network-inspector-insert-header-table (jss-io-response-headers io))
        (jss-section-marker)
        (insert "Response Data: ") (insert-text-button "[view raw]" 'action 'identity) (insert "\n")
        (jss-section-marker))

      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer (jss-io-buffer io))))

(provide 'jss-network-inspector)
