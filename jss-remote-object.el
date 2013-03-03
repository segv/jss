
(defun jss-insert-remote-object (text identifier)
  (save-excursion
    (let ((start (point))
          (inhibit-read-only t))
      (jss-insert-button text 'jss-console-expand-js-object
                         :other-properties (list 'jss-js-object-id identifier))
      (insert "\n"))))

(defun jss-expand-remote-object ()
  (interactive)
  (lexical-let* ((start (point))
                 (id (get-text-property start 'jss-js-object-id)))
    (if id
        (jss-deferred-add-callback
         (jss-get-object-properties console id)
         (lambda (properties)
           (with-current-buffer (jss-console-buffer console)
             (let ((location (jss-find-property-block 'jss-js-object-id id))
                   (inhibit-read-only t))
               (goto-char (car location))
               (delete-region (car location) (cdr location))
               (dolist (p properties)
                 (insert (format "%s" p) "\n")))))))))


(provide 'jss-remote-object)
