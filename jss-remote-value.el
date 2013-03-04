
(defmethod jss-insert-remote-value ((value jss-generic-remote-primitive))
  (insert (jss-limit-string-length (jss-remote-value-string value) 60)))

(defmethod jss-insert-remote-value ((value jss-generic-remote-non-primitive))
  (let ((start (point)))
    (jss-wrap-with-text-properties (list 'jss-remote-value value)
      (insert (jss-limit-string-length (jss-remote-value-string value) 60)))
    (jss-add-text-button start (point) 'jss-remote-value-expand-at-point)))

(defun jss-remote-value-expand-at-point ()
  (interactive)
  (let ((value (get-text-property (point) 'jss-remote-value)))
    (unless value
      (error "No value at point."))
    (jss-remote-value-expond value)))

(defmethod jss-remote-value-expond ((value jss-generic-remote-object))
  (let ((location (jss-find-property-block 'jss-remote-value value :test 'eq))
        (inhibit-read-only t))
    (save-excursion
      (delete-region (car location) (cdr location))
      (jss-wrap-with-text-properties (list 'jss-remote-value value)
        (insert "{expanding " (jss-remote-value-string value) "}")
        (lexical-let ((buffer (current-buffer))
                      (value value))
          (jss-deferred-add-callback
           (jss-remote-object-get-properties value (jss-current-tab))
           (lambda (properties)
             (with-current-buffer buffer
               (save-excursion
                 (let ((location (jss-find-property-block 'jss-remote-value value :test 'eq))
                       (inhibit-read-only t))
                   (delete-region (car location) (cdr location))
                   (insert (jss-remote-value-string value) "{")
                   (let ((left-column (current-column)))
                     (loop
                      for first = t then nil
                      for prop in properties
                      unless first do (insert ",\n")
                      unless first do (move-to-column left-column t)
                      do (insert (car prop) ":")
                      do (jss-insert-remote-value (cdr prop))))
                   (insert "}")))))))))))

(provide 'jss-remote-value)
