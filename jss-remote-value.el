
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
    (jss-remote-value-expand value)))

(defmacro* replace-property-block ((property-name property-value &key (test 'eq)) &body body)
  (declare (indent 1))
  (let ((loc (gensym)))
    `(let ((,loc (jss-find-property-block ',property-name ,property-value :test ,test))
           (inhibit-read-only t))
       (save-excursion
         (goto-char (car ,loc))
         (delete-region (car ,loc) (cdr ,loc))
         (jss-wrap-with-text-properties (list ',property-name ,property-value)
           ,@body)))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-object))
  (replace-property-block (jss-remote-value value :test 'eq)
                          (insert "{expanding " (jss-remote-value-string value) "}")
                          (lexical-let ((buffer (current-buffer))
                                        (value value))
                            (jss-deferred-add-callback
                             (jss-remote-object-get-properties value (jss-current-tab))
                             (lambda (properties)
                               (with-current-buffer buffer
                                 (replace-property-block (jss-remote-value value :test 'eq)
                                                         (insert (jss-remote-value-string value) "{")
                                                         (let ((left-column (current-column)))
                                                           (loop
                                                            for first = t then nil
                                                            for prop in properties
                                                            unless first do (insert ",\n")
                                                            unless first do (move-to-column left-column t)
                                                            do (insert (car prop) ":")
                                                            do (jss-insert-remote-value (cdr prop))))
                                                         (insert "}"))))))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-function))
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-add-callback
     (jss-remote-function-get-source-location value)
     'jss-script-display-at-position)))

(defmethod jss-remote-value-expand ((value jss-generic-remote-array))
  (lexical-let ((tab (jss-current-tab))
                (value value)
                (buffer (current-buffer)))
    (replace-property-block (jss-remote-value value :test 'eq)
      (insert "[expanding]")
      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (let ((integer-properties '())
               (other-properties '()))
           (dolist (prop properties)
             (if (string-to-number (car prop))
                 (push (cons (string-to-number (car prop)) (cdr prop)) integer-properties)
               (push prop other-properties)))
           (replace-property-block (jss-remote-value value :test 'eq)
                                   (with-current-buffer buffer
                                     (loop
                                      initially (insert "[")
                                      for item in (cl-sort integer-properties '< :key 'car)
                                      for first = t then nil
                                      unless first do (insert ", ")
                                      do (jss-insert-remote-value (cdr item))
                                      finally (insert "]"))))))))))

(provide 'jss-remote-value)
