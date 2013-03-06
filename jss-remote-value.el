
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

(defmacro* jss-replace-with-default-property ((property-name property-value &key (test 'eq)) &body body)
  (declare (indent 1))
  (let ((loc (gensym)) (prop-val (gensym)))
    `(let* ((,prop-val ,property-value)
            (,loc (jss-find-property-block ',property-name ,prop-val :test ,test))
            (inhibit-read-only t))
       (save-excursion
         (goto-char (car ,loc))
         (delete-region (car ,loc) (cdr ,loc))
         (let ((start (point)))
           (prog1
               (progn ,@body)
             (jss-add-text-property-unless-exists (car ,loc) (point)
                                                  ',property-name
                                                  ,prop-val)))))))

(put 'jss-replace-with-default-property 'lisp-indent-function 1)

(defun jss-add-text-property-unless-exists (start end property-name property-value)  
  (save-excursion
    (block nil
      (goto-char start)
      (while (< (point) end)
        (setf start (point))
        (while (not (get-text-property (point) property-name))
          (forward-char 1)
          (when (= (point) end)
            (add-text-properties start (point) (list property-name property-value))
            (return)))
        (add-text-properties start (point) (list property-name property-value))
        (when (get-text-property (point) property-name)
          (forward-char 1)
          (when (= (point) end)
            (return)))))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-object))
  (jss-replace-with-default-property
      (jss-remote-value value :test 'eq)
    (insert "{expanding " (jss-remote-value-string value) "}")
    (lexical-let ((buffer (current-buffer))
                  (value value))
      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (with-current-buffer buffer
           (jss-replace-with-default-property
               (jss-remote-value value :test 'eq)
             (let ((left-column (+ 2 (current-column))))
               (insert (jss-remote-value-string value) " {\n")
               (loop for first = t then nil
                     for (prop . more) on properties
                     do (indent-to-column left-column)
                     do (insert (car prop) ":")
                     do (jss-insert-remote-value (cdr prop))
                     when more
                     do (insert ",\n"))
               (insert "}")))))))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-function))
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-add-callback
     (jss-remote-function-get-source-location value)
     'jss-script-display-at-position)))

(defmethod jss-remote-value-expand ((value jss-generic-remote-array))
  (lexical-let ((tab (jss-current-tab))
                (value value)
                (buffer (current-buffer)))
    (jss-replace-with-default-property (jss-remote-value value :test 'eq)
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
           (jss-replace-with-default-property (jss-remote-value value :test 'eq)
             (with-current-buffer buffer
               (loop
                initially (insert "[")
                for item in (cl-sort integer-properties '< :key 'car)
                for first = t then nil
                unless first do (insert ", ")
                do (jss-insert-remote-value (cdr item))
                finally (insert "]"))))))))))

(provide 'jss-remote-value)
