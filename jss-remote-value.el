
(defmethod jss-insert-remote-value ((value jss-generic-remote-primitive))
  (jss-remote-value-insert-description value))

(defmethod jss-insert-remote-value ((value jss-generic-remote-non-primitive))
  (let ((start (point)))
    (jss-wrap-with-text-properties (list 'jss-remote-value value 'jss-remote-value-collapsed t)
      (jss-remote-value-insert-description value))
    (jss-add-text-button start (point) 'jss-remote-value-expand-at-point)
    (jss-autoexpand-small-remote-object value jss-remote-value-auto-expand-property-limit)))

(defvar jss-remote-value-auto-expand-property-limit 6)

(defvar jss-remote-value-expand/pre-computed-properties nil)

(defun jss-autoexpand-small-remote-object (object max-property-length)
  (lexical-let ((object object)
                (max-property-length max-property-length))
    (jss-deferred-add-callback
     (jss-remote-object-get-properties object (jss-current-tab))
     (lambda (properties)
       (when (<= (length properties) max-property-length)
         (let ((jss-remote-value-expand/pre-computed-properties properties))
           (jss-remote-value-expand object)))))))

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

(defmethod jss-remote-value-collapsed ((value jss-generic-remote-object))
  (let ((loc (jss-find-property-block 'jss-remote-value value)))
    (save-excursion
      (goto-char (car loc))
      (get-text-property (point) 'jss-remote-value-collapsed))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-object))
  (when (jss-remote-value-collapsed value)

    (jss-replace-with-default-property
        (jss-remote-value value :test 'eq)
      (insert "{expanding ")
      (jss-remote-value-insert-description value)
      (insert "}"))
    
    (lexical-let* ((buffer (current-buffer))
                   (value value)
                   (expander (lambda (properties)
                               (with-current-buffer buffer
                                 (jss-replace-with-default-property
                                     (jss-remote-value value :test 'eq)
                                   (let ((left-column (+ 2 (current-column))))
                                     (jss-remote-value-insert-description value)
                                     (insert " {\n")
                                     (loop for first = t then nil
                                           for (prop . more) on properties
                                           do (indent-to-column left-column)
                                           do (jss-insert-with-highlighted-whitespace (car prop))
                                           do (insert ": ")
                                           do (jss-insert-remote-value (cdr prop))
                                           when more
                                           do (insert ",\n"))
                                     (insert "}")))))))

      (if jss-remote-value-expand/pre-computed-properties
          (funcall expander jss-remote-value-expand/pre-computed-properties)
        (jss-deferred-add-callback
         (jss-remote-object-get-properties value (jss-current-tab))
         expander)))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-function))
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-add-backs
     (jss-remote-function-get-source-location value tab)
     (lambda (location)
       (apply 'jss-script-display-at-position location))
     (lambda (error)
       (message "No source location for function.")))))

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

(defun jss-expand-nearest-remote-value ()
  (interactive)
  (let ((nearest-before nil)
        (nearest-before-distance 0)
        (nearest-after nil)
        (nearest-after-distance 0))
    (save-excursion
      (while (and (not (get-text-property (point) 'jss-remote-value-collapsed))
                  (< (point-min) (point)))
        (backward-char 1)
        (incf nearest-before-distance))
      (if (get-text-property (point) 'jss-remote-value-collapsed)
          (setf nearest-before (get-text-property (point) 'jss-remote-value))
        (setf nearest-before-distance nil)))
    (save-excursion
      (while (and (not (get-text-property (point) 'jss-remote-value-collapsed))
                  (< (point) (point-max)))
        (forward-char 1)
        (incf nearest-after-distance))
      (if (get-text-property (point) 'jss-remote-value-collapsed)
          (setf nearest-after (get-text-property (point) 'jss-remote-value))
        (setf nearest-after-distance nil)))
    (cond
     ((and nearest-before-distance nearest-after-distance)
      (jss-remote-value-expand (if (<= nearest-before-distance nearest-after-distance)
                                   nearest-before
                                 nearest-after)))
     (nearest-before-distance (jss-remote-value-expand nearest-before))
     (nearest-after-distance  (jss-remote-value-expand nearest-after))
     (t (message "No expandable values around point.")))))

(provide 'jss-remote-value)
