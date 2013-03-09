
(defmethod jss-insert-remote-value ((value jss-generic-remote-primitive))
  (jss-remote-value-insert-description value))

(defmethod jss-insert-remote-value ((value jss-generic-remote-non-primitive))
  (let ((start (point)))
    (jss-wrap-with-text-properties (list 'jss-remote-value value 'jss-remote-value-collapsed t)
      (jss-remote-value-insert-description value))
    (jss-add-text-button start (point) 'jss-remote-value-expand-at-point)))

(defmethod jss-insert-remote-value :after ((value jss-generic-remote-object))
  (jss-autoexpand-small-remote-object value jss-remote-value-auto-expand-property-limit))

(defvar jss-remote-value-auto-expand-property-limit 6)

(defmethod jss-autoexpand-small-remote-object ((object jss-generic-remote-object) max-property-length)
  (when max-property-length
    (lexical-let ((object object)
                  (max-property-length max-property-length)
                  (buffer (current-buffer)))
      (jss-deferred-add-callback
       (jss-remote-object-get-properties object (jss-current-tab))
       (lambda (properties)
         (when (and max-property-length
                    (<= (length properties) max-property-length))
           (jss-remote-value-replace-with-properties object properties buffer)))))))

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
      (jss-wrap-with-text-properties (list 'jss-remote-value-collapsed t)
        (insert "{expanding ")
        (jss-remote-value-insert-description value)
        (insert "}")))
    
    (lexical-let ((buffer (current-buffer))
                  (value value))

      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (jss-remote-value-replace-with-properties value properties buffer))))))

(defun jss-remote-value-insert-as-object-properties (alist separator identp)
  (loop for first = t then nil
        for (prop . more) on alist
        when identp
          do (indent-to-column left-column)
        do (jss-insert-with-highlighted-whitespace (car prop))
        do (insert ": ")
        do (jss-insert-remote-value (cdr prop))
        when more
        do (insert separator)))

(defmethod jss-remote-value-replace-with-properties ((value jss-generic-remote-object) properties buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (jss-remote-value-collapsed value) 
        (jss-replace-with-default-property
            (jss-remote-value value :test 'eq)
          (let ((left-column (+ 2 (current-column)))
                (jss-remote-value-auto-expand-property-limit
                 ;; resetting
                 ;; jss-remote-value-auto-expand-property-limit here
                 ;; prevents use from sending off mny (possibly
                 ;; hundreds) of outaexpand requests when expanding a
                 ;; huge object.
                 ;;
                 ;; the justificiation is that if you're expanding a
                 ;; huge object you can go and find the specific
                 ;; properties you're interested in yourself,
                 ;; autoexpansion is only really useful at the outermost level
                 (if (<= (length properties) jss-remote-value-auto-expand-property-limit)
                     jss-remote-value-auto-expand-property-limit
                   nil)))
            (jss-remote-value-insert-description value)
            (insert " {\n")
            (jss-remote-value-insert-as-object-properties properties ",\n" t)
            (insert "}")))))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-function))
  (jss-deferred-add-backs
   (jss-remote-function-get-source-location value (jss-current-tab))
   (lambda (location)
     (apply 'jss-script-display-at-position location))
   (lambda (error)
     (message "No source location for function."))))

(defmethod jss-remote-value-expand ((value jss-generic-remote-array))
  (lexical-let ((tab (jss-current-tab))
                (value value)
                (buffer (current-buffer)))
    (jss-replace-with-default-property (jss-remote-value value :test 'eq)
      (insert "[expanding]")
      (jss-deferred-add-callback
       (jss-remote-object-get-properties value (jss-current-tab))
       (lambda (properties)
         (jss-remote-value-replace-with-properties value properties buffer))))))

(defmethod jss-remote-value-replace-with-properties ((value jss-generic-remote-array) properties buffer)
  (let ((integer-properties '())
        (other-properties '()))
    (dolist (prop properties)
      (destructuring-bind (key . value) prop
        (if (save-match-data (string-match "^[[:digit:]]+$" key))
            (push (cons (string-to-number key) value) integer-properties)
          (push prop other-properties))))
    (jss-replace-with-default-property (jss-remote-value value :test 'eq)
      (with-current-buffer buffer
        (loop
         initially (insert "[")
         for item in (cl-sort integer-properties '< :key 'car)
         for first = t then nil
         unless first do (insert ", ")
         do (jss-insert-remote-value (cdr item))
         finally (insert "]"))
        (insert " {")
        (jss-remote-value-insert-as-object-properties other-properties ", " nil)
        (insert "}")))))

(defun jss-remote-value-expand-at-point ()
  (interactive)
  (let ((value (get-text-property (point) 'jss-remote-value)))
    (unless value
      (error "No value at point."))
    (jss-remote-value-expand value)))

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
