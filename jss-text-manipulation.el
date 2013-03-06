(defface jss-button-face '((t :underline t))
  "Face used for jss-buttons.")

(defvar jss-button-map (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'jss-invoke-primary-action)
                         (define-key map (kbd "SPC") 'jss-invoke-secondary-action)
                         map))

(defun jss-insert-button (label &rest jss-add-text-button-args)
  (let ((start (point)))
    (insert-and-inherit label)
    (apply 'jss-add-text-button start (point) jss-add-text-button-args)
    label))

(defun* jss-add-text-button (start end primary-action &key secondary-action other-properties)
  (let ((o (make-overlay start end (current-buffer))))
    (overlay-put o 'face 'jss-button-face)
    (overlay-put o 'keymap jss-button-map)
    (add-text-properties start end
                         (append (list 'jss-button t
                                       'read-only t
                                       'rear-nonsticky t)
                                 (when primary-action
                                   (list 'jss-primary-action primary-action))
                                 (when secondary-action
                                   (list 'jss-secondary-action secondary-action))
                                 other-properties))))

(defun jss-invoke-property (property-name)
  (when (get-text-property (point) property-name)
    (call-interactively (get-text-property (point) property-name))
    t))

(defun jss-invoke-primary-action ()
  (interactive)
  (or (jss-invoke-property 'jss-primary-action)
      (call-interactively 'self-insert-command)))

(defun jss-invoke-secondary-action ()
  (interactive)
  (or (jss-invoke-property 'jss-secondary-action)
      (call-interactively 'self-insert-command)))

(defun jss-next-button ()
  (interactive)
  (when (get-text-property (point) 'jss-button)
    ;; in a button, move past it
    (goto-char (or (next-single-property-change (point) 'jss-button)
                   (point-max))))
  (let ((next-button (next-single-property-change (point) 'jss-button)))
    (if next-button
        (goto-char next-button)
      (goto-char (point-min))
      (unless (get-text-property (point) 'jss-button) ; if buffer starts with a button just stay here
        (setf next-button (next-single-property-change (point) 'jss-button))
        (when next-button
          (goto-char next-button))))))

(defun jss-log-event (event)
  (with-current-buffer (get-buffer-create " *jss-events*")
    (insert (format ";; %s\n" (format-time-string "%Y-%m-%dT%T")))
    (dolist (event-part event)
      (insert (prin1-to-string event-part) "\n"))
    (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")))

(defun jss-comment-char (string)
  (jss-wrap-with-text-properties (list 'face font-lock-comment-face
                                       'font-lock-face font-lock-comment-face)
    (insert-and-inherit string)))

(defun jss-eol-mark ()
  (when (member (preceding-char) (list ?  ?\n ?\t ?\r))
    (jss-comment-char "$"))
  (insert-and-inherit "\n"))

(defun jss-insert-with-highlighted-whitespace (string)
  (save-match-data
    (when (string= "" string)
      (jss-comment-char "^$"))
    (when (string-match "^[ \t\r\n\f]" string)
      (jss-comment-char "^"))
    (loop
     for char across string
     do (case char
          (?\t (jss-comment-char "\\t"))
          (?\n (jss-comment-char "\\n"))
          (?\r (jss-comment-char "\\r"))
          (?\f (jss-comment-char "\\f"))
          (t (insert-and-inherit (char-to-string char)))))
    (when (string-match "[ \t\r\n\f]$" string)
      (jss-comment-char "$"))))

(defun jss-section-marker ()
  (insert-and-inherit "--------------------------------\n"))

(defun jss-have-next-property-block (property-name)
  (or (get-text-property (point) property-name)
      (next-single-property-change (point) property-name)))

(defun jss-have-previous-property-block (property-name)
  (or (get-text-property (point) property-name)
      (previous-single-property-change (point) property-name)))

(defun jss-start-of-next-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-start-of-current-property-block property-name)))
    (let ((next-change (next-single-property-change (point) property-name)))
      (when next-change
        (return (goto-char next-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-max))
          (error "Unable to find start of next block with property %s" property-name))
        (forward-char 1))
      (return (point)))))

(defun jss-end-of-previous-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-end-of-current-property-block property-name)))

    (let ((previous-change (if (eobp) ;; previous-single-property-change works differently at eobp, a char by char search is easier
                               nil
                             (previous-single-property-change (point) property-name))))
      (when previous-change
        (return (goto-char previous-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-min))
          (error "Unable to find previous block with property %s" property-name))
        (backward-char 1))
      (return (point)))))

(defun jss-start-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get start of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-min))
        (return (point)))
      (backward-char 1))
    (forward-char 1))
  (point))

(defun jss-end-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get end of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-max))
        (return))
      (forward-char 1)))
  (point))

(defun* jss-find-property-block (property-name property-value &key (test 'equal))
  (save-excursion
    (goto-char (point-max))
    (let (block-start block-end)

      (while (not (funcall test (get-text-property (point) property-name) property-value))
        (when (= (point) (point-min))
          (error "Unable to find block with property %s %s to %s in buffer %s." property-name test property-value (current-buffer)))
        (backward-char 1))
      (setf block-end (min (1+ (point)) (point-max)))

      (block nil
        (while (and (funcall test (get-text-property (point) property-name) property-value)
                    (< (point-min) (point)))
          (backward-char 1)))
      (setf block-start (min (1+ (point)) (point-max)))

      (cons block-start block-end))))

(defun* jss-delete-property-block (property-name property-value &key (test 'equal))
  (let ((location (jss-find-property-block property-name property-value :test test))
        (inhibit-read-only t))
    (delete-region (car location) (cdr location))))

(defun jss-insert-with-properties (property-list format-control &rest format-args)
  (let ((start (point)))
    (insert-and-inherit (apply 'format format-control format-args))
    (add-text-properties start (point) property-list)))

(defmacro jss-wrap-with-text-properties (properties &rest body)
  (declare (indent 1))
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1
           (progn ,@body)
         (let ((inhibit-read-only t))
           (add-text-properties ,start (point) ,properties))))))

(defun jss-limit-string-length (string max-length)
  (if (< max-length (length string))
      (format "%s...[snip]...%s"
              (substring string 0 (/ max-length 2))
              (substring string (- (length string) (/ max-length 2)) (length string)))
      string))

(provide 'jss-text-manipulation)
