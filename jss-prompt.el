;;; the jss prompt is designed so that it can be embedded in multiple
;;; places (the console buffer and the debugger for now).

(defvar jss-prompt-counter 0)

(defvar jss-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jss-prompt-eval-or-newline)
    (define-key map (kbd "C-a") 'jss-prompt-beginning-of-line)
    map))

(defun jss-insert-prompt (submit-function)
  (unless (or (bobp)
              (= (point) (line-beginning-position)))
    (insert "\n"))
  (let ((start (point))
        (inhibit-read-only t))
    (jss-insert-with-properties (list 'read-only t
                                      'rear-nonsticky t
                                      'jss-prompt-marker t
                                      'jss-prompt-submit-function submit-function)
                                ">")
    (jss-insert-with-properties (list 'read-only nil 'jss-prompt-input t) " ")
    (add-text-properties start (point)
                         (list 'keymap jss-prompt-map
                               'jss-prompt t
                               'jss-prompt-id (incf jss-prompt-counter)))))

(defun jss-prompt-goto-next ()
  (jss-start-of-next-property-block 'jss-prompt)
  (jss-start-of-next-property-block 'jss-prompt-input))

(defun jss-before-last-prompt ()
  (goto-char (point-max))
  (jss-end-of-previous-property-block 'jss-prompt)
  (jss-start-of-current-property-block 'jss-prompt))

(defun jss-prompt-eval-or-newline ()
  (interactive)
  (block nil

    (let (submit-function
          (start (point))
          (prompt-id (get-text-property (point) 'jss-prompt-id)))
      (unless prompt-id
        
        (if (get-text-property (1- (point)) 'jss-prompt-id)
            (backward-char 1)
            (setf prompt-id (get-text-property (1- (point)) 'jss-prompt-id))
          (warn "Not in a prompt.")
          (return)))      
      ;; find the marker for this prompt and get the submit-function
      (let ((marker-end (previous-single-property-change (point) 'jss-prompt-marker)))
        (unless marker-end
          (error "Unable to find end of marker before %d" (point)))
        (let ((submit-property (get-text-property (1- marker-end) 'jss-prompt-submit-function)))
          (if submit-property
              (setf submit-function submit-property)
            (error "Missing jss-prompt-submit-function at %s" (point)))))

      (when (get-text-property (point) 'jss-prompt-marker)
        (goto-char (next-single-property-change (point) 'jss-prompt-marker)))
      
      (let ((input-start (save-excursion
                           (jss-start-of-current-property-block 'jss-prompt-input)))
            (input-end (save-excursion
                         (jss-end-of-current-property-block 'jss-prompt-input)))
            (input-complete-p nil))
        (save-restriction
          (narrow-to-region input-start input-end)
          (setf input-complete-p (null (js2-ast-root-errors (js2-parse))))
          (if input-complete-p
              (jss-prompt-submit prompt-id
                                 submit-function
                                 input-start input-end)
            (goto-char start)
            (jss-prompt-insert-newline)))))))

(defun jss-prompt-submit (prompt-id submit-function start end)
  (let ((inhibit-read-only t))

    (let (start)
      (jss-start-of-current-property-block 'jss-prompt)
      (setf start (point))
      (jss-end-of-current-property-block 'jss-prompt)
      (add-text-properties start (point) (list 'read-only t)))
    (unless (= (point) (line-beginning-position))
      (insert-and-inherit "\n"))
    (let ((start (point)))
      (jss-insert-with-properties (list 'jss-prompt-output t)
                                  "// Evaluating...")
      (remove-text-properties start (point) (list 'jss-prompt-input)))
    (lexical-let ((current-buffer (current-buffer))
                  (prompt-id prompt-id))
      (jss-deferred-add-backs
       (funcall submit-function (buffer-substring-no-properties start end))
       (lambda (remote-object)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt-id remote-object)))
       (lambda (error)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt-id error)))))
    (jss-insert-prompt submit-function)))

(defun jss-prompt-update-output (prompt-id remote-object)
  (save-excursion
    (let ((prompt-location (jss-find-property-block 'jss-prompt-id prompt-id))
          (inhibit-read-only t))
      (goto-char (cdr prompt-location))
      (unless (get-text-property (point) 'jss-prompt-output)
        (backward-char 1)
        (unless (get-text-property (point) 'jss-prompt-output)
          (error "Unable to find jss-prompt-output for prompt %s (currently at %s)" prompt-id (point))))
      (jss-start-of-current-property-block 'jss-prompt-output)
      (delete-region (point) (cdr prompt-location))
      (jss-remote-value-insert remote-object))))

(defun jss-prompt-insert-newline ()
  (insert "\n")
  (js2-indent-line))

(defun jss-prompt-beginning-of-line (&optional n)
  (interactive "P")
  (beginning-of-line n)
  (when (get-text-property (point) 'jss-prompt-marker)
    (let ((past-prompt-marker (next-single-property-change (point)' jss-prompt-marker)))
      (if past-prompt-marker
          (goto-char (min (1+ past-prompt-marker) (point-max)))
        (goto-char (point-max))))))

(provide 'jss-prompt)
