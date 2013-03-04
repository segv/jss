;;; the jss prompt is designed so that it can be embedded in multiple
;;; places (the console buffer and the debugger for now).

(defvar jss-prompt-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map js2-mode-map)
    map))

(define-key jss-prompt-map (kbd "RET") 'jss-prompt-eval-or-newline)
(define-key jss-prompt-map (kbd "C-c C-c") 'jss-prompt-eval)
(define-key jss-prompt-map (kbd "C-a") 'jss-prompt-beginning-of-line)

(defvar jss-prompt-counter 0)

(defclass jss-prompt ()
  ((submit-function :initarg :submit-function :accessor jss-prompt-submit-function)
   (id :initform (incfo jss-prompt-counter) :reader jss-prompt-id)
   (buffer :initarg :buffer :reader jss-prompt-buffer)))

(defun jss-insert-prompt (submit-function)
  (unless (or (bobp) (= (point) (line-beginning-position)))
    (insert "\n"))
  (let ((prompt (make-instance 'jss-prompt
                               :submit-function submit-function
                               :buffer (current-buffer)))
        (inhibit-read-only t))
    (jss-wrap-with-text-properties (list 'jss-prompt prompt)

      (jss-wrap-with-text-properties (list 'read-only t
                                           'rear-nonsticky t
                                           'jss-prompt-marker t)
        (insert ">"))

      (jss-wrap-with-text-properties (list 'read-only nil
                                           'jss-prompt-input t
                                           'keymap jss-prompt-map)
        (insert " ")))))

(defmethod jss-prompt-start-of-input ((prompt jss-prompt))
  (save-excursion
    (goto-char (car (jss-find-property-block 'jss-prompt prompt :test 'eq)))
    (goto-char (jss-start-of-next-property-block 'jss-prompt-input))
    (point)))

(defmethod jss-prompt-end-of-input ((prompt jss-prompt))
  (save-excursion
    (goto-char (jss-prompt-start-of-input prompt))
    (goto-char (jss-end-of-current-property-block 'jss-prompt-input))
    (point)))

(defmethod jss-prompt-start-of-output ((prompt jss-prompt))
  (save-excursion
    (goto-char (car (jss-find-property-block 'jss-prompt prompt :test 'eq)))
    (goto-char (jss-start-of-next-property-block 'jss-prompt-output))
    (point)))

(defmethod jss-prompt-end-of-output ((prompt jss-prompt))
  (save-excursion
    (goto-char (jss-prompt-start-of-output prompt))
    (goto-char (jss-end-of-current-property-block 'jss-prompt-output))
    (point)))

(defun jss-before-last-prompt ()
  (goto-char (point-max))
  (jss-end-of-previous-property-block 'jss-prompt)
  (jss-start-of-current-property-block 'jss-prompt))

(defun jss-prompt-next-input ()
  (jss-start-of-next-property-block 'jss-prompt)
  (jss-end-of-current-property-block 'jss-prompt-marker)
  (when (and (get-text-property (point) 'jss-prompt-input)
             (looking-at " "))
    (forward-char 1)))

(defun* jss-prompt-current-prompt (&optional (warn t))
  "Returns the prompt object around point. Uses some heuristics
  to figure out what the current prompt is."
  (save-excursion
    (block nil
      (when (get-text-property (point) 'jss-prompt)
        ;; directly in a prompt
        (return (get-text-property (point) 'jss-prompt)))
      (unless (bobp)
        (backward-char 1)
        (when (get-text-property (point) 'jss-prompt)
          (return (get-text-property (point) 'jss-prompt))))
      (warn "Not currently in a prompt."))))

(defmethod jss-prompt-input-text ((prompt jss-prompt))
  (with-current-buffer (jss-prompt-buffer prompt)
    (let* ((location (jss-find-property-block 'jss-prompt prompt :test 'eq))
           (start (car location)))
      (goto-char start)
      (let ((input-start (jss-start-of-next-property-block 'jss-prompt-input))
            (input-end   (jss-end-of-current-property-block 'jss-prompt-input)))
        (when (or (< (cdr location) input-start)
                  (< (cdr location) input-end))
          (error "prompt-input for prompt %s is outside of the prompt itself." prompt))
        (goto-char input-start)
        (buffer-substring-no-properties
         input-start
         input-end)))))

(defun jss-prompt-eval-or-newline ()
  (interactive)
  (block nil
    (let ((prompt (jss-prompt-current-prompt))
          (js2-errors '()))

      (with-temp-buffer
        (insert (jss-prompt-input-text prompt))
        (setf js2-errors (js2-ast-root-errors (js2-parse))))

      (if js2-errors
          (progn
            (insert-and-inherit "\n")
            (js2-indent-line))
        (jss-prompt-submit prompt)))))

(defun jss-prompt-eval ()
  (interactive)
  (let ((prompt (jss-prompt-current-prompt)))
    (when prompt
      (jss-prompt-submit prompt))))

(defmethod jss-prompt-submit ((prompt jss-prompt))
  (let ((inhibit-read-only t))
    (goto-char (jss-prompt-start-of-input prompt))
    (jss-wrap-with-text-properties (list 'read-only t)
      (goto-char (jss-prompt-end-of-input prompt))
      (insert-and-inherit "\n"))
    (jss-wrap-with-text-properties (list 'read-only t
                                         'jss-prompt-output t
                                         'jss-prompt prompt)
      (insert "// Evaluating..."))
    
    (lexical-let ((current-buffer (current-buffer))
                  (prompt prompt))
      (jss-deferred-add-backs
       (funcall (jss-prompt-submit-function prompt)
                (jss-prompt-input-text prompt))
       (lambda (remote-object)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt remote-object)
           ))
       (lambda (error)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt error)))))
    (goto-char (jss-prompt-end-of-output prompt))
    (jss-insert-prompt (jss-prompt-submit-function prompt))))

(defmethod jss-prompt-update-output ((prompt jss-prompt) remote-object)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (jss-prompt-start-of-output prompt))
      (delete-region (point) (jss-prompt-end-of-output prompt))
      (jss-wrap-with-text-properties (list 'read-only t
                                           'jss-prompt prompt
                                           'jss-prompt-output t)
       (jss-insert-remote-value remote-object)))))

(defun jss-prompt-beginning-of-line (&optional n)
  (interactive "P")
  (beginning-of-line n)
  (when (get-text-property (point) 'jss-prompt-marker)
    (let ((past-prompt-marker (next-single-property-change (point)' jss-prompt-marker)))
      (if past-prompt-marker
          (goto-char (min (1+ past-prompt-marker) (point-max)))
        (goto-char (point-max))))))

(provide 'jss-prompt)
