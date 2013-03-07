;;; the jss prompt is designed so that it can be embedded in multiple
;;; places (the console buffer and the debugger for now).

;;; TODO: A prompt is, by default, inactive, when it's activated the
;;; buffer's keymap is stored away, motion is limited ot whithin the
;;; prompt itself and the prompt map is used. when done (via C-c C-c),
;;; the prompt is made read-noly and the prompt keymap disappears

(defvar jss-prompt-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map jss)
    map))

(define-key jss-prompt-map (kbd "RET") 'jss-prompt-eval-or-newline)
(define-key jss-prompt-map (kbd "C-a") 'jss-prompt-beginning-of-line)

(define-key jss-prompt-map (kbd "M-p") 'jss-prompt-insert-previous-input)
(define-key jss-prompt-map (kbd "<up>") 'jss-prompt-insert-previous-input)

(define-key jss-prompt-map (kbd "M-n") 'jss-prompt-insert-next-input)
(define-key jss-prompt-map (kbd "<down>") 'jss-prompt-insert-next-input)

(make-variable-buffer-local
 (defvar jss-prompt-input-history '()))

(make-variable-buffer-local
 (defvar jss-prompt-input-history/last-inserted nil))

(defvar jss-prompt-counter 0)

(defclass jss-prompt ()
  ((submit-function :initarg :submit-function :accessor jss-prompt-submit-function)
   (id :initform (incfo jss-prompt-counter) :reader jss-prompt-id)
   (buffer :initarg :buffer :reader jss-prompt-buffer)
   (active-p :initform t :accessor jss-prompt-active-p)
   (history :initarg :history :reader jss-prompt-history)
   (history-offset :initform nil :accessor jss-prompt-history-offset)))

(defun jss-insert-prompt (submit-function &optional history)
  (unless (or (bobp) (= (point) (line-beginning-position)))
    (insert "\n"))
  (let ((prompt (make-instance 'jss-prompt
                               :submit-function submit-function
                               :buffer (current-buffer)
                               :history history))
        (inhibit-read-only t)
        (input-overlay (make-overlay (point) (point) (current-buffer) nil t)))
    
    (overlay-put input-overlay 'keymap jss-prompt-map)
    
    (jss-wrap-with-text-properties (list 'jss-prompt prompt
                                         'jss-prompt-input input-overlay)

      (jss-wrap-with-text-properties (list 'read-only t
                                           'rear-nonsticky t
                                           'jss-prompt-marker t)
        (insert "> "))

      (let ((start (point)))
        (insert "\n")
        (move-overlay input-overlay start (point))))
    (backward-char 1)))

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
  (let ((overlay (get-text-property (point) 'jss-prompt-input)))
    (if overlay
        (goto-char (overlay-start overlay))
      (error "Unable to find next input."))))

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

(defmethod jss-prompt-input-overlay ((prompt jss-prompt))
  (with-current-buffer (jss-prompt-buffer prompt)
    (save-excursion
      (let* ((location (jss-find-property-block 'jss-prompt prompt :test 'eq))
             (overlay (get-text-property (car location) 'jss-prompt-input)))
        (or overlay
            (error "Unable to find input overlay for %s." prompt))))))

(defmethod jss-prompt-input-location ((prompt jss-prompt))
  (let ((overlay (jss-prompt-input-overlay prompt)))
    (cons (overlay-start overlay) (overlay-end overlay))))

(defmethod jss-prompt-start-of-input ((prompt jss-prompt))
  (car (jss-prompt-input-location prompt)))

(defmethod jss-prompt-end-of-input ((prompt jss-prompt))
  (cdr (jss-prompt-input-location prompt)))

(defmethod jss-prompt-input-text ((prompt jss-prompt))
  (with-current-buffer (jss-prompt-buffer prompt)
    (save-excursion
      (let ((location (jss-prompt-input-location prompt)))
        (buffer-substring-no-properties (car location)
                                        (cdr location))))))

(defun jss-prompt-eval-or-newline (force-eval)
  "Evaluate the current input or insert a newline if js2 thinks
there are syntax errors in teh input.

Supply a prefix arg to force sending the current text"
  (interactive "P")
  (block nil
    (let ((prompt (jss-prompt-current-prompt)))

      (when force-eval
        (return (jss-prompt-submit prompt)))

      (let ((js2-parse-errors (with-temp-buffer
                                (insert (jss-prompt-input-text prompt))
                                (js2-ast-root-errors (js2-parse)))))
        
        (if js2-parse-errors
            (progn
              (message "Input has errors: %s" js2-parse-errors)
              (insert-and-inherit "\n")
              (js2-indent-line)
              (return))
          (jss-prompt-submit prompt))))))

(defmethod jss-prompt-submit ((prompt jss-prompt))
  (push (jss-prompt-input-text prompt) jss-prompt-input-history)
  (setf jss-prompt-input-history/last-inserted jss-prompt-input-history)
  (let ((inhibit-read-only t)
        (overlay (jss-prompt-input-overlay prompt)))
    (lexical-let ((current-buffer (current-buffer))
                  (prompt prompt)
                  (input-text (jss-prompt-input-text prompt)))
      (add-text-properties (overlay-start overlay) (overlay-end overlay)
                           (list 'read-only t))
      (goto-char (overlay-end overlay))
      (delete-overlay overlay)
      (jss-wrap-with-text-properties (list 'read-only t
                                           'jss-prompt-output t
                                           'jss-prompt prompt)
        (insert "\n// Evaluating..."))
      
      (setf (jss-prompt-active-p prompt) nil)
      (jss-deferred-add-backs
       (funcall (jss-prompt-submit-function prompt) input-text)
       (lambda (remote-object)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt remote-object)))
       (lambda (error)
         (with-current-buffer current-buffer
           (jss-prompt-update-output prompt error))))
      (goto-char (jss-prompt-end-of-output prompt))
      (jss-insert-prompt (jss-prompt-submit-function prompt)
                         (cons input-text (jss-prompt-history prompt))))))

(defmethod jss-prompt-update-output ((prompt jss-prompt) remote-object)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (jss-prompt-start-of-output prompt))
      (delete-region (point) (jss-prompt-end-of-output prompt))
      (jss-wrap-with-text-properties (list 'read-only t
                                           'jss-prompt prompt
                                           'jss-prompt-output t)
        (insert "\n")
        (jss-insert-remote-value remote-object)))))

(defun jss-prompt-beginning-of-line (&optional n)
  (interactive "P")
  (beginning-of-line n)
  (when (get-text-property (point) 'jss-prompt-marker)
    (let ((past-prompt-marker (next-single-property-change (point)' jss-prompt-marker)))
      (if past-prompt-marker
          (goto-char (min (1+ past-prompt-marker) (point-max)))
        (goto-char (point-max))))))

(defmethod jss-prompt-set-input-text ((prompt jss-prompt) input-text)
  (let* ((location (jss-prompt-input-location prompt))
         (inhibit-read-only t)
         (properties (text-properties-at (car location))))    
    (delete-region (car location) (cdr location))
    (goto-char (car location))
    (jss-wrap-with-text-properties properties
      (insert input-text))))

(defsetf jss-prompt-input-text jss-prompt-set-input-text)

(defun jss-active-prompt ()
  (let* ((prompt (jss-prompt-current-prompt)))
    (unless prompt
      (error "No prompt at point."))
    (unless (jss-prompt-active-p prompt)
      (error "Current prompt is no longer active."))
    prompt))

(defun jss-prompt-insert-from-history (prompt history-delta first-time)
  (when (jss-prompt-history prompt)
    (if (null (jss-prompt-history-offset prompt))
        (setf (jss-prompt-history-offset prompt) first-time)
      (setf (jss-prompt-history-offset prompt) (+ history-delta (jss-prompt-history-offset prompt))))
    (setf (jss-prompt-history-offset prompt) (mod (jss-prompt-history-offset prompt)
                                                  (length (jss-prompt-history prompt)))
          (jss-prompt-input-text prompt) (nth (jss-prompt-history-offset prompt) (jss-prompt-history prompt)))))

(defun jss-prompt-insert-previous-input ()
  (interactive)
  (jss-prompt-insert-from-history (jss-active-prompt)
                                  +1
                                  0))

(defun jss-prompt-insert-next-input ()
  (interactive)
  (let ((prompt (jss-active-prompt)))
    (jss-prompt-insert-from-history prompt
                                    -1
                                    (1- (length  (jss-prompt-history prompt))))))

(provide 'jss-prompt)
