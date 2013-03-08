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

(define-key jss-prompt-map (kbd "M-n") 'jss-prompt-insert-next-input)

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
   (history-offset :initform nil :accessor jss-prompt-history-offset)

   (marker-overlay :initform nil :accessor jss-prompt-marker-overlay)
   (input-overlay  :initform nil :accessor jss-prompt-input-overlay)
   (output-overlay :initform nil :accessor jss-prompt-output-overlay)))

(defun* jss-insert-prompt (submit-function &key local-map keymap previous-prompt)
  (unless (or (bobp) (= (point) (line-beginning-position)))
    (insert "\n"))
  (let ((prompt (make-instance 'jss-prompt
                               :submit-function submit-function
                               :buffer (current-buffer)
                               :history (if previous-prompt
                                            (cons (jss-prompt-input-text previous-prompt)
                                                  (jss-prompt-history previous-prompt))
                                          '())))
        (inhibit-read-only t))

    (jss-wrap-with-text-properties (list 'jss-prompt prompt)

      (let ((marker-start (point)))
        (jss-wrap-with-text-properties (list 'read-only t 'rear-nonsticky t)
          (insert "> "))
        (setf (jss-prompt-marker-overlay prompt) (make-overlay marker-start (point) (current-buffer) t)))
      
      (let ((input-start (point)))
        (jss-wrap-with-text-properties (list 'jss-prompt-input-end-marker t)
          (insert "\n"))
        (setf (jss-prompt-input-overlay prompt) (make-overlay input-start (point)))
        (overlay-put (jss-prompt-input-overlay prompt) 'face 'highlight)

        (cl-flet ((make-parent-map (child)
                    (let ((map (copy-keymap child)))
                      (set-keymap-parent map jss-prompt-map)
                      map)))
          (cond
           (local-map
            (overlay-put (jss-prompt-input-overlay prompt) 'local-map (make-parent-map local-map)))
           (keymap
            (overlay-put (jss-prompt-input-overlay prompt) 'keymap (make-parent-map keymap)))
           (t
            (overlay-put (jss-prompt-input-overlay prompt) 'keymap jss-prompt-map))))))
    prompt))

(defmethod jss-prompt-start-of-input ((prompt jss-prompt))
  (overlay-start (jss-prompt-input-overlay prompt)))

(defmethod jss-prompt-start-of-output ((prompt jss-prompt))
  (overlay-start (jss-prompt-output-overlay prompt)))

(defmethod jss-prompt-end-of-output ((prompt jss-prompt))
  (overlay-end (jss-prompt-output-overlay prompt)))

(defun jss-before-last-prompt ()
  (goto-char (point-max))
  (jss-end-of-previous-property-block 'jss-prompt)
  (let ((last-prompt (get-text-property (point) 'jss-prompt)))
    (goto-char (overlay-start (jss-prompt-marker-overlay last-prompt)))))

(defun jss-prompt-next-input ()
  (jss-start-of-next-property-block 'jss-prompt)
  (let ((next-prompt (get-text-property (point) 'jss-prompt)))
    (goto-char (overlay-start (jss-prompt-input-overlay next-prompt)))))

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
    (buffer-substring-no-properties (overlay-start (jss-prompt-input-overlay prompt))
                                    (overlay-end (jss-prompt-input-overlay prompt)))))

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

      (let ((output-start (point)))
        (insert "\n// Evaluating...")
        (setf (jss-prompt-output-overlay prompt) (make-overlay output-start (point))))
      
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

      (goto-char (jss-prompt-start-of-input
                  (jss-insert-prompt (jss-prompt-submit-function prompt)
                                     :previous-prompt prompt))))))

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
