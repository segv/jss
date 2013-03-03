(require 'jss-browser-api)

(make-variable-buffer-local
 (defvar jss-current-debugger-instance))

(defun jss-current-debugger ()
  jss-current-debugger-instance)

(define-derived-mode jss-debugger-mode jss-super-mode "JSS Debugger"
  ""
  (setf jss-current-debugger-instance jss-debugger
        jss-current-tab-instance (jss-debugger-tab jss-debugger))
  (add-hook 'kill-buffer-hook 'jss-debugger-kill nil t)
  (widen)
  (delete-region (point-min) (point-max))
  (insert "Paused on " (jss-debugger-message jss-debugger) "\n\n")
  (loop
   for count upfrom 1
   for frame in (jss-debugger-stack-frames jss-debugger)
   for start = (point)
   do (insert (format "Frame %d: " count))
   do (jss-insert-with-highlighted-whitespace (jss-frame-function-name frame))
   do (insert "\n")
   do (add-text-properties start (point) (list 'jss-frame-label t))
   do (when (jss-frame-source-position frame)
        (insert "Source location: ")
        (jss-insert-with-highlighted-whitespace (jss-frame-source-position frame))
        (insert "\n"))
   do (insert "Scope:\n")
   do (insert "Console:\n")
   do (lexical-let ((frame frame))
        (jss-insert-prompt (lambda (text)
                             (jss-evaluate frame text))))
   do (insert "\n")
   do (let ((inhibit-read-only t))
        (add-text-properties start (point) (list 'jss-frame frame)))
   do (jss-section-marker))
  (goto-char (point-min))
  (jss-before-last-prompt)
  t)

(define-key jss-debugger-mode-map (kbd "c") 'jss-debugger-stepper-continue)

(defun jss-debugger-stepper-continue ()
  (interactive)
  (jss-debugger-continue (jss-current-debugger))
  (kill-buffer (current-buffer)))

(defun jss-debugger-goto-source ()
  (interactive)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame (error "No frame at point."))
    (lexical-let* ((location (cdr (assoc 'location (slot-value frame 'properties))))
                   (scriptId (cdr (assoc 'scriptId location)))
                   (lineNumber (cdr (assoc 'lineNumber location)))
                   (columnNumber (cdr (assoc 'columnNumber location))))
      
      (jss-deferred-add-backs
       (jss-chrome-send-request (jss-debugger-tab (jss-current-debugger))
                                (list "Debugger.getScriptSource" (cons 'scriptId scriptId)))
       (lambda (response)
         (with-current-buffer (get-buffer-create (generate-new-buffer-name "*JSS Script*"))
           (insert (cdr (assoc 'scriptSource response)))
           (goto-char (point-min))
           (forward-line lineNumber)
           (forward-char columnNumber)
           (insert (propertize "^" 'face font-lock-comment-face))
           (display-buffer (current-buffer))))))))

(defun jss-debugger-toggle-frame-visibility ()
  (interactive)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame (error "No frame at point."))
    (save-excursion
      (let ((frame-location (jss-find-property-block 'jss-frame frame))
            (inhibit-read-only t))
        (goto-char (car frame-location))
        (if (get-text-property (point) 'jss-frame-hidden)
            (progn
              (remove-text-properties (car frame-location) (cdr frame-location) (list 'invisible 'jss-frame-hidden))
              (add-text-properties (car frame-location) (cdr frame-location)    (list 'jss-frame-visible t)))
          (goto-char (next-single-property-change (point) 'jss-frame-label))
          (remove-text-properties (car frame-location) (cdr frame-location) (list 'jss-frame-visible))
          (add-text-properties (car frame-location) (cdr frame-location)    (list 'jss-frame-hidden t))
          (add-text-properties (point) (cdr frame-location) (list 'invisible t)))))))

(defun jss-debugger-kill ()
  (interactive)
  )

(provide 'jss-debugger)
