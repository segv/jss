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
   do (insert (format "Frame %d: " count ))
   do (jss-insert-with-highlighted-whitespace (jss-frame-function-name frame))
   do (insert "\n")
   do (insert "Source location: ")
   do (if (jss-frame-source-url frame)
          (insert (jss-frame-source-url frame) ":" (jss-frame-source-position frame)))
   do (insert "[goto source]")
   do (insert "\n")
   do (insert "Scope:\n")
   do (insert "Console:\n")
   do (lexical-let ((frame frame))
        (jss-insert-prompt (lambda (text)
                             (jss-evaluate frame text))))
   do (insert "\n")
   do (jss-section-marker))
  ;;(insert-text-button (jss-frame-source-position frame)
  ;;                    'action (lambda (button) (call-interactively 'jss-debugger-goto-source))
  ;;                    'jss-frame frame)
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

(provide 'jss-debugger)
