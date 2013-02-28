(require 'jss-browser-api)

(defun jss-current-debugger ()
  jss-current-debugger-instance)

(define-derived-mode jss-debugger-mode jss-super-mode "JSS Debugger"
  ""
  (setf jss-current-debugger-instance jss-debugger
        jss-current-tab-instance (jss-debugger-tab jss-debugger))
  (widen)
  (delete-region (point-min) (point-max))
  (insert (jss-debugger-message jss-debugger) "\n\n")
  (dolist (frame (jss-debugger-stack-frames jss-debugger))
    (jss-insert-with-highlighted-whitespace (jss-frame-function-name frame))
    (insert " " (jss-frame-source-url frame) ":")
    (insert-text-button (jss-frame-source-position frame)
                        'action (lambda (button) (call-interactively 'jss-debugger-goto-source))
                        'jss-frame frame)
    (insert "\n"))
  t)

(define-key jss-debugger-mode-map (kbd "c") 'jss-debugger-stepper-continue)

(defun jss-debugger-stepper-continue ()
  (interactive)
  (jss-debugger-continue (jss-current-debugger))
  (kill-buffer (current-buffer)))

(defun jss-debugger-goto-source ()
  (interactive)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame
      (error "No frame at point."))
    ))

(provide 'jss-debugger)
