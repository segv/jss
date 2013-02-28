(require 'jss-browser-api)

(define-derived-mode jss-debugger-mode jss-super-mode "JSS Debugger"
  ""
  (setf jss-current-debugger-instance jss-debugger
        jss-current-tab-instance (jss-debugger-tab jss-debugger))
  (widen)
  (delete-region (point-min) (point-max))
  (insert (jss-debugger-message jss-debugger) "\n\n")
  (dolist (frame (jss-debugger-stack-frames jss-debugger))
    (insert (jss-frame-function-name frame) " " (jss-frame-source-url frame) ":")
    (insert-text-button (jss-frame-source-position frame)
                        'action (lambda (button) (call-interactively 'jss-debugger-goto-source))
                        'jss-frame frame)
    (insert "\n"))
  t)

(defun jss-debugger-goto-source ()
  (interactive)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame
      (error "No frame at point."))
    ))

(provide 'jss-debugger)
