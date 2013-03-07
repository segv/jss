(require 'jss-browser-api)

(make-variable-buffer-local
 (defvar jss-current-debugger-instance))

(defun jss-current-debugger ()
  jss-current-debugger-instance)

(defun jss-debugger-mode* (dbg)
  (let ((jss-debugger dbg))
    (jss-debugger-mode)))

(defvar jss-debugger-mode-map (make-sparse-keymap))

(define-key jss-debugger-mode-map (kbd "t") 'jss-frame-toggle-visibility)
(define-key jss-debugger-mode-map (kbd "n") 'jss-frame-next)
(define-key jss-debugger-mode-map (kbd "p") 'jss-frame-previous)
(define-key jss-debugger-mode-map (kbd "e") 'jss-frame-goto-exception)
(define-key jss-debugger-mode-map (kbd "s") 'jss-frame-goto-source)

(define-key jss-debugger-mode-map (kbd "r") 'jss-debugger-stepper-resume)
(define-key jss-debugger-mode-map (kbd "q") 'jss-debugger-stepper-resume)
(define-key jss-debugger-mode-map (kbd "i") 'jss-debugger-stepper-step-into)
(define-key jss-debugger-mode-map (kbd "v") 'jss-debugger-stepper-step-over)
(define-key jss-debugger-mode-map (kbd "o") 'jss-debugger-stepper-step-out)

(make-variable-buffer-local
 (defvar jss-debugger-num-frames nil))

;;; TODO: jss-debugger-resume-points (the opposite of breakpoints)

(defcustom jss-ignorable-exception-functions '()
  "List of functions to call on a new exception, if any of them
  return true the exception is automatically resumed.")

(pushnew 'jss-is-jquery-exception jss-ignorable-exception-functions)
(pushnew 'jss-is-3rd-party-exception jss-ignorable-exception-functions)

(setf jss-ignorable-exception-functions '(jss-is-3rd-party-exception))

(defun jss-with-first-stack-frame-url (debugger thunk)
  (lexical-let ((thunk thunk)
                (debugger debugger))
    (let ((frames (jss-debugger-stack-frames debugger)))
      (when (first frames)
        (jss-deferred-then
         (jss-frame-get-source-location (first frames))
         (lambda (location)
           (let* ((script (first location)))
             (when script
               (let ((url (jss-script-url script)))
                 (when url
                   (funcall thunk url debugger)))))))))))

(defun jss-is-jquery-exception (debugger)
  (jss-with-first-stack-frame-url
   (lambda (url debugger)
     (save-match-data
       (when (string-match ".*/jquery[-.0-9]+\\(min\\)\\.js$" url)
         t)))))

(defun jss-is-3rd-party-exception (debugger)
  (jss-with-first-stack-frame-url
   (lambda (url debugger)
     (let ((tab-url (jss-tab-url (jss-debugger-tab debugger))))
       (when tab-url
         (let* ((script-url (url-generic-parse-url url))
                (script-host (url-host script-url))
                (tab-url (url-generic-parse-url tab-url))
                (tab-host (url-host tab-url)))
           (not (string= tab-host script-host))))))))

(define-derived-mode jss-debugger-mode jss-super-mode "JSS Debugger"
  ""
  (setf jss-current-debugger-instance jss-debugger
        jss-current-tab-instance (jss-debugger-tab jss-debugger))
  (add-hook 'kill-buffer-hook 'jss-debugger-kill nil t)
  (widen)
  (delete-region (point-min) (point-max))

  (block nil
    (dolist (func jss-ignorable-exception-functions)
      (let ((cond (funcall func jss-debugger)))
        (if (jss-deferred-p cond)
            (lexical-let ((buffer (current-buffer)))
              (jss-deferred-then cond
                                 (lambda (value)
                                   (when (eql t value)
                                     (when (buffer-live-p buffer)
                                       (message "Auto-resuming exception.")
                                       (jss-debugger-stepper-resume))))))
          (if cond
              t
            (when (buffer-live-p buffer)
              (message "Auto-resuming exception.")
              (jss-debugger-stepper-resume))
            (return t)))))
    
    (jss-debugger-insert-message (jss-current-debugger))
    (unless (bolp) (insert "\n"))
    (insert "Paused on ")
    (jss-wrap-with-text-properties (list 'jss-debugger-exception t)
      (jss-insert-remote-value (jss-debugger-exception (jss-current-debugger))))
    (insert "\n\n")
    (loop
     initially (setf jss-debugger-num-frames 0)
     for frame in (jss-debugger-stack-frames jss-debugger)
     do (incf jss-debugger-num-frames)
     do (jss-debugger-insert-frame frame (1- jss-debugger-num-frames)))
    (goto-char (car (jss-find-property-block 'jss-debugger-exception t)))
    (setf buffer-read-only t)
    t))

(defvar jss-frame-label-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jss-debugger-mode-map)
    (define-key map (kbd "c") 'jss-debugger-frame-goto-prompt)))

(defmethod jss-debugger-insert-frame ((frame jss-generic-stack-frame) count)
  (jss-wrap-with-text-properties (list 'jss-frame frame 'jss-frame-count count)
    (jss-wrap-with-text-properties (list 'jss-frame-label t
                                         'keymap jss-frame-label-map)
      (insert (format "Frame %d: " count))
      (jss-insert-with-highlighted-whitespace (jss-frame-function-name frame))
      (insert "\n"))
    (jss-wrap-with-text-properties (list 'invisible t)
      (when (jss-frame-source-hint frame)
        (jss-wrap-with-text-properties (list 'jss-frame-source-position t)
          (insert "Source location: ")
          (jss-insert-with-highlighted-whitespace (jss-frame-source-hint frame))
          (insert "\n")))
      (insert "Scope:\n")
      (insert "--not yet implemented--\n")
      (jss-section-marker))))

(defmacro define-jss-debugger-step-function (name method)
  `(defun ,name ()
     (interactive)
     (,method (jss-current-debugger))
     (kill-buffer (current-buffer))))

(define-jss-debugger-step-function jss-debugger-stepper-resume    jss-debugger-resume)
(define-jss-debugger-step-function jss-debugger-stepper-step-into jss-debugger-step-into)
(define-jss-debugger-step-function jss-debugger-stepper-step-over jss-debugger-step-over)
(define-jss-debugger-step-function jss-debugger-stepper-step-out  jss-debugger-step-out)

(defun jss-frame-goto-source ()
  (interactive)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame (error "No frame at point."))
    (jss-deferred-add-backs
     (jss-frame-get-source-location frame)
     (lambda (location)
       (apply 'jss-script-display-at-position location)))))

(defun jss-frame-parts-locations (point)
  (let ((frame (get-text-property (point) 'jss-frame)))
    (unless frame
      (error "No frame at point."))
    (save-excursion
      (destructuring-bind (start . end)
          (jss-find-property-block 'jss-frame frame :test 'eq)
        (goto-char start)
        ;; note that jss-start-of-next-property-block moves point to the start and returns it
        (setf frame-label-start (jss-start-of-next-property-block 'jss-frame-label))
        (setf frame-label-end   (jss-end-of-current-property-block 'jss-frame-label))
        (list frame
              :start start :end end
              :frame-label-start frame-label-start
              :frame-label-end   frame-label-end))))) 

(defmacro* with-frame-at-point ((frame &rest location-args) &body body)
  (declare (indent 1))
  `(destructuring-bind (,frame ,@location-args)
       (jss-frame-parts-locations (point))
     ,@body))

(defun jss-frame-hide ()
  (interactive)
  (with-frame-at-point (frame &key start end frame-label-start frame-label-end)
    (let ((inhibit-read-only t))
      (add-text-properties frame-label-end end (list 'invisible t)))))

(defun jss-frame-show ()
  (interactive)
  (with-frame-at-point (frame &key start end frame-label-start frame-label-end)
    (let ((inhibit-read-only t))
      (remove-text-properties frame-label-end end (list 'invisible)))))

(defun jss-frame-toggle-visibility ()
  (interactive)
  (with-frame-at-point (frame &key start end frame-label-start frame-label-end)
    (if (get-text-property (1+ frame-label-end) 'invisible)
        (jss-frame-show)
      (jss-frame-hide))))

(defun jss-frame-next ()
  (interactive)
  (let* ((current-count (get-text-property (point) 'jss-frame-count))
         (next-id (if current-count
                      (mod (1+ current-count) jss-debugger-num-frames)
                    0)))
    (goto-char (car (jss-find-property-block 'jss-frame-count next-id)))))

(defun jss-frame-previous ()
  (interactive)
  (let* ((current-count (get-text-property (point) 'jss-frame-count))
         (next-id (if current-count
                      (if (= current-count 0)
                          (1- jss-debugger-num-frames)
                        (1- current-count))
                    0)))
    (goto-char (car (jss-find-property-block 'jss-frame-count next-id)))))

(defun jss-frame-goto-exception ()
  (interactive)
  (goto-char
   (car (jss-find-property-block 'jss-debugger-exception t :test 'eq))))

(defun jss-debugger-kill ()
  (interactive)
  (jss-debugger-cleanup (jss-current-debugger)))

(defun jss-debugger-frame-goto-prompt ()
  (interactive)
  t)

(provide 'jss-debugger)
