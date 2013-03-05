
(define-derived-mode jss-script-mode jss-super-mode "JSS Script"
  ""
  (setf buffer-read-only t))

(defmethod jss-script-display-at-position ((script jss-generic-script) line-number column-number)
  (if (jss-script-buffer script)
      (jss-script-mark-offset script line-number column-number)
    (lexical-let ((script script)
                  (line-number line-number)
                  (column-number column-number))
      (jss-deferred-then
       (jss-script-get-body script)
       (lambda (body)
         (setf (jss-script-buffer script) (generate-new-buffer (format "*JSS Script %s*" (jss-script-id script))))
         (with-current-buffer (jss-script-buffer script)
           (jss-script-mode)
           (let ((inhibit-read-only t))
             (delete-region (point-min) (point-max))
             (insert body)))
         (jss-script-mark-offset script line-number column-number))))))

(defface jss-script-line-marker-face '((t :background "grey60"))
  "Face used to highlight the area around point.")

(defmethod jss-script-mark-offset ((script jss-generic-script) line-number column-number)
  (with-current-buffer (jss-script-buffer script)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (remove-text-properties (point-min) (point-max) 'jss-script-offset-marker)
      (dolist (o (overlays-in (point-min) (point-max)))
        (delete-overlay o))
      (forward-line line-number)
      (forward-char column-number)
      (let ((marker-point (point))
            (inhibit-read-only t))
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay 'before-string "@@@")
          (overlay-put overlay 'face 'font-lock-comment-face))
        (let (start end)
          (loop
           repeat 25
           when (not (or (bolp) (bobp))) do (backward-char 1))
          (setf start (point))
          (goto-char marker-point)
          (loop
           repeat 25
           when (not (or (eolp) (eobp))) do (forward-char 1))
          (setf end (point))
          (let ((overlay (make-overlay start end (current-buffer))))
            (overlay-put overlay 'face 'jss-script-line-marker-face)))
        (goto-char marker-point)))
    (display-buffer (current-buffer))
    (let ((recenter-redislay t))
      (recenter nil))))

(provide 'jss-script)
