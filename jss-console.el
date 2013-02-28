(require 'cl)
(require 'eieio)

(define-derived-mode jss-console-mode jss-super-mode "JSS Console"
  "Mode for interactiing with a remote javascirpt console."
  (add-hook 'kill-buffer-hook 'jss-console-kill)
  ;; assume caller bind jss-console
  (setf jss-current-console-instance jss-console)
  
  (jss-console-insert-prompt)
  (jss-console-ensure-connection)
  t)

(define-key jss-console-mode-map (kbd "C-c C-r") 'jss-console-ensure-connection)
(define-key jss-console-mode-map (kbd "RET") 'jss-console-send-or-newline)
(define-key jss-console-mode-map (kbd "C-a") 'jss-console-beginning-of-line)
(define-key jss-console-mode-map (kbd "C-c C-o") 'jss-console-clear-buffer)
(define-key jss-console-mode-map (kbd "C-c C-r") 'jss-console-reload-page)

(defun jss-current-tab ()
  (or jss-current-tab-instance
      (if (jss-current-console)
          (jss-console-tab (jss-current-console))
          nil)))

(defun jss-tab-goto-console (&optional tab)
  (interactive (list (let ((tab-id (get-text-property (point) 'jss-tab-id)))
                       (unless tab-id
                         (error "No tab at point."))
                       (let ((tab (jss-browser-find-tab (jss-current-browser) tab-id)))
                         (unless tab
                           (error "Unable to find tab with id %s in current browser (%s)" tab-id (jss-current-browser)))
                         tab))))
  (switch-to-buffer
   (jss-console-buffer
    (jss-tab-ensure-console tab))))

(defmethod jss-tab-ensure-console ((tab jss-generic-tab))
  (or (jss-tab-console tab)
      (let ((console (jss-tab-make-console tab :tab tab)))
        (setf (jss-tab-console tab) console)
        (with-current-buffer (jss-console-buffer console)
          (let ((jss-console console))
            (jss-console-mode)))
        console)))

(defun jss-console-insert-prompt (&rest other-properties)
  (let ((inhibit-read-only t)
        (start (point)))
    (insert "> ")
    (add-text-properties start (point)
                         (list*
                          'read-only t
                          'jss-console-prompt t
                          'rear-nonsticky t
                          other-properties))))

(defun jss-console-ensure-connection ()
  (interactive)
  (unless (jss-current-console)
    (error "No current console object. Can't open console here."))
  (unless (jss-tab-connected-p (jss-console-tab (jss-current-console)))
    (jss-console-insert-message (jss-current-console) "// info // Connecting...")
    (lexical-let ((buf (current-buffer)))
      (jss-deferred-then
        (jss-tab-connect (jss-console-tab (jss-current-console)))
        (lambda (tab)
          (with-current-buffer buf
            (jss-console-insert-message (jss-current-console) "// info // Connected.")))))))

(defun jss-have-next-property-block (property-name)
  (or (get-text-property (point) property-name)
      (next-single-property-change (point) property-name)))

(defun jss-have-previous-property-block (property-name)
  (or (get-text-property (point) property-name)
      (previous-single-property-change (point) property-name)))

(defun jss-start-of-next-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-start-of-current-property-block property-name)))
    (let ((next-change (next-single-property-change (point) property-name)))
      (when next-change
        (return (goto-char next-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-max))
          (error "Unable to find start of next block with property %s" property-name))
        (forward-char 1))
      (return (point)))))

(defun jss-end-of-previous-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-end-of-current-property-block property-name)))

    (let ((previous-change (if (eobp) ;; previous-single-property-change works differently at eobp, a char by char search is easier
                               nil
                             (previous-single-property-change (point) property-name))))
      (when previous-change
        (return (goto-char previous-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-min))
          (error "Unable to find previous block with property %s" property-name))
        (backward-char 1))
      (return (point)))))

(defun jss-start-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get start of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-min))
        (return))
      (backward-char 1))
    (forward-char 1))
  (point))

(defun jss-end-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get end of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-max))
        (return))
      (forward-char 1)))
  (point))

(defun jss-console-delete-property-block (property-name property-value)
  (save-excursion
    (goto-char (point-max))
    (let (block-start block-end)

      (while (not (equal (get-text-property (point) property-name) property-value))
        (when (= (point) (point-min))
          (error "Unable to find block with property %s equal to %s." property-name property-value))
        (backward-char 1))
      (setf block-end (min (1+ (point)) (point-max)))

      (block nil
        (while (and (equal (get-text-property (point) property-name) property-value)
                    (< (point-min) (point)))
          (backward-char 1)))
      (setf block-start (min (1+ (point)) (point-max)))
      
      (let ((inhibit-read-only t))
        (delete-region block-start block-end)))))

(defun jss-console-before-last-prompt ()
  (jss-console-after-last-prompt)
  (backward-char 1)
  (jss-start-of-current-property-block 'jss-console-prompt))

(defun jss-console-after-last-prompt ()
  (goto-char (point-max))
  (jss-end-of-previous-property-block 'jss-console-prompt))

(defun jss-console-kill ()
  (interactive)
  (let ((console (jss-current-console)))
    (when console
      (setf (jss-tab-console (jss-console-tab console)) nil
            (jss-console-tab console) nil))
    console))

(defun jss-console-beginning-of-line (&optional n)
  (interactive "P")
  (beginning-of-line n)
  (when (get-text-property (point) 'jss-console-prompt)
    (goto-char (or (next-single-property-change (point)' jss-console-prompt)
                   (point-max)))))

(defun jss-console-send-or-newline ()
  (interactive)
  (cond
   ((get-text-property (point) 'read-only)
    t)
   (t
    (let ((start (save-excursion (or (jss-console-after-last-prompt)
                                     (point-max))))
          (end   (point-max)))
      (save-restriction
        (narrow-to-region start end)
        (let ((js2-errors (js2-ast-root-errors (js2-parse)))
              (inhibit-read-only t))
          (if js2-errors    
              (progn
                (message "Syntax errors in input (%s)" js2-errors)
                (insert "\n")
                (js2-indent-line))
            (lexical-let ((input (buffer-substring-no-properties (point-min) (point-max)))
                          (console (jss-current-console)))
              (insert "\n")
              (add-text-properties (point-min) (point-max) '(read-only t rear-nonsticky t))
              (jss-console-insert-prompt)
              
              (jss-deferred-then
                (jss-console-evaluate console input)
                (lambda (response)
                  (jss-console-insert-message console response)))))))))))

(defmethod jss-console-insert-message ((console jss-generic-console) message-text &rest other-properties)
  (jss-console-format-message console "%s" message-text :properties other-properties))

(defmethod jss-console-format-message ((console jss-generic-console) format-string &rest format-args-and-properties)
  (let ((properties nil)
        (format-args nil))
    (if (member :properties format-args-and-properties)
        (loop
         for head on format-args-and-properties
         if (eql (car head) :properties)
         do (setf properties (second head)
                  head (cdr head))
         else do (push (car head) format-args)
         finally (setf format-args (reverse format-args)))
      (setf format-args format-args-and-properties))
    (with-current-buffer (jss-console-buffer console)
      (save-excursion
        (jss-console-before-last-prompt)
        (let ((start (point))
              (inhibit-read-only t))
          (insert (apply 'format format-string format-args) "\n")
          (when properties
            (unless (getf properties 'read-only)
              (setf properties (list* 'read-only t properties)))
            (add-text-properties start (point) properties)))))))

(defun jss-limit-string-length (string max-length)
  (if (< max-length (length string))
      (format "%s...[snip]...%s"
              (substring string 0 (/ max-length 2))
              (substring string (- (length string) (/ max-length 2)) (length string)))
      string))

(defmethod jss-console-insert-io-line ((console jss-generic-console) io)
  (with-current-buffer (jss-console-buffer console)
    (save-excursion
      (jss-console-before-last-prompt)
      (let ((start (point))
            (inhibit-read-only t))
        (insert "// log // "
                (ecase (first (first (jss-io-lifecycle io)))
                  (:sent "Requested")
                  (:loading-finished "Loaded")
                  (:data-received "Data for")
                  (:loading-failed "Failed")
                  (:served-from-cache "From cache")
                  (:served-from-memory-cache "From memory cache")
                  (:response-received "Got response"))
                " ")
        (let ((button-start (point)))
          (insert (jss-limit-string-length (jss-io-request-url io) 80))
          (make-text-button button-start (point)
                            'action (lambda (button)
                                      (call-interactively 'jss-console-switch-to-io-inspector))))
        
        (insert "\n")
        (add-text-properties start (point)
                             (list 'read-only t
                                   'jss-io-id (jss-io-uid io)))))))

(defmethod jss-console-insert-request ((console jss-generic-console) io)
  (jss-console-insert-io-line console io))

(defmethod jss-console-update-request-message ((console jss-generic-console) io)
  (with-current-buffer (jss-console-buffer console)
    (jss-console-delete-property-block 'jss-io-id (jss-io-uid io))
    (jss-console-insert-io-line console io)))

(defun jss-console-clear-buffer ()
  (interactive)
  (let ((console (jss-current-console)))
    (jss-console-clear console)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eql 'jss-network-inspector-mode major-mode)
                   (jss-current-io)
                   (eql (jss-console-tab console) (jss-io-tab (jss-current-io))))
          (kill-buffer buf))))
    
    (jss-console-before-last-prompt)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point)))
    
    (jss-console-after-last-prompt)
    (unless (eobp)
      (forward-char 1))))

(defun jss-console-reload-page ()
  (interactive)
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-then
      (jss-tab-reload tab)
      (lambda (response)
        (jss-console-format-message (jss-tab-console tab) "Triggered page reload.")
        response))))

(provide 'jss-console)
