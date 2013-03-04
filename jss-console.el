(require 'cl)
(require 'eieio)
(require 'jss-prompt)

(define-derived-mode jss-console-mode jss-super-mode "JSS Console"
  "Major mode for interactiing with a remote web browser tab.

A console buffer consists of a list of messages, representing
notifications from the browser, and a prompt (which may grow to
be a list of inputs and vaules).



Messages from the server are of the form \"// status // message\"

The prompt is always a simple \"> \"

I/O requests are logged to the console and clickable.

Keys

  C-c C-r - reload tab's page
  RET - evaluate prompt or follow link
  C-c C-o - clear console

  C-c C-p - previous input
  C-c C-n - next input

"
  (add-hook 'kill-buffer-hook 'jss-console-kill nil t)
  ;; assume caller binds jss-console
  (setf jss-current-console-instance jss-console)
  
  (jss-insert-prompt (lambda (text)
                       (jss-evaluate (jss-console-tab (jss-current-console)) text)))
  (jss-console-ensure-connection)
  t)

(define-key jss-console-mode-map (kbd "C-c C-r") 'jss-console-ensure-connection)
(define-key jss-console-mode-map (kbd "C-c C-o") 'jss-console-clear-buffer)
(define-key jss-console-mode-map (kbd "C-c C-r") 'jss-console-reload-page)

(defun jss-console-mode* (console)
  (let ((jss-console console))
    (jss-console-mode)))

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
          (jss-console-mode* console))
        console)))

(defun jss-console-ensure-connection ()
  (interactive)
  (unless (jss-current-console)
    (error "No current console object. Can't open console here."))
  (unless (jss-tab-connected-p (jss-console-tab (jss-current-console)))
    (jss-console-insert-message (jss-current-console) "// info // Connecting...")
    (lexical-let ((buf (current-buffer)))
      (jss-deferred-add-backs
        (jss-tab-connect (jss-console-tab (jss-current-console)))
        (lambda (tab)
          (with-current-buffer buf
            (jss-console-insert-message (jss-current-console) "// info // Connected.")))))))

(defun jss-console-kill ()
  (interactive)
  (jss-console-cleanup (jss-current-console))
  (jss-current-console))

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
        (jss-before-last-prompt)
        (let ((start (point))
              (inhibit-read-only t))
          (insert (apply 'format format-string format-args) "\n")
          (unless (getf properties 'read-only)
            (setf properties (list* 'read-only t properties)))
          (add-text-properties start (point) properties))))))

(defmethod jss-console-insert-io-line ((console jss-generic-console) io)
  (with-current-buffer (jss-console-buffer console)
    (save-excursion
      (jss-before-last-prompt)
      (jss-wrap-with-text-properties `(jss-io-id ,(jss-io-id io) read-only t)
        (let ((inhibit-read-only t))
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
          (insert (jss-io-id io) " ")
          (jss-insert-button (jss-limit-string-length (jss-io-request-url io) 80)
                             'jss-console-switch-to-io-inspector)
          (insert "\n"))))))

(defmethod jss-console-insert-request ((console jss-generic-console) io)
  (jss-console-insert-io-line console io))

(defmethod jss-console-update-request-message ((console jss-generic-console) io)
  (with-current-buffer (jss-console-buffer console)
    (jss-delete-property-block 'jss-io-id (jss-io-id io))
    (jss-console-insert-io-line console io)))

(defun jss-console-clear-buffer ()
  (interactive)
  (let ((console (jss-current-console)))
    (jss-console-clear console)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eql 'jss-io-mode major-mode)
                   (jss-current-io)
                   (eql (jss-console-tab console) (jss-io-tab (jss-current-io))))
          (kill-buffer buf))))
    
    (jss-before-last-prompt)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point)))
    (goto-char (point-max))))

(defun jss-console-reload-page ()
  (interactive)
  (lexical-let ((tab (jss-current-tab)))
    (jss-deferred-add-backs
      (jss-tab-reload tab)
      (lambda (response)
        (jss-console-format-message (jss-tab-console tab) "Triggered page reload.")))))

(provide 'jss-console)
