(require 'cl)
(require 'eieio)
(require 'jss-browser-api)

(define-derived-mode jss-browser-mode jss-super-mode "JSS Browser"
  "Major mode for listing information about a browser, mainly a list of available tabs.

After connecting to a specific browser instance, via
`jss-connect`, this mode presents a list of open tabs and the
ability to attach a console to a particular tab."
  ;; bound by caller
  (setf jss-current-browser-instance jss-browser)
  (jss-browser-mode-refresh))

(defun jss-browser-mode* (browser)
  (let ((jss-browser browser))
    (jss-browser-mode)))

(define-key jss-browser-mode-map (kbd "g") 'jss-browser-mode-refresh)

(make-variable-buffer-local
 (defvar jss-current-browser-instance nil))

(defun jss-current-browser ()
  jss-current-browser-instance)

(defun jss-browser-delete-and-insert-header ()
  (widen)
  (delete-region (point-min) (point-max))
  (insert (jss-browser-description (jss-current-browser)) "\n\n"))

(defun jss-browser-mode-refresh ()
  (interactive)
  (setf buffer-read-only t)
  
  (let ((inhibit-read-only t))
    (jss-browser-delete-and-insert-header)
    (insert "[ Connecting... ]"))

  (lexical-let ((jss-browser-buffer (current-buffer)))
    (jss-deferred-add-backs
     (jss-browser-get-tabs (jss-current-browser))
     (lambda (browser)
       (with-current-buffer jss-browser-buffer
         (let ((inhibit-read-only t))
           (jss-browser-delete-and-insert-header)
           (if (jss-browser-tabs browser)
               (progn
                 (dolist (tab (jss-browser-tabs browser))
                   (insert (format "%s.%s - %s\n" (jss-tab-id tab) (jss-tab-title tab) (jss-tab-url tab)))
                   (when (jss-tab-debugger-p tab)
                     (insert "  ")
                     (jss-insert-button (if (jss-tab-console tab)
                                            "[ goto console ]"
                                          "[ open console ]")
                                         'jss-tab-goto-console
                                         :other-properties (list 'jss-tab-id (jss-tab-id tab)))
                     (insert "\n")))
                 (goto-char (point-min))
                 (jss-next-button))
             (insert "No tabs found.")))))
     (lambda (message)
       (with-current-buffer jss-browser-buffer
         (let ((inhibit-read-only t))
           (jss-browser-delete-and-insert-header)
           (insert "\nConnection error: " message)))))))

(defstruct jss-browser
  connector
  label)

(defcustom jss-browsers
  (list (make-jss-browser :connector 'jss-chrome-connect
                          :label "Google Chrome")
        (make-jss-browser :connector 'jss-firefox-connect
                          :label "Firefox"))
  "List of known browsers"
  :group 'jss)

(defcustom jss-browser-default-host "127.0.0.1"
  "Default port for the browser's debugging api."
  :group 'jss)

(defvar jss-connect/select-browser-history '())

(defun jss-connect (browser-label)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Browser: "
                                        (mapcar (lambda (browser-spec)
                                                  (cons (jss-browser-label browser-spec) browser-spec))
                                                jss-browsers)
                                        nil
                                        t
                                        (first jss-connect/select-browser-history)
                                        'jss-connect/select-browser-history))))
  (let ((browser-spec (find browser-label jss-browsers :key 'jss-browser-label :test 'string=)))
    (assert browser-spec nil "Unable to find browser named %s" browser-spec)
    (call-interactively (jss-browser-connector browser-spec))))

(provide 'jss-browser)
