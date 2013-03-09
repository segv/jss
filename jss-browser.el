(require 'cl)
(require 'eieio)
(require 'jss-browser-api)

(define-derived-mode jss-browser-mode jss-super-mode "JSS Browser"
  "This is mode used by buffers created with jss-connect. It serves,
mainly, to list the tabs in the browser that can be debugged.

The first specifies the backend, `webkit` or `firefox` used to
communicate with the browser and the host and port we're
currently connected to (or trying to connect to).

For each tab we list the currently visited url and provide a
button, \"[open console]\" which jumps to the corresponding tab's
console buffer.

The browser mode also displays a list of links to the jss
doucementation.

Note: While the browser buffer attempts to keep itself in sync
with the state of the browser (by refreshing when opening and
closing jss consoles), it is possible for changes to be made in
the browser which aren't communicated to jss. For this reason
manually running jss-browser-mode-refresh (usually bound to
\"g\") will required from time to time."
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

(defun jss-browser-refresh (browser)
  (with-current-buffer (jss-browser-buffer browser)
    (jss-browser-mode-refresh)))

(defun jss-browser-mode-refresh ()
  (interactive)
  (setf buffer-read-only t
        (jss-browser-buffer (jss-current-browser)) (current-buffer))
  (let ((inhibit-read-only t))
    (jss-browser-delete-and-insert-header)
    (insert (format "[ Connecting to %s:%s... ]"
                    (jss-browser-host (jss-current-browser))
                    (jss-browser-port (jss-current-browser)))))

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
                 (jss-browser-insert-help-topics)
                 (goto-char (point-min))
                 (jss-next-button))
             (insert "No debuggable tabs found.")
             (jss-browser-insert-help-topics)
             (goto-char (point-min))))))
     (lambda (error)
       (with-current-buffer jss-browser-buffer
         (let ((inhibit-read-only t))
           (jss-browser-delete-and-insert-header)
           (insert "\nConnection error:\n\n" (prin1-to-string error))
           (jss-browser-insert-help-topics)
           (goto-char (point-min))
           (signal (first error) (rest error) )))))))

(defun jss-browser-insert-help-topics ()
  (insert "\n\n")
  (insert "JSS Help:\n")
  (dolist (help-topic '(("browser mode" jss-browser-mode)
                        ("console mode" jss-console-mode)
                        ("debugger mode" jss-debugger-mode)
                        ("remote-objects" jss-insert-remote-value)
                        ("the prompt" jss-insert-prompt)))
    (insert "  ")
           
    (jss-insert-button (first help-topic) `(lambda ()
                                             (interactive)
                                             (describe-function ',(second help-topic))))
    (insert "\n")))

(defclass jss-browser-connection-details ()
  ((label         :accessor jss-browser-spec-label :initarg :label)
   (browser-class :accessor jss-browser-spec-class :initarg :class)
   (default-host  :accessor jss-browser-spec-default-host :initarg :default-host :initform "127.0.0.1")
   (default-port  :accessor jss-browser-spec-default-port :initarg :default-port)))

(defcustom jss-browsers
  (list (make-instance 'jss-browser-connection-details
                      :label "webkit"
                      :default-port "9222"
                      :class 'jss-webkit-browser)
        (make-instance 'jss-browser-connection-details
                       :label "firefox"
                       :default-port "6000"
                       :class 'jss-firefox-browser))
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
                                                  (cons (jss-browser-spec-label browser-spec) browser-spec))
                                                jss-browsers)
                                        nil
                                        t
                                        (first jss-connect/select-browser-history)
                                        'jss-connect/select-browser-history))))
  (let ((browser-spec (find browser-label jss-browsers :key 'jss-browser-spec-label :test 'string=)))
    (assert browser-spec nil "Unable to find browser named %s" browser-spec)
    (let ((host (read-from-minibuffer "Host: " (jss-browser-spec-default-host browser-spec)))
          (port (read-from-minibuffer "Port: " (jss-browser-spec-default-port browser-spec))))
      (with-current-buffer (get-buffer-create (format "*JSS Webkit @%s:%s*" host port))
        (switch-to-buffer (current-buffer))
        (jss-browser-mode* (make-instance (jss-browser-spec-class browser-spec)
                                          :host host
                                          :port port))))))

(provide 'jss-browser)
