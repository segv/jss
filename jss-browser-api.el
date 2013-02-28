;;; API (super classes and generic functions) for interacting with browsers, tabs, stacks, etc.

(require 'cl)
(require 'eieio)

(defclass jss-generic-browser ()
  ())

(defgeneric jss-browser-get-tabs (browser)
  "Refreshes the list of availale tabs.

Since we store references to tab objects in various buffers it is
important that this method modify, but no recreate, any already
existing tab objects.")

(defgeneric jss-browser-description (browser))

(defgeneric jss-browser-tabs (browser))

(defgeneric jss-browser-find-tab (browser tab-id))

(defgeneric jss-browser-register-tab (browser tab))

(defclass jss-generic-tab ()
  ((browser :initarg :browser :accessor jss-tab-browser)
   (console :initform nil :accessor jss-tab-console)
   (io :initform (make-hash-table :test 'equal)
       :accessor jss-tab-io)))

(make-variable-buffer-local
 (defvar jss-current-tab-instance nil))

(defun jss-current-tab ()
  (or jss-current-tab-instance
      (if (jss-current-console)
          (jss-console-tab (jss-current-console))
        nil)))

(defgeneric jss-tab-debugger-p (tab))

(defgeneric jss-tab-title (tab))

(defgeneric jss-tab-url (tab))

(defgeneric jss-tab-connected-p (tab))

(defgeneric jss-tab-connect (tab))

(defgeneric jss-tab-disconnect (tab))

(defgeneric jss-tab-make-console (tab &rest initargs))

(defgeneric jss-tab-open-debugger (tab debugger))

(defclass jss-generic-console ()
  ((tab :initarg :tab
        :initform nil
        :accessor jss-console-tab)))

(make-variable-buffer-local
 (defvar jss-current-console-instance nil))

(defun jss-current-console ()
  jss-current-console-instance)

(defgeneric jss-console-buffer (console))

(defmethod jss-console-buffer ((console jss-generic-console))
  (get-buffer-create
   (format "*JSS-Console/%s*" (jss-tab-id (jss-console-tab console)))))

(defgeneric jss-console-insert-io (console io))

(defclass jss-generic-io ()
  ((start-time :accessor jss-io-start :initarg :start-time)
   (lifecycle :initform '() :accessor jss-io-lifecycle :initarg :lifecycle)
   (buffer :initform nil :accessor jss-io-buffer)))

(defgeneric jss-io-uid (io))

(defgeneric jss-io-request-headers (io))

(defgeneric jss-io-response-headers (io))

(defgeneric jss-io-request-method (io))

(defgeneric jss-io-request-url (io))

(defgeneric jss-io-response-status (io)
  "Either an integer specifying the status code or nil specifying that we're still waiting for the response.")

(defgeneric jss-tab-get-io (tab io-id))

(defmethod jss-tab-get-io ((tab jss-generic-tab) io-id)
  (gethash io-id (jss-tab-io tab)))

(defgeneric jss-tab-register-io (tab io-id io-object))

(defmethod jss-tab-register-io ((tab jss-generic-tab) io-id io-object)
  (setf (gethash io-id (jss-tab-io tab)) io-object))

(defmethod jss-io-buffer-name ((io jss-generic-io))
  (or (slot-value io 'buffer)
      (setf (slot-value io 'buffer) (get-buffer-create (format "*JSS IO %s*" (jss-io-uid io))))))

(defclass jss-generic-debugger ()
  ((buffer :accessor jss-debugger-buffer)
   (tab    :accessor jss-debugger-tab :initarg :tab)))

(defgeneric jss-debugger-stack-frames (debugger))

(defgeneric jss-debugger-message (debugger))

;;; nb: do NOT name the debugger parameter debugger. it messes with emacs in strange ways.
(defmethod jss-tab-open-debugger ((tab jss-generic-tab) dbg)
  (setf (jss-debugger-buffer dbg) (get-buffer-create (generate-new-buffer-name "*JSS Debugger*"))
        (jss-debugger-tab dbg) tab)
  (with-current-buffer (jss-debugger-buffer dbg)
    (let ((jss-debugger dbg))
      (jss-debugger-mode))
    (switch-to-buffer-other-window (current-buffer))))

(defclass jss-generic-stack-frame ()
  ())

(defgeneric jss-frame-function-name (frame))

(defgeneric jss-frame-source-url (frame))

(defgeneric jss-frame-source-position (frame))

(provide 'jss-browser-api)

