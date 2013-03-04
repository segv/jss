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

(defclass jss-generic-tab ()
  ((browser :initarg :browser :accessor jss-tab-browser)
   (console :initform nil :accessor jss-tab-console)
   (ios :initform (make-hash-table :test 'equal)
        :accessor jss-tab-ios)
   (scripts :initform (make-hash-table :test 'equal)
            :accessor jss-tab-scripts)))

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

(defgeneric jss-tab-object-properties (tab object-id))

(defgeneric jss-tab-get-script (tab script-id))

(defmethod jss-tab-get-script ((tab jss-generic-tab) script-id)
  (gethash script-id (jss-tab-scripts tab)))

(defgeneric jss-tab-set-script (tab script-id script))
(defsetf jss-tab-get-script jss-tab-set-script)

(defmethod jss-tab-set-script ((tab jss-generic-tab) script-id script)
  (setf (gethash script-id (jss-tab-scripts tab)) script))

(defgeneric jss-evaluate (context text))

(defclass jss-generic-console ()
  ((tab :initarg :tab
        :initform nil
        :accessor jss-console-tab)))

(make-variable-buffer-local
 (defvar jss-current-console-instance nil))

(defun jss-current-console ()
  jss-current-console-instance)

(defgeneric jss-console-clear (console))

(defgeneric jss-console-buffer (console))

(defmethod jss-console-buffer ((console jss-generic-console))
  (get-buffer-create
   (format "*JSS Console/%s*" (jss-tab-id (jss-console-tab console)))))

(defgeneric jss-console-insert-io (console io))

(defgeneric jss-console-cleanup (console))

(defmethod jss-console-cleanup :after ((console jss-generic-console))
  (setf (jss-tab-console (jss-console-tab console)) nil
        (jss-console-tab console) nil))

(defclass jss-generic-io ()
  ((tab :accessor jss-io-tab :initform nil)
   (start-time :accessor jss-io-start :initarg :start-time)
   (lifecycle :initform '() :accessor jss-io-lifecycle :initarg :lifecycle)
   (buffer :initform nil :accessor jss-io-buffer)))

(defgeneric jss-io-id (io))

(defgeneric jss-io-request-headers (io))

(defgeneric jss-io-response-headers (io))

(defgeneric jss-io-request-method (io))

(defgeneric jss-io-request-url (io))

(defgeneric jss-io-response-status (io)
  "Either an integer specifying the status code or nil specifying that we're still waiting for the response.")

(defgeneric jss-io-response-content-type (io))

(defgeneric jss-io-response-content-length (io))

(defgeneric jss-tab-get-io (tab io-id))

(defmethod jss-tab-get-io ((tab jss-generic-tab) io-id)
  (gethash io-id (jss-tab-ios tab)))

(defmethod jss-tab-set-io ((tab jss-generic-tab) io-id io-object)
  (if (null (jss-io-tab io-object))
      (setf (jss-io-tab io-object) tab
            (gethash io-id (jss-tab-ios tab)) io-object)
    (unless (eq tab (jss-io-tab io-object))
      (error "Attempt to add IO %s to tab %s, but it's already registered with %s."
             io-object tab (jss-io-tab io-object)))))

(defsetf jss-tab-get-io jss-tab-set-io)

(defgeneric jss-tab-unregister-io (tab io-id io-object))

(defmethod jss-tab-unregister-io ((tab jss-generic-tab) io)
  (remhash (jss-io-id io) (jss-tab-ios tab))
  io)

(defmethod jss-io-buffer-name ((io jss-generic-io))
  (or (slot-value io 'buffer)
      (setf (slot-value io 'buffer) (get-buffer-create (format "*JSS IO %s*" (jss-io-id io))))))

(defclass jss-generic-debugger ()
  ((buffer :accessor jss-debugger-buffer)
   (tab    :accessor jss-debugger-tab :initarg :tab)))

(defgeneric jss-debugger-stack-frames (debugger))

(defgeneric jss-debugger-exception (debugger))

(defgeneric jss-debugger-resume    (debugger))
(defgeneric jss-debugger-step-into (debugger))
(defgeneric jss-debugger-step-over (debugger))
(defgeneric jss-debugger-step-out  (debugger))

;;; nb: do NOT name the debugger parameter debugger. it messes with emacs in strange ways.
(defmethod jss-tab-open-debugger ((tab jss-generic-tab) dbg)
  (setf (jss-debugger-buffer dbg) (get-buffer-create (generate-new-buffer-name "*JSS Debugger*"))
        (jss-debugger-tab dbg) tab)
  (with-current-buffer (jss-debugger-buffer dbg)
    (jss-debugger-mode* dbg)
    (switch-to-buffer-other-window (current-buffer))))

(defgeneric jss-debugger-cleanup (debugger))

(defmethod jss-debugger-cleanup ((debugger jss-generic-debugger))
  t)

(defclass jss-generic-stack-frame ()
  ((debugger :initarg :debugger :accessor jss-frame-debugger)))

(defgeneric jss-frame-function-name (frame))

(defgeneric jss-frame-source-position (frame))

(defgeneric jss-frame-restart (frame))

(defvar jss-remote-value-counter 0)

(defclass jss-generic-remote-value ()
  ((id :accessor jss-remote-value-id
       :initform (incf jss-remote-value-counter)
       :initarg :id)))

(defgeneric jss-remote-value-string (remote-object))

(defclass jss-generic-remote-primitive (jss-generic-remote-value)
  ((value :initarg :value :accessor jss-remote-primitive-value)))

(defclass jss-generic-remote-boolean (jss-generic-remote-primitive) ())

(defclass jss-generic-remote-true (jss-generic-remote-boolean) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-true)) "true")

(defclass jss-generic-remote-false (jss-generic-remote-boolean) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-false)) "false")

(defclass jss-generic-remote-string (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((string jss-generic-remote-string))
  (prin1-to-string (jss-remote-primitive-value string)))

(defclass jss-generic-remote-number (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((number jss-generic-remote-number))
  (let ((value (jss-remote-primitive-value number)))
    (if (integerp value)
        (format "%d" value)
      (format "%g" value))))

(defclass jss-generic-remote-NaN (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-NaN)) "NaN")

(defclass jss-generic-remote-infinitiy (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-infinitiy)) "+Inf")

(defclass jss-generic-remote-undefined (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-undefined)) "undefined")

(defclass jss-generic-remote-no-value (jss-generic-remote-primitive) ())
(defmethod jss-remote-value-string ((object jss-generic-remote-no-value)) "no value.")

(defclass jss-generic-remote-non-primitive (jss-generic-remote-value) ())

(defclass jss-generic-remote-object (jss-generic-remote-non-primitive) ())

(defmethod jss-remote-value-string ((object jss-generic-remote-object))
  (let ((class-name  (jss-remote-object-class-name object))
        (label  (jss-remote-object-label object)))
    (if (string= label class-name)
        (format "[%s]" label)
      (format "[%s %s]" class-name label))))

(defmethod jss-remote-object-get-properties (object))

(defclass jss-generic-remote-function (jss-generic-remote-non-primitive) ())

(defclass jss-generic-remote-array (jss-generic-remote-object) ())

(provide 'jss-browser-api)

