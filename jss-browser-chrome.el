;;; https://developers.google.com/chrome-developer-tools/docs/protocol/1.0/debugger#events
(require 'url)
(require 'websocket)
(require 'json)

(require 'jss-browser-api)
(require 'jss-deferred)

;;; Browser connector function

(defun jss-chrome-connect (host port)
  (interactive (list (read-from-minibuffer "Host: " jss-browser-default-host)
                     (read-from-minibuffer "Post: " "9222")))
  (with-current-buffer (get-buffer-create (format "*Google Chrome@%s:%s*" host port))
    (switch-to-buffer (current-buffer))
    (let ((jss-browser (make-instance 'jss-chrome-browser
                                      :host host
                                      :port port)))
      (jss-browser-mode))))

;;; The browser API implementation

(defclass jss-chrome-browser (jss-generic-browser)
  ((host :initarg :host)
   (port :initarg :port)
   (tabs :initform '())))

(defmethod jss-browser-description ((browser jss-chrome-browser))
  (format "Google Chrome @ %s:%s\nNB: Only displaying tabs that can be debugged." (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-chrome-remote-debugging-url ((browser jss-chrome-browser))
  (format "http://%s:%s/json" (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-browser-tabs ((browser jss-chrome-browser))
  (mapcar 'cdr (slot-value browser 'tabs)))

(defmethod jss-browser-find-tab ((browser jss-chrome-browser) tab-id)
  (cdr (cl-assoc tab-id (slot-value browser 'tabs) :test 'string=)))

(defmethod jss-browser-get-tabs ((browser jss-chrome-browser))
  (lexical-let ((d (make-jss-deferred))
                (browser browser))
    (url-retrieve
     (jss-chrome-remote-debugging-url browser)
     (lambda (status)
       (if status
           (if (getf status :error)
               (jss-deferred-errorback d (getf status :error))
             (jss-deferred-errorback d (format "Unrecognized error: %s" (prin1-to-string status))))
         (progn
           (widen)
           (jss-log-event (list "google chrome GET"
                                (jss-chrome-remote-debugging-url browser)
                                (buffer-substring-no-properties (point-min) (point-max))))
           (goto-char (point-min))
           (if (save-match-data
                 (looking-at "^HTTP/1\\.1 200 OK$"))
               (progn
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (let ((tab-data (json-read)))
                   (loop
                    with new-tabs = '()
                    for tab-data across tab-data
                    for debugger-url = (cdr (assoc 'webSocketDebuggerUrl tab-data))
                    when debugger-url
                    do (let* ((tab (make-instance 'jss-chrome-tab
                                                  :json-data tab-data
                                                  :browser browser
                                                  :debugger-url debugger-url) )
                              (tab-id (jss-tab-id tab))
                              (existing-tab (jss-browser-find-tab browser tab-id)))
                         (when existing-tab
                           (setf (slot-value tab 'json-data) tab-data))
                         (push (cons tab-id tab) new-tabs))
                    finally (setf (slot-value browser 'tabs) new-tabs)
                    finally (jss-deferred-callback d browser))))
             (let ((status (save-match-data
                             (if (looking-at "^HTTP/1\\.1 \\(.*\\)$")
                                 (match-string 1)
                               "No status received."))))
               (jss-deferred-errorback d (format "Bad status: %s" (prin1-to-string status)))))))))
    d))

;;; The tab API implementation

(defclass jss-chrome-tab (jss-generic-tab)
  ((json-data :initarg :json-data)
   (debugger-url :initarg :debugger-url)
   (websocket :initform nil)
   (request-counter :initform 0)
   (requests :initform (make-hash-table :test 'equal))
   (scripts :initform (make-hash-table :test 'equal)))) 

(defmethod jss-tab-debugger-p ((tab jss-chrome-tab))
  (slot-value tab 'debugger-url))

(defmethod jss-chrome-tab-json-prop ((tab jss-chrome-tab) prop-name)
  (cdr (assoc prop-name (slot-value tab 'json-data))))

(defmethod jss-tab-title ((tab jss-chrome-tab))
  (jss-chrome-tab-json-prop tab 'title))

(defmethod jss-tab-url ((tab jss-chrome-tab))
  (jss-chrome-tab-json-prop tab 'url))

(defmethod jss-tab-id ((tab jss-chrome-tab))
  (save-match-data
    (let ((debugger-url (jss-chrome-tab-json-prop tab 'webSocketDebuggerUrl)))
      (if debugger-url
          (if (string-match "^ws://.*devtools/page/\\(.*\\)$" debugger-url)
              (match-string 1 debugger-url)
            debugger-url)
        (error "Can not compute id for %s." tab)))))

(defmethod jss-chrome-tab-debugger-url ((tab jss-chrome-tab))
  (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data))))

(defmethod jss-tab-reload ((tab jss-chrome-tab))
  (jss-chrome-send-request tab '("Page.reload")))

(defmethod jss-tab-connected-p ((tab jss-chrome-tab))
  (slot-value tab 'websocket))

(defmethod jss-tab-connect ((tab jss-chrome-tab))
  (when (slot-value tab 'websocket)
    (error "Already connected. Disconnect first."))
  (lexical-let ((tab tab)
                (debugger-url (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data)))))
    (jss-log-event (list :websocket debugger-url :open))
    (lexical-let* ((ws-open (make-jss-deferred))
                   (socket (websocket-open debugger-url
                                           :on-message (lambda (websocket frame)
                                                         (jss-chrome-tab-websocket/on-message tab websocket frame))
                                           :on-open (lambda (websocket)
                                                      (jss-chrome-tab-websocket/on-open tab websocket)
                                                      (jss-deferred-callback ws-open tab))
                                           :on-close (lambda (websocket)
                                                       (jss-chrome-tab-websocket/on-close tab websocket))
                                           :on-error (lambda (websocket action error)
                                                       (jss-chrome-tab-websocket/on-error tab websocket action error)
                                                       (jss-deferred-errorback ws-open (list websocket action error))))))
      (setf (slot-value tab 'websocket) socket)
      
      (lexical-let* ((connect-deferred (make-jss-deferred)))
        ;; setup a deferred chain to send Console.enable after opening the ws connection
        (jss-deferred-add-backs
         ws-open
         (lambda (tab)
           (lexical-let* ((tab tab)
                          (console (jss-tab-console tab)))
             (jss-deferred-add-backs
              (jss-chrome-tab-enable tab "Console")
              (lambda (response)
                (jss-deferred-add-backs
                 (jss-deferred-wait-on-all
                  (jss-chrome-tab-enable tab "Network")
                  (jss-chrome-tab-enable tab "Debugger")
                  (jss-chrome-tab-enable tab "Page"))
                 (lambda (v)
                   (jss-deferred-add-backs
                    (jss-chrome-send-request tab '("Debugger.setPauseOnExceptions" (state . "all")))
                    (lambda (v)
                      (jss-deferred-callback connect-deferred tab)))))))))
          (lambda (v)
            (jss-deferred-errorback connect-deferred v)))
        
        connect-deferred))))

(defmethod jss-chrome-tab-enable ((tab jss-chrome-tab) domain)
  (lexical-let* ((tab tab)
                 (console (jss-tab-console tab))
                 (domain domain))
    (jss-deferred-add-backs
      (jss-chrome-send-request tab (list (format "%s.enable" domain)))
      (lambda (response)
        (jss-console-format-message console "// debug // %s enabled." domain))
      (lambda (response)
        (jss-console-format-message console "// debug // Could not enable %s: %s" domain response)))))

(defmethod jss-chrome-tab-websocket/on-message ((tab jss-chrome-tab) websocket frame)
  (jss-log-event (list :websocket
                       (jss-chrome-tab-debugger-url tab)
                       :on-message
                       websocket
                       frame))
  (let* ((message (with-temp-buffer
                    (insert (websocket-frame-payload frame))
                    (goto-char (point-min))
                    (json-read)))
         (request-id (cdr (assoc 'id message)))
         (request-deferred (gethash request-id (slot-value tab 'requests)))
         (console (jss-tab-ensure-console tab))
         (err (cdr (assoc 'error message))))
    (if err
        (if request-deferred
            (jss-deferred-errorback request-deferred err)
          (progn
            (jss-log-event (list :google-chrome
                                 (jss-chrome-tab-debugger-url tab)
                                 :unhandled-error
                                 err))
            (jss-console-format-message console
                                        "// Unhandled error: %s (%s)"
                                        (cdr (assoc 'message err))
                                        (cdr (assoc 'code    err)))))
      
      (if request-deferred
          (jss-deferred-callback request-deferred (cdr (assoc 'result message)))
        (let* ((method (cdr (assoc 'method message)))
               (handler (gethash method jss-chrome-notification-handlers)))
          (if handler
              (funcall handler
                       tab
                       (cdr (assoc 'params message))
                       message)
            (jss-log-event (list :google-chrome
                                 (jss-chrome-tab-debugger-url tab)
                                 :unknown-request-id
                                 message request-id))))))))

(defmethod jss-chrome-tab-websocket/on-open ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-open)))

(defmethod jss-chrome-tab-websocket/on-close ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-close))
  (when (jss-tab-console tab)
    (jss-console-format-message (jss-tab-console tab) "// ERROR // Remote end closed connection."))
  (setf (slot-value tab 'websocket) nil))

(defmethod jss-chrome-tab-websocket/on-error ((tab jss-chrome-tab) websocket action error)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-error websocket action error)))

(defvar jss-chrome-notification-handlers (make-hash-table :test 'equal))

(defmacro define-jss-chrome-notification-handler (name args &rest body)
  `(setf (gethash ,name jss-chrome-notification-handlers)
         (lambda (tab params message)
           (lexical-let ,(mapcar (lambda (arg-name)
                                   (list arg-name `(cdr (assoc ',arg-name params))))
                                 args)
             (lexical-let ((tab tab)
                           (console (jss-tab-console tab)))
               ,@body)))))

(defvar jss-debugger-object-group-count 0)

(defclass jss-chrome-debugger (jss-generic-debugger)
  ((object-group-id :initform (incf jss-debugger-object-group-count))
   (callFrames :initarg :callFrames :reader jss-debugger-stack-frames)
   (reason :initarg :reason)
   (data :initarg :data)))

(defmethod jss-debugger-message ((d jss-chrome-debugger))
  (format "%s %s" (slot-value d 'reason) (slot-value d 'data)))

(defmethod jss-debugger-continue ((d jss-chrome-debugger))
  (jss-deferred-add-errorback
    (jss-chrome-send-request (jss-debugger-tab d) '("Debugger.resume"))
    (lambda (err)
      (error "Failed to resume execution: %s" err))))

(defmethod jss-debugger-cleanup ((d jss-chrome-debugger))
  (lexical-let ((d d)
                (object-group (jss-chrome-object-group d)))
    (jss-deferred-add-errorback
     (jss-chrome-send-request (jss-debugger-tab d)
                              '("Runtime.releaseObjectGroup" `(objectGroup . ,object-group)))
     (lambda (err)
       (error "Failed to release object group %s on %s." object-group d)))))

(defclass jss-chrome-stack-frame (jss-generic-stack-frame)
  ((properties :initarg :properties)))

(defmethod jss-chrome-stack-frame-id ((frame jss-chrome-stack-frame))
  (cdr (assoc 'callFrameId (slot-value frame 'properties))))

(defmethod jss-frame-function-name ((frame jss-chrome-stack-frame))
  (cdr (assoc 'functionName (slot-value frame 'properties))))

(defmethod jss-frame-source-position ((frame jss-chrome-stack-frame))
  (let* ((loc (cdr (assoc 'location (slot-value frame 'properties))))
         (lineNumber (cdr (assoc 'lineNumber loc)))
         (columnNumber (cdr (assoc 'columnNumber loc)))
         (scriptId (cdr (assoc 'scriptId loc)))
         (script (jss-tab-get-script (jss-debugger-tab (jss-frame-debugger frame))
                                     scriptId))
         (url (cdr (assoc 'url script))))
    (if url
        (format "%s:%s:%s" url lineNumber columnNumber)
      nil)))

(define-jss-chrome-notification-handler "Debugger.breakpointResolved" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.globalObjectCleared" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.paused" (callFrames reason data)
  (jss-console-format-message console "// ERROR // %s" reason)
  (let ((jss-debugger (make-instance 'jss-chrome-debugger
                                     :reason reason
                                     :data (cond 
                                            ((string= "exception" reason)
                                             (let ((oid (cdr (assoc 'objectId data))))
                                               (if oid
                                                   (jss-deferred-add-backs
                                                    (jss-chrome-send-request tab `("Runtime.getProperties"
                                                                                   (objectId . ,oid)
                                                                                   (ownProperties . ,json-false)))
                                                    (lambda (response)
                                                      ;; (message "getProperties: %s" response)
                                                      ))))
                                             data)
                                            (t data))
                                     :tab tab)))

    (setf (slot-value jss-debugger 'callFrames)
          (loop
           for frame across callFrames
           collect (make-instance 'jss-chrome-stack-frame :properties frame :debugger jss-debugger)))
    
    (jss-tab-open-debugger tab jss-debugger)))

(define-jss-chrome-notification-handler "Debugger.resumed" ()
  t)

(define-jss-chrome-notification-handler "Debugger.scriptFailedToParse" (url scriptSource startLine errorLine errorMessage)
  t)

(define-jss-chrome-notification-handler "Debugger.scriptParsed" (url scriptId scriptSource startLine startColumn endLine endColumn isContentScript sourceMapURL)
  (setf (jss-tab-get-script tab scriptId) params)
  t)

(defmethod jss-chrome-send-request ((tab jss-chrome-tab) request)
  (let* ((ws (slot-value tab 'websocket))
         (request-id (incf (slot-value tab 'request-counter)))
         (payload (list* (cons 'id request-id)
                         (cons 'method (first request))
                         (when (rest request)
                           (list (cons 'params (rest request))))))
         (text (json-encode payload))
         (deferred (make-jss-deferred)))
    
    (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab)
                         payload
                         text))
    (setf (gethash request-id (slot-value tab 'requests)) deferred)
    (websocket-send-text ws text)
    deferred))

(defclass jss-chrome-console (jss-generic-console)
  ((messages :initform (make-hash-table) :accessor jss-chrome-console-messages)))

(defmethod jss-tab-make-console ((tab jss-chrome-tab) &rest initargs)
  (apply 'make-instance 'jss-chrome-console initargs))

(defmethod jss-chrome-object-group ((tab jss-chrome-tab))
  (format "jssConsoleEvaluate_%s" (jss-tab-id tab)))

(defmethod jss-chrome-object-group ((frame jss-chrome-stack-frame))
  (jss-chrome-object-group (jss-frame-debugger frame)))

(defmethod jss-chrome-object-group ((debugger jss-chrome-debugger))
  (format "jssDebuggerEvaluate_%d" (slot-value debugger 'object-group-id)))

(defmethod jss-evaluate ((tab jss-chrome-tab) js-code)
  (jss-deferred-then
   (jss-chrome-send-request tab
                            `("Runtime.evaluate"
                              (expression . ,js-code)
                              (objectGroup . ,(jss-chrome-object-group tab))))
   (lambda (result)
     (make-jss-chrome-remote-object result))
   (lambda (response)
     (make-jss-chrome-evaluation-error response))))

(defclass jss-chrome-evaluation-error ()
  ((properties :initarg :properties)))

(defun make-jss-chrome-evaluation-error (properties)
  (make-instance 'jss-chrome-evaluation-error :properties properties))

(defmethod jss-remote-value-insert ((error jss-chrome-evaluation-error))
  (insert "// error // "
          (format "code: %s; " (cdr (assoc 'code (slot-value error 'properties))))
          (format "message: %s; " (cdr (assoc 'message (slot-value error 'properties))))
          (format "data: %s; " (prin1-to-string (cdr (assoc 'data (slot-value error 'properties)))))))

(defmethod jss-evaluate ((frame jss-chrome-stack-frame) js-code)
  (jss-deferred-then
   (jss-chrome-send-request (jss-debugger-tab (jss-frame-debugger frame))
                            `("Debugger.evaluateOnCallFrame"
                              (expression . ,js-code)
                              (callFrameId . ,(jss-chrome-stack-frame-id frame))
                              (objectGroup . ,(jss-chrome-object-group frame))))
   (lambda (result)
     (make-jss-chrome-remote-object result))
   (lambda (response)
     (make-jss-chrome-evaluation-error response))))

(defclass jss-chrome-remote-value      (jss-generic-remote-value) ())
(defclass jss-chrome-remote-true       (jss-generic-remote-true) ())
(defclass jss-chrome-remote-false      (jss-generic-remote-true) ())
(defclass jss-chrome-remote-string     (jss-generic-remote-string) ())
(defclass jss-chrome-remote-number     (jss-generic-remote-number) ())
(defclass jss-chrome-remote-NaN        (jss-generic-remote-NaN) ())
(defclass jss-chrome-remote-undefined  (jss-generic-remote-undefined) ())

(defclass jss-chrome-remote-object-mixin ()
  ((description :initarg :description :accessor jss-chrome-remote-object-description)
   (className   :initarg :className   :accessor jss-chrome-remote-object-className)
   (objectId    :initarg :objectId    :accessor jss-chrome-remote-object-id)))

(defmethod jss-remote-object-class-name ((o jss-chrome-remote-object-mixin))
  (jss-chrome-remote-object-className o))

(defmethod jss-remote-object-label ((o jss-chrome-remote-object-mixin))
  (jss-chrome-remote-object-description o))

(defclass jss-chrome-remote-object (jss-generic-remote-object jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-array  (jss-generic-remote-array jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-date (jss-generic-remote-object jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-node (jss-generic-remote-object jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-null (jss-generic-remote-object jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-regexp (jss-generic-remote-object jss-chrome-remote-object-mixin) ())

(defclass jss-chrome-remote-function   (jss-generic-remote-function)
  ((description :initarg :description :accessor jss-chrome-remote-object-description)
   (objectId    :initarg :objectId    :accessor jss-chrome-remote-object-id)))

(defclass jss-chrome-remote-no-value (jss-generic-remote-no-value) ())

(defun make-jss-chrome-remote-object (result)
  (let* ((result (cdr (assoc 'result result)))
         (type (cdr (assoc 'type result)))
         (value (cdr (assoc 'value result))))
    (if result
        (cond
         ((string= type "boolean")
          (make-instance (ecase value
                           ((t) 'jss-chrome-remote-true)
                           (:json-false 'jss-chrome-remote-false))))
         
         
         ((string= type "number")
          (make-instance 'jss-chrome-remote-number :value value))
         
         ((string= type "string")
          (make-instance 'jss-chrome-remote-string :value value))
         
         ((string= type "undefined")
          (make-instance 'jss-chrome-remote-undefined))
         
         ((string= type "function")
          (make-instance 'jss-chrome-remote-function
                         :description (cdr (assoc 'description result))
                         :objectId (cdr (assoc 'objectId result))))
         
         ((string= type "object")
          (let* ((subtype (cdr (assoc 'subtype result))))
            (make-instance (cond
                            ((string= subtype "array")  'jss-chrome-remote-array)
                            ((string= subtype "date")   'jss-chrome-remote-date)
                            ((string= subtype "node")   'jss-chrome-remote-node)
                            ((string= subtype "null")   'jss-chrome-remote-null)
                            ((string= subtype "regexp") 'jss-chrome-remote-regexp)
                            (t                          'jss-chrome-remote-object))
                           :className (cdr (assoc 'className result))
                           :description (cdr (assoc 'description result))
                           :objectId (cdr (assoc 'objectId result)))))

         
         (t
          (error "Unknown result type %s" type)))
      (make-instance 'jss-chrome-remote-no-value))))

(defmethod jss-get-object-properties ((tab jss-chrome-tab) object-id)
  (jss-deferred-then (jss-chrome-send-request (jss-console-tab console)
                                              (list "Runtime.getProperties"
                                                    (cons 'objectId object-id)
                                                    (cons 'ownProperties :json-false)))
                     (lambda (response)
                       (loop for r across (cdr (assoc 'result response)) collect r))))

(defmethod jss-console-cleanup ((console jss-chrome-console))
  (jss-chrome-send-request (list "Runtime.releaseObjectGroup"
                                 (cons 'objectGroup (jss-chrome-console-object-group console))))
  t)

(define-jss-chrome-notification-handler "Console.messageAdded" (message)
  (jss-console-format-message console "// %s // %s%s"
                              (cdr (assoc 'type message))
                              (cdr (assoc 'text message))
                              (let ((url (cdr (assoc 'url message)))
                                    (line (cdr (assoc 'line message))))
                                (if url
                                    (if line
                                        (format " %s:%s" url line)
                                      (format " %s" url))
                                  ""))))

(define-jss-chrome-notification-handler "Console.messagesCleared" ()
  t)

(define-jss-chrome-notification-handler "Console.messageRepeatCountUpdated" (count)
  t)

(defclass jss-chrome-io (jss-generic-io)
  ((properties :accessor jss-chrome-io-properties :initarg :properties)
   (response :accessor jss-chrome-io-response :initform nil)
   (responseBody :accessor jss-chrome-io-responseBody :initform nil)))

(defmethod jss-io-request-headers ((io jss-chrome-io))
  (cdr (assoc 'headers (cdr (assoc 'request (jss-chrome-io-properties io))))))

(defmethod jss-io-request-method ((io jss-chrome-io))
  (cdr (assoc 'method (cdr (assoc 'request (jss-chrome-io-properties io))))))

(defmethod jss-io-request-url ((io jss-chrome-io))
  (cdr (assoc 'url (cdr (assoc 'request (jss-chrome-io-properties io))))))

(defmethod jss-io-response-headers ((io jss-chrome-io))
  (if (assoc 'redirectResponse (jss-chrome-io-properties io))
      (cdr (assoc 'headers (cdr (assoc 'redirectResponse (jss-chrome-io-properties io)))))
      (cdr (assoc 'headers (jss-chrome-io-response io)))))

(defmethod jss-io-response-content-type ((io jss-chrome-io))
  (cdr (assoc 'Content-Type (jss-io-response-headers io))))

(defmethod jss-io-response-content-length ((io jss-chrome-io))
  (cdr (assoc 'Content-Length (jss-io-response-headers io))))

(defmethod jss-io-response-data ((io jss-chrome-io))
  (if (jss-chrome-io-responseBody io)
      (let ((base64-p (cdr (assoc 'base64Encoded (jss-chrome-io-responseBody io))))
            (body (cdr (assoc 'body (jss-chrome-io-responseBody io)))))
        (if (eql :json-false base64-p)
            body
          (base64-decode-string body)))
    ;; fwiw nil means something different from "" (no data vs 0 bytes of ata)
    nil))

(defmethod jss-io-response-status ((io jss-chrome-io))
  (flet ((make-status-line (response)
                           (format "%s %s"
                                   (cdr (assoc 'status response))
                                   (cdr (assoc 'statusText response)))))
    (if (assoc 'redirectResponse (jss-chrome-io-properties io))
        (make-status-line (cdr (assoc 'redirectResponse (jss-chrome-io-properties io))))
      (make-status-line (jss-chrome-io-response io)))))

(defmethod jss-io-id ((io jss-chrome-io))
  (cdr (assoc 'requestId (jss-chrome-io-properties io))))

(define-jss-chrome-notification-handler "Network.requestWillBeSent" (requestId loaderId documentURL request timestamp initiator stackTrace redirectResponse)
  (let ((io (make-instance 'jss-chrome-io
                           :properties params
                           :start-time timestamp
                           :lifecycle (list (list :sent timestamp)))))
    (setf (jss-tab-get-io tab requestId) io)
    (jss-console-insert-request console io)))

(defmacro with-existing-io (io-id &rest body)
  `(let ((io (jss-tab-get-io tab ,io-id)))
     (if io
         (progn ,@body)
       (jss-log-event (list :chrome-io
                            :unknown-requestId
                            ,io-id)))))
(put 'with-existing-io 'lisp-indent-function 1)

(define-jss-chrome-notification-handler "Network.dataReceived" (requestId timestamp dataLength encodedDataLength)
  (with-existing-io requestId
    (push (list :data-received timestamp
                :data-length dataLength
                :encoded-data-length encodedDataLength)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.loadingFailed" (requestId timestamp errorText canceled)
  (with-existing-io requestId
    (push (list :loading-failed timestamp
                :error-text errorText
                :canceled canceled)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.loadingFinished" (requestId timestamp)
  (with-existing-io requestId
    (push (list :loading-finished timestamp)
          (jss-io-lifecycle io))
    (lexical-let ((io io)
                  (requestId requestId))
      (jss-deferred-add-backs
        (jss-chrome-send-request tab `("Network.getResponseBody" (requestId . ,requestId)))
        (lambda (response)
          (setf (jss-chrome-io-responseBody io) response))
        (lambda (response)
          ;; don't really know what else to do if this fails.
          (jss-log-event (list "Network.getResponseBody" :error requestId response)))))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.requestServedFromCache" (requestId)
  (with-existing-io requestId
    (push (list :served-from-cache nil)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.requestServedFromMemoryCache" (requestId loaderId documentURL timestamp initiator resource)
  (with-existing-io requestId
    (push (list :served-from-memory-cache timestamp
                :loader-id loaderId
                :document-url documentURL
                :initiator initiator
                :resource resource)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.responseReceived" (requestId loaderId timestamp type response)
  (with-existing-io requestId
    (push (list :response-received timestamp
                :loader-id loaderId
                :type type
                :response response)
          (jss-io-lifecycle io))
    (setf (jss-chrome-io-response io) response)
    
   (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Page.loadEventFired" (timestamp)
  (jss-console-insert-message console "// log // page loaded"))

(define-jss-chrome-notification-handler "Page.domContentEventFired" (timestamp)
  (jss-console-insert-message console "// log // dom content"))

;;; not in docs?
(define-jss-chrome-notification-handler "Page.frameNavigated" (frame)
  (jss-console-format-message console "// log // frame %s navigated to %s" (cdr (assoc 'id frame)) (cdr (assoc 'url frame))))

;;; not in docs?
(define-jss-chrome-notification-handler "Page.frameDetached" (frameId)
  (jss-console-format-message console "// log // frame %s detached" frameId))

(provide 'jss-browser-chrome)
