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
  (format "Google Chrome @ %s:%s" (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-chrome-remote-debugging-url ((browser jss-chrome-browser))
  (format "http://%s:%s/json" (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-browser-tabs ((browser jss-chrome-browser))
  (mapcar 'cdr (slot-value browser 'tabs)))

(defmethod jss-browser-get-tabs ((browser jss-chrome-browser))
  (lexical-let ((d (make-jss-deferred))
                (browser browser))
    (url-retrieve
     (jss-chrome-remote-debugging-url browser)
     (lambda (status)
       (if status
           (if (getf status :error)
               (jss-deferred-errorback d (prin1-to-string (getf status :error)))
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
                 (widen)
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (let ((tab-data (json-read)))
                   (loop
                    for tab-data across tab-data
                    for tab = (make-instance 'jss-chrome-tab
                                             :json-data tab-data
                                             :browser browser
                                             :debugger-url (cdr (assoc 'webSocketDebuggerUrl tab-data)))
                    for tab-id = (jss-tab-id tab)
                    for existing-tab = (jss-browser-find-tab browser tab-id)
                    if existing-tab
                      do (setf (slot-value tab 'json-data) tab-data)
                    else
                      do (jss-browser-register-tab browser tab)
                    finally (jss-deferred-callback d browser))))
             (let ((status (save-match-data
                             (looking-at "^HTTP/1\\.1 \\(.*\\)$")
                             (match-string 1))))
               (jss-deferred-errorback d (format "Bad status: %s" (prin1-to-string status)))))))))
    d))

;;; The tab API implementation

(defvar *jss-chrome-virtual-id-counter* 0)

(defclass jss-chrome-tab (jss-generic-tab)
  ((json-data :initarg :json-data)
   (debugger-url :initarg :debugger-url)
   (virtual-id :initform nil)
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
        (or (slot-value tab 'virtual-id)
            (setf (slot-value tab 'virtual-id) (format "-%d-" (incf *jss-chrome-virtual-id-counter*))))))))

(defmethod jss-browser-find-tab ((browser jss-chrome-browser) tab-id)
  (cdr (cl-assoc tab-id (slot-value browser 'tabs) :test 'string=)))

(defmethod jss-browser-register-tab ((browser jss-chrome-browser) tab)
  (let* ((tab-id (jss-tab-id tab))
         (cell (cl-assoc tab-id (slot-value browser 'tabs) :test 'string=)))
    (if cell
        (setf (cdr cell) tab)
      (push (cons tab-id tab) (slot-value browser 'tabs))))
  tab)

(defmethod jss-chrome-tab-debugger-url ((tab jss-chrome-tab))
  (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data))))

(defmethod jss-tab-get-script ((tab jss-chrome-tab) scriptId)
  (gethash scriptId (slot-value tab 'scripts)))

(defmethod jss-tab-set-script ((tab jss-chrome-tab) scriptId script)
  (setf (gethash scriptId (slot-value tab 'scripts)) script))

(defsetf jss-tab-get-script jss-tab-set-script)

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
        (jss-deferred-then
          ws-open
          (lambda (tab)
            (lexical-let* ((tab tab)
                           (console (jss-tab-console tab)))
              (jss-deferred-then
               (jss-chrome-tab-enable tab "Console")
               (lambda (response)
                 (jss-deferred-then
                  (jss-deferred-wait-on-all
                   (jss-chrome-tab-enable tab "Network")
                   (jss-chrome-tab-enable tab "Debugger")
                   (jss-chrome-tab-enable tab "Page"))
                  (lambda (v)
                    (jss-deferred-then
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
    (jss-deferred-then
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
    (when err
      (warn "Got err %s" err))
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

(defclass jss-chrome-debugger (jss-generic-debugger)
  ((callFrames :initarg :callFrames :reader jss-debugger-stack-frames)
   (reason :initarg :reason)
   (data :initarg :data)))

(defmethod jss-debugger-message ((d jss-chrome-debugger))
  (format "%s %s" (slot-value d 'reason) (slot-value d 'data)))

(defmethod jss-debugger-continue ((d jss-chrome-debugger))
  (jss-deferred-error
    (jss-chrome-send-request (jss-debugger-tab d) '("Debugger.resume"))
    (lambda (err)
      (error "Failed to resume execution: %s" err))))

(defclass jss-chrome-stack-frame (jss-generic-stack-frame)
  ((properties :initarg :properties)
   (debugger)))

(defmethod jss-frame-function-name ((frame jss-chrome-stack-frame))
  (cdr (assoc 'functionName (slot-value frame 'properties))))

(defmethod jss-frame-source-url ((frame jss-chrome-stack-frame))
  (let* ((loc (cdr (assoc 'location (slot-value frame 'properties))))
         (scriptId (cdr (assoc 'scriptId loc)))
         (script (jss-tab-get-script (jss-debugger-tab (slot-value frame 'debugger)) scriptId)))
    (if script
        (cdr (assoc 'url script))
        scriptId)))

(defmethod jss-frame-source-position ((frame jss-chrome-stack-frame))
  (let* ((loc (cdr (assoc 'location (slot-value frame 'properties))))
         (lineNumber (cdr (assoc 'lineNumber loc)))
         (columnNumber (cdr (assoc 'columnNumber loc))))
    (format "%s:%s" lineNumber columnNumber)))

(define-jss-chrome-notification-handler "Debugger.breakpointResolved" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.globalObjectCleared" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.paused" (callFrames reason data)
  (jss-console-format-message console "// ERROR // %s" reason)
  (let ((jss-debugger (make-instance 'jss-chrome-debugger
                                     :callFrames (loop
                                                  for frame across callFrames
                                                  collect (make-instance 'jss-chrome-stack-frame
                                                                         :properties frame))
                                     :reason reason
                                     :data (cond 
                                            ((string= "exception" reason)
                                             (let ((oid (cdr (assoc 'objectId data))))
                                               (if oid
                                                   (jss-deferred-then
                                                    (jss-chrome-send-request tab `("Runtime.getProperties"
                                                                                   (objectId . ,oid)
                                                                                   (ownProperties . ,json-false)))
                                                    (lambda (response)
                                                      (message "getProperties: %s" response)))))
                                             data)
                                            (t data))
                                     :tab tab)))
    (dolist (frame (jss-debugger-stack-frames jss-debugger))
      (setf (slot-value frame 'debugger) jss-debugger))
    (jss-tab-open-debugger tab jss-debugger)))

(define-jss-chrome-notification-handler "Debugger.resumed" ()
  t)

(define-jss-chrome-notification-handler "Debugger.scriptFailedToParse" (url scriptSource startLine errorLine errorMessage)
  t)

(define-jss-chrome-notification-handler "Debugger.scriptParsed" (url scriptId scriptSource startLine startColumn endLine endColumn isContentScript sourceMapURL)
  (setf (jss-tab-get-script tab scriptId) params)
  t)

(defmethod jss-chrome-tab-websocket/on-open ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-open)))

(defmethod jss-chrome-tab-websocket/on-close ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-close))
  (when (jss-tab-console tab)
    (jss-console-format-message (jss-tab-console tab) "// ERROR // Remote end closed connection."))
  (setf (slot-value tab 'websocket) nil))

(defmethod jss-chrome-tab-websocket/on-error ((tab jss-chrome-tab) websocket action error)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-error websocket action error)))

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

(defmethod jss-console-evaluate ((console jss-chrome-console) js-code)
  (lexical-let ((console console))
    (jss-deferred-then
      (jss-chrome-send-request (jss-console-tab console)
                               `("Runtime.evaluate"
                                 (expression . ,js-code)
                                 (objectGroup . "jssConsoleEvaluate")))
      (lambda (response)
        (let* ((result (cdr (assoc 'result response)))
               (type (cdr (assoc 'type result)))
               (value (cdr (assoc 'value result))))
          (cond
           ((string= type "boolean") (ecase value
                                       ((t) "true")
                                       (:json-false "false")))
           ((string= type "function") "function () { ... }")
           ((string= type "number") (if (integerp value)
                                        (format "%d" value)
                                      (format "%g" value)))
           ((string= type "object") (let ((class-name (cdr (assoc 'className result)))
                                          (description (cdr (assoc 'description result))))
                                      (if (string= class-name description)
                                          (format "[%s]" description)
                                        (format "[%s %s]" class-name description))))
           ((string= type "string") (prin1-to-string value))
           ((string= type "undefined") "undefined")
           (t
            (error "Unknown result type %s" type))))))))

(defmethod jss-console-clear ((console jss-chrome-console))
  t)

(define-jss-chrome-notification-handler "Console.messageAdded" (message)
  (jss-console-format-message console "// %s // %s" (cdr (assoc 'type message)) (cdr (assoc 'text message))))

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

(defmethod jss-io-uid ((io jss-chrome-io))
  (cdr (assoc 'requestId (jss-chrome-io-properties io))))

(define-jss-chrome-notification-handler "Network.requestWillBeSent" (requestId loaderId documentURL request timestamp initiator stackTrace redirectResponse)
  (let ((io (jss-tab-register-io tab requestId
                                 (make-instance 'jss-chrome-io
                                                :properties params
                                                :start-time timestamp
                                                :lifecycle (list (list :sent timestamp))))))
    (jss-console-insert-request console io)))

(defmacro with-existing-io (io-id &rest body)
  `(let ((io (jss-tab-get-io tab ,io-id)))
     (if io
         (progn ,@body)
       (warn "IO %s does not exist." ,io-id))))
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
      (jss-deferred-then
        (jss-chrome-send-request tab `("Network.getResponseBody" (requestId . ,requestId)))
        (lambda (response)
          (setf (jss-chrome-io-responseBody io) response))
        (lambda (response)
          (warn "Network.getResponseBody(requestId: %s) => %s"
                requestId
                response
                ;(cdr (assoc 'message response))
                ;(cdr (assoc 'code response))
                ))))
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

(provide 'jss-browser-chrome)
