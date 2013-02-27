;;; https://developers.google.com/chrome-developer-tools/docs/protocol/1.0/debugger#events
(require 'url)
(require 'websocket)
(require 'json)

(require 'jss-browser-api)

;;; Browser connector function

(defun jss-chrome-connect (host port)
  (interactive (list (read-from-minibuffer "Host: " jss-browser-default-host)
                     (read-from-minibuffer "Post: " "9222")))
  (with-current-buffer (get-buffer-create (format "*Google Chrome@%s:%s*" host port))
    (switch-to-buffer (current-buffer))
    (jss-browser-mode)
    (setf jss-current-browser-instance (make-instance 'jss-chrome-browser
                                                      :host host
                                                      :port port))
    (jss-browser-mode-refresh)))

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
  (lexical-let ((d (deferred:new))
                (browser browser))
    (url-retrieve
     (jss-chrome-remote-debugging-url browser)
     (lambda (status)
       (if status
           (if (getf status :error)
               (deferred:errorback d (prin1-to-string (getf status :error)))
             (deferred:errorback d (format "Unrecognized error: %s" (prin1-to-string status))))
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
                    finally (deferred:callback d browser))))
             (let ((status (save-match-data
                             (looking-at "^HTTP/1\\.1 \\(.*\\)$")
                             (match-string 1))))
               (deferred:errorback d (format "Bad status: %s" (prin1-to-string status)))))))))
    d))

;;; The tab API implementation

(defvar *jss-chrome-virtual-id-counter* 0)

(defclass jss-chrome-tab (jss-generic-tab)
  ((json-data :initarg :json-data)
   (debugger-url :initarg :debugger-url)
   (virtual-id :initform nil)
   (websocket :initform nil)
   (request-counter :initform 0)
   (requests :initform (make-hash-table))))

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

(defmethod jss-tab-connected-p ((tab jss-chrome-tab))
  (slot-value tab 'websocket))

(defmethod jss-tab-connect ((tab jss-chrome-tab))
  (when (slot-value tab 'websocket)
    (error "Already connected. Disconnect first."))
  (lexical-let ((tab tab)
                (debugger-url (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data)))))
    (jss-log-event (list :websocket debugger-url :open))
    (lexical-let* ((ws-open (deferred:new))
                   (socket (websocket-open debugger-url
                                           :on-message (lambda (websocket frame)
                                                         (jss-chrome-tab-websocket/on-message tab websocket frame))
                                           :on-open (lambda (websocket)
                                                      (jss-chrome-tab-websocket/on-open tab websocket)
                                                      (deferred:callback ws-open tab))
                                           :on-close (lambda (websocket)
                                                       (jss-chrome-tab-websocket/on-close tab websocket))
                                           :on-error (lambda (websocket action error)
                                                       (jss-chrome-tab-websocket/on-error tab websocket action error)
                                                       (deferred:errorback ws-open (list websocket action error))))))
      (setf (slot-value tab 'websocket) socket)
      
      (lexical-let* ((connect-deferred (deferred:new)))
        ;; setup a deferred chain to send Console.enable after opening the ws connection
        (deferred:then
          ws-open
          (lambda (tab)
            (lexical-let* ((tab tab)
                           (console (jss-tab-console tab)))
              (deferred:nextc (jss-chrome-tab-enable tab "Console")
                (lambda (response)
                  (deferred:nextc
                    (deferred:parallel
                      (jss-chrome-tab-enable tab "Network")
                      (jss-chrome-tab-enable tab "Debugger")
                      (jss-chrome-tab-enable tab "Page"))
                    (lambda (v)
                      (jss-chrome-send-request
                       tab '("Debugger.setPauseOnExceptions" (state . "all"))
                       (lambda (v)
                         (deferred:callback connect-deferred tab)))))))))
          (lambda (v)
            (deferred:errorback connect-deferred v)))
        
        connect-deferred))))

(defmethod jss-chrome-tab-enable ((tab jss-chrome-tab) domain)
  (lexical-let* ((tab tab)
                 (console (jss-tab-console tab))
                 (domain domain))
    (jss-chrome-send-request tab
                             (list (format "%s.enable" domain))
                             (lambda (response)
                               (jss-console-insert-message console (format "// INFO // %s enabled." domain)))
                             (lambda (response)
                               (jss-console-insert-message console (format "// INFO // Could not enable %s: %s" domain response))))))

(defmethod jss-chrome-tab-websocket/on-message ((tab jss-chrome-tab) websocket frame)
  (jss-log-event (list :websocket
                       (jss-chrome-tab-debugger-url tab)
                       :on-message
                       websocket
                       frame))
  (let ((message (with-temp-buffer
                   (insert (websocket-frame-payload frame))
                   (goto-char (point-min))
                   (json-read)))
        (console (jss-tab-ensure-console tab)))
    (if (assoc 'error message)
        (progn
          (jss-log-event (list :google-chrome
                               (jss-chrome-tab-debugger-url tab)
                               :error
                               (cdr (assoc 'error message))))
          (jss-console-insert-message console
                                      (format "// Error: %s (%s)"
                                              (cdr (assoc 'message (cdr (assoc 'error message))))
                                              (cdr (assoc 'code (cdr (assoc 'error message)))))))
      (let ((request-id (cdr (assoc 'id message))))
        (if request-id
            (deferred:callback (gethash request-id (slot-value tab 'requests)) (cdr (assoc 'result message)))
          (let* ((method (cdr (assoc 'method message)))
                 (handler (gethash method jss-chrome-notification-handlers)))
            (if handler
                (funcall (lambda (params)
                           (funcall handler tab params))
                         (cdr (assoc 'params message)))
              (jss-log-event (list :google-chrome
                                   (jss-chrome-tab-debugger-url tab)
                                   :unknown-request-id
                                   message request-id)))))))))

(defvar jss-chrome-notification-handlers (make-hash-table :test 'equal))

(defmacro define-jss-chrome-notification-handler (name args &rest body)
  `(setf (gethash ,name jss-chrome-notification-handlers)
         (lambda (tab params)
           (lexical-let ,(mapcar (lambda (arg-name)
                                   (list arg-name `(cdr (assoc ',arg-name params))))
                                 args)
             (lexical-let ((tab tab)
                           (console (jss-tab-console tab)))
               ,@body)))))

(define-jss-chrome-notification-handler "Debugger.breakpointResolved" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.globalObjectCleared" (breakpointId location)
  t)

(define-jss-chrome-notification-handler "Debugger.paused" (callFrames reason data)
  (jss-console-insert-message (jss-tab-console tab) (format "// ERROR // %s/%s" reason data)))

(define-jss-chrome-notification-handler "Debugger.resumed" ()
  t)

(define-jss-chrome-notification-handler "Debugger.scriptFailedToParse" (url scriptSource startLine errorLine errorMessage)
  t)

(define-jss-chrome-notification-handler "Debugger.scriptParsed" (url scriptSource startLine startColumn endLine endColumn isContentScript sourceMapURL)
  t)

(defmethod jss-chrome-tab-websocket/on-open ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-open)))

(defmethod jss-chrome-tab-websocket/on-close ((tab jss-chrome-tab) websocket)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) :on-close))
  (when (jss-tab-console tab)
    (jss-console-format-message (jss-tab-console tab) "// ERROR // Remote end closed connection."))
  (setf (slot-value tab 'websocket) nil))

(defmethod jss-chrome-tab-websocket/on-error ((tab jss-chrome-tab) websocket action error)
  (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab) websocket action error)))

(defmethod jss-chrome-send-request ((tab jss-chrome-tab) request then &optional error)
  (let* ((ws (slot-value tab 'websocket))
         (request-id (incf (slot-value tab 'request-counter)))
         (payload (list* (cons 'id request-id)
                         (cons 'method (first request))
                         (when (rest request)
                           (list (cons 'params (rest request))))))
         (text (json-encode payload))
         (deferred (deferred:new)))
    
    (jss-log-event (list :websocket (jss-chrome-tab-debugger-url tab)
                         payload
                         text))
    (setf (gethash request-id (slot-value tab 'requests)) deferred)
    (websocket-send-text ws text)
    (deferred:then deferred then error)))

(defclass jss-chrome-console (jss-generic-console)
  ((messages :initform (make-hash-table) :accessor jss-chrome-console-messages)))

(defmethod jss-tab-make-console ((tab jss-chrome-tab) &rest initargs)
  (apply 'make-instance 'jss-chrome-console initargs))

(defmethod jss-console-evaluate ((console jss-chrome-console) js-code)
  (lexical-let ((console console))
    (jss-chrome-send-request (jss-console-tab console)
                             `("Runtime.evaluate" (expression . ,js-code) (objectGroup . "jssConsoleEvaluate"))
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

(define-jss-chrome-notification-handler "Console.messageAdded" (message)
  (jss-console-insert-message console message))

(define-jss-chrome-notification-handler "Console.messagesCleared" ()
  t)

(define-jss-chrome-notification-handler "Console.messageRepeatCountUpdated" (count)
  t)

(defclass jss-chrome-io (jss-generic-io)
  ((properties :accessor jss-chrome-io-properties :initarg :properties)
   (response :accessor jss-chrome-io-response :initform nil)))

(defmethod jss-io-request-headers ((io jss-chrome-io))
  (cdr (assoc 'headers (cdr (assoc 'request (jss-chrome-io-properties io))))))

(defmethod jss-io-response-headers ((io jss-chrome-io))
  (cdr (assoc 'headers (jss-chrome-io-response io))))

(defmethod jss-io-uid ((io jss-chrome-io))
  (cdr (assoc 'requestId (jss-chrome-io-properties io))))

(define-jss-chrome-notification-handler "Network.requestWillBeSent" (requestId loaderId documentURL request timestamp initiator stackTrace redirectResponse)
  (let ((io (jss-tab-register-io tab requestId
                                 (make-instance 'jss-chrome-io
                                                :properties params
                                                :start-time timestamp
                                                :url (cdr (assoc 'url request))
                                                :lifecycle (list (list :sent timestamp))))))
    (jss-console-insert-request console io)))

(define-jss-chrome-notification-handler "Network.dataReceived" (requestId timestamp dataLength encodedDataLength)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :data-received timestamp
                :data-length dataLength
                :encoded-data-length encodedDataLength)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.loadingFailed" (requestId timestamp errorText canceled)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :loading-failed timestamp
                :error-text errorText
                :canceled canceled)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.loadingFinished" (requestId timestamp)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :loading-finished timestamp)
          (jss-io-lifecycle io))
   (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.requestServedFromCache" (requestId)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :served-from-cache nil)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.requestServedFromMemoryCache" (requestId loaderId documentURL timestamp initiator resource)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :served-from-memory-cache timestamp
                :loader-id loaderId
                :document-url documentURL
                :initiator initiator
                :resource resource)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-chrome-notification-handler "Network.responseReceived" (requestId loaderId timestamp type response)
  (let ((io (jss-tab-get-io tab requestId)))
    (push (list :response-received timestamp
                :loader-id loaderId
                :type type
                :response response)
          (jss-io-lifecycle io))
    (setf (jss-chrome-io-response io) response)
   (jss-console-update-request-message console io)))

(provide 'jss-browser-chrome)
