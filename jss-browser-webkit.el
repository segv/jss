;;; https://developers.google.com/chrome-developer-tools/docs/protocol/1.0/debugger#events
;;; http://trac.webkit.org/browser/trunk/Source/WebCore/inspector/Inspector.json
(require 'url)
(require 'websocket)
(require 'json)

(require 'jss-browser-api)
(require 'jss-deferred)

;;; Browser connector function

(defun jss-webkit-connect (host port)
  (interactive (list (read-from-minibuffer "Host: " jss-browser-default-host)
                     (read-from-minibuffer "Post: " "9222")))
  (with-current-buffer (get-buffer-create (format "*JSS Webkit @%s:%s*" host port))
    (switch-to-buffer (current-buffer))
    (jss-browser-mode* (make-instance 'jss-webkit-browser
                                      :host host
                                      :port port))))

;;; The browser API implementation

(defclass jss-webkit-browser (jss-generic-browser)
  ((host :initarg :host)
   (port :initarg :port)
   (tabs :initform '())))

(defmethod jss-browser-description ((browser jss-webkit-browser))
  (format "Webkit @ %s:%s\nNB: Only displaying tabs that can be debugged." (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-webkit-remote-debugging-url ((browser jss-webkit-browser))
  (format "http://%s:%s/json" (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-browser-tabs ((browser jss-webkit-browser))
  (mapcar 'cdr (slot-value browser 'tabs)))

(defmethod jss-browser-find-tab ((browser jss-webkit-browser) tab-id)
  (cdr (cl-assoc tab-id (slot-value browser 'tabs) :test 'string=)))

(defmethod jss-browser-get-tabs ((browser jss-webkit-browser))
  (lexical-let ((d (make-jss-deferred))
                (browser browser))
    (url-retrieve
     (jss-webkit-remote-debugging-url browser)
     (lambda (status)
       (if status
           (if (getf status :error)
               (jss-deferred-errorback d (getf status :error))
             (jss-deferred-errorback d (format "Unrecognized error: %s" (prin1-to-string status))))
         (progn
           (widen)
           (jss-log-event (list "Webkit GET"
                                (jss-webkit-remote-debugging-url browser)
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
                    do (let* ((tab (make-instance 'jss-webkit-tab
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
                               (format "No status received from %s" (jss-webkit-remote-debugging-url browser))))))
               (jss-deferred-errorback d (list 'bad-status (prin1-to-string status)))))))))
    d))

;;; The tab API implementation

(defclass jss-webkit-tab (jss-generic-tab)
  ((json-data :initarg :json-data)
   (debugger-url :initarg :debugger-url)
   (websocket :initform nil)
   (request-counter :initform 0)
   (requests :initform (make-hash-table :test 'equal)))) 

(defmethod jss-tab-debugger-p ((tab jss-webkit-tab))
  (slot-value tab 'debugger-url))

(defmethod jss-webkit-tab-json-prop ((tab jss-webkit-tab) prop-name)
  (cdr (assoc prop-name (slot-value tab 'json-data))))

(defmethod jss-tab-title ((tab jss-webkit-tab))
  (jss-webkit-tab-json-prop tab 'title))

(defmethod jss-tab-url ((tab jss-webkit-tab))
  (jss-webkit-tab-json-prop tab 'url))

(defmethod jss-tab-id ((tab jss-webkit-tab))
  (save-match-data
    (let ((debugger-url (jss-webkit-tab-json-prop tab 'webSocketDebuggerUrl)))
      (if debugger-url
          (if (string-match "^ws://.*devtools/page/\\(.*\\)$" debugger-url)
              (match-string 1 debugger-url)
            debugger-url)
        (error "Can not compute id for %s." tab)))))

(defmethod jss-webkit-tab-debugger-url ((tab jss-webkit-tab))
  (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data))))

(defmethod jss-tab-reload ((tab jss-webkit-tab))
  (jss-webkit-send-request tab '("Page.reload")))

(defmethod jss-tab-connected-p ((tab jss-webkit-tab))
  (slot-value tab 'websocket))

(defmethod jss-tab-connect ((tab jss-webkit-tab))
  (when (slot-value tab 'websocket)
    (error "Already connected. Disconnect first."))
  (lexical-let ((tab tab)
                (debugger-url (cdr (assoc 'webSocketDebuggerUrl (slot-value tab 'json-data)))))
    (jss-log-event (list :websocket debugger-url :open))
    (lexical-let* ((ws-open (make-jss-deferred))
                   (socket (websocket-open debugger-url
                                           :on-message (lambda (websocket frame)
                                                         (jss-webkit-tab-websocket/on-message tab websocket frame))
                                           :on-open (lambda (websocket)
                                                      (jss-webkit-tab-websocket/on-open tab websocket)
                                                      (jss-deferred-callback ws-open tab))
                                           :on-close (lambda (websocket)
                                                       (jss-webkit-tab-websocket/on-close tab websocket))
                                           :on-error (lambda (websocket action error)
                                                       (jss-webkit-tab-websocket/on-error tab websocket action error)
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
              (jss-webkit-tab-enable tab "Console")
              (lambda (response)
                (jss-deferred-add-backs
                 (jss-deferred-wait-on-all
                  (jss-webkit-tab-enable tab "Network")
                  (jss-webkit-tab-enable tab "Debugger")
                  (jss-webkit-tab-enable tab "Page"))
                 (lambda (v)
                   (jss-deferred-add-backs
                    (jss-webkit-send-request tab '("Debugger.setPauseOnExceptions" (state . "all")))
                    (lambda (v)
                      (jss-deferred-callback connect-deferred tab)))))))))
          (lambda (v)
            (jss-deferred-errorback connect-deferred v)))
        
        connect-deferred))))

(defmethod jss-tab-set-debugger-sensitivity ((tab jss-webkit-tab) sensitivity)
  (let ((new-state (ecase sensitivity
                     (:all "all")
                     (:uncaught "uncaught")
                     (:never "none"))))
    (jss-webkit-send-request-or-error tab
                                      (list "Debugger.setPauseOnExceptions"
                                            (cons 'state new-state))
                                      "Failed to setPauseOnExceptions(%s,%s)" sensitivity new-state)))

(defmethod jss-webkit-tab-enable ((tab jss-webkit-tab) domain)
  (lexical-let* ((tab tab)
                 (console (jss-tab-console tab))
                 (domain domain))
    (jss-deferred-add-backs
      (jss-webkit-send-request tab (list (format "%s.enable" domain)))
      (lambda (response)
        (jss-console-debug-message console "%s enabled." domain))
      (lambda (response)
        (jss-console-debug-message console "Could not enable %s: %s" domain response)))))

(defmethod jss-webkit-tab-websocket/on-message ((tab jss-webkit-tab) websocket frame)
  (jss-log-event (list :websocket
                       (jss-webkit-tab-debugger-url tab)
                       :on-message
                       websocket
                       frame))
  (let* ((message (with-temp-buffer
                    (insert (websocket-frame-payload frame))
                    (goto-char (point-min))
                    (json-read)))
         (request-id (cdr (assoc 'id message)))
         (requests (slot-value tab 'requests))
         (request-deferred (gethash request-id requests))
         (console (jss-tab-ensure-console tab))
         (err (cdr (assoc 'error message))))
    (if err
        (if request-deferred
            (progn
              (remhash request-id requests)
              (jss-deferred-errorback request-deferred err))
          (progn
            (jss-log-event (list :google-webkit
                                 (jss-webkit-tab-debugger-url tab)
                                 :unhandled-error
                                 err))
            (jss-console-error-message console
                                       "Unhandled error: %s (%s)"
                                       (cdr (assoc 'message err))
                                       (cdr (assoc 'code    err)))))
      
      (if request-deferred
          (progn
            (remhash request-id requests)
            (jss-deferred-callback request-deferred (cdr (assoc 'result message))))
        (let* ((method (cdr (assoc 'method message)))
               (handler (gethash method jss-webkit-notification-handlers)))
          (if handler
              (funcall handler
                       tab
                       (cdr (assoc 'params message))
                       message)
            (jss-log-event (list :google-webkit
                                 (jss-webkit-tab-debugger-url tab)
                                 :unknown-request-id
                                 message request-id))))))))

(defmethod jss-webkit-tab-websocket/on-open ((tab jss-webkit-tab) websocket)
  (jss-log-event (list :websocket (jss-webkit-tab-debugger-url tab) :on-open)))

(defmethod jss-webkit-tab-websocket/on-close ((tab jss-webkit-tab) websocket)
  (jss-log-event (list :websocket (jss-webkit-tab-debugger-url tab) :on-close))
  (when (jss-tab-console tab)
    (jss-console-error-message (jss-tab-console tab) "Remote end closed connection."))
  (setf (slot-value tab 'websocket) nil))

(defmethod jss-webkit-tab-websocket/on-error ((tab jss-webkit-tab) websocket action error)
  (jss-log-event (list :websocket (jss-webkit-tab-debugger-url tab) :on-error websocket action error)))

(defvar jss-webkit-notification-handlers (make-hash-table :test 'equal))

(defmacro define-jss-webkit-notification-handler (name args &rest body)
  `(setf (gethash ,name jss-webkit-notification-handlers)
         (lambda (tab params message)
           (lexical-let ,(mapcar (lambda (arg-name)
                                   (list arg-name `(cdr (assoc ',arg-name params))))
                                 args)
             (lexical-let ((tab tab)
                           (console (jss-tab-console tab)))
               ,@body)))))

(defvar jss-debugger-object-group-count 0)

(defclass jss-webkit-debugger (jss-generic-debugger)
  ((object-group-id :initform (incf jss-debugger-object-group-count))
   (callFrames :initarg :callFrames :reader jss-debugger-stack-frames)
   (reason :initarg :reason)
   (data :initarg :data)))

(defmethod jss-debugger-exception ((d jss-webkit-debugger))
  (make-jss-webkit-remote-object (slot-value d 'data)))

(defmethod jss-debugger-insert-message ((d jss-webkit-debugger))
  (insert (slot-value d 'reason)))

(defun jss-webkit-send-request-or-error (target request error-control &rest error-args)
  (jss-deferred-add-errorback
   (jss-webkit-send-request target request)
   (lambda (err)
     (apply 'error (concat error-control ":%s") (append error-args err)))))

(defmethod jss-debugger-resume ((d jss-webkit-debugger))
  (jss-webkit-send-request-or-error (jss-debugger-tab d) '("Debugger.resume") "Failed to resume execution"))

(defmethod jss-debugger-step-into ((d jss-webkit-debugger))
  (jss-webkit-send-request-or-error (jss-debugger-tab d) '("Debugger.stepInto") "Failed to step into"))

(defmethod jss-debugger-step-out ((d jss-webkit-debugger))
  (jss-webkit-send-request-or-error (jss-debugger-tab d) '("Debugger.stepOut") "Failed to step out"))

(defmethod jss-debugger-step-over ((d jss-webkit-debugger))
  (jss-webkit-send-request-or-error (jss-debugger-tab d) '("Debugger.stepOver") "Failed to step over"))

(defmethod jss-debugger-cleanup ((d jss-webkit-debugger))
  (lexical-let ((d d)
                (object-group (jss-webkit-object-group d)))
    (jss-deferred-add-errorback
     (jss-webkit-send-request (jss-debugger-tab d)
                              (list "Runtime.releaseObjectGroup" `(objectGroup . ,object-group)))
     (lambda (err)
       (error "Failed to release object group %s on %s." object-group d)))))

(defclass jss-webkit-stack-frame (jss-generic-stack-frame)
  ((properties :initarg :properties)))

(defmethod jss-webkit-stack-frame-id ((frame jss-webkit-stack-frame))
  (cdr (assoc 'callFrameId (slot-value frame 'properties))))

(defmethod jss-frame-function-name ((frame jss-webkit-stack-frame))
  (cdr (assoc 'functionName (slot-value frame 'properties))))

(defmethod jss-webkit-location-data (tab location)
  (list :script (jss-tab-get-script tab (cdr (assoc 'scriptId location)))
        :script-id (cdr (assoc 'scriptId location))
        :line-number (cdr (assoc 'lineNumber location))
        :column-number (cdr (assoc 'columnNumber location))))

(defmethod jss-frame-source-hint ((frame jss-webkit-stack-frame))
  (destructuring-bind (&key script line-number column-number &allow-other-keys)
      (jss-webkit-location-data (jss-debugger-tab (jss-frame-debugger frame))
                                (cdr (assoc 'location (slot-value frame 'properties))))
    
    (if (and script (jss-script-url script))
        (format "%s:%s:%s" (jss-script-url script) line-number column-number)
      nil)))

(defmethod jss-frame-get-source-location ((frame jss-webkit-stack-frame))
  (destructuring-bind (&key script script-id line-number column-number)
      (jss-webkit-location-data (jss-debugger-tab (jss-frame-debugger frame))
                                (cdr (assoc 'location (slot-value frame 'properties))))
    (if script
        (make-jss-completed-deferred
         :callback (list script line-number column-number))
      (unless script-id
        (error "Want to get source-location of frame with no frame-id. %s :(" frame))
      (let ((tab (jss-debugger-tab (jss-frame-debugger frame))))
        (setf (jss-tab-get-script tab script-id) (make-instance 'jss-webkit-script :properties (list (cons 'url (format "injected://%s" script-id))
                                                                                                     (cons 'scriptId script-id))))
        (make-jss-completed-deferred
         :callback (list (jss-tab-get-script tab script-id)
                         (or line-number 0)
                         (or column-number 0)))))))

(define-jss-webkit-notification-handler "Debugger.breakpointResolved" (breakpointId location)
  t)

(define-jss-webkit-notification-handler "Debugger.globalObjectCleared" (breakpointId location)
  t)

(define-jss-webkit-notification-handler "Debugger.paused" (callFrames reason data)
  (jss-console-error-message console "Debugger paused on %s" reason)
  (let ((jss-debugger (make-instance 'jss-webkit-debugger
                                     :reason reason
                                     :data (cond 
                                            ((string= "exception" reason)
                                             (let ((oid (cdr (assoc 'objectId data))))
                                               (if oid
                                                   (jss-deferred-add-backs
                                                    (jss-webkit-send-request tab `("Runtime.getProperties"
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
           collect (make-instance 'jss-webkit-stack-frame :properties frame :debugger jss-debugger)))
    
    (jss-tab-open-debugger tab jss-debugger)))

(define-jss-webkit-notification-handler "Debugger.resumed" ()
  t)

(define-jss-webkit-notification-handler "Debugger.scriptFailedToParse" (url scriptSource startLine errorLine errorMessage)
  t)

(defclass jss-webkit-script (jss-generic-script)
  ((properties :initarg :properties)))

(defmethod jss-script-url ((script jss-webkit-script))
  (cdr (assoc 'url (slot-value script 'properties))))

(defmethod jss-script-id ((script jss-webkit-script))
  (cdr (assoc 'scriptId (slot-value script 'properties))))

(defmethod jss-script-get-body ((script jss-webkit-script))
  (jss-deferred-then
   (jss-webkit-send-request (jss-script-tab script)
                            (list "Debugger.getScriptSource" (assoc 'scriptId (slot-value script 'properties))))
   (lambda (response)
     (cdr (assoc 'scriptSource response)))))

(define-jss-webkit-notification-handler "Debugger.scriptParsed" (url scriptId scriptSource startLine startColumn endLine endColumn isContentScript sourceMapURL)
  (setf (jss-tab-get-script tab scriptId)
        (make-instance 'jss-webkit-script :properties params))
  t)

(defmethod jss-webkit-send-request ((tab jss-webkit-tab) request)
  (let* ((ws (slot-value tab 'websocket))
         (request-id (incf (slot-value tab 'request-counter)))
         (payload (list* (cons 'id request-id)
                         (cons 'method (first request))
                         (when (rest request)
                           (list (cons 'params (rest request))))))
         (text (json-encode payload))
         (deferred (make-jss-deferred)))
    
    (jss-log-event (list :websocket (jss-webkit-tab-debugger-url tab)
                         payload
                         text))
    (setf (gethash request-id (slot-value tab 'requests)) deferred)
    (websocket-send-text ws text)
    deferred))

(defclass jss-webkit-console (jss-generic-console)
  ((messages :initform (make-hash-table) :accessor jss-webkit-console-messages)))

(defmethod jss-tab-make-console ((tab jss-webkit-tab) &rest initargs)
  (apply 'make-instance 'jss-webkit-console initargs))

(defmethod jss-webkit-object-group ((tab jss-webkit-tab))
  (format "jssConsoleEvaluate_%s" (jss-tab-id tab)))

(defmethod jss-webkit-object-group ((console jss-webkit-console))
  (jss-webkit-object-group (jss-console-tab console)))

(defmethod jss-webkit-object-group ((frame jss-webkit-stack-frame))
  (jss-webkit-object-group (jss-frame-debugger frame)))

(defmethod jss-webkit-object-group ((debugger jss-webkit-debugger))
  (format "jssDebuggerEvaluate_%d" (slot-value debugger 'object-group-id)))

(defmethod jss-evaluate ((tab jss-webkit-tab) js-code)
  (jss-deferred-then
   (jss-webkit-send-request tab
                            `("Runtime.evaluate"
                              (expression . ,js-code)
                              (objectGroup . ,(jss-webkit-object-group tab))
                              (generatePreview . t)))
   (lambda (result)
     (make-jss-webkit-remote-object (cdr (assoc 'result result))))
   (lambda (response)
     (make-jss-webkit-evaluation-error response))))

(defclass jss-webkit-evaluation-error (jss-generic-remote-object)
  ((properties :initarg :properties)))

(defun make-jss-webkit-evaluation-error (properties)
  (make-instance 'jss-webkit-evaluation-error :properties properties))

(defmethod jss-remote-value-string ((error jss-webkit-evaluation-error))
  (format "// error // code: %s; message: %s; data: %s"
          (cdr (assoc 'code (slot-value error 'properties)))
          (cdr (assoc 'message (slot-value error 'properties)))
          (prin1-to-string (cdr (assoc 'data (slot-value error 'properties))))))

(defmethod jss-evaluate ((frame jss-webkit-stack-frame) js-code)
  (jss-deferred-then
   (jss-webkit-send-request (jss-debugger-tab (jss-frame-debugger frame))
                            `("Debugger.evaluateOnCallFrame"
                              (expression . ,js-code)
                              (callFrameId . ,(jss-webkit-stack-frame-id frame))
                              (objectGroup . ,(jss-webkit-object-group frame))
                              (generatePreview . t)))
   (lambda (result)
     (make-jss-webkit-remote-object (cdr (assoc 'result result))))
   (lambda (response)
     (make-jss-webkit-evaluation-error response))))

(defclass jss-webkit-remote-object-mixin ()
  ((description :initarg :description :accessor jss-webkit-remote-object-description)
   (className   :initarg :className   :accessor jss-webkit-remote-object-className)
   (objectId    :initarg :objectId    :accessor jss-webkit-remote-object-id)))

(defmethod jss-remote-object-class-name ((o jss-webkit-remote-object-mixin))
  (jss-webkit-remote-object-className o))

(defmethod jss-remote-object-label ((o jss-webkit-remote-object-mixin))
  (jss-webkit-remote-object-description o))

(defmethod jss-remote-object-get-properties ((object jss-webkit-remote-object-mixin) tab)
  (jss-deferred-then
   (jss-webkit-send-request tab (list "Runtime.getProperties"
                                      (cons 'objectId (jss-webkit-remote-object-id object))
                                      (cons 'ownProperties t)))
   (lambda (response)
     (loop
      for prop across (cdr (assoc 'result response))
      collect (cons (cdr (assoc 'name prop))
                    (make-jss-webkit-remote-object (cdr (assoc 'value prop))))))))

(defclass jss-webkit-remote-object (jss-generic-remote-object jss-webkit-remote-object-mixin) ())

(defclass jss-webkit-remote-array  (jss-generic-remote-array jss-webkit-remote-object-mixin) ())

(defclass jss-webkit-remote-date (jss-generic-remote-object jss-webkit-remote-object-mixin) ())

(defclass jss-webkit-remote-node (jss-generic-remote-object jss-webkit-remote-object-mixin) ())

(defclass jss-webkit-remote-regexp (jss-generic-remote-object jss-webkit-remote-object-mixin) ())

(defclass jss-webkit-remote-function   (jss-generic-remote-function)
  ((description :initarg :description :accessor jss-webkit-remote-object-description)
   (objectId    :initarg :objectId    :accessor jss-webkit-remote-object-id)))

(defmethod jss-remote-function-get-location ((function jss-webkit-remote-function) tab)
  (jss-deferred-then
   (jss-webkit-send-request tab (list "Debugger.getFunctionDetails" (cons 'functionId (jss-webkit-remote-object-id function))))
   (lambda (response)
     (let* ((details (cdr (assoc 'details response)))
            (location (cdr (assoc 'location details))))
       (when location
         (list (jss-tab-get-script tab (cdr (assoc 'scriptId location)))
               (cdr (assoc 'lineNumber location))
               (cdr (assoc 'columnNumber location))))))))

(defmethod jss-remote-value-string ((func jss-webkit-remote-function))
  (replace-regexp-in-string "[ \t\n\r\f]+"
                            " "
                            (jss-webkit-remote-object-description func)))

(defun make-jss-webkit-remote-object (result)
  (let* ((type (cdr (assoc 'type result)))
         (value (cdr (assoc 'value result))))
    (if result
        (cond
         ((string= type "boolean")
          (make-instance (ecase value
                           ((t) 'jss-generic-remote-true)
                           (:json-false 'jss-generic-remote-false))))
         
         
         ((string= type "number")
          (cond
           (value
            (make-instance 'jss-generic-remote-number :value value))
           ((string= "Infinity" (cdr (assoc 'description result)))
            (make-instance 'jss-generic-remote-plus-infinity))
           ((string= "-Infinity" (cdr (assoc 'description result)))
            (make-instance 'jss-generic-remote-minus-infinity))
           ((string= "NaN" (cdr (assoc 'description result)))
            (make-instance 'jss-generic-remote-NaN))
           (t
             (error "Got number, not infinity, but no value :("))))
         
         ((string= type "string")
          (make-instance 'jss-generic-remote-string :value value))
         
         ((string= type "undefined")
          (make-instance 'jss-generic-remote-undefined))
         
         ((string= type "function")
          (make-instance 'jss-webkit-remote-function
                         :description (cdr (assoc 'description result))
                         :objectId (cdr (assoc 'objectId result))))
         
         ((string= type "object")
          (let* ((subtype (cdr (assoc 'subtype result))))
            (if (string= subtype "null")
                (make-instance 'jss-generic-remote-null)
              (make-instance (cond
                              ((string= subtype "array")  'jss-webkit-remote-array)
                              ;; ((string= subtype "date")   'jss-webkit-remote-date)
                              ;; ((string= subtype "node")   'jss-webkit-remote-node)
                              ;; ((string= subtype "regexp") 'jss-webkit-remote-regexp)
                              (t                          'jss-webkit-remote-object))
                             :className (cdr (assoc 'className result))
                             :description (cdr (assoc 'description result))
                             :objectId (cdr (assoc 'objectId result))))))

         
         (t
          (error "Unknown result type %s" type)))
      (make-instance 'jss-generic-remote-no-value))))

(defmethod jss-get-object-properties ((tab jss-webkit-tab) object-id)
  (jss-deferred-then (jss-webkit-send-request (jss-console-tab console)
                                              (list "Runtime.getProperties"
                                                    (cons 'objectId object-id)
                                                    (cons 'ownProperties :json-false)))
                     (lambda (response)
                       (loop for r across (cdr (assoc 'result response)) collect r))))

(defmethod jss-console-cleanup ((console jss-webkit-console))
  (jss-webkit-send-request-or-error (jss-console-tab console)
                                    (list "Runtime.releaseObjectGroup"
                                          (cons 'objectGroup (jss-webkit-object-group console)))
                                    "Failed to cleanup object group %s" (jss-webkit-object-group console)))

(defmethod jss-console-close ((console jss-webkit-console))
  (jss-console-cleanup console)
  (websocket-close (slot-value (jss-console-tab console) 'websocket)))

(define-jss-webkit-notification-handler "Console.messageAdded" (message)
  (jss-console-format-message console
                              (ecase (cdr (assoc 'type message))
                                (debug 'debug)
                                (log 'log)
                                (warning 'warn)
                                (error 'error)
                                (tip 'error)
                                ((nil) 'log))
                              "// %s // %s%s"
                              (cdr (assoc 'type message))
                              (cdr (assoc 'text message))
                              (let ((url (cdr (assoc 'url message)))
                                    (line (cdr (assoc 'line message))))
                                (if url
                                    (if line
                                        (format " %s:%s" url line)
                                      (format " %s" url))
                                  ""))))

(define-jss-webkit-notification-handler "Console.messagesCleared" ()
  t)

(define-jss-webkit-notification-handler "Console.messageRepeatCountUpdated" (count)
  t)

(defclass jss-webkit-io (jss-generic-io)
  ((properties :accessor jss-webkit-io-properties :initarg :properties)
   (response :accessor jss-webkit-io-response :initform nil)
   (responseBody :accessor jss-webkit-io-responseBody :initform nil)))

(defmethod jss-io-request-headers ((io jss-webkit-io))
  (cdr (assoc 'headers (cdr (assoc 'request (jss-webkit-io-properties io))))))

(defmethod jss-io-request-method ((io jss-webkit-io))
  (cdr (assoc 'method (cdr (assoc 'request (jss-webkit-io-properties io))))))

(defmethod jss-io-request-url ((io jss-webkit-io))
  (cdr (assoc 'url (cdr (assoc 'request (jss-webkit-io-properties io))))))

(defmethod jss-io-response-headers ((io jss-webkit-io))
  (if (assoc 'redirectResponse (jss-webkit-io-properties io))
      (cdr (assoc 'headers (cdr (assoc 'redirectResponse (jss-webkit-io-properties io)))))
      (cdr (assoc 'headers (jss-webkit-io-response io)))))

(defmethod jss-io-response-content-type ((io jss-webkit-io))
  (cdr (assoc 'mimeType (jss-webkit-io-response io))))

(defmethod jss-io-response-content-length ((io jss-webkit-io))
  (cdr (assoc 'content-length (jss-io-response-headers io))))

(defmethod jss-io-response-data ((io jss-webkit-io))
  (if (jss-webkit-io-responseBody io)
      (let ((base64-p (cdr (assoc 'base64Encoded (jss-webkit-io-responseBody io))))
            (body (cdr (assoc 'body (jss-webkit-io-responseBody io)))))
        (if (eql :json-false base64-p)
            body
          (base64-decode-string body)))
    ;; fwiw nil means something different from "" (no data vs 0 bytes of ata)
    nil))

(defmethod jss-io-response-status ((io jss-webkit-io))
  (flet ((make-status-line (response)
                           (format "%s %s"
                                   (cdr (assoc 'status response))
                                   (cdr (assoc 'statusText response)))))
    (if (assoc 'redirectResponse (jss-webkit-io-properties io))
        (make-status-line (cdr (assoc 'redirectResponse (jss-webkit-io-properties io))))
      (make-status-line (jss-webkit-io-response io)))))

(defmethod jss-io-id ((io jss-webkit-io))
  (cdr (assoc 'requestId (jss-webkit-io-properties io))))

(define-jss-webkit-notification-handler "Network.requestWillBeSent" (requestId loaderId documentURL request timestamp initiator stackTrace redirectResponse)
  (let ((io (make-instance 'jss-webkit-io
                           :properties params
                           :start-time timestamp
                           :lifecycle (list (list :sent timestamp)))))
    (setf (jss-tab-get-io tab requestId) io)
    (jss-console-insert-request console io)))

(defmacro with-existing-io (io-id &rest body)
  `(let ((io (jss-tab-get-io tab ,io-id)))
     (if io
         (progn ,@body)
       (jss-log-event (list :webkit-io
                            :unknown-requestId
                            ,io-id)))))
(put 'with-existing-io 'lisp-indent-function 1)

(define-jss-webkit-notification-handler "Network.dataReceived" (requestId timestamp dataLength encodedDataLength)
  (with-existing-io requestId
    (push (list :data-received timestamp
                :data-length dataLength
                :encoded-data-length encodedDataLength)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Network.loadingFailed" (requestId timestamp errorText canceled)
  (with-existing-io requestId
    (push (list :loading-failed timestamp
                :error-text errorText
                :canceled canceled)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Network.loadingFinished" (requestId timestamp)
  (with-existing-io requestId
    (push (list :loading-finished timestamp)
          (jss-io-lifecycle io))
    (lexical-let ((io io)
                  (requestId requestId))
      (jss-deferred-add-backs
        (jss-webkit-send-request tab `("Network.getResponseBody" (requestId . ,requestId)))
        (lambda (response)
          (setf (jss-webkit-io-responseBody io) response))
        (lambda (response)
          ;; don't really know what else to do if this fails.
          (jss-log-event (list "Network.getResponseBody" :error requestId response)))))
    (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Network.requestServedFromCache" (requestId)
  (with-existing-io requestId
    (push (list :served-from-cache nil)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Network.requestServedFromMemoryCache" (requestId loaderId documentURL timestamp initiator resource)
  (with-existing-io requestId
    (push (list :served-from-memory-cache timestamp
                :loader-id loaderId
                :document-url documentURL
                :initiator initiator
                :resource resource)
          (jss-io-lifecycle io))
    (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Network.responseReceived" (requestId loaderId timestamp type response)
  (with-existing-io requestId
    (push (list :response-received timestamp
                :loader-id loaderId
                :type type
                :response response)
          (jss-io-lifecycle io))
    (setf (jss-webkit-io-response io) response)
    
   (jss-console-update-request-message console io)))

(define-jss-webkit-notification-handler "Page.loadEventFired" (timestamp)
  (jss-console-log-message console "page loaded"))

(define-jss-webkit-notification-handler "Page.domContentEventFired" (timestamp)
  (jss-console-log-message console "dom content"))

;;; not in docs?
(define-jss-webkit-notification-handler "Page.frameNavigated" (frame)
  (jss-console-log-message console "frame %s navigated to %s" (cdr (assoc 'id frame)) (cdr (assoc 'url frame))))

;;; not in docs?
(define-jss-webkit-notification-handler "Page.frameDetached" (frameId)
  (jss-console-log-message console "frame %s detached" frameId))

(provide 'jss-browser-webkit)
