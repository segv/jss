;;; https://wiki.mozilla.org/Remote_Debugging_Protocol
;;; https://developer.mozilla.org/en-US/docs/Tools/Web_Console/remoting
;;; file:///mozilla-release/toolkit/devtools

;;;  { "to":"root", "type":"listTabs" }
;;; ->  { "from":"root", "tabs":[tab, ...], "selected":selected }

(require 'jss-browser-api)

(defclass jss-firefox-browser (jss-generic-browser)
  ((connection :accessor jss-firefox-browser-connection :initform nil)))

(defmethod jss-browser-tabs ((browser jss-firefox-browser))
  '())

(defclass jss-firefox-connection ()
  ((host :initarg :host :initform "127.0.0.1")
   (port :initarg :port :initform 6000)
   (proc :initarg :proc)
   (state :initform nil)
   (on-open :initarg :on-open)
   (on-close :initarg :on-close)))

(defvar jss-firefox-processes (make-hash-table :test 'eq))

(defmethod jss-firefox-browser-connect ((browser jss-firefox-browser))
  (let* ((host (jss-browser-host browser))
         (port (jss-browser-port browser))
         (port (if (stringp port)
                   (string-to-number port)
                 port))
         conn
         proc)
    (lexical-let* ((browser browser)
                   (deferred (make-jss-deferred)))
      (setf proc (make-network-process :name "firefox connect"
                                       :buffer (get-buffer-create "*firefox proc*")
                                       :server nil
                                       :host host
                                       :service port
                                       :coding '(utf-8 . utf-8)
                                       :nowait t
                                       :filter 'jss-firefox-connection-filter)
            conn (make-instance 'jss-firefox-connection
                                :host host
                                :port port
                                :on-open (lambda ()
                                           (jss-deferred-callback deferred browser))
                                :on-close (lambda ()
                                            (dolist (tab (jss-browser-tabs browser))
                                              (jss-console-error-message (jss-tab-console tab) "Connection closed."))
                                            (setf (jss-firefox-browser-connection browser) nil))
                                :proc)
            (gethash proc jss-firefox-processes) conn)
      (set-process-sentinel proc 'jss-firefox-connection-sentinel)
      deferred)))

(defun jss-firefox-connection-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      (goto-char (point-min))
      (if (looking-at "\\([0-9]+\\):")
          (let ((length (string-to-number (match-string 1))))
            (goto-match (match-end 0))
            (if (= length (- (point-max) (point)))
                (let ((json (json-read)))
                  (delete-region (point-min) (point-max))
                  (set-marker (process-mark proc) (point))
                  (jss-firefox-connection-message proc json))
              (message "Incomplete message.")))
        (error "Syntax error, message does not start with length marker.")))))

(defun jss-firefox-connection-sentinel (proc event)
  (message "Sentinel saw %s -> %s" proc event)
  (let ((connection (gethash proc jss-firefox-processes)))
    (unless connection
      (error "Sentinel event on proces %s for unknown connection." proc))
    (with-slots (state) connection
      (setf state (cond
                   ((string= "open\n" event)
                    (unless (eql nil state)
                      (error "Invalid state transition. Was %s but got a %s event on %s.") state event proc)
                    :open)
                   ((cl-member event '("deleted\n" "finished\n" "connection broken by remote peer\n") :test 'string=)
                    (unless (eql :open state)
                      (error "Invalid state transition. Was %s but got a %s event on %s.") state event proc)
                    :closed)
                   (t (error "Unknown process event %s" (prin1-to-string event)))))
      (ecase state
        (:open (funcall (slot-value connection 'on-open)))
        (:closed (funcall (slot-value connection 'on-close))))))
  t)

(defun jss-firefox-make-json-message (json)
  (let ((string (json-encode json)))
    (format "%d:%s" (length string) string)))

(defmethod jss-browser-description ((browser jss-firefox-browser))
  (format "Mozilla Firefoxt @ %s:%s\nNB: Only displaying tabs that can be debugged." (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-browser-get-tabs ((browser jss-firefox-browser))
  (lexical-let (connect-deferred
                (tabs-deferred (make-jss-deferred)))
    (if (jss-firefox-browser-connection browser)
        (setf connect-deferred (make-jss-completed-deferred :callback browser))
      (setf connect-deferred (jss-firefox-browser-connect browser)))

    (jss-deferred-add-callback connect-deferred
                               (lambda (browser)
                                 (jss-deferred-add-callback
                                  (jss-firefox-send-message '((to . "root")
                                                              (type . "listTabs")))
                                  (lambda (response)
                                    (jss-deferred-callback tabs-deferred (cdr (assoc 'tabs response)))))))
    tabs-deferred))

(defmethod jss-firefox-initialize-connection ((browser jss-firefox-browser))
  (let ((conn (jss-firefox-browser-connection browser)))
    (if (and conn (jss-firefox-connection-open-p conn))
        (make-jss-completed-deferred :callback browser)
      (setf (jss-firefox-browser-connection browser) (jss-firefox-browser-connect browser))
      (jss-firefox-connection-open (jss-firefox-browser-connection browser)))
    conn))

(provide 'jss-browser-firefox)
