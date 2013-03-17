;;; jss-browser-firefox.el -- firefox implementation of jss's browser api
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; https://wiki.mozilla.org/Remote_Debugging_Protocol
;;; https://developer.mozilla.org/en-US/docs/Tools/Web_Console/remoting
;;; file:///mozilla-release/toolkit/devtools

;;;  { "to":"root", "type":"listTabs" }
;;; ->  { "from":"root", "tabs":[tab, ...], "selected":selected }

(defclass jss-firefox-browser (jss-generic-browser)
  ((connection :accessor jss-firefox-browser-connection :initform nil)

   (chrome-debugger-actor)
   (console-actor)
   (profiler-actor)))

(defmethod jss-browser-cleanup ((browser jss-firefox-browser))
  (jss-log-event (list :firefox :browser :cleanup browser))
  (jss-when-bind (conn (jss-firefox-browser-connection browser))
    (jss-firefox-connection-cleanup (jss-firefox-browser-connection browser))))

(defclass jss-firefox-tab (jss-generic-tab)
  ((properites :initarg :properties)))

(defmethod jss-tab-url ((tab jss-firefox-tab))
  (cdr (assoc 'url (slot-value tab 'properites))))

(defmethod jss-tab-title ((tab jss-firefox-tab))
  (cdr (assoc 'title (slot-value tab 'properites))))

(defmethod jss-tab-id ((tab jss-firefox-tab))
  (cdr (assoc 'actor (slot-value tab 'properites))))

(defmethod jss-tab-available-p ((tab jss-firefox-tab))
  (cdr (assoc 'consoleActor (slot-value tab 'properites))))

(defclass jss-firefox-connection ()
  ((host :initarg :host :initform "127.0.0.1")
   (port :initarg :port :initform 6000)
   (proc :initarg :proc)
   (state :initform nil)
   (on-open :initarg :on-open)
   (on-close :initarg :on-close)
   (message-queue :initform (make-jss-queue)
                  :accessor jss-firefox-connection-message-queue)
   (message-in-transit :initform nil
                       :accessor jss-firefox-connection-message-in-transit)
   (ready :initform nil :accessor jss-firefox-connection-ready)))

(defmethod jss-firefox-connection-cleanup ((conn jss-firefox-connection))
  (jss-log-event (list :firefox :connection :cleanup conn))
  (if (processp (slot-value conn 'proc))
      (delete-process (slot-value conn 'proc))
    (error "%s is not a process :(")))

(defmethod jss-firefox-browser-connect ((browser jss-firefox-browser))
  (let* ((host (jss-browser-host browser))
         (port (jss-browser-port browser))
         (port (if (stringp port)
                   (string-to-number port)
                 port))
         conn
         proc)
    (lexical-let* ((browser browser)
                   (deferred (make-jss-deferred))
                   (buffer (generate-new-buffer (format " *firefox@%s:%s*" host port))))
      (with-current-buffer buffer
        (setf jss-current-browser-instance browser))
      (setf proc (make-network-process :name "firefox connect"
                                       :buffer buffer
                                       :server nil
                                       :host host
                                       :service port
                                       :coding '(utf-8 . utf-8)
                                       :nowait t
                                       :filter 'jss-firefox-process-filter)
            conn (make-instance 'jss-firefox-connection
                                :host host
                                :port port
                                :on-open (lambda ()
                                           (jss-deferred-callback deferred browser))
                                :on-close (lambda ()
                                            (dolist (tab (jss-browser-tabs browser))
                                              (jss-console-error-message (jss-tab-console tab) "Connection closed."))
                                            (jss-firefox-connection-cleanup (jss-firefox-browser-connection browser))
                                            (setf (jss-firefox-browser-connection browser) nil))
                                :proc proc)
            (jss-firefox-browser-connection browser) conn)

      ;; firefox will always send us a 'hello' message when the conncetion opens, so setup a handler for it.
      (lexical-let ((conn (jss-firefox-browser-connection browser)))
        (setf (jss-firefox-connection-message-in-transit conn)
              (make-jss-deferred
               (lambda (json)
                 (jss-log-event (list :firefox :hello json))))))
      (set-process-sentinel proc 'jss-firefox-process-sentinel)
      deferred)))

(defun jss-firefox-process-filter (proc string)
  (jss-log-event (list :firefox :filter string))
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
            (goto-char (match-end 0))
            (if (= length (- (point-max) (point)))
                (let ((json (json-read)))
                  (delete-region (point-min) (point-max))
                  (set-marker (process-mark proc) (point))
                  (jss-firefox-handle-message (jss-current-browser) json))
              (jss-log-event (list :firefox :process-filter :message-incomplete))))
        (error "Syntax error, message does not start with length marker.")))))

(defun jss-firefox-process-sentinel (proc event)
  (jss-log-event (list :firefox :sentinel proc event))
  (with-current-buffer (process-buffer proc)
    (let* ((browser (or (jss-current-browser)
                        (error "Sentinel event on proces %s but no browser." proc)))
           (connection (or (jss-firefox-browser-connection browser)
                           (error "Browser %s has not connection, but get proc event %s on %s." browser event proc))))
      
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
          (:closed (funcall (slot-value connection 'on-close)))))))
  t)

(defmethod jss-browser-description ((browser jss-firefox-browser))
  (format "Mozilla Firefox @ %s:%s\nNB: Only displaying tabs that can be debugged." (slot-value browser 'host) (slot-value browser 'port)))

(defmethod jss-browser-get-tabs ((browser jss-firefox-browser))
  (lexical-let (connect-deferred
                (tabs-deferred (make-jss-deferred)))
    (if (jss-firefox-browser-connection browser)
        (setf connect-deferred (make-jss-completed-deferred :callback browser))
      (setf connect-deferred (jss-firefox-browser-connect browser)))

    (jss-deferred-add-callback
     connect-deferred
     (lambda (browser)
       (jss-deferred-add-callback
        (jss-firefox-send-message browser '((to . "root") (type . "listTabs")))
        (lexical-let ((browser browser))
          (lambda (response)
            (jss-log-event (list :firefox :listTabs response))
            (loop for properites across (cdr (assoc 'tabs response))
                  for existing-tab = (jss-browser-find-tab browser (cdr (assoc 'actor properites)))
                  ;; title, url, actor and consoleActor
                  if existing-tab
                    do (setf (slot-value existing-tab 'properites) properites)
                  else
                    do (let ((tab (make-instance 'jss-firefox-tab :properties properites)))
                         (push (cons (jss-tab-id tab) tab) (slot-value browser 'tabs)))
                  finally (jss-log-event (list :firefox :listTabs (jss-browser-tabs browser)))
                  finally (jss-deferred-callback tabs-deferred browser)))))))
    
    tabs-deferred))

(defmethod jss-firefox-initialize-connection ((browser jss-firefox-browser))
  (let ((conn (jss-firefox-browser-connection browser)))
    (if (and conn (jss-firefox-connection-open-p conn))
        (make-jss-completed-deferred :callback browser)
      (setf (jss-firefox-browser-connection browser) (jss-firefox-browser-connect browser))
      (jss-firefox-connection-open (jss-firefox-browser-connection browser)))
    conn))

(defmethod jss-firefox-send-message ((tab jss-firefox-tab) message)
  (jss-firefox-send-message (jss-tab-browser tab) message))

(defmethod jss-firefox-send-message ((browser jss-firefox-browser) message)
  (jss-firefox-send-message (jss-firefox-browser-connection browser) message))

(defun jss-firefox-encode-json-message (object)
  (let ((json-false :json-false)
        (json-null nil))
    (let ((string (json-encode object)))
      (format "%d:%s" (length string) string))))

(defmethod jss-firefox-send-message ((connection jss-firefox-connection) message)
  (jss-log-event (list :firefox :send-message connection message))
  (let* ((deferred (make-jss-deferred)))
    (jss-enqueue (jss-firefox-connection-message-queue connection) (cons message deferred))
    (jss-firefox-connection-process-next-message connection)
    deferred))

(defmethod jss-firefox-connection-process-next-message ((connection jss-firefox-connection))
  (when (and (not (jss-firefox-connection-message-in-transit connection))
             (not (jss-queue-empty-p (jss-firefox-connection-message-queue connection))))
    (destructuring-bind (message . deferred)
        (jss-dequeue (jss-firefox-connection-message-queue connection))
      (jss-log-event (list :firefox :process-message message deferred))
      (jss-firefox-connection-write-message connection message deferred)
      deferred)))

(defmethod jss-firefox-connection-write-message ((connection jss-firefox-connection) message deferred)
  (let ((json (jss-firefox-encode-json-message message)))
    (jss-log-event (list :firefox :write-message connection message json))
    (setf (jss-firefox-connection-message-in-transit connection) deferred)
    (process-send-string (slot-value connection 'proc) json)
    deferred))

(defmethod jss-firefox-handle-message ((browser jss-firefox-browser) json)
  (jss-firefox-handle-message (jss-firefox-browser-connection browser) json))

(defmethod jss-firefox-handle-message ((connection jss-firefox-connection) json)
  (jss-log-event (list :firefox :handle-message json))
  (let ((deferred (jss-firefox-connection-message-in-transit connection)))
    (unless deferred
      (error "Got message %s from %s but don't have a message in transit to respond." json))
    (setf (jss-firefox-connection-message-in-transit connection) nil)
    (jss-deferred-callback deferred json)
    (jss-firefox-connection-process-next-message connection)))

(provide 'jss-browser-firefox)
