;;; https://wiki.mozilla.org/Remote_Debugging_Protocol
;;; https://developer.mozilla.org/en-US/docs/Tools/Web_Console/remoting

;;;  { "to":"root", "type":"listTabs" }
;;; ->  { "from":"root", "tabs":[tab, ...], "selected":selected }

(require 'jss-browser-api)

(defclass jss-firefox-connection ()
  ((host :initarg :host :initform "127.0.0.1")
   (port :initarg :port :initform 6000)
   (proc)))

(defvar jss-firefox-open-connections (make-hash-table :test 'equal))

(defmethod jss-firefox-open-connection ((c jss-firefox-connection))
  (push c jss-firefox-open-connections)
  (setf (slot-value c 'proc) (make-network-process :name "firefox connect"
                                                   :buffer (get-buffer-create "*firefox proc*")
                                                   :server nil
                                                   :host (slot-value c 'host)
                                                   :service (slot-value c 'port)
                                                   :coding '(utf-8 . utf-8)
                                                   :nowait t
                                                   :filter 'jss-firefox-connection-filter))
  (set-process-sentinel (slot-value c 'proc) 'jss-firefox-connection-sentinel)
  c)

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
  (message "Sentinel saw %s -> %s" proc event))

(defun jss-firefox-make-json-message (json)
  (let ((string (json-encode json)))
    (format "%d:%s" (length string) string)))

(provide 'jss-browser-firefox)
