(require 'eieio)

(defclass jss-deferred ()
  ((callbacks :initarg :callbacks :accessor jss-deferred-callbacks)
   (errorbacks :initarg :errorbacks :accessor jss-deferred-errorbacks)
   (state :initform (cons :waiting nil) :accessor jss-deferred-state)))

(defun make-jss-deferred (&optional callback errorback)
  (make-instance 'jss-deferred
                 :callbacks (if callback
                                (list callback)
                              '())
                 :errorbacks (if errorback
                                 (list errorback)
                               '())))

(defun* make-jss-completed-deferred (&key callback errorback)
  (let ((d (make-jss-deferred)))
    (cond
     ((and callback (not errorback))
      (jss-deferred-callback d callback))
     ((and errorback (not callback))
      (jss-deferred-errorback d errorback))
     (t
      (error "Invalid arguments to make-jss-completed-deferred. Exactly one of :callback, :errorback must be specified.")))
    d))

(defmacro appendf (place &rest elements)
  `(setf ,place (append ,place ,@elements)))

(defmethod jss-deferred-add-callback ((d jss-deferred) callback)
  (if (eql :ok (car (jss-deferred-state d)))
      (funcall callback (cdr (jss-deferred-state d)))
    (appendf (jss-deferred-callbacks d) (list callback))))

(defmethod jss-deferred-add-errorback ((d jss-deferred) errorback)
  (if (eql :fail (car (jss-deferred-state d)))
      (funcall errorback (cdr (jss-deferred-state d)))
    (appendf (jss-deferred-errorbacks d) (list errorback))))

(defmethod jss-deferred-add-backs ((d jss-deferred) &optional callback errorback)
  (lexical-let ((new-deferred (make-jss-deferred)))
    (when callback  (jss-deferred-add-callback d callback))
    (when errorback (jss-deferred-add-errorback d errorback)))  
  d)

(defmethod jss-deferred-callback ((d jss-deferred) value)
  (while (jss-deferred-callbacks d)
    (funcall (pop (jss-deferred-callbacks d)) value))
  (setf (jss-deferred-state d) (cons :ok value))
  value)

(defmethod jss-deferred-errorback ((d jss-deferred) value)
  (while (jss-deferred-errorbacks d)
    (funcall (pop (jss-deferred-errorbacks d)) value))
  (setf (jss-deferred-state d) (cons :fail value))
  value)

(defun jss-deferred-then (before callback &optional errorback)
  "Creates a new deferred which is triggered after `before`. 

after, the returned deferred, will be passed the result of
applying callback to `before`'s value."
  (lexical-let ((after (make-jss-deferred))
                (callback callback)
                (errorback errorback))
    (jss-deferred-add-callback before
                               (lambda (value)
                                 (jss-deferred-callback after (funcall callback value))))
    (if errorback
        (jss-deferred-add-errorback before
                                    (lambda (value)
                                      (jss-deferred-errorback after (funcall errorback value))))
      (jss-deferred-errorback before (lambda (value) (jss-deferred-errorback after value))))
    after))

(defun jss-deferred-wait-on-all (&rest deferreds)
  (lexical-let ((after (make-jss-deferred))
                (successes '())
                (failures  '())
                (pending   '()))
    (dolist (this deferreds)
      (lexical-let ((this this))
        (jss-deferred-add-backs this
                                (lambda (value)
                                  (setf pending (delq this pending)
                                        successes (cons this value))
                                  (when (null pending)
                                    (if failures
                                        (jss-deferred-errorback after (list failures successes))
                                      (jss-deferred-callback after (list successes)))))
                                (lambda (value)
                                  (setf pending (delq this pending)
                                        failures (cons this value))
                                  (when (null pending)
                                    (jss-deferred-errorback after (list failures successes)))))))
    after))

(provide 'jss-deferred)
