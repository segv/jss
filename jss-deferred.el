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

(defmethod jss-deferred-then ((d jss-deferred) &optional callback errorback)
  (when callback
    (jss-deferred-add-callback d callback))
  (when errorback
    (jss-deferred-add-errorback d errorback))  
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

(defun jss-deferred-wait-on-all (&rest deferreds)
  (lexical-let ((d (make-jss-deferred))
                (done '())
                (pending deferreds))
    (dolist (this deferreds)
      (lexical-let ((this this))
        (jss-deferred-then this
                           (lambda (value)
                             (setf pending (delq this pending)
                                   done (cons this done))
                             (when (null pending)
                               (jss-deferred-callback d done))
                             value))))
    d))

(provide 'jss-deferred)
