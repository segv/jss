
(defvar jss-io-cleaners (make-hash-table :test 'equal))

(defmacro* define-jss-io-cleaner (content-type (data) &body body)
  (declare (indent 2))
  (let ((type (gensym)))
    `(let ((,type ',content-type))
       (dolist (type (if (listp ,type) ,type (list ,type)))
         (setf (gethash ',content-type jss-io-cleaners) (lambda (,data) ,@body)))
       ',content-type)))

(defun jss-io-fontify-data-with-mode (data mode)
  (let (out (buffer (generate-new-buffer " *jss-fontification*")))
    (with-current-buffer buffer
      (insert data)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (setf out (buffer-substring (point-min) (point-max))))
    out))

(define-jss-io-cleaner "text/html" (html)
  (jss-io-fontify-data-with-mode html 'nxml-mode))

(define-jss-io-cleaner "text/css" (css)
  (jss-io-fontify-data-with-mode css 'css-mode))

(define-jss-io-cleaner ("application/javascript" "text/javascript") (js)
  (jss-io-fontify-data-with-mode js 'js2-mode))

(define-jss-io-cleaner ("application/json") (json)
  (let (parsed)
    (with-temp-buffer
      (insert (jss-io-response-data io))
      (goto-char (point-min))
      (setf parsed (json-read)))
    (insert (pp-to-string parsed))))

(provide 'jss-io-pretty-printers)
