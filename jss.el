(require 'js2-mode)

(defvar jss-super-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map text-mode-map))
    map))

(define-derived-mode jss-super-mode text-mode "Generic JSS Mode"
  "Functionality common to all JSS modes."
  t)

(defun jss-log-event (event)
  (with-current-buffer (get-buffer-create " *jss-events*")
    (insert (format ";; %s\n" (format-time-string "%Y-%m-%dT%T")))
    (dolist (event-part event)
      (insert (prin1-to-string event-part) "\n"))
    (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")))

(defun jss-comment-char (string)
  (insert (propertize string
                      'face font-lock-comment-face
                      'font-lock-face font-lock-comment-face)))

(defun jss-eol-mark ()
  (when (member (preceding-char) (list ?  ?\n ?\t ?\r))
    (jss-comment-char "$"))
  (insert "\n"))

(defun jss-insert-with-highlighted-whitespace (string)
  (save-match-data
    (when (string= "" string)
      (jss-comment-char "^$"))
    (when (string-match "^[ \t\r\n\f]" string)
      (jss-comment-char "^"))
    (insert string)
    (when (string-match "[ \t\r\n\f]$" string)
      (jss-comment-char "$"))))

(defun jss-section-marker ()
  (insert "--------------------------------\n"))

(defun jss-have-next-property-block (property-name)
  (or (get-text-property (point) property-name)
      (next-single-property-change (point) property-name)))

(defun jss-have-previous-property-block (property-name)
  (or (get-text-property (point) property-name)
      (previous-single-property-change (point) property-name)))

(defun jss-start-of-next-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-start-of-current-property-block property-name)))
    (let ((next-change (next-single-property-change (point) property-name)))
      (when next-change
        (return (goto-char next-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-max))
          (error "Unable to find start of next block with property %s" property-name))
        (forward-char 1))
      (return (point)))))

(defun jss-end-of-previous-property-block (property-name)
  (block nil
    (when (get-text-property (point) property-name)
      (return (jss-end-of-current-property-block property-name)))

    (let ((previous-change (if (eobp) ;; previous-single-property-change works differently at eobp, a char by char search is easier
                               nil
                             (previous-single-property-change (point) property-name))))
      (when previous-change
        (return (goto-char previous-change)))
      (while (not (get-text-property (point) property-name))
        (when (= (point) (point-min))
          (error "Unable to find previous block with property %s" property-name))
        (backward-char 1))
      (return (point)))))

(defun jss-start-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get start of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-min))
        (return))
      (backward-char 1))
    (forward-char 1))
  (point))

(defun jss-end-of-current-property-block (property-name)
  (unless (get-text-property (point) property-name)
    (error "Attempting to get end of current block with property %s, but point doesn't have this property." property-name))
  (block nil
    (while (get-text-property (point) property-name)
      (when (= (point) (point-max))
        (return))
      (forward-char 1)))
  (point))

(defun jss-find-property-block (property-name property-value)
  (save-excursion
    (goto-char (point-max))
    (let (block-start block-end)

      (while (not (equal (get-text-property (point) property-name) property-value))
        (when (= (point) (point-min))
          (error "Unable to find block with property %s equal to %s." property-name property-value))
        (backward-char 1))
      (setf block-end (min (1+ (point)) (point-max)))

      (block nil
        (while (and (equal (get-text-property (point) property-name) property-value)
                    (< (point-min) (point)))
          (backward-char 1)))
      (setf block-start (min (1+ (point)) (point-max)))

      (cons block-start block-end))))

(defun jss-delete-property-block (property-name property-value)
  (let ((location (jss-find-property-block property-name property-value))
        (inhibit-read-only t))
    (delete-region (car location) (cdr location))))

(defun jss-insert-with-properties (property-list format-control &rest format-args)
  (let ((start (point)))
    (insert-and-inherit (apply 'format format-control format-args))
    (add-text-properties start (point) property-list)))

(require 'jss-browser-api)
(require 'jss-browser-chrome)
(require 'jss-browser-firefox)
(require 'jss-browser)
(require 'jss-console)
(require 'jss-io)
(require 'jss-debugger)

(provide 'jss)
