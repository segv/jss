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

(require 'jss-browser-api)
(require 'jss-browser-chrome)
(require 'jss-browser-firefox)
(require 'jss-browser)
(require 'jss-console)
(require 'jss-network-inspector)
(require 'jss-debugger)

(provide 'jss)
