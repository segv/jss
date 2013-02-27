(setf debug-on-error t
      websocket-callback-debug-on-error t
      )
(add-to-list 'load-path "/Users/mb/m/.emacs/emacs-websocket/")
(add-to-list 'load-path "/Users/mb/m/.emacs/js2-mode/")
(add-to-list 'load-path "/Users/mb/m/.emacs/jss/")
(defun dbg () (interactive) (setf deferred:debug-on-signal t))
(require 'jss)

