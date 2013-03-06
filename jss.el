(require 'js2-mode)

(define-derived-mode jss-super-mode text-mode "Generic JSS Mode"
  "Functionality common to all JSS modes."
  t)

(define-key jss-super-mode-map (kbd "TAB") 'jss-next-button)

(require 'jss-text-manipulation)
(require 'jss-browser-api)
(require 'jss-script)
(require 'jss-remote-value)
(require 'jss-browser-webkit)
(require 'jss-browser-firefox)
(require 'jss-browser)
(require 'jss-console)
(require 'jss-io)
(require 'jss-debugger)

(provide 'jss)
