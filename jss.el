
(define-derived-mode jss-super-mode text-mode "Generic JSS Mode"
  "Functionality common to all JSS modes."
  t)

(define-key jss-super-mode-map (kbd "<tab>") 'jss-next-button)
(define-key jss-super-mode-map (kbd "<S-tab>") 'jss-previous-button)

(require 'easymenu)
(require 'js2-mode)
(require 'cl)
(require 'eieio)
(require 'json)

(require 'jss-text-manipulation)
(require 'jss-deferred)
(require 'jss-browser-api)
(require 'jss-script)
(require 'jss-remote-value)
(require 'jss-browser-webkit)
(require 'jss-browser-firefox)
(require 'jss-browser)
(require 'jss-prompt)
(require 'jss-console)
(require 'jss-io-pretty-printers)
(require 'jss-io)
(require 'jss-debugger)
(require 'jss-http-repl)

(provide 'jss)
