;;; https://wiki.mozilla.org/Remote_Debugging_Protocol
;;; https://developer.mozilla.org/en-US/docs/Tools/Web_Console/remoting

;;;  { "to":"root", "type":"listTabs" }
;;; ->  { "from":"root", "tabs":[tab, ...], "selected":selected }

(require 'jss-browser-api)

(provide 'jss-browser-firefox)
