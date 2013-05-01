;;; jss.el --- emacs interface to webkit/firefox debuggers

;; Copyright (C) 2013 Edward Marco Baringer

;; Author: Marco Baringer <mb@bese.it>
;; Version: 0.7
;; Keywords: languages
;; Package-Requires: ((emacs "24.1") (websocket "0") (js2-mode "0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; An emacs implementation of the client side protocol of webkit and
;; firefox's over-the-wire debugging protocols.

;; This is just the top file for jsSlime. It simply loads the various
;; .el files in the right order

;;; Code:

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

(require 'jss-super-mode)
(require 'jss-http-repl)

(provide 'jss)

;;; jss.el ends here
