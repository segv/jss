;;; jss.el -- top file for jsSlime, just loads the various .el files in the right order
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
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

(define-derived-mode jss-super-mode text-mode "Generic JSS Mode"
  "Functionality common to all JSS modes."
  t)

(define-key jss-super-mode-map (kbd "<tab>") 'jss-next-button)
(define-key jss-super-mode-map (kbd "<S-tab>") 'jss-previous-button)

(require 'easymenu)
;(require 'js2-mode)
(require 'cl)
(require 'eieio)
(require 'json)

(require 'jss-utils)
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
