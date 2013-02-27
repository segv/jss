# slime, but for javascript.

## Installation

### Requires:

1. Emacs 23 (for json and eieio)
2. websocket: https://github.com/ahyatt/emacs-websocket.git
3. deferred: http://www.emacswiki.org/emacs/download/deferred.el (but including in source tree)

### Setup

As for most emacs modes:

  (add-to-list 'load-path "/path/to/jss/")
  (require 'jss)

## Start-Up

### Chrome

   google-chrome --remote-debugging-port=9222

If you're running chrome on a remote machine don't forget to setup the ssh port forwarding.

### Firefox

It's a pain.

### Emacs

  M-x jss-connect
