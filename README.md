# slime, but for javascript.

## Installation

### Requires:

1. Emacs 23 (for json and eieio)
1. websocket: https://github.com/ahyatt/emacs-websocket.git

### Setup

As for most emacs modes:

    (add-to-list 'load-path "/path/to/jss/")
    (require 'jss)

## Start-Up

### Chrome

    google-chrome --remote-debugging-port=9222

Note that chrome only listens for connection from localhost, you'll
need to setup port forwarding (ssh or otherwise) to connect to a
remote browser.

### Firefox

It's a pain.

### Emacs

Once you have a browser running run `M-x jss-connect` from within
emacs and specify the remote browser type ("Google Chrome" or
"Firefox"), the host and the port. You'll be presented with a list of
tabs. Hit `RET` on any of the console buttons to open a console.

## The Console

The console prints out messages, either network traffic or console.log
messages, and has a prompt at the bottom for sending javascript code
to the browser.

Keys:

* C-c C-r - reload current tab
* RET - If point is on a request link (an underlined url) goto io buffer
* TAB - goto next request link

## The Debugger

The debugger appears whenever there's an unhandlede exception from the
browser. It displays a list of stack frames and the exception itself.

Keys:

* t - toggle frame visibilty
* n/p - next/previous frame
* e - goto excption object (hit RET to expand exception objcet)
* s - goto frame source
* r/q - resume/continue debugger

## IO Inspector

For a given request shows the request, headers and payload, and
response (headers and payload)

## Remote Objects

Just hit RET on them to expand objects (support for
functions/arrays/dates/regexps is coming but not yet implemented).

In most buffers that have objects visibile just hit TAB to jump to the
next one.