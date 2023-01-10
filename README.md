# hi
This is a HTTP/1.1 daemon written in Common Lisp.

name pending obviously...

## Building
First, clone the repository somewhere, and make sure you have quicklisp installed. After that you can load the build-sbcl.lisp file from sbcl like this:
`sbcl --load build-sbcl.lisp`

This should generate the executable for the app.

## Using
Simply run the binary apropriate for your platform. It runs on port 8080 by default, and you can access the web page by going to http://localhost:8080/

Instead of a binary distribution you can also load `run.lisp` with your favourite Common Lisp implementation.

Currently, it will just serve the current directory as it is. If there is no path specified, it will serve index.html. If the file is not found, it will try to serve 404.html, or just send a simple built-in 404 message if that file is not found either.
