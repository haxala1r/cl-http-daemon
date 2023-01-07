# hi

This is a HTTP/1.1 daemon written in Common Lisp. Binary distributions will come soon.

## Building
First, clone the repository somewhere, and make sure your ASDF installation is aware of it. If you're using quicklisp, make sure you specify the path to the .asd file in this repo in quicklisp/local-projects/system-index.txt

Or, instead of that, you can run this from your favourite CL implementation in the repo's root:
`(asdf:load-asd (merge-pathnames "http-daemon.asd" (uiop:getcwd)))`

After that, it's simply a matter of running this on your favourite CL implementation:
`(asdf:make "http-daemon")`

And this will generate an executable called `cl-http-daemon` in the repo's root.

## Using
Simply run the binary apropriate for your platform. It runs on port 8080 by default, and you can access the web page by going to http://localhost:8080/

Currently, it will just serve the current directory as it is. If there is no path specified, it will serve index.html. If the file is not found, it will try to serve 404.html, or just send a simple built-in 404 message if that file is not found either.
