(in-package :http-daemon)

(defvar *server-port* nil)
(if (null *server-port*)
  (setf *server-port* 8080))

(defvar *server-socket* nil)
(defvar *active-connections* nil)
(defvar *current-conn* nil)

(defun remove-item (item l)
  (let ((newl nil))
    (loop
      (when (null l)
	(return-from remove-item newl))
      (when (eql (car l) item)
	(return-from remove-item (append newl (cdr l))))
      (setf l (cdr l)))))

(defun close-conn (conn)
  (setf *active-connections* (remove-item conn *active-connections*))
  (socket-close conn))

;; This class holds the information about a request
;; in an easy to access format.
(defclass http-request()
  ((req-method
     :initform nil
     :initarg :req-method
     :accessor req-method)
   (req-path
     :initform "/"
     :initarg :req-path
     :accessor req-path)
   (req-version
     :initform nil
     :accessor req-version)
   (req-host
     :initform nil
     :initarg :req-host
     :accessor req-host)
   (req-agent
     :initform nil
     :accessor req-agent)
   (req-connection
     :initform "close"
     :accessor req-connection)))

;; make http-request printable, so that we can debug.
(defmethod print-object((req http-request) out)
  (with-slots (req-agent req-method req-path req-host) req
    (print-unreadable-object (req out :type t)
      (format t "user ~a wanted to ~a ~a at ~a from etc." req-agent req-method req-path req-host))))

;; Parse a request and return an object containing the necessary info.
;; NOTE: this just returns a parsed object containing the useful data.
;; POST request bodies etc. are just ignored for now.
(defun parse-request(str)
  (let ((lines (split-str (remove #\return str) #\linefeed))
	(req (make-instance 'http-request)))
    ; first line contains method, path and version.
    (let ((fline (split-str (first lines) #\Space)))
      (setf (req-method req) (first fline))
      (setf (req-path req) (second fline))
      (setf (req-version req) (third fline))
      (if (not (string= (req-method req) "GET"))
	(error "ERROR: methods other than GET aren't supported (yet)"))
      (if (not (string= (req-version req) "HTTP/1.1"))
	(error "ERROR: client is not using HTTP/1.1")))
    ; the rest can be in a pretty random order, so we gotta deal with
    ; that.
    (loop for i from 1  to (1- (length lines))
	  do (let ((field (split-str (nth i lines) #\: :once t)))
	       (cond 
		 ((string= (first field) "Host")
		  (setf (req-host req) (subseq (second field) 1)))
	         ((string= (first field) "User-Agent")
		  (setf (req-agent req) (subseq (second field) 1)))
		 ((string= (first field) "Connection")
		  (setf (req-connection req) (subseq (second field) 1))) )))
    req))

(defun get-content-type(file-path)
  (if (equal file-path nil)
    (return-from get-content-type "Content-Type: unknown"))
  (let ((filetype (pathname-type file-path)))
    (cond 
      ((string= filetype "txt")  "Content-Type: text/plain")
      ((string= filetype "html") "Content-Type: text/html")
      ((string= filetype "xml")  "Content-Type: text/xml")
      ((string= filetype "js")   "Content-Type: text/javascript")
      ((string= filetype "css")  "Content-Type: text/css")
      ((string= filetype "md")   "Content-Type: text/markdown")
      ((string= filetype "mp4")  "Content-Type: audio/mp4")
      ((string= filetype "jpeg") "Content-Type: image/jpeg")
      ((string= filetype "png")  "Content-Type: image/png")
      ((string= filetype "zip")  "Content-Type: application/zip")
      (t "Content-Type: text/plain"))))

(defun make-response(req)
  (let ((str (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
	(file-name (probe-file (subseq (req-path req) 1)))
	(file-arr (make-array 0 :fill-pointer 0 :adjustable t :element-type 'unsigned-byte)))
    ;; This displays a default 404 page if no page was found.
    ;; TODO: make the user specify a custom 404 page.
    (if (not (null file-name))
      (read-file-to-array file-arr file-name)
      (add-str file-arr (str-to-bytes "<h1>404 NOT FOUND</h1>")))
    ;; status line
    (add-str str (req-version req))
    (add-str str (if (null file-name)
		   " 404 NOT FOUND"
		   " 200 OKAY"))
    (add-newline str)
    
    ;; Maybe add some more headers?
    (add-str str (get-content-type file-name))
    (add-newline str)
 
    (add-str str "Content-Length: ")
    (add-str str (write-to-string (length file-arr)))
    (add-newline str)

    (add-str str "Connection: ")
    (add-str str (req-connection req))
    (add-newline str)

    (add-newline str)
    ;; Data if file exists.
    ;; Since data might be binary, we are simply going to turn str
    ;; into a byte vector, then add the file's data to it.
    (setf str (str-to-bytes str))
    (add-str str file-arr)
    str))

(defun handle-conn ()
  (loop
    (let ((req (parse-request (bytes-to-str (read-until-block *current-conn*)))))
      ;; perform some checks on certain parameters
      (if (string= (req-path req) "/")
        (setf (req-path req) "/index.html"))
      ; sock-write defined in stream.lisp
      (sock-write *current-conn* (make-response req))
      (force-output (socket-stream *current-conn*))
      (if (string= (req-connection req) "close")
	(return))))
  (close-conn *current-conn*))

(defun cleanup ()
  (format t "performing cleanup~%")
  (loop for i in *active-connections*
	do (socket-close i))
  (format t "All connections have been closed~%")
  (socket-close *server-socket*)
  (format t "Server socket stopped~%"))

;convenience
(defmacro while (con &rest body)
  `(do () ((not ,con) ())
     ,@body))

(defun main ()
  (parse-args)
  (print *args-parsed*)
  ;; TODO: proper error checking
  (setf *server-port* (parse-integer (or (get-arg "-p") "8080")))
  (setf *server-socket* (socket-listen "0.0.0.0" *server-port*))
  (while t
    (let ((new-conn (socket-accept *server-socket* :element-type '(unsigned-byte 8))))
      (push new-conn *active-connections*)
      (make-thread 'handle-conn 
		   :initial-bindings (acons '*current-conn* new-conn nil)))))
