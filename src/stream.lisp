(in-package :http-daemon) 

(defun sock-write(sock bytes)
  (write-sequence bytes (socket-stream sock))
  (force-output (socket-stream sock)))

(defun sock-write-str(sock str)
  (sock-write sock (str-to-bytes str)))

;; Reads data from socket until it blocks.
;; Note, if there's no data at the start,
;; it will still block and wait until there's
;; data to read. Any blocks after that will make
;; it stop
(defun read-until-block (sock)
  (let ((sock-stream (socket-stream sock)) 
	(buf (make-array '(20)
	       :adjustable t
	       :fill-pointer 0
	       :element-type '(unsigned-byte 8))))
    (loop 
      (vector-push-extend (read-byte sock-stream) buf)
      (if (null (listen sock-stream))
        (return-from read-until-block buf)))))

;; TODO: improve this algorithm.
;; reading a file byte-by-byte is really inefficient,
;; find a way to bulk-read
(defun read-file-to-array(arr f)
  (with-open-file (s f :element-type 'unsigned-byte) 
    (let ((b 0))
      (loop 
	(setf b (read-byte s nil nil))
        (if (null b)
	  (return)
	  (vector-push-extend b arr))))))
