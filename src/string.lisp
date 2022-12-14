(in-package :http-daemon)

;; Some conversion functions.
;; The sockets are all set to have :element-type '(unsigned-byte 8)
;; this is because we want to allow for the transfer of binary files as well.
;; so we can't set it to character. In order to send strings over a connection like
;; this, we need to first convert them to a byte array, and vice versa.
(defun str-to-bytes(str)
  (let ((byte-v (make-array '(20) :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- (length str))
	  do (vector-push-extend (char-code (aref str i)) byte-v))
    byte-v))

(defun bytes-to-str(bytes)
  (let ((str (make-array (length bytes) :fill-pointer 0 :element-type 'character)))
    (loop for i from 0 to (1- (length bytes))
	  do (vector-push (code-char (aref bytes i)) str))
    str))

;; adds str2 to the end of str1 (assuming str1 is adjustable)
(defun add-str(str1 str2)
  (loop for i across str2
	do (vector-push-extend i str1)))

;; add carriage return and linefeed characters to a string
(defun add-newline(str)
  (vector-push-extend #\return str)
  (vector-push-extend #\linefeed str))

;; split a string on every occurrance of chr
(defun split-str(str chr &key once)
  (let ((ret nil) (last-split 0))
    (loop for i from 0 to (1- (length str))
	  do (when (CHAR= (aref str i) chr) 
	       (setf ret (append ret (list (subseq str last-split i))))
	       (setf last-split (1+ i))
	       (if once
		 (return))))
    (append ret (list (subseq str last-split (length str))))))
