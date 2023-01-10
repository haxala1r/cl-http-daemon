(in-package :http-daemon)

;; todo: get something more portable here
(defvar *raw-args* nil)
(defvar *args-parsed* nil)

;; TODO: error checking.
(defun parse-args ()
  (setf *raw-args* (uiop:command-line-arguments))
  (if (> 2 (length *raw-args*))
    (return-from parse-args))
  (loop for i from 0 to (- (length *raw-args*) 2)
	do (when (CHAR= (char (nth i *raw-args*) 0) #\-)
	     (push (list (nth i *raw-args*) 
			 (nth (1+ i) *raw-args*))
		   *args-parsed*)
	     (incf i))))

(defun get-arg (key)
  (loop for i in *args-parsed*
	do (if (string= (first i) key)
	     (return (second i)))))
