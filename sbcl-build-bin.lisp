(load "src/proxy-package.lisp")

;; change the binary name to have the .exe extension on non-unix
;; systems
(defvar *exe-file-name* "cl-http-daemon")
(if (null (find :unix *features*))
  (setf *exe-file-name* (concatenate 'string *exe-file-name* ".exe")))
(sb-ext:save-lisp-and-die *exe-file-name* 
			  :executable t
			  :toplevel #'local.http-server:main)
