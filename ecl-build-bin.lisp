#-ecl
(error "You're not using ECL!~%")

(defvar *exe-file-name* "cl-http-daemon")
(if (null (find :unix *features*))
  (setf *exe-file-name* (concatenate 'string *exe-file-name* ".exe")))

(ext:install-c-compiler)
(c:build-program *exe-file-name*
		 :lisp-files (list 
			       (compile-file "src/proxy-package.lisp" :system-p t)))
