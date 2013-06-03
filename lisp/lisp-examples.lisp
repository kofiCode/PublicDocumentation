(in-package :cl)
(defpackage :lib-pkg 
  (:use :cl) 
  (:export :say-hello)
  )
(defun say-hello () "hello")

(in-package :lib-pkg)

(defpackage :app (:use :cl))

(in-package :app)

(defun get-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))

(get-all-symbols 'sb-thread)


