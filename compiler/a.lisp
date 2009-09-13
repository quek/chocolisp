(in-package "COMMON-LISP-USER")

(defun foo ()
  (bar 1))

(defun bar (n)
  (let ((n 100))
    (print n))
  (print n))

(foo)
(print "ok")