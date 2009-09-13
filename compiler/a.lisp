(in-package "COMMON-LISP-USER")

(defun foo ()
  (bar 1))

(defun bar (n)
  (print n))

(foo)
(print "ok")