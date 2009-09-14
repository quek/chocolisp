(in-package "COMMON-LISP-USER")

(defun foo ()
  (bar "百"))

(defun bar (n)
  (let ((n 100))
    (print n))
  (print n))

(foo)
(print "ok")