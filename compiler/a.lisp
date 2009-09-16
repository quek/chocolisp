(in-package "COMMON-LISP-USER")

(defvar *x* "*x* is 701")

(defun foo ()
  (print *x*)
  (let ((*x* "*x* is 702"))
    (print *x*)
    (bar "百"))
  (print *x*))

(defun bar (n)
  (print *x*)
  (let ((n 100))
    (setq *x* "*x* is 703")
    (print n)
    (setq n 99)
    (print n))
  (print n)
  (print *x*))

(foo)
(print "ok")