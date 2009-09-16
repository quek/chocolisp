(in-package "COMMON-LISP-USER")

(defvar *x* "*x* is 701")

(defun foo ()
  (print *x*)
  (let ((*x* "*x* is 702"))
    (print *x*)
    (bar "百")))

(defun bar (n)
  (print *x*)
  (let ((n 100))
    (print n)
    (setq n 99)
    (print n))
  (print n))

(foo)
(print "ok")