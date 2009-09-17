(in-package "COMMON-LISP-USER")

(defvar *x* "*x* is 701")

(defun foo ()
  ((lambda (x) (print x)) "start foo...")
  (print *x*)
  (let ((*x* "*x* is 702"))
    (print *x*)
    (bar "百"
         (lambda (x)
           (print "lambda")
           (print x))))
  (print *x*))

(defun bar (n f)
  (funcall f n)
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