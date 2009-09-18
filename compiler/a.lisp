(chimacho::in-package "CHIMACHO")

(defvar *x* "*x* is 701")

(defun foo ()
  (is ((lambda (x) (print x)) "start foo...")
      "start foo...")
  (print *x*)
  (let ((*x* "*x* is 702"))
    (print *x*)
    (bar '(hello "百")
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

(defmacro unless (test form)
  (list 'if test form))

(unless nil
  (foo))

(print "ok")