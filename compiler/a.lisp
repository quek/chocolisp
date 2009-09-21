(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *package* (find-package "CHIMACHO")))

(let ((x 0))
  (defun get-x ()
    x))
(is 0 (get-x))

(defun %cadr (x)
  (car (cdr x)))
(defun %caddr (x)
  (car (cdr (cdr x))))
(defun %mapcar (f list)
  (if list
      (cons (funcall f (car list)) (%mapcar f (cdr list)))))

(let ((count 0))
  (defun count-inc ()
    (setq count (+ count 1)))
  (defun count-get ()
    count))
(count-inc)
(is 1 (count-get))
(count-inc)
(is 2 (count-get))

(flet ((%cadr (x)
         (+ x 1))
       (%caddr (x)
         (%cadr x)))
  (is 2 (%cadr 1))
  (is 20 (%caddr '(10 20 30))))

(labels ((%cadr (x)
           (+ x 1))
         (%caddr (x)
           (%cadr x)))
  (is 2 (%cadr 1))
  (is 3 (%caddr 2)))

(defun %mapcar-test (fun args)
  (%mapcar (lambda (x) (+ x 1)) args))
(is 9 (let ((x (%mapcar-test #'print '(1 2 3))))
        (+ (car x) (%cadr x) (%caddr x))))

(defun let-lambda-test (args)
  (let ((vars (%mapcar (lambda (x) (+ x 1))
                      args)))
    vars))
(is 5 (let ((ret (let-lambda-test '(1 2))))
        (+ (car ret) (%cadr ret))))

(is 0 (+))
(is 1 (+ 1))
(is 3 (+ 1 2))
(is 6 (+ 1 2 3))

(let* ((x 10)
       (y x))
  (is 10 y))

(let* ((x 1)
       (y (+ x x)))
  (is 2 y))

(let ((x 1))
  (let* ((x 2)
         (y x))
    (is 2 y)))

(let ((x 1))
  (let ((x 2)
        (y x))
    (is 1 y)))

(defvar *x* "*x* is 701")

(defun foo ()
  (is ((lambda (x) (print x)) "start foo...")
      "start foo...")
  (is "*x* is 701" (print *x*))
  (let ((*x* "*x* is 702"))
    (is "*x* is 702" (print *x*))
    (bar '(hello "百")
         (lambda (x)
           (print "lambda")
           (print x)))
    (is "*x* is 703" (print *x*)))
  (is "*x* is 701" (print *x*)))

(defun bar (n f)
  (funcall f n)
  (is "*x* is 702" (print *x*))
  (let ((n 100))
    (setq *x* "*x* is 703")
    (print n)
    (setq n 99)
    (print n))
  (print n)
  (print "*x* is 702"))

(defun my-list (&rest list)
  list)

(foo)
(print (my-list 11 22 33 44 55))
(funcall (function print) "function print!")
(funcall (function (lambda (x) (print x))) "function lambda!")
(print "ok")
