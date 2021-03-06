(in-package :chimacho)

(defun keyword-arg-fun1 (&key a)
  a)

(is 1 (keyword-arg-fun1 :a 1))

(let ((hash ($make-hash-table)))
  ($sethash hash "key1" "val1")
  ($sethash hash :key2 2)
  (is "val1" ($gethash hash "key1"))
  (is 2 ($gethash hash :key2)))

(defun optional1 (&optional a)
  a)
(is nil (optional1))
(is 1 (optional1 1))

(defun optional2 (&optional (a 10))
  a)
(is 10 (optional2))
(is 20 (optional2 20))

(defun optional3 (a &optional (b 20))
  (+ a b))
(is 21 (optional3 1))
(is 11 (optional3 1 10))

(defun optional4 (a &optional (b a))
  (+ a b))
(is 2 (optional4 1))
(is 3 (optional4 1 2))

(defun optional5 (a &optional (b (+ a a)))
  (+ a b))
(is 6 (optional5 2))
(is 3 (optional5 1 2))

(defun tagbody-test1 (flag)
  (let ((n 1))
    (tagbody
       (setq n (tagbody-test2 flag #'(lambda () (go out))))
     out)
    n))
(defun tagbody-test2 (flag escape)
  (if flag (funcall escape) 2))
(is 2 (tagbody-test1 nil))
(is 1 (tagbody-test1 t))

(is 15 (let (val)
         (tagbody
            (setq val 1)
            (go point-a)
            (setq val (+ val 16))
          point-c
            (setq val (+ val 04))
            (go point-b)
            (setq val (+ val 32))
          point-a
            (setq val (+ val 02))
            (go point-c)
            (setq val (+ val 64))
          point-b
            (setq val (+ val 08)))
         val))

(is nil (block empty))
(is 2 (block whocares 1 2))
(is 2 (let ((x 1))
        (block stop (setq x 2) (return-from stop) (setq x 3))
        x))
(is 2 (block early (return-from early 2)))
(is 1 (block outer (block inner (return-from outer 1)) 2))
(is 2 (block twin (block twin (return-from twin 1)) 2))
(is 1 (block b
        (flet ((b1 () (return-from b 1)))
          (block b (b1) (print 'unreachable))
          2)))

(is #\a #\a)

(is 1 (when t 1))

(defun deep-let (n)
  (let ()
    (let ()
      (lambda ()
        (let ()
          (let ()
            n))))))
(let ((f1 (deep-let 1))
      (f2 (deep-let 2)))
  (is 1 (funcall f1))
  (is 2 (funcall f2)))

(defun let-over-lambda-0 (n)
  (let ((x (+ 1 n)))
    (lambda (message &rest args)
      (case message
        (:get x)
        (:set (setq x (car args)))))))
(defun let-over-lambda-1 (n)
  (let ((super (let-over-lambda-0 n)))
    (lambda (&rest args)
      (apply super args))))
(let ((x (let-over-lambda-1 1)))
  (is 2 (funcall x :get))
  (funcall x :set 7)
  (is 7 (funcall x :get)))


(defun defun-lambda-let ()
  (lambda (msg)
    (let ()
      (car msg))))
(is 2 (funcall (defun-lambda-let) '(2)))

(defun msg ()
  (lambda (message)
    (case message
      (t (let ((ret (apply #'car message nil)))
           ret)))))
(is 3 (funcall (msg) '(3)))

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

(let ((x 10))
  (flet ((g ()
           x))
    (is 10 (g))))

(labels ((my-len (l n)
           (if l
               (my-len (cdr l) (+ n 1))
               n)))
  (is 3 (my-len '(a a a) 0)))

(labels ((%cadr (x)
           (+ x 1))
         (%caddr (x)
           (%cadr x)))
  (is 2 (%cadr 1))
  (is 3 (%caddr 2)))

(let ((x 1))
  (labels ((f ()
             x))
    (is 1 (f))))

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

(let ((x 1))
  (let* ((x (+ x 1)))
    (is 2 x)))

(let* ((x 1))
  (let ()
    (is 1 x)))

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
