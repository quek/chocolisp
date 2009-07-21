(+ 10 20)

(print "Hello")

(if 1
    10
    20)

(if nil
    100
    200)

(progn
  (defun my-print (arg)
    (print arg))
  (my-print "")
  (my-print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  ((lambda (x) (my-print x)) "Good")
  ((lambda (x)
     ((lambda () (my-print x)))) "bye!"))
