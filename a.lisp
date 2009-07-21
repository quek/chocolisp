(+ 10 20)
(print "Hello")

(if 1
    10
    20)

(if nil
    100
    200)

(progn
  (print ";;;;")
  ((lambda (x) (print x)) "Good")
  ((lambda (x)
     ((lambda () (print x)))) "bye!"))
