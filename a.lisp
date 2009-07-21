(+ 10 20)
(print "Hello")

(progn
  (print ";;;;")
  ((lambda (x) (print x)) "Good")
  ((lambda (x)
     ((lambda () (print x)))) "bye!"))
