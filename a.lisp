(+ 10 20)
(print "Hello")
(progn
  ((lambda (x) (print x)) "Good")
  (print " ")
  (print "bye!"))
