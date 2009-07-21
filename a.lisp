(+ 10 20)

(print "Hello")

(if 1
    10
    20)

(if nil
    100
    200)

(progn
  (setq lll ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (print "")
  (print lll)
  (print (eq 0 0))
  (print (eq (quote a) (quote a)))
  (print (eq "a" "a"))
  (defun my-print (arg)
    (print arg))
  (my-print lll)
  ((lambda (x) (my-print x)) "Good")
  ((lambda (x)
     ((lambda () (my-print x)))) "bye!"))
