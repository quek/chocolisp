(chimacho::in-package "CHIMACHO")

(defun list (&rest list)
  list)

(defmacro when (test &rest form)
  (list 'if test (cons 'progn form)))

(defmacro unless (test &rest form)
  `(if ,test nil (progn ,@form)))

(defun symbolp (x)
  (eq 'symbol (type-of x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(is 1 (when t 1))
(print (when t 1))
