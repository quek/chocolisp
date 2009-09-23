(in-package :chimacho)

(defun $symbol-package (symbol)
  ($get-attribute symbol "package"))

(defun $symbol-name (symbol)
  ($get-attribute symbol "name"))

(defun $package-name (package)
  ($get-attribute package "name"))

(let ((counter 0))
  (defun $gensym (prefix)
    (setq counter (+ 1 counter))
    (make-symbol (string+ prefix counter))))

(defun $rplaca (cons x)
  ($set-attribute cons "car" x))

(defun $rplacd (cons x)
  ($set-attribute cons "cdr" x))
