(in-package :chimacho)

(defun repl ()
  (princ "start repl.")
  ($terpri *standard-output*)
  (loop
    ($terpri *standard-output*)
    (princ "> ")
    (print (eval ($read *standard-input*))))
  (princ "end repl.")
  ($terpri *standard-output*))
