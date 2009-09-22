(declaim (optimize (debug 3) (safety 3)))

(defpackage :chimacho
    (:use :cl))

(in-package :chimacho)

(load (compile-file
       (merge-pathnames "compiler.lisp" *load-truename*)))


;;(defun compile-pir-to-pbc (pir-file pbc-file)
;;  (sb-ext:run-program "parrot"
;;                      (list "-o" pbc-file pir-file)
;;                      :search t
;;                      :wait t
;;                      :output *standard-output*))

(defun compile-and-run (file)
  (parrot-compile-file file)
  (locally (declare (optimize (speed 0)))
    (sb-posix:chdir "/home/ancient/letter/parrot/chocolisp/"))
  (sb-ext:run-program "parrot"
                      (list "chocolisp.pir")
                      :search t
                      :wait t
                      :output *standard-output*))

;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/chimacho/read.lisp")
;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/compiler/compiler.lisp")
;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/compiler/a.lisp")