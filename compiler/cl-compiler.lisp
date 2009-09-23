(declaim (optimize (debug 3) (safety 3)))

(defpackage :chimacho
    (:use :cl))

(in-package :chimacho)

(defun compile-pir-to-pbc (pir-file pbc-file)
  (sb-ext:run-program "parrot"
                      (list "-o" pbc-file pir-file)
                      :search t
                      :wait t
                      :output *standard-output*))

(defun compile-and-run (file)
  (parrot-compile-file file)
  (locally (declare (optimize (speed 0)))
    (sb-posix:chdir "/home/ancient/letter/parrot/chocolisp/"))
  (sb-ext:run-program "parrot"
                      (list "chocolisp.pir")
                      :search t
                      :wait t
                      :output *standard-output*))

(defun open-input-file (file)
  (open file))

(defun open-output-file (file)
  (open file :direction :output :if-exists :supersede))

(defun $read-char (stream)
  (read-char stream nil))

(defun $read-line (stream)
  (read-line stream nil))

(defun $write-string (str stream)
  (write-string str stream))

(defun $terpri (stream)
  (terpri stream))

(defun $close (stream)
  (close stream))

(defun string+ (&rest args)
  (apply #'concatenate 'string
         (mapcar #'princ-to-string args)))

(defun $error (message)
  (error message))

(defun $char (str i)
  (char str i))

(defun $char-code (c)
  (char-code c))

(defun $read (stream)
  (read stream nil))

(defun $macroexpand (form)
  (macroexpand form))

(defun $symbol-package (symbol)
  (symbol-package symbol))

(defun $symbol-name (symbol)
  (symbol-name symbol))

(defun $package-name (package)
  (package-name package))

(defun $gensym (prefix)
  (gensym prefix))

(defun is (x y)
  (unless (equal x y)
    (error "[~a] is not [~a]." x y)))

;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/chimacho/read.lisp")
;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/compiler/compiler.lisp")
;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/compiler/a.lisp")