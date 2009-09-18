(cl:defpackage :chimacho
    (:use :cl)
  (:shadow #:defun
           #:defmacro
           #:defvar
           #:in-package))

(cl:in-package :chimacho)

(cl:defmacro defun (&whole whole &rest rest)
  (declare (ignore rest))
  whole)

(cl:defmacro defmacro (&whole whole &rest rest)
  (declare (ignore rest))
  whole)

(cl:defmacro defvar (&whole whole &rest rest)
  (declare (ignore rest))
  whole)

(cl:defmacro in-package (&whole whole string-designator)
  (declare (ignore rest))
  whole)
