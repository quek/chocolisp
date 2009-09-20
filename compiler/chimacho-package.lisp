(cl:defpackage :chimacho
    (:use :cl)
  (:shadow #:defun
           #:defmacro
           #:defvar
           #:in-package
           ;; chimach.list
           #:list
           #:when
           #:unless))

(cl:in-package :chimacho)

(cl:defun defun (&rest rest)
  (cons 'defun rest))

(cl:defmacro defmacro (&whole whole &rest rest)
  (declare (ignore rest))
  whole)

(cl:defmacro defvar (&whole whole &rest rest)
  (declare (ignore rest))
  whole)

(cl:defmacro in-package (&whole whole string-designator)
  (declare (ignore string-designator))
  whole)
