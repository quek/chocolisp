#|
(in-package "a")
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (SETQ *PACKAGE* (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE "a")))
とかはどうするんだ？
.namespace ["a"]
でいいのか。

トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。

(let ((x (defun f () 'foo))))
とかの場合は .const 'Sub' x = 'f' すればいいかな。
|#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :defclass-star))

(defpackage :chocolisp.compiler
    (:use :common-lisp :defclass-star))

(in-package :chocolisp.compiler)


(defclass* object () ())

(defclass* program (object) ())

(defclass* reference (program)
  (var))

(defclass* local-reference (reference) ())

(defclass* global-reference (reference) ())

(defclass* predefined-reference (reference) ())

(defclass* dynamic-reference (reference) ())

(defclass* defun-form (program)
  ((name)
   (lambda-list)
   (body)))

(defclass* local-assignment (program)
  ((reference)
   (form)))

(defclass* global-assignment (program)
  ((var)
   (form)))

(defclass* dynamic-assignment (program)
  ((reference)
   (form)))

(defclass* regular-function (program)
  ((vars)
   (body)))

(defclass* alternative (program)
  ((test)
   (then)
   (else)))

(defclass* progn-form (program)
  ((first)
   (last)))

(defclass* constant (program)
  ((value)))

(defclass* application (program) ())

(defclass* regular-application (application)
  ((function)
   (arguments)))

(defclass* predefined-application (application)
  ((var)
   (arguments)))

(defclass* fixlet (program)
  ((vars)
   (arguments)
   (body)))

(defclass* arguments (program)
  ((first)
   (others)))

(defclass* noargument (program) ())

(defclass* var (object)
  ((name)))

(defclass* global-var (var) ())

(defclass* predefiend-var (var)
  ((description)))

(defclass* local-var (var)
  ((mutable?)
   (dotted?)))

(defun objectify (form)
  (if (atom form)
      (if (symbolp form)
          (objectify-reference form)
          (objectify-quotation form))
      (case (car form)
        (defun
            (objectify-defun (cadr form) (caddr form) (cdddr form)))
        (t ()))))

(defun objectify-defun (name lambda-list body)
  (make-instance 'defun-form :name name :lambda-list lambda-list
                 :body (objectify body)))

(defun compile-toplevel (form)
  (if (atom form)
      (progn
        "トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。")
      (case (car form)
        (defun
            (compile-defun (cadr form) (cddr form)))
        (t
           "トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。"
           ()))))

(defun compile-form (form)
  (if (atom form)
      (if (symbolp form)
          (compile-reference form)
          (compile-quote form))
      (case (car form)
        (defun
            (compile-defun (cadr form) (cddr form)))
        (t))))

(defun compile-reference (var)
  var)

(defun compile-quote (value)
  value)

(defun compile-defun (name body)
  (format t ".sub '~a'~%" name)
  (compile-form body)
  (format t ".end~%"))

