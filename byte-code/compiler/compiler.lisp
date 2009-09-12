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

(defclass* lambda-form (program)
  ((vars)
   (body)))

(defclass* if-form (program)
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

(defclass* let-form (program)
  ((vars)
   (values)
   (body)))

(defclass* arguments (program)
  ((first)
   (others)))

(defclass* no-argument (program) ())

(defclass* var (object)
  ((name)))

(defclass* global-var (var) ())

(defclass* predefiend-var (var)
  ((description)))

(defclass* local-var (var)
  ((mutable?)
   (dotted?)))


(defun extend-r (r vars &optional (kind :local))
  (reduce (lambda (r var)
            (cons (cons var kind) r))
          vars
          :initial-value r))

(defun objectify (form r d f)
  (if (atom form)
      (if (symbolp form)
          (objectify-reference form r d f)
          (objectify-quotation form))
      (case (car form)
        (if
            (objectify-if (cadr form) (caddr form) (cadddr form)
                          r d f))
        (let
            (objectify-let (cadr form) (caddr form) r d f))
        (lambda
            (objectify-lambda (cadr form) (caddr form) r d f))
        (progn
          (objectify-progn (cdr form) r d f))
        (flet
            )
        (labels
            )
        (defun
            (objectify-defun (cadr form) (caddr form) (cdddr form) r d f))
        (t (objectify-application (car form) (cdr form) r d f)))))

(defun objectify-quotation (value)
  (make-instance 'constant :value value))

(defun objectify-reference (var r d f)
  (case (var-kind var r d f)
    (:local
       (make-instance 'local-reference :var var))
    (:global
       (make-instance 'global-reference :var var))
    (:dynamic
       (make-instance 'dynamic-reference :var var))))

(defun var-kind (var r d f)
  (declare (ignore d f))
  (let ((x (assoc var r)))
    (if x
        (cdr x)
        :global)))

(defun objectify-if (test then else r d f)
  (make-instance 'if-form
                 :test (objectify test r d f)
                 :then (objectify then r d f)
                 :else (objectify else r d f)))

(defun objectify-let (bindings body r d f)
  (loop with new-r = r
        for bind in bindings
        if (atom bind)
          collect bind into vars
          and collect nil into values
          and do (setq new-r (cons (cons bind :local) new-r))
        else
          collect (car bind) into vars
          and collect (objectify (cadr bind) r d f) into values
          and do (setq new-r (cons (cons (car bind) :local) new-r))
        end
        finally (return (make-instance 'let-form
                                       :vars vars
                                       :values values
                                       :body (objectify body new-r d f)))))

(defun objectify-lambda (vars body r d f)
  (make-instance 'lambda-form
                 :vars vars
                 :body (objectify body (extend-r r vars) d f)))

(defun objectify-defun (name lambda-list body r d f)
  (make-instance 'defun-form :name name :lambda-list lambda-list
                 :body (objectify-progn body (extend-r r lambda-list) d f)))

(defun objectify-progn (body r d f)
  (if (null body)
      (make-instance 'constant :value nil)
      (if (null (cdr body))
          (objectify (car body) r d f)
          (make-instance 'progn-form
                         :first (objectify (car body) r d f)
                         :last (objectify-progn (cdr body) r d f)))))

(defun objectify-application (fun args r d f)
  (let ((objected-args (mapcar (lambda (arg) (objectify arg r d f))
                               args)))
    (make-instance 'regular-application
                   :function fun
                   :arguments objected-args)))

(defgeneric pir (program))

(defvar *var-counter*)
(defvar *label-counter*)

(let (var)
  (defun next-var (&optional (kind "P"))
    (setf var (format nil "$~a~d" kind (incf *var-counter*))))
  (defun current-var ()
    var))

(let (var)
  (defun next-label (&optional (name "L"))
    (setf var (format nil "~a~d" name (incf *label-counter*))))
  (defun current-label ()
    var))


(defun parrot-var (lisp-var)
  (format nil "p_~a" lisp-var))

(defun prt (format &rest args)
  (apply #'format t (concatenate 'string "~4t" format) args)
  (terpri))

(defun prt-top (format &rest args)
  (apply #'format t format args)
  (terpri))

(defun prt-label (label)
  (format t "~a:~%" label))

(defmethod pir ((self defun-form))
  (let ((*var-counter* 0)
        (*label-counter* 0))
    (prt-top ".sub '~a'" (name-of self))
    (mapc (lambda (arg)
            (prt ".param pmc ~a" (parrot-var arg)))
          (lambda-list-of self))
    (let ((ret (pir (body-of self))))
      (prt ".return(~a)" ret))
    (prt-top ".end")))

(defmethod pir ((self local-reference))
  (let ((value (next-var)))
    (prt "~a = ~a" value (parrot-var (var-of self)))
    value))

(defmethod pir ((self global-reference))
  "$P1 = find_symbol('name of var')
   $P2 = getattribute $P1, 'value'"
  (let ((symbol (next-var))
        (value  (next-var)))
  (prt "~a = find_symbol(\"~a\")"
          symbol (symbol-name (var-of self)))
  (prt "~a = getattribute ~a, 'value'"
          value symbol)
    value))

(defmethod pir ((self constant))
  (let ((var (next-var)))
    (prt "~a = ~a" var (value-of self))
    var))

(defmethod pir ((self if-form))
  (let ((test (next-var "I"))
        (result (next-var))
        (else-label (next-label "ELSE"))
        (end-label (next-label "ENDIF")))
    (prt "~a = 'nullp'(~a)" test (pir (test-of self)))
    (prt "if ~a goto ~a" test else-label)
    (prt "~a = ~a" result (pir (then-of self)))
    (prt "goto ~a" end-label)
    (prt-label else-label)
    (prt "~a = ~a" result (pir (else-of self)))
    (prt-label end-label)
    result))

(defmethod pir ((self regular-application))
  (let ((return-value (next-var))
        (fun (symbol-name (function-of self)))
        (args (next-var)))
    (prt "~a = new 'Array'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (arguments-of self))
    (prt "~a = '~a'(~a :flat)'" return-value fun args)
    return-value))

(progn
  (pir (objectify '(defun foo1 () 1) nil nil nil))
  (pir (objectify '(defun foo2 () a) nil nil nil))
  (pir (objectify '(defun foo3 (a) a) nil nil nil))
  (pir (objectify '(defun foo4 (a) (foo1 a 1 "abc")) nil nil nil))
  (pir (objectify '(defun foo5 (a) (if a 1 2)) nil nil nil))
  )

#|
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
|#
