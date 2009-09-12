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
(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)
                   (compilation-speed 0)))

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

(defclass* flat-function (defun-form)
  ((lambdas nil)))

(defclass* closure (program)
  ((name)
   (outer)
   (lambda-list)
   (body)))

(defclass* extracted-let (program)
  ((name)
   (values)))

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

(defun walk-object (object function &rest args)
  (mapc (lambda (slot-definition)
          (let* ((slot-name (sb-mop:slot-definition-name slot-definition))
                 (slot-value (slot-value object slot-name)))
            (when (typep slot-value 'program)
              (let ((new-value (apply function slot-value args)))
                (setf (slot-value object slot-name) new-value)))))
        (sb-mop:class-slots (class-of object)))
  object)

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


(defgeneric extract-let (object flat-function outer))

(defmethod extract-let ((self program) flat-function outer)
  (walk-object self #'extract-let flat-function outer))

(defmethod extract-let ((self defun-form) flat-function outer)
  (let* ((flat-function (make-instance 'flat-function
                                       :name (name-of self)
                                       :lambda-list (lambda-list-of self)))
         (extracted-body (extract-let (body-of self) flat-function
                                      flat-function)))
    (setf (body-of flat-function) extracted-body)
    flat-function))

(defmethod extract-let ((self let-form) flat-function outer)
  (let* ((closure-name (gensym "sub"))
         (closure (make-instance 'closure
                                 :name closure-name
                                 :outer (name-of outer)
                                 :lambda-list (vars-of self)
                                 :body nil)))
    (push closure (lambdas-of flat-function))
    (setf (body-of closure) (extract-let (body-of self) flat-function
                                         closure))
    (make-instance 'extracted-let
                   :name closure-name
                   :values (values-of self))))

(defgeneric pir (program))

(defvar *pir-stream* *standard-output*)

(defvar *var-counter*)
(defvar *label-counter*)
(defvar *sub-stack* nil)

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
  (apply #'format *pir-stream* (concatenate 'string "~8t" format) args)
  (terpri *pir-stream*))

(defun prt-top (format &rest args)
  (apply #'format *pir-stream* format args)
  (terpri *pir-stream*))

(defun prt-label (label)
  (format *pir-stream* "~a:~%" label))

(defmethod pir ((self defun-form))
  (let ((*var-counter* 0)
        (*label-counter* 0)
        (*sub-stack* (cons (name-of self) *sub-stack*)))
    (prt-top ".sub '~s'" (name-of self))
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
  (let ((var (next-var))
        (value (value-of self)))
    (prt "~a = new ~s" var
         (typecase value
           (integer "Integer")
           (string "String")))
    (prt "~a = ~a" var value)
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

(defmethod pir ((self progn-form))
  (pir (first-of self))
  (pir (last-of self)))

(defmethod pir ((self regular-application))
  (let ((return-value (next-var))
        (fun (symbol-name (function-of self)))
        (args (next-var)))
    (prt "~a = new 'ResizablePMCArray'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (arguments-of self))
    (prt "~a = '~s'(~a :flat)" return-value fun args)
    return-value))

(defmethod pir ((self let-form))
  (let ((sub-name (gensym "sub"))
        (sub (next-var))
        (args (next-var))
        (result (next-var)))
    (prt ".const 'Sub' ~a = '~s'" sub sub-name)
    (prt "~a = new 'ResizablePMCArray'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (values-of self))
    (prt "~a = ~a(~a :flat)" result sub args)
    result))

(defmethod pir :after ((self flat-function))
  (mapc #'pir (lambdas-of self)))

(defmethod pir ((self extracted-let))
  (let ((var (next-var))
        (args (next-var))
        (result (next-var)))
    (prt ".const 'Sub' ~a = '~s'" var (name-of self))
    (prt "~a = new 'ResizablePMCArray'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (values-of self))
    (prt "~a = ~a(~a :flat)" result var args)
    result))

(defmethod pir ((self closure))
  (let ((*var-counter* 0)
        (*label-counter* 0)
        (*sub-stack* (cons (name-of self) *sub-stack*)))
    (prt-top ".sub '~s' :outer('~s')" (name-of self) (outer-of self))
    (mapc (lambda (arg)
            (prt ".param pmc ~a" (parrot-var arg)))
          (lambda-list-of self))
    (let ((ret (pir (body-of self))))
      (prt ".return(~a)" ret))
    (prt-top ".end")))

#+nil
(progn
  (pir (objectify '(defun foo1 () 1) nil nil nil))
  (pir (objectify '(defun foo2 () a) nil nil nil))
  (pir (objectify '(defun foo3 (a) a) nil nil nil))
  (pir (objectify '(defun foo4 (a) (foo1 a 1 "abc")) nil nil nil))
  (pir (objectify '(defun foo5 (a) (if a 1 2)) nil nil nil))
  (pir (objectify '(defun foo7 (x) (let ((x 1)) (foo x)) (foo x)) nil nil nil))
  )

(defun compile-and-run (form &optional (file "/tmp/a.pir"))
  (format t "~&=====================================~%")
  (with-open-file (out file :direction :output
                       :if-exists :supersede)
    (let ((*pir-stream* (make-broadcast-stream out *standard-output*)))
      (format *pir-stream* "
.sub main
        'FOO'(100)
.end
.sub 'SAY'
        .param pmc x
        say x
.end
")
      (pir (extract-let (objectify form nil nil nil) nil nil))))
  (format t "~&=====================================~%")
  (sb-ext:run-program "parrot" (list file) :search t
                      :wait t
                      :output *standard-output*))

(compile-and-run '(defun foo (x)
                   (let ((x 7))
                     (say x))
                   (say x)))
