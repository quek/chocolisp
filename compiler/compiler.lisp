#|
defun は .sub してるだけだが、
.const 'Sub' $P1 = 'fun'
を symbol-function に setf した方がいいのかな？
(let ((*packgae* :baha)) とかした場合の挙動が不明。。。
|#
(declaim (optimize (debug 3) (safety 3)))

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

(defclass* free-reference (reference) ())

(defclass* in-package-form (program)
  ((name)))

(defclass* defun-form (program)
  ((name)
   (arguments)
   (body)))

(defclass* flat-function (defun-form)
  ((inner-functions nil)
   (lexical-store nil)
   (outers nil)
   (modifiers nil)))

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

(defgeneric toplevelp (x))

(defmethod toplevelp ((self object))
  nil)

(defmethod toplevelp ((self in-package-form))
  t)

(defmethod toplevelp ((self flat-function))
  t)

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
            (objectify-let (cadr form) (cddr form) r d f))
        (lambda
            (objectify-lambda (cadr form) (cddr form) r d f))
        (progn
          (objectify-progn (cdr form) r d f))
        (flet
            )
        (labels
            )
        (defun
            (objectify-defun (cadr form) (caddr form) (cdddr form) r d f))
        (in-package
           (objectify-in-package (cadr form)))
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
        finally (return
                  (make-instance 'let-form
                                 :vars vars
                                 :values values
                                 :body (objectify-progn body new-r d f)))))

(defun objectify-lambda (vars body r d f)
  (make-instance 'lambda-form
                 :vars vars
                 :body (objectify body (extend-r r vars) d f)))

(defun objectify-defun (name lambda-list body r d f)
  (make-instance 'defun-form :name name :arguments lambda-list
                 :body (objectify-progn body (extend-r r lambda-list) d f)))

(defun objectify-progn (body r d f)
  (if (null body)
      (make-instance 'constant :value nil)
      (if (null (cdr body))
          (objectify (car body) r d f)
          (make-instance 'progn-form
                         :first (objectify (car body) r d f)
                         :last (objectify-progn (cdr body) r d f)))))

(defun objectify-in-package (name)
  (make-instance 'in-package-form :name name))

(defun make-arguments (args)
  (if args
      (make-instance 'arguments
                     :first (car args)
                     :others (make-arguments (cdr args)))
      (make-instance 'no-argument)))

(defun objectify-application (fun args r d f)
  (let ((objected-args (make-arguments
                        (loop for arg in args
                              collect (objectify arg r d f)))))
    (make-instance 'regular-application
                   :function fun
                   :arguments objected-args)))

(defun set-lexical-var (var outers)
  (loop for flat-function in outers
        if (member var (arguments-of flat-function))
          do (progn (push var (lexical-store-of flat-function))
                    (return))))

(defgeneric extract-let (object outers))

(defmethod extract-let ((self local-reference) outers)
  (with-accessors ((var var-of)) self
    (if (member var (arguments-of (car outers)))
        self
        (progn
          (set-lexical-var var outers)
          (make-instance 'free-reference :var var)))))

(defmethod extract-let ((self program) outers)
  (walk-object self #'extract-let outers))

(defmethod extract-let ((self defun-form) outers)
  (with-accessors ((name name-of) (arguments arguments-of) (body body-of))
      self
    (let* ((flat-function (make-instance 'flat-function
                                         :name name
                                         :arguments arguments))
           (extracted-body (extract-let body
                                        (cons flat-function outers))))
      (setf (body-of flat-function) extracted-body)
      flat-function)))

(defmethod extract-let ((self let-form) outers)
  (let* ((name (gensym "sub"))
         (flat-function (make-instance 'flat-function
                                       :name name
                                       :outers outers
                                       :arguments (vars-of self)
                                       :body nil)))
    (push flat-function (inner-functions-of (car outers)))
    (setf (body-of flat-function)
          (extract-let (body-of self)
                       (cons flat-function outers)))
    (make-instance 'extracted-let
                   :name name
                   :values (values-of self))))


(defvar *pir-stream* *standard-output*)

(defvar *current-package* (find-package :common-lisp))
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

(defun parrot-sub-name (symbol)
  (if (symbol-package symbol)
      (format nil "~s" (symbol-name symbol))
      (format nil "~s" (format nil "~s" symbol))))

(defun prt (format &rest args)
  (apply #'format *pir-stream* (concatenate 'string "~8t" format) args)
  (terpri *pir-stream*))

(defun prt-top (format &rest args)
  (apply #'format *pir-stream* format args)
  (terpri *pir-stream*))

(defun prt-label (label)
  (format *pir-stream* "~a:~%" label))

(defgeneric pir (program &key &allow-other-keys))

(defmethod pir ((self flat-function) &key)
  (with-slots (name arguments body outers lexical-store modifiers) self
    (let ((*var-counter* 0)
          (*label-counter* 0)
          (*sub-stack* (cons name *sub-stack*))
          (modifiers (format nil "~{ ~a~}" modifiers)))
      (if outers
          (prt-top ".sub ~a :outer('~a')~a"
                   (parrot-sub-name name) (name-of (car outers)) modifiers)
          (prt-top ".sub ~a~a" (parrot-sub-name name) modifiers))
      (loop for var in arguments
            do (prt ".param pmc ~a" (parrot-var var)))
      (loop for var in lexical-store
            do (prt ".lex '~a', ~a" (parrot-var var) (parrot-var var)))
      (let ((ret (pir body)))
        (prt ".return(~a)" ret))
      (prt-top ".end~%"))))

(defmethod pir :after ((self flat-function) &key)
  (mapc #'pir (inner-functions-of self)))

(defmethod pir ((self in-package-form) &key)
  (prt-top ".namespace [ ~s ]~%" (name-of self))
  (setf *current-package* (find-package (name-of self))))

(defmethod pir ((self local-reference) &key)
  (let ((value (next-var)))
    (prt "~a = ~a" value (parrot-var (var-of self)))
    value))

(defmethod pir ((self free-reference) &key)
  (let ((value (next-var)))
    (prt "~a = find_lex '~a'" value (parrot-var (var-of self)))
    value))

(defmethod pir ((self global-reference) &key)
  "$P1 = find_symbol('name of var')
   $P2 = getattribute $P1, 'value'"
  (let ((symbol (next-var))
        (value  (next-var)))
    (prt "~a = find_symbol(\"~a\")"
         symbol (symbol-name (var-of self)))
    (prt "~a = getattribute ~a, 'value'"
         value symbol)
    value))

(defmethod pir ((self constant) &key)
  (let ((var (next-var))
        (value (value-of self)))
    (typecase value
      (integer
         (prt "~a = new ~s" var "Integer")
         (prt "~a = ~s" var value))
      (string
         (prt "~a = new ~s" var "String")
         (prt "~a = utf8:unicode:~s" var value)))
    var))

(defmethod pir ((self if-form) &key)
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

(defmethod pir ((self progn-form) &key)
  (pir (first-of self))
  (pir (last-of self)))

(defmethod pir ((self regular-application) &key)
  (let ((return-value (next-var))
        (fun (function-of self))
        (args (next-var)))
    (prt "~a = new 'ResizablePMCArray'" args)
    (pir (arguments-of self) :array args)
    (if (eq *current-package* (symbol-package fun))
        (prt "~a = ~s(~a :flat)" return-value (symbol-name fun) args)
        (let ((fun-var (next-var)))
          (prt "~a = get_hll_global [ ~s ], ~s"
               fun-var
               (package-name (symbol-package fun))
               (symbol-name fun))
          (prt "~a  = ~a(~a :flat)" return-value fun-var args)))
    return-value))

(defmethod pir ((self arguments) &key array)
  (prt "push ~a, ~a" array (pir (first-of self)))
  (pir (others-of self) :array array))

(defmethod pir ((self no-argument) &key array)
  array)

(defmethod pir ((self let-form) &key)
  (let ((sub-name (gensym "sub"))
        (sub (next-var))
        (args (next-var))
        (result (next-var)))
    (prt ".const 'Sub' ~a = '~s'" sub (parrot-sub-name sub-name))
    (prt "~a = new 'ResizablePMCArray'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (values-of self))
    (prt "~a = ~a(~a :flat)" result sub args)
    result))

(defmethod pir ((self extracted-let) &key)
  (let ((var (next-var))
        (args (next-var))
        (result (next-var)))
    (prt ".const 'Sub' ~a = ~a" var (parrot-sub-name (name-of self)))
    (prt "~a = new 'ResizablePMCArray'" args)
    (mapc (lambda (arg)
            (prt "push ~a, ~a" args (pir arg)))
          (values-of self))
    (prt "~a = ~a(~a :flat)" result var args)
    result))

(defun parrot-compile-file (file &optional
                            (pir-file (namestring
                                       (make-pathname :type "pir"
                                                      :defaults file)))
                            (pbc-file (namestring
                                       (make-pathname :type "pbc"
                                                      :defaults file))))
  (compile-lisp-to-pir file pir-file)
  (compile-pir-to-pbc pir-file pbc-file))

(defun compile-lisp-to-pir (lisp-file pir-file)
  (let* ((objects (with-open-file (in lisp-file)
                    (let ((*package* *package*))
                      (loop for form = (read in nil) while form
                            if (and (consp form) (eq 'in-package (car form)))
                              do (setf *package* (find-package (cadr form)))
                            collect (extract-let
                                     (objectify form nil nil nil)
                                            nil))))))
    (with-open-file (*pir-stream* pir-file :direction :output
                                  :if-exists :supersede)
      (put-common-header)
      (loop for x in objects
            if (toplevelp x)
              do (pir x)
            else
              do (pir (make-instance 'flat-function
                           :name (gensym "init")
                           :arguments nil
                           :body x
                           :modifiers '(":anon" ":init" ":load")))))))

(defun compile-pir-to-pbc (pir-file pbc-file)
  (sb-ext:run-program "parrot"
                      (list "-o" pbc-file pir-file)
                      :search t
                      :wait t
                      :output *standard-output*))

(defun put-common-header ()
  (format  *pir-stream* ".HLL \"chocolisp\"~%~%"))

(defun compile-and-run (form &optional (file "/tmp/a.pir"))
  (format t "~&=====================================~%")
  (with-open-file (out file :direction :output
                       :if-exists :supersede)
    (let ((*pir-stream* (make-broadcast-stream out *standard-output*)))
      (format *pir-stream* "
.sub main
        'FOO'(100, 123)
.end
.sub 'PRINT'
        .param pmc x
        say x
.end
")
      (pir (extract-let (objectify form nil nil nil) nil))))
  (format t "~&=====================================~%")
  (sb-ext:run-program "parrot" (list file) :search t
                      :wait t
                      :output *standard-output*))
#+nil
(compile-and-run '(defun foo (x y)
                   (let ((x "まみむめも♪"))
                     (print x)
                     (print y))
                   (print x)))


(parrot-compile-file "a.lisp")
