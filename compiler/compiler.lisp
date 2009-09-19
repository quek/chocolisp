#|
defun は .sub してるだけだが、
.const 'Sub' $P1 = 'fun'
を symbol-function に setf した方がいいのかな？
(let ((*packgae* :baha)) とかした場合の挙動が不明。。。

(compile-file "/tmp/a.lisp")
:COMPILE-TOPLEVEL
(load "/tmp/a.lisp")
:EXECUTE  -> :load
(load "/tmp/a")
:LOAD-TOPLEVEL  -> :init
ってこと？

TODO
tailcall
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
  ((var)))

(defclass* local-reference (reference) ())

(defclass* lexical-reference (reference) ())

(defclass* dynamic-reference (reference) ())

(defclass* assignment (program)
  ((var)
   (form)))

(defclass* local-assignment (assignment)
  ())

(defclass* lexical-assignment (assignment)
  ())

(defclass* dynamic-assignment (assignment)
  ())

(defclass* constant (program)
  ((value)))

(defclass* if-form (program)
  ((test)
   (then)
   (else)))

(defclass* progn-form (program)
  ((first)
   (last)))

(defclass* let-form (program)
  ((vars)
   (values)
   (body)))

(defclass* lambda-form (program)
  ((lambda-list)
   (arguments)
   (body)))

(defclass* application (program)
  ((arguments)))

(defclass* regular-application (application)
  ((function)))

(defclass* lambda-application (application)
  ((lambda)))

(defclass* arguments (program)
  ((first)
   (others)))

(defclass* no-argument (program) ())

(defclass* abstract-function ()
  ((symbol)))

(defclass* local-function (abstract-function) ())

(defclass* global-function (abstract-function) ())

(defclass* in-package-form (program)
  ((name)))

(defclass* eval-when-form (program)
  ((situations)
   (form)))

(defclass* defvar-form (program)
  ((symbol)
   (value)))

(defclass* defun-form (program)
  ((name)
   (lambda-list)
   (arguments)
   (body)))

(defclass* defmacro-form (defun-form) ())

(defclass* flat-function (defun-form)
  ((inner-functions nil)
   (lexical-store nil)
   (outers nil)
   (modifiers nil)))

(defclass* flat-macro-function (flat-function) ())

(defclass* extracted-let (program)
  ((name)
   (values)))

(defclass* extracted-lambda (program)
  ((name)))

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

(defun extend-r (r vars)
  (cons vars r))

(defun objectify (form r d f)
  (if (atom form)
      (if (symbolp form)
          (objectify-reference form r d f)
          (objectify-quotation form))
      (case (car form)
        (quote
           (objectify-quotation (cadr form)))
        (if
            (objectify-if (cadr form) (caddr form) (cadddr form)
                          r d f))
        (let
            (objectify-let (cadr form) (cddr form) r d f))
        (lambda
            (objectify-lambda (cadr form) (cddr form) r d f))
        (progn
          (objectify-progn (cdr form) r d f))
        (setq
           ;; TODO 1 つだけじゃない
           (objectify-setq (cadr form) (caddr form) r d f))
        (flet
            )
        (labels
            )
        (eval-when
            (objectify-eval-when (cadr form) (cddr form) r d f))
        (chimacho::defun
            (objectify-defun (cadr form) (caddr form) (cdddr form) r d f))
        (chimacho::defmacro
            (objectify-defmacro (cadr form) (caddr form) (cdddr form) r d f))
        (chimacho::in-package
           (objectify-in-package (cadr form)))
        (chimacho::defvar
            (objectify-defvar (cadr form) (caddr form) r d f))
        (t (objectify-application (car form) (cdr form) r d f)))))

(defparameter *info* nil)

(defun get-info (object key)
  (assoc key (cdr (assoc object *info*))))

(defun set-info (object key value)
  (let ((info (assoc object *info*)))
    (if info
        (let ((key-value (assoc key (cdr info))))
          (if key-value
              (setf (cdr key-value) value)
              (setf (cdr info) (acons key value (cdr info)))))
        (setf *info* (acons object (acons key value nil) *info*)))))

(defun special-var-p (symbol)
  (eq (cdr (get-info symbol :kind)) :special))

(defun macro-function-p (symbol)
  (cdr (get-info symbol :macro-function)))

(defun var-kind (var r d f)
  (declare (ignore d f))
  (if (member var (car r))
      :local
      (labels ((f (x)
                 (if (endp x)
                     :dynamic
                     (if (member var (car x))
                         :lexical
                         (f (cdr x))))))
        (f (cdr r)))))

(defun objectify-setq (symbol value-form r d f)
  (make-instance (case (var-kind symbol r d f)
                   (:local
                      'local-assignment)
                   (:lexical
                      'lexical-assignment)
                   (:dynamic
                      'dynamic-assignment))
                 :var symbol
                 :form (objectify value-form r d f)))

(defun objectify-defvar (symbol value-form r d f)
  ;; TODO r とか d を拡張しなきゃいけないのでは？
  (set-info symbol :kind :special)
  (make-instance 'defvar-form :symbol symbol
                 :value (objectify value-form r d f)))

(defun objectify-eval-when (situations form r d f)
  (make-instance 'eval-when-form :situations situations
                 :form (objectify-progn form r d f)))

(defun objectify-quotation (value)
  (make-instance 'constant :value value))

(defun objectify-reference (var r d f)
  (case (var-kind var r d f)
    (:local
       (make-instance 'local-reference :var var))
    (:lexical
       (make-instance 'lexical-reference :var var))
    (:dynamic
       (make-instance 'dynamic-reference :var var))))

(defun objectify-if (test then else r d f)
  (make-instance 'if-form
                 :test (objectify test r d f)
                 :then (objectify then r d f)
                 :else (objectify else r d f)))

(defun objectify-let (bindings body r d f)
  (let* ((bindings (mapcar (lambda (x)
                            (if (atom x)
                                (cons x nil)
                                x))
                          bindings))
         (vars (mapcar #'car bindings))
         (values (mapcar (lambda (x)
                           (objectify (cadr x) r d f))
                         bindings)))
    (make-instance 'let-form
                   :vars vars
                   :values values
                   :body (objectify-progn body (extend-r r vars) d f))))

(defun objectify-lambda (vars body r d f)
  (make-instance 'lambda-form
                 :lambda-list vars
                 :arguments (parse-lambda-list vars)
                 :body (objectify-progn body (extend-r r vars) d f)))

(defun parse-lambda-list (x)
  (if (null x)
      nil
      (case (car x)
        ((&rest &key &optional &allow-other-keys &body &whole)
           (parse-lambda-list (cdr x)))
        (t (if (keywordp (car x))
               (parse-lambda-list (cdr x))
               (cons (car x) (parse-lambda-list (cdr x))))))))

(defun objectify-defun (name lambda-list body r d f)
  (make-instance 'defun-form
                 :name name
                 :lambda-list lambda-list
                 :arguments (parse-lambda-list lambda-list)
                 :body (objectify-progn body (extend-r r lambda-list) d f)))

(defun objectify-defmacro (name lambda-list body r d f)
  (make-instance 'defmacro-form
                 :name name
                 :lambda-list lambda-list
                 :arguments (parse-lambda-list lambda-list)
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
  (if (symbolp fun)
      (objectify-application-symbol fun args r d f)
      (if (and (consp fun)
               (eq (car fun) 'lambda))
          (objectify-application-lambda fun args r d f)
          (error "~a is not applicable." fun))))

(defun objectify-application-symbol (fun args r d f)
  (let ((fun (if (eq *package* (symbol-package fun))
                 (make-instance 'local-function :symbol fun)
                 (make-instance 'global-function :symbol fun)))
        (objected-args (make-arguments
                        (mapcar (lambda (x)
                                  (objectify x r d f))
                                args))))
    (make-instance 'regular-application
                   :function fun
                   :arguments objected-args)))

(defun objectify-application-lambda (lambda-form args r d f)
  (let ((lambda-form (objectify-lambda (cadr lambda-form)
                                       (cddr lambda-form)
                                       r d f))
        (objected-args (make-arguments
                        (mapcar (lambda (x) (objectify x r d f))
                                args))))
    (make-instance 'lambda-application
                   :lambda lambda-form
                   :arguments objected-args)))

(defun set-lexical-var (var outers)
  (if (null outers)
      nil
      (if (member var (arguments-of (car outers)))
          (push var (lexical-store-of (car outers)))
          (set-lexical-var var (cdr outers)))))

(defgeneric 東京ミュミュ-metamorphose! (object outers))

(defmethod 東京ミュミュ-metamorphose! ((self local-reference) outers)
  (with-accessors ((var var-of)) self
    (if (member var (arguments-of (car outers)))
        self
        (progn
          (set-lexical-var var outers)
          self))))

(defmethod 東京ミュミュ-metamorphose! ((self program) outers)
  (walk-object self #'東京ミュミュ-metamorphose! outers))

(defmethod 東京ミュミュ-metamorphose! ((self defun-form) outers)
  (with-slots (name lambda-list arguments body) self
      self
    (let* ((flat-function (make-instance 'flat-function
                                         :name name
                                         :lambda-list lambda-list
                                         :arguments arguments))
           (extracted-body (東京ミュミュ-metamorphose!
                            body (cons flat-function outers))))
      (setf (body-of flat-function) extracted-body)
      flat-function)))

(defmethod 東京ミュミュ-metamorphose! ((self defmacro-form) outers)
  (with-slots (name lambda-list arguments body) self
    (let* ((flat-function (make-instance 'flat-macro-function
                                         :name name
                                         :lambda-list lambda-list
                                         :arguments arguments))
           (extracted-body (東京ミュミュ-metamorphose!
                            body (cons flat-function outers))))
      (setf (body-of flat-function) extracted-body)
      flat-function)))

(defmethod 東京ミュミュ-metamorphose! ((self let-form) outers)
  (let* ((name (gensym "let"))
         (flat-function (make-instance 'flat-function
                                       :name name
                                       :outers outers
                                       :lambda-list (vars-of self)
                                       :arguments (vars-of self)
                                       :body nil)))
    (push flat-function (inner-functions-of (car outers)))
    (setf (body-of flat-function)
          (東京ミュミュ-metamorphose! (body-of self)
                       (cons flat-function outers)))
    (make-instance 'extracted-let
                   :name name
                   :values (values-of self))))

(defmethod 東京ミュミュ-metamorphose! ((self lambda-form) outers)
  (let* ((name (gensym "lambda"))
         (flat-function
          (make-instance 'flat-function
                         :name name
                         :outers outers
                         :lambda-list (lambda-list-of self)
                         :arguments (arguments-of self)
                         :body nil)))
    (push flat-function (inner-functions-of (car outers)))
    (setf (body-of flat-function)
          (東京ミュミュ-metamorphose! (body-of self)
                                      (cons flat-function outers)))
    (make-instance 'extracted-lambda :name name)))

(defmethod 東京ミュミュ-metamorphose! ((self lambda-application) outers)
  (let* ((name (gensym "lambda"))
         (flat-function
          (make-instance
           'flat-function
           :name name
           :outers outers
           :lambda-list (lambda-list-of (lambda-of self))
           :arguments (arguments-of (lambda-of self))
           :body nil)))
    (push flat-function (inner-functions-of (car outers)))
    (setf (body-of flat-function)
          (東京ミュミュ-metamorphose! (body-of (lambda-of self))
                                      (cons flat-function outers)))
    (make-instance 'regular-application
                   :function (make-instance 'local-function :symbol name)
                   :arguments (arguments-of self))))


(defvar *pir-stream* *standard-output*)

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
  (with-output-to-string (out)
    (write-string "p_" out)
    (map nil (lambda (c)
               (if (alpha-char-p c)
                   (write-char c out)
                   (princ (char-code c) out)))
         (format nil "~s" lisp-var))))

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

(defun pir-nil ()
  (let ((var (next-var)))
    (prt "~a = get_hll_global \"NIL\"" var)
    var))

(defgeneric pir (program &key &allow-other-keys))

(defmethod pir ((self local-assignment) &key)
  (with-slots (var form) self
    (let ((value (pir form)))
      (prt "~a = ~a" (parrot-var var) value)
      value)))

(defmethod pir ((self dynamic-assignment) &key)
  (with-slots (var form) self
    (let ((value (pir form)))
      (prt "store_dynamic_lex '~a', ~a" (parrot-var var) value)
      value)))

(defun prt-intern-symbol (symbol)
  (let ((package (next-var))
        (var (next-var)))
    (prt "~a = find_package(~s)"
         package (package-name (symbol-package symbol)))
    (prt "~a = ~a.'intern'(~s)" var package (symbol-name symbol))
    var))

(defmethod pir ((self defvar-form) &key)
  ;; TOOD これでいいのか？
  (let* ((symbol (symbol-of self))
         (value (value-of self))
         (sym-var (prt-intern-symbol symbol)))
    (prt "~a.'specialize'()" sym-var)
    (prt "setattribute ~a, 'value', ~a" sym-var (pir value))
    sym-var))

(defun prt-push-dynamic (symbol)
  (let ((var (parrot-var symbol)))
    (prt ".lex '~a', ~a" var var)))

(defun pir-lambda-list (lambda-list)
  (if (null lambda-list)
      nil
      (if (eq (car lambda-list) '&rest)
          (progn
            (let ((var (parrot-var (cadr lambda-list))))
              (prt ".param pmc ~a :slurpy" var)
              (pir-lambda-list (cddr lambda-list))
              (prt "~a = array_to_list(~a)" var var)))
          (progn
            (prt ".param pmc ~a" (parrot-var (car lambda-list)))
            (pir-lambda-list (cdr lambda-list))))))

(defmethod pir ((self flat-function) &key)
  (with-slots (name lambda-list arguments body outers lexical-store modifiers)
      self
    (let ((*var-counter* 0)
          (*label-counter* 0)
          (modifiers (format nil "~{ ~a~}" modifiers)))
      (if outers
          (prt-top ".sub ~a :outer(~a)~a"
                   (parrot-sub-name name)
                   (parrot-sub-name (name-of (car outers)))
                   modifiers)
          (prt-top ".sub ~a~a" (parrot-sub-name name) modifiers))
      (pir-lambda-list lambda-list)
      (mapc (lambda (var)
              (if (special-var-p var)
                  (prt-push-dynamic var)))
            arguments)
      (mapc (lambda (var)
              (prt ".lex '~a', ~a" (parrot-var var) (parrot-var var)))
            lexical-store)
      (let ((ret (pir body)))
        (prt ".return(~a)" ret)))
    (prt-top ".end~%")))

(defmethod pir :after ((self flat-function) &key)
  (mapc #'pir (inner-functions-of self)))

(defmethod pir :after ((self flat-macro-function) &key)
  (with-slots (name) self
    (set-info name :macro-function t)))

(defmethod pir ((self in-package-form) &key)
  (prt-top ".namespace [ ~s ]~%" (name-of self))
  (setq *package* (find-package (name-of self))))

(defmethod pir ((self local-reference) &key)
  (let ((value (next-var)))
    (prt "~a = ~a" value (parrot-var (var-of self)))
    value))

(defmethod pir ((self lexical-reference) &key)
  (let ((value (next-var)))
    (prt "~a = find_lex '~a'" value (parrot-var (var-of self)))
    value))

(defmethod pir ((self dynamic-reference) &key)
  (let* ((symbol (var-of self))
         (var (parrot-var symbol))
         (value (next-var)))
    (prt "~a = dynamic_scope_value('~a', ~s, ~s)"
         value
         var
         (package-name (symbol-package symbol))
         (symbol-name symbol))
    value))

(defun pir-constant (value)
  (if (atom value)
      (pir-atom value)
      (pir-cons value)))

(defun pir-cons (cons)
  (let ((var (next-var)))
    (prt "~a = cons(~a, ~a)"
         var (pir-constant (car cons)) (pir-constant (cdr cons)))
    var))

(defun pir-symbol (symbol)
  (prt-intern-symbol symbol))

(defun pir-atom (atom)
  (etypecase atom
    (integer
       (let ((var (next-var)))
         (prt "~a = new ~s" var "Integer")
         (prt "~a = ~s" var atom)
         var))
    (string
       (let ((var (next-var)))
         (prt "~a = new ~s" var "String")
         (prt "~a = utf8:unicode:~s" var atom)
         var))
    (symbol
       (pir-symbol atom))))

(defmethod pir ((self constant) &key)
  (pir-constant (value-of self)))

(defmethod pir ((self if-form) &key)
  (let ((result (next-var))
        (else-label (next-label "ELSE"))
        (end-label (next-label "ENDIF")))
    (prt "eq_addr ~a, ~a, ~a" (pir-nil) (pir (test-of self)) else-label)
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
  (let ((args (next-var)))
    (prt "~a = new 'ResizablePMCArray'" args)
    (pir (arguments-of self) :array args)
    (pir (function-of self) :args args)))

(defmethod pir ((self local-function) &key args)
  (let ((return-value (next-var)))
    (prt "~a = ~a(~a :flat)"
         return-value (parrot-sub-name (symbol-of self)) args)
    return-value))

(defmethod pir ((self global-function) &key args)
  (let ((symbol (symbol-of self))
        (fun-var (next-var))
        (return-value (next-var)))
    (prt "~a = get_hll_global [ ~s ], ~s"
         fun-var
         (package-name (symbol-package symbol))
         (symbol-name symbol))
    (prt "~a  = ~a(~a :flat)" return-value fun-var args)
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

(defmethod pir ((self extracted-lambda) &key)
  (let ((var (next-var)))
    (prt ".const 'Sub' ~a = ~a" var (parrot-sub-name (name-of self)))
    var))

(defmethod pir ((self eval-when-form) &key)
  (with-slots (situations form) self
    (when (member :compile-toplevel situations)
      ;; TODO PIR 的これでいい？ eval の実装の問題か? よくないよね？
      (eval form))
    (let ((modifiers `(,@(when (member :load-toplevel situations)
                           '(":load"))
                         ,@(when (member :execute situations)
                             '(":init")))))
      (when modifiers
        (pir (make-instance
              'flat-function
              :name (gensym "init")
              :lambda-list nil
              :arguments nil
              :body form
              :modifiers `(":anon" ,@modifiers)))))))


(defun parrot-compile-file (file &optional
                            (pir-file (namestring
                                       (make-pathname :type "pir"
                                                      :defaults file)))
                            (pbc-file (namestring
                                       (make-pathname :type "pbc"
                                                      :defaults file))))
  (compile-lisp-to-pir file pir-file)
  (compile-pir-to-pbc pir-file pbc-file))

(defun %macroexpand (form)
  (let ((expanded-form (macroexpand-1 form)))
    (if (eq expanded-form form)
        form
        (%macroexpand expanded-form))))

(defun read-loop (in)
  (let ((form (read in nil)))
    (when form
      (let* ((expanded-form (print (%macroexpand form)))
             (object (東京ミュミュ-metamorphose!
                      (objectify expanded-form nil nil nil) nil)))
        (if (toplevelp object)
            (pir object)
            (pir (make-instance
                  'flat-function
                  :name (gensym "init")
                  :lambda-list nil
                  :arguments nil
                  :body object
                  :modifiers '(":anon" ":init" ":load")))))
      (read-loop in))))

(defun compile-lisp-to-pir (lisp-file pir-file)
  (with-open-file (in lisp-file)
    (with-open-file (*pir-stream* pir-file :direction :output
                                  :if-exists :supersede)
      (put-common-header)
      (let ((*package* *package*))
        (read-loop in)))))

(defun compile-pir-to-pbc (pir-file pbc-file)
  (sb-ext:run-program "parrot"
                      (list "-o" pbc-file pir-file)
                      :search t
                      :wait t
                      :output *standard-output*))

(defun put-common-header ()
  (format  *pir-stream* ".HLL \"chocolisp\"~%~%"))

(defun compile-and-run (&optional (file "a.lisp"))
  (parrot-compile-file (merge-pathnames file *load-truename*))
  (locally (declare (optimize (speed 0)))
    (sb-posix:chdir "/home/ancient/letter/parrot/chocolisp/"))
  (sb-ext:run-program "parrot"
                      (list "chocolisp.pir")
                      :search t
                      :wait t
                      :output *standard-output*))

(compile-and-run)
