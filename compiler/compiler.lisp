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

(defpackage :chocolisp.compiler
    (:use :common-lisp))

(in-package :chocolisp.compiler)

(defvar *pir-stream* *standard-output*)
(defvar *var-counter*)
(defvar *label-counter*)

(defun objectify (form r f)
  (let ((form (%macroexpand form)))
    (if (atom form)
        (if (symbolp form)
            (objectify-reference form r f)
            (objectify-quotation form))
        (case (car form)
          (quote
             (objectify-quotation (cadr form)))
          (if
              (objectify-if (cadr form) (caddr form) (cadddr form)
                            r f))
          (let
              (objectify-let (cadr form) (cddr form) r f))
          (let*
              (objectify-let* (cadr form) (cddr form) r f))
          (lambda
              (objectify-lambda (cadr form) (cddr form) r f))
          (function
             (objectify-function (cadr form) r f))
          (progn
            (objectify-progn (cdr form) r f))
          (setq
             ;; TODO 1 つだけじゃない
             (objectify-setq (cadr form) (caddr form) r f))
          (flet
              )
          (labels
              )
          (eval-when
              (objectify-eval-when (cadr form) (cddr form) r f))
          (chimacho::defun
              (objectify-defun (cadr form) (caddr form) (cdddr form) r f))
          (chimacho::defmacro
              (objectify-defmacro (cadr form) (caddr form) (cdddr form) r f))
          (chimacho::in-package
             (objectify-in-package (cadr form)))
          (chimacho::defvar
              (objectify-defvar (cadr form) (caddr form) r f))
          (t (objectify-application (car form) (cdr form) r f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun objectify-setq (symbol value-form r f)
  (ecase (var-kind symbol r f)
    (:local
       (make-local-assignment symbol (objectify value-form r f)))
    (:lexical
       (make-lexical-assignment symbol (objectify value-form r f)))
    (:dynamic
       (make-dynamic-assignment symbol (objectify value-form r f)))))

(defun objectify-defvar (symbol value-form r f)
  (set-info symbol :kind :special)
  (make-defvar symbol (objectify value-form r f)))

(defun objectify-eval-when (situations form r f)
  (make-eval-when  situations (objectify-progn form r f)))

(defun objectify-quotation (value)
  (make-constant value))

(defun objectify-reference (var r f)
  (case (var-kind var r f)
    (:local
       (make-local-reference var))
    (:lexical
       (make-lexical-reference var))
    (:dynamic
       (make-dynamic-reference var))))

(defun objectify-if (test then else r f)
  (make-if (objectify test r f)
           (objectify then r f)
           (objectify else r f)))

(defun objectify-let (bindings body r f)
  (let* ((bindings (mapcar (lambda (x)
                             (if (atom x)
                                 (cons x nil)
                                 x))
                           bindings))
         (vars (mapcar #'car bindings))
         (values (mapcar (lambda (x)
                           (objectify (cadr x) r f))
                         bindings)))
    (make-let vars
              values
              (objectify-progn body (extend-r r vars) f))))

(defun objectify-let* (bindings body r f)
  (let* ((bindings (mapcar (lambda (x)
                             (if (atom x)
                                 (cons x nil)
                                 x))
                           bindings))
         (vars (mapcar #'car bindings)))
    (make-let* (%make-let*-bindings bindings r f nil)
               (objectify-progn body (extend-r r vars) f))))

(defun objectify-lambda (vars body r f)
  (make-lambda vars (objectify-progn body (extend-r r vars) f)))

(defun objectify-function (name r f)
  (if (symbolp name)
      (if (eq *package* (symbol-package name))
          (make-extracted-lambda name)
          (make-global-function-reference name))
      (if (and (consp name)
               (eq 'lambda (car name)))
          (let ((vars (cadr name))
                (body (cddr name)))
            (make-lambda vars (objectify-progn body (extend-r r vars) f)))
          (error "Invalid function name ~s." name))))

(defun objectify-defun (name lambda-list body r f)
  (make-defun name
              lambda-list
              (objectify-progn body (extend-r r lambda-list) f)))

(defun objectify-defmacro (name lambda-list body r f)
  (set-info name :macro-function t)
  (make-defmacro name
                 lambda-list
                 (objectify-progn body (extend-r r lambda-list) f)))

(defun objectify-progn (body r f)
  (if (null body)
      (make-constant nil)
      (if (null (cdr body))
          (objectify (car body) r f)
          (make-progn (objectify (car body) r f)
                      (objectify-progn (cdr body) r f)))))

(defun objectify-in-package (name)
  (setq *package* (find-package name))
  (make-in-package name))

(defun objectify-application (fun args r f)
  (if (symbolp fun)
      (objectify-application-symbol fun args r f)
      (if (and (consp fun)
               (eq (car fun) 'lambda))
          (objectify-application-lambda fun args r f)
          (error "~a is not applicable." fun))))

(defun objectify-application-symbol (fun args r f)
  (let ((fun (if (eq *package* (symbol-package fun))
                 (make-local-function fun)
                 (make-global-function fun)))
        (objected-args (list-to-arguments
                        (mapcar (lambda (x)
                                  (objectify x r f))
                                args))))
    (make-regular-application fun objected-args)))

(defun objectify-application-lambda (lambda-form args r f)
  (let ((lambda-form (objectify-lambda (cadr lambda-form)
                                       (cddr lambda-form)
                                       r f))
        (objected-args (list-to-arguments
                        (mapcar (lambda (x) (objectify x r f))
                                args))))
    (make-lambda-application lambda-form objected-args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-object (&rest vars)
  (let (self
        (vars (make-vars vars)))
    (setq self
          (lambda (message &rest args)
            (ecase message
              (:toplevelp
                 nil)
              (:all-vars
                 vars)
              (:get
                 (cdr (assoc (car args) vars)))
              (:set
                 (let ((cons (assoc (car args) vars)))
                   (if cons
                       (rplacd cons (cadr args)))))
              (:add
                 (let ((cons (assoc (car args) vars)))
                   (if cons
                       (rplacd cons (cons (cadr args) (cdr cons)))))))))))

(defun make-program (&rest vars)
  (let ((super (apply #'make-object vars))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:東京ミュウミュウ-metamorphose!
                      (apply #'walk self (funcall self :all-vars) message args)
                      self)
                   (t
                      (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-reference (var)
  (let ((super (make-program :var var))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-local-reference (var)
  (let ((super (make-reference var))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let ((var (funcall self :get :var)))
                   (unless (member var
                                   (collect-vars
                                    (funcall (caar args) :get :lambda-list)))
                     (set-lexical-var var (car args)))
                   self))
              (:pir
                 (let ((var (funcall self :get :var))
                       (value (next-var)))
                   (prt "~a = ~a" value (parrot-var var))
                   value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-lexical-reference (var)
  (let ((super (make-reference var))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let ((var (funcall self :get :var))
                       (outers (car args)))
                   (set-lexical-var var outers))
                 self)
              (:pir
                 (let ((var (funcall self :get :var))
                       (value (next-var)))
                   (prt "~a = find_lex '~a'" value (parrot-var var))
                   value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-dynamic-reference (var)
  (let ((super (make-reference var))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((var (funcall self :get :var))
                       (value (next-var)))
                   (prt "~a = dynamic_scope_value('~a', ~s, ~s)"
                        value
                        (parrot-var var)
                        (package-name (symbol-package var))
                        (symbol-name var))
                   value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-assignment (var form)
  (let ((super (make-program :var var :form form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((value (funcall (funcall self :get :form) :pir)))
                   (prt "~a = ~a" (parrot-var (funcall self :get :var)) value)
                   value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-local-assignment (var form)
  (let ((super (make-assignment var form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-lexical-assignment (var form)
  (let ((super (make-assignment var form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-dynamic-assignment (var form)
  (let ((super (make-assignment var form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((var (funcall self :get :var))
                       (form (funcall self :get :form)))
                   (let ((value (funcall form :pir)))
                     (prt "store_dynamic_lex '~a', ~a" (parrot-var var) value)
                     value)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-constant (value)
  (let ((super (make-program :value value))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (pir-constant (funcall self :get :value)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-if (test then else)
  (let ((super (make-program :test test :then then :else else))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((test (funcall self :get :test))
                       (then (funcall self :get :then))
                       (else (funcall self :get :else))
                       (result (next-var))
                       (else-label (next-label "ELSE"))
                       (end-label (next-label "ENDIF")))
                   (prt "eq_addr ~a, ~a, ~a"
                        (prt-nil)
                        (funcall test :pir)
                        else-label)
                   (prt "~a = ~a" result (funcall then :pir))
                   (prt "goto ~a" end-label)
                   (prt-label else-label)
                   (prt "~a = ~a" result (funcall else :pir))
                   (prt-label end-label)
                   result))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-progn (first rest)
  (let ((super (make-program :first first :rest rest))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((first (funcall self :get :first self))
                       (rest (funcall self :get :rest self)))
                   (funcall first :pir)
                   (funcall rest :Pir)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-let (vars values body)
  (let ((super (make-program :vars vars :values values :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let* ((vars (funcall self :get :vars))
                        (values (funcall self :get :values))
                        (body (funcall self :get :body))
                        (outers (car args))
                        (name (gensym "let"))
                        (flat-function (make-flat-function
                                        name
                                        vars
                                        nil
                                        nil
                                        nil
                                        outers
                                        nil)))
                   (if outers
                       (funcall (car outers) :add
                                :inner-functions flat-function))
                   (funcall flat-function :set :body
                            (funcall body :東京ミュウミュウ-metamorphose!
                                     (cons flat-function outers)))
                   (make-extracted-let name values)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-let* (bindings body)
  (let ((super (make-program :bindings bindings :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let* ((bindings (funcall self :get :bindings))
                        (body (funcall self :get :body))
                        (outers (car args))
                        (name (gensym "let"))
                        (flat-let*-function (make-flat-let*-function
                                             name
                                             nil
                                             nil
                                             nil
                                             nil
                                             outers
                                             nil)))
                   (if outers
                       (funcall (car outers) :add
                                :inner-functions flat-let*-function))
                   (funcall flat-let*-function :set :bindings
                            (funcall bindings :東京ミュウミュウ-metamorphose!
                                     (cons flat-let*-function outers)))
                   (funcall flat-let*-function :set :body
                            (funcall body :東京ミュウミュウ-metamorphose!
                                     (cons flat-let*-function outers)))
                   (make-extracted-let name nil)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-lambda (lambda-list body)
  (let ((super (make-program :lambda-list lambda-list :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let* ((lambda-list (funcall self :get :lambda-list))
                        (body (funcall self :get :body))
                        (outers (car args))
                        (name (gensym "lambda"))
                        (flat-function (make-flat-function
                                        name
                                        lambda-list
                                        nil
                                        nil
                                        nil
                                        outers
                                        nil)))
                   (if outers
                       (funcall (car outers) :add
                                :inner-functions flat-function))
                   (funcall flat-function
                            :set
                            :body
                            (funcall body :東京ミュウミュウ-metamorphose!
                                     (cons flat-function outers)))
                   (make-extracted-lambda name)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-application (&rest args)
  (let ((super (apply #'make-program args))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-regular-application (function arguments)
  (let ((super (make-application :function function :arguments arguments))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((function (funcall self :get :function))
                       (arguments (funcall self :get :arguments))
                       (args (next-var)))
                   (prt "~a = new 'ResizablePMCArray'" args)
                   (funcall arguments :pir args)
                   (funcall function :pir args)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-lambda-application (lambda arguments)
  (let ((super (make-application :lambda lambda :arguments arguments))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let* ((lambda (funcall self :get :lambda))
                        (arguments (funcall self :get :arguments))
                        (lambda-list (funcall lambda :get :lambda-list))
                        (body (funcall lambda :get :body))
                        (outers (car args))
                        (name (gensym "lambda"))
                        (flat-function (make-flat-function name
                                                           lambda-list
                                                           nil
                                                           nil
                                                           nil
                                                           outers
                                                           nil)))
                   (if outers
                       (funcall (car outers) :add
                                :inner-functions flat-function))
                   (funcall flat-function
                            :set
                            :body
                            (funcall body
                                     :東京ミュウミュウ-metamorphose!
                                     (cons flat-function outers)))
                   (make-regular-application
                    (make-local-function name)
                    (funcall arguments :東京ミュウミュウ-metamorphose!
                             (cons flat-function outers)))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-arguments (first rest)
  (let ((super (make-program :first first :rest rest))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((first (funcall self :get :first))
                       (rest  (funcall self :get :rest))
                       (array (car args)))
                   (prt "push ~a, ~a" array (funcall first :pir))
                   (funcall rest :pir array)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-no-argument ()
  (let ((super (make-program))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir (let ((array (car args)))
                      array))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun %make-let*-bindings (bindings r f ex-r)
  (if bindings
      (let ((first (car bindings)))
        (make-let*-bindings (car first)
                            (objectify (cadr first) (extend-r r ex-r) f)
                            (%make-let*-bindings (cdr bindings) r f
                                                 (cons (car first) ex-r))))
      (make-let*-no-bindings)))

(defun make-let*-bindings (var value rest)
  (let ((super (make-program :var var :value value :rest rest))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((var (funcall self :get :var))
                       (value  (funcall self :get :value))
                       (rest  (funcall self :get :rest)))
                   (prt ".local pmc ~a" (parrot-var var))
                   (prt "~a = ~a" (parrot-var var) (funcall value :pir))
                   (if (special-var-p var)
                       (prt-push-dynamic var))
                   (funcall rest :pir)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-let*-no-bindings ()
  (let ((super (make-program))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir nil)
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))


(defun make-function (symbol)
  (let ((super (make-program :symbol symbol))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-local-function (symbol)
  (let ((super (make-function symbol))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((symbol (funcall self :get :symbol))
                       (args (car args))
                       (return-value (next-var)))
                   (prt "~a = ~a(~a :flat)"
                        return-value (parrot-sub-name symbol) args)
                   return-value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-global-function (symbol)
  (let ((super (make-function symbol))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((symbol (funcall self :get :symbol))
                       (args (car args))
                       (fun-var (next-var))
                       (return-value (next-var)))
                   (prt "~a = get_hll_global [ ~s ], ~s"
                        fun-var
                        (package-name (symbol-package symbol))
                        (symbol-name symbol))
                   (prt "~a  = ~a(~a :flat)" return-value fun-var args)
                   return-value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-in-package (name)
  (let ((super (make-program :name name))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:pir
                 (let ((name (funcall self :get :name)))
                   (prt-top ".namespace [ ~s ]~%" name)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-eval-when (situations form)
  (let ((super (make-program :situations situations :form form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let* ((situations (funcall self :get :situations))
                        (form (funcall self :get :form))
                        (modifiers (cons ":anon" nil)))
                   (when (member :compile-toplevel situations)
                     (eval form))
                   (if (member :load-toplevel situations)
                       (setq modifiers (cons ":load" modifiers)))
                   (if (member :execute situations)
                       (setq modifiers (cons ":init" modifiers)))
                   (if modifiers
                       (funcall (make-flat-function (gensym "init")
                                                    nil
                                                    form
                                                    nil
                                                    nil
                                                    nil
                                                    modifiers)
                                :pir))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-defvar (symbol form)
  (let ((super (make-program :symbol symbol :form form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let* ((symbol (funcall self :get :symbol))
                        (form   (funcall self :get :form))
                        (sym-var (prt-intern-symbol symbol)))
                   (prt "~a.'specialize'()" sym-var)
                   (prt "setattribute ~a, 'value', ~a"
                        sym-var
                        (funcall form :pir))
                   sym-var))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-defun (name lambda-list body)
  (let ((super (make-program :name name :lambda-list lambda-list :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:東京ミュウミュウ-metamorphose!
                 (let* ((name (funcall self :get :name))
                        (lambda-list (funcall self :get :lambda-list))
                        (body (funcall self :get :body))
                        (outers (car args))
                        (flat-function (make-flat-function  name
                                                            lambda-list
                                                            nil
                                                            nil
                                                            nil
                                                            outers
                                                            nil))
                        (extracted-body (funcall
                                         body
                                         :東京ミュウミュウ-metamorphose!
                                         (cons flat-function outers))))
                   (funcall flat-function :set :body extracted-body)
                   flat-function))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-defmacro (name lambda-list body)
  (let ((super (make-program :name name :lambda-list lambda-list :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:東京ミュウミュウ-metamorphose!
                 (let* ((name (funcall self :get :name))
                        (lambda-list (funcall self :get :lambda-list))
                        (body (funcall self :get :body))
                        (outers (car args))
                        (flat-function (make-flat-macro-function  name
                                                                  lambda-list
                                                                  nil
                                                                  nil
                                                                  nil
                                                                  outers
                                                                  nil))
                        (extracted-body (funcall
                                         body
                                         :東京ミュウミュウ-metamorphose!
                                         (cons flat-function outers))))
                   (funcall flat-function :set :body extracted-body)
                   flat-function))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-flat-function (name
                           lambda-list
                           body
                           inner-functions
                           lexical-store
                           outers
                           modifiers)
  (let ((super (make-program :name name :lambda-list lambda-list :body body
                             :inner-functions inner-functions
                             :lexical-store lexical-store
                             :outers outers
                             :modifiers modifiers))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:pir
                 (let ((*var-counter* 0)
                       (*label-counter* 0)
                       (name (funcall self :get :name))
                       (lambda-list (funcall self :get :lambda-list))
                       (arguments (collect-vars lambda-list))
                       (body (funcall self :get :body))
                       (outers (funcall self :get :outers))
                       (inner-functions (funcall self :get :inner-functions))
                       (lexical-store (funcall self :get :lexical-store))
                       (modifiers (format nil "~{ ~a~}"
                                          (funcall self :get :modifiers))))
                   (if outers
                       (prt-top ".sub ~a :outer(~a)~a"
                                (parrot-sub-name name)
                                (parrot-sub-name (funcall (car outers)
                                                          :get :name))
                                modifiers)
                       (prt-top ".sub ~a~a" (parrot-sub-name name) modifiers))
                   (pir-lambda-list lambda-list)
                   (mapc (lambda (var)
                           (if (special-var-p var)
                               (prt-push-dynamic var)))
                         arguments)
                   (mapc (lambda (var)
                           (prt ".lex '~a', ~a"
                                (parrot-var var) (parrot-var var)))
                         lexical-store)
                   (let ((ret (funcall body :pir)))
                     (prt ".return(~a)" ret))
                   (prt-top ".end~%")
                   (mapc (lambda (x)
                           (funcall x :pir))
                         inner-functions)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-flat-let*-function (name
                                bindings
                                body
                                inner-functions
                                lexical-store
                                outers
                                modifiers)
  (let ((super (make-program :name name :bindings bindings :body body
                             :inner-functions inner-functions
                             :lexical-store lexical-store
                             :outers outers
                             :modifiers modifiers))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:pir
                 (let ((*var-counter* 0)
                       (*label-counter* 0)
                       (name (funcall self :get :name))
                       (bindings (funcall self :get :bindings))
                       (body (funcall self :get :body))
                       (outers (funcall self :get :outers))
                       (inner-functions (funcall self :get :inner-functions))
                       (lexical-store (funcall self :get :lexical-store))
                       (modifiers (format nil "~{ ~a~}"
                                          (funcall self :get :modifiers))))
                   (if outers
                       (prt-top ".sub ~a :outer(~a)~a"
                                (parrot-sub-name name)
                                (parrot-sub-name (funcall (car outers)
                                                          :get :name))
                                modifiers)
                       (prt-top ".sub ~a~a" (parrot-sub-name name) modifiers))
                   (funcall bindings :pir)
                   (mapc (lambda (var)
                           (prt ".lex '~a', ~a"
                                (parrot-var var) (parrot-var var)))
                         lexical-store)
                   (let ((ret (funcall body :pir)))
                     (prt ".return(~a)" ret))
                   (prt-top ".end~%")
                   (mapc (lambda (x)
                           (funcall x :pir))
                         inner-functions)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-flat-macro-function (name
                                 lambda-list
                                 body
                                 inner-functions
                                 lexical-store
                                 outers
                                 modifiers)
  "見てのとおり、これはいらないかも。"
  (let ((super (make-flat-function name
                                   lambda-list
                                   body
                                   inner-functions
                                   lexical-store
                                   outers
                                   modifiers))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-extracted-let (name values)
  (let ((super (make-program :name name :values values))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((name (funcall self :get :name))
                       (values (funcall self :get :values))
                       (var (next-var))
                       (args (next-var))
                       (result (next-var)))
                   (prt ".const 'Sub' ~a = ~a"
                        var (parrot-sub-name name))
                   (prt "~a = new 'ResizablePMCArray'" args)
                   (mapc (lambda (arg)
                           (prt "push ~a, ~a" args (funcall arg :pir)))
                         values)
                   (prt "~a = ~a(~a :flat)" result var args)
                   result))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-extracted-lambda (name)
  (let ((super (make-program :name name))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((name (funcall self :get :name))
                       (var (next-var)))
                   (prt ".const 'Sub' ~a = ~a" var (parrot-sub-name name))
                   var))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-global-function-reference (name)
  (let ((super (make-program :name name))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((name (funcall self :get :name))
                       (var (next-var)))
                   (prt "~a = get_hll_global [ ~s ], ~s"
                        var
                        (package-name (symbol-package name))
                        (symbol-name name))
                   var))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-vars (vars)
  (if (null vars)
      nil
      (cons (cons (car vars) (cadr vars))
            (make-vars (cddr vars)))))

(defun walk (self vars message &rest args)
  (if (null vars)
      self
      (let ((key (caar vars))
            (val (cdar vars)))
        (when (functionp val)
          (funcall self :set key (apply val message args)))
        (apply #'walk self (cdr vars) message args))))

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

(defun prt-nil ()
  (let ((var (next-var)))
    (prt "~a = get_hll_global \"NIL\"" var)
    var))

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

(defun prt-intern-symbol (symbol)
  (let ((package (next-var))
        (var (next-var)))
    (prt "~a = find_package(~s)"
         package (package-name (symbol-package symbol)))
    (prt "~a = ~a.'intern'(~s)" var package (symbol-name symbol))
    var))

(defun pir-atom (atom)
  (typecase atom
    (string
       (let ((var (next-var)))
         (prt "~a = new ~s" var "String")
         (prt "~a = utf8:unicode:~s" var atom)
         var))
    (symbol
       (pir-symbol atom))
    (t
       (let ((var (next-var)))
         (prt "~a = box ~s" var atom)
         var))))

(defun set-lexical-var (var outers)
  (if (null outers)
      nil
      (if (member var (collect-vars
                       (funcall (car outers) :get :lambda-list)))
          (funcall (car outers) :add :lexical-store var)
          (set-lexical-var var (cdr outers)))))

(defun collect-vars (x)
  (if (null x)
      nil
      (if (eq (car x) '&rest)
          (collect-vars (cdr x))
          (cons (car x) (collect-vars (cdr x))))))

(defun list-to-arguments (list)
  (if list
      (make-arguments (car list) (list-to-arguments (cdr list)))
      (make-no-argument)))

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

(defun var-kind (var r f)
  (declare (ignore f))
  (if (member var (car r))
      :local
      (labels ((f (x)
                 (if (endp x)
                     :dynamic
                     (if (member var (car x))
                         :lexical
                         (f (cdr x))))))
        (f (cdr r)))))

(defun prt-push-dynamic (symbol)
  (let ((var (parrot-var symbol)))
    (prt ".lex '~a', ~a" var var)))

(defun extend-r (r vars)
  (cons vars r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parrot-compile-file (file &optional
                            (pir-file (namestring
                                       (make-pathname :type "pir"
                                                      :defaults file)))
                            (pbc-file (namestring
                                       (make-pathname :type "pbc"
                                                      :defaults file))))
  (declare (ignorable pbc-file))
  (compile-lisp-to-pir file pir-file)
  ;;(compile-pir-to-pbc pir-file pbc-file)
  )

(defun %macroexpand (form)
  (let ((expanded-form (macroexpand-1 form)))
    (if (eq expanded-form form)
        form
        (%macroexpand expanded-form))))

(defun read-loop (in)
  (let ((form (read in nil)))
    (print form)
    (when form
      (let ((object (objectify form nil nil)))
        (if (funcall object :toplevelp)
            (setq object (funcall object :東京ミュウミュウ-metamorphose! nil))
            (let ((flat-function (make-flat-function
                                  (gensym "toplevel")
                                  nil
                                  nil
                                  nil
                                  nil
                                  nil
                                  '(":anon" ":init" ":load"))))
              (setq object (funcall object
                                    :東京ミュウミュウ-metamorphose!
                                    (list flat-function)))
              (funcall flat-function :set :body object)
              (setq object flat-function)))
        (funcall object :pir))
      (read-loop in))))

(defun compile-lisp-to-pir (lisp-file pir-file)
  (with-open-file (in lisp-file)
    (with-open-file (*pir-stream* pir-file :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
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

(defun compile-and-run (&optional (file "/home/ancient/letter/parrot/chocolisp/compiler/a.lisp"))
  (parrot-compile-file file)
  (locally (declare (optimize (speed 0)))
    (sb-posix:chdir "/home/ancient/letter/parrot/chocolisp/"))
  (sb-ext:run-program "parrot"
                      (list "chocolisp.pir")
                      :search t
                      :wait t
                      :output *standard-output*))

;;(compile-and-run "/home/ancient/letter/parrot/chocolisp/compiler/parrot-compiler.lisp")