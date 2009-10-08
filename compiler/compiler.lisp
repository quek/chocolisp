(declaim (optimize (debug 3) (safety 3)))

(in-package :chimacho)

(defvar *pir-stream* *standard-output*)
(defvar *var-counter* 0)
(defvar *label-counter* 0)
(defvar *in-eval* nil)
(defvar *compile-toplevel* nil)
(defvar *block* nil)
(defvar *tags* nil)

(defun objectify (form r f)
  (print form)
  (if (atom form)
      (if (symbolp form)
          (objectify-reference form r)
          (objectify-quotation form))
      (case (car form)
        (parrot-code
           (make-parrot-code (cdr form)))
        (quote
           (objectify-quotation (cadr form)))
        (block
            (objectify-block (cadr form) (cddr form) r f))
        (return-from
         (objectify-return-from (cadr form) (cddr form) r f))
        (tagbody
           (if (null (cdr form))
               (objectify-quotation nil)
               (objectify-tagbody (cdr form) r f)))
        (go
           (objectify-go (cadr form) r))
        (if
            (objectify-if (cadr form) (caddr form) (cadddr form) r f))
        (let
            (objectify-let (cadr form) (cddr form) r f))
        (let*
            (objectify-let* (cadr form) (cddr form) r f))
        (function
           (objectify-function (cadr form) r f))
        (progn
          (objectify-progn (cdr form) r f))
        (setq
           ;; TODO 1 つだけじゃない
           (objectify-setq (cadr form) (caddr form) r f))
        (flet
            (objectify-flet (cadr form) (cddr form) r f))
        (labels
            (objectify-labels (cadr form) (cddr form) r f))
        (eval-when
            (objectify-eval-when (cadr form) (cddr form) r f))
        (the
            (objectify-progn (cddr form) r f))
        (defun
            (objectify-defun (cadr form) (caddr form) (cdddr form) r f))
        (defvar
            (objectify-defvar (cadr form) (caddr form) r f))
        (in-package
           (objectify
            ($list 'eval-when '(:compile-toplevel :load-toplevel :execute)
                   ($list 'setq '*package*
                          ($list 'find-package (cadr form)))) r f))
        (declare
           (objectify nil r f))
        (t
           (if (symbolp (car form))
               (if (macro-function (car form))
                   (objectify ($macroexpand form) r f)
                   (objectify-application (car form) (cdr form) r f))
               (objectify-application (car form) (cdr form) r f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun objectify-block (name form r f)
  (let* ((*block* (cons name *block*)))
    (make-block name
                (length *block*)
                (objectify (cons 'progn form) r f))))

(defun objectify-return-from (name result r f)
  (let ((block ($member name *block*)))
    (if block
        (make-return-from name (length block)
                          (objectify (cons 'progn result) r f))
        ($error (string+ name " in unknown block.")))))

(defun collect-tags (body tags)
  (if body
      (if (atom (car body))
          (collect-tags (cdr body) (cons (car body) tags))
          (collect-tags (cdr body) tags))
      tags))

(defun objectify-tagbody (body r f)
  (let* ((*tags* (collect-tags body *tags*))
         (seq ($mapcar (lambda (x)
                         (if (atom x)
                             (make-tag x)
                             (objectify x (cons (cons (cons :tag *tags*)
                                                      (car r))
                                                (cdr r))
                                        f)))
                       body)))
    (make-tagbody *tags* seq)))

(defun local-go-p (tag env)
  (if env
      (if (and (consp (car env))
               (eq :tag (caar env)))
          ($member tag (cdar env))
          (local-go-p tag (cdr env)))))

(defun objectify-go (tag r)
  (if (local-go-p tag (car r))
      (make-local-go tag)
      (make-lexical-go tag)))

(defun objectify-flet (flet-form body-form r f)
  (let* ((fnames ($mapcar (lambda (form)
                            (let ((name (car form)))
                              (cons name ($gensym ($symbol-name name)))))
                          flet-form))
         (lambdas ($mapcar (lambda (name form)
                             (make-lambda
                              (cdr name)
                              (cadr form)
                              (objectify (cons 'progn (cddr form))
                                         (extend-r r (cadr form)) f)))
                           fnames flet-form))
         (new-f (extend-f f fnames))
         (body (objectify (cons 'progn body-form) r new-f)))
    (make-flet lambdas body)))

(defun %objectify-labels (acc labels-form body-form r f)
  (if labels-form
      (let* ((def (car labels-form))
             (label (car def))
             (gensym-label ($gensym ($symbol-name label)))
             (lambda-list (cadr def))
             (body (cddr def))
             (new-f (extend-f f ($list (cons label gensym-label)))))
        (%objectify-labels (cons (make-lambda
                                  gensym-label
                                  lambda-list
                                  (objectify (cons 'progn  body)
                                             (extend-r r lambda-list)
                                             new-f))
                                 acc)
                           (cdr labels-form)
                           body-form
                           r
                           new-f))
      (make-flet acc (objectify (cons 'progn body-form) r f))))

(defun objectify-labels (labels-form body-form r f)
  (%objectify-labels nil labels-form body-form r f))

(defun objectify-setq (symbol value-form r f)
  (if (and *compile-toplevel*
           (eq 'cl:*package* symbol))
      (make-change-package
       value-form (make-dynamic-assignment symbol (objectify value-form r f)))
      (case (var-kind symbol r)
        (:local
           (make-local-assignment symbol (objectify value-form r f)))
        (:lexical
           (make-lexical-assignment symbol (objectify value-form r f)))
        (:dynamic
           (make-dynamic-assignment symbol (objectify value-form r f)))
        (t
           ($error (string+ symbol " is unknown variable."))))))

(defun objectify-defvar (symbol value-form r f)
  (set-info symbol :kind :special)
  (make-defvar symbol (objectify value-form r f)))

(defun objectify-eval-when (situations form r f)
  (let ((*compile-toplevel* ($member :compile-toplevel situations)))
    (make-eval-when situations (objectify (cons 'progn form) r f) (cons 'progn form))))

(defun objectify-quotation (value)
  (make-constant value))

(defun objectify-reference (var r)
  (if (keywordp var)
      (make-constant var)
      (case (var-kind var r)
        (:local
           (make-local-reference var))
        (:lexical
           (make-lexical-reference var))
        (:dynamic
           (make-dynamic-reference var)))))

(defun objectify-if (test then else r f)
  (make-if (objectify test r f)
           (objectify then r f)
           (objectify else r f)))

(defun objectify-let (bindings body r f)
  (let* ((bindings ($mapcar (lambda (x)
                              (if (atom x)
                                  (cons x nil)
                                  x))
                            bindings))
         (vars ($mapcar #'car bindings))
         (values  (list-to-arguments ($mapcar (lambda (x)
                                                (objectify (cadr x) r f))
                                              bindings))))
    (make-let vars
              values
              (objectify (cons 'progn body) (extend-r r vars) f))))

(defun objectify-let* (bindings body r f)
  (if bindings
      (let* ((bind (let ((x (car bindings)))
                     (if (atom x) (cons x nil) x)))
             (vars (cons (car bind) nil))
             (value (cadr bind)))
        (make-let vars
                  (list-to-arguments (cons (objectify value r f) nil))
                  (objectify-let* (cdr bindings)
                                  body
                                  (extend-r r vars)
                                  f)))
      (objectify (cons 'progn body) r f)))

(defun objectify-lambda (lambda-list body r f)
  (make-lambda ($gensym "lambda")
               lambda-list
               (objectify (cons 'progn body)
                          (extend-r r (collect-vars lambda-list))
                          f)))

(defun objectify-function (name r f)
  (if (symbolp name)
      (if (eq *package* ($symbol-package name))
          (make-extracted-lambda name)
          (make-global-function-reference name))
      (if (and ($consp name)
               (eq 'lambda (car name)))
          (let ((lambda-list (cadr name))
                (body (cddr name)))
            (objectify-lambda lambda-list body r f))
          ($error (string+ "Invalid function name " name)))))

(defun objectify-defun (name lambda-list body r f)
  (make-defun name
              lambda-list
              (objectify (cons 'progn body) (extend-r r (collect-vars lambda-list)) f)))

(defun objectify-progn (body r f)
  (if (null body)
      (make-constant nil)
      (if (null (cdr body))
          (objectify (car body) r f)
          (make-progn (objectify (car body) r f)
                      (objectify (cons 'progn (cdr body)) r f)))))

(defun objectify-application (fun args r f)
  (if (symbolp fun)
      (if ($assoc fun f)
          (objectify-application-local-function fun args r f)
          (objectify-application-symbol fun args r f))
      (if (and ($consp fun)
               (eq (car fun) 'lambda))
          (objectify-application-lambda fun args r f)
          ($error (string+ fun " is not applicable.")))))

(defun objectify-application-local-function (fun args r f)
  (make-regular-application
   (make-local-function (cdr ($assoc fun f)))
   (list-to-arguments ($mapcar (lambda (x)
                                 (objectify x r f))
                               args))))

(defun objectify-application-symbol (fun args r f)
  (let ((fun (if (eq *package* ($symbol-package fun))
                 (make-local-function fun)
                 (make-global-function fun)))
        (objected-args (list-to-arguments
                        ($mapcar (lambda (x)
                                   (objectify x r f))
                                 args))))
    (make-regular-application fun objected-args)))

(defun objectify-application-lambda (lambda-form args r f)
  (let ((lambda-form (objectify-lambda (cadr lambda-form)
                                       (cddr lambda-form)
                                       r f))
        (objected-args (list-to-arguments
                        ($mapcar (lambda (x) (objectify x r f))
                                 args))))
    (make-lambda-application lambda-form objected-args)))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun collect-regsiters (form acc)
  (if form
      (if (and (keywordp (car form))
               (not (assoc (car form) acc)))
          (collect-regsiters (cdr form) (acons (car form) (next-var) acc))
          (collect-regsiters (cdr form) acc))
      acc))

(defun make-parrot-code (form)
  (let ((super (make-program :form form))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (let ((form (funcall self :get :form))
                            (regs nil)
                            (code "")
                            (var (next-var)))
                        ($mapcar (lambda (x)
                                   (if (keywordp x)
                                       (progn
                                         (unless (assoc x regs)
                                           (setq regs (acons x (next-var) regs)))
                                         (setq code (string+ code (cdr (assoc x regs)))))
                                       (setq code (string+ code x))))
                                 form)
                        (prt var " = " code)
                        var))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-object (&rest vars)
  (let (self
        (vars (make-vars vars)))
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp
                 nil)
              (:all-vars
                 vars)
              (:get
                 (cdr ($assoc (car args) vars)))
              (:set
                 (let ((cons ($assoc (car args) vars)))
                   (if cons
                       ($rplacd cons (cadr args)))))
              (:add
                 (let ((cons ($assoc (car args) vars)))
                   (if cons
                       ($rplacd cons (cons (cadr args) (cdr cons))))))
              (t
                 ($error (string+ message " in unknown message."))))))))

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

(defun make-block (name type form)
  (let ((super (make-program :name name :type type :form form))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (let ((form (funcall self :get :form))
                            (handler (next-var))
                            (handler-label (next-label "BLOCK"))
                            (skip-label (next-label "BLOCK")))
                        (prt handler " = new 'ExceptionHandler'")
                        (prt "set_addr " handler ", " handler-label)
                        (prt handler ".'handle_types'(" (princ-to-string type) ")")
                        (prt "push_eh " handler)
                        (let ((ret (funcall form :pir))
                              (ex (next-var))
                              (can-handle-label (next-label "BLOCK")))
                          (prt "pop_eh")
                          (prt "goto " skip-label)
                          (prt-label handler-label)
                          (prt ".get_results(" ex ")")
                          (prt "pop_eh")
                          (prt "$I0 = " handler ".'can_handle'(" ex ")")
                          (prt "if $I0 goto " can-handle-label)
                          (prt "rethrow " ex)
                          (prt-label can-handle-label)
                          (prt ret " = " ex "['payload']")
                          (prt-label skip-label)
                          ret)))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-return-from (name type result)
  (let ((super (make-program :name name :type type :result result))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (let ((type (funcall self :get :type))
                            (result (funcall self :get :result))
                            (ex-var (next-var))
                            (dummy-ret-var (next-var)))
                        (prt ex-var " = new 'Exception'")
                        (prt ex-var "['type'] = " (princ-to-string type))
                        (prt ex-var "['payload'] = " (funcall result :pir))
                        (prt "throw " ex-var)
                        dummy-ret-var))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-tagbody (tags body)
  (let ((super (make-program :tags tags :body body))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:東京ミュウミュウ-metamorphose!
                      (funcall self :set :body
                               ($mapcar (lambda (x)
                                          (apply x :東京ミュウミュウ-metamorphose! args))
                                        (funcall self :get :body)))
                      self)
                   (:pir
                      (let ((tags (funcall self :get :tags))
                            (body (funcall self :get :body)))
                        ($mapcar (lambda (x)
                                   (let ((cont-var (parrot-cont x)))
                                     (prt ".local pmc " cont-var)
                                     (prt cont-var " = new 'Continuation'")
                                     (prt "set_addr " cont-var ", " (parrot-tag x))
                                     (prt ".lex '" cont-var "', " cont-var)))
                                 tags)
                        ($mapcar (lambda (x)
                                   (apply x :pir args))
                                 body)
                        (funcall (make-constant nil) :pir)))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-tag (tag)
  (let ((super (make-program :tag tag))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (prt-label (parrot-tag (funcall self :get :tag))))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-local-go (tag)
  (let ((super (make-program :tag tag))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (let ((tag (parrot-tag (funcall self :get :tag))))
                        (prt "goto " tag)
                        (next-var)))
                   (t (let ((ret (apply super message args)))
                        (if (eq ret super) self ret))))))))

(defun make-lexical-go (tag)
  (let ((super (make-program :tag tag))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:pir
                      (let ((var (parrot-cont (funcall self :get :tag))))
                        (prt ".local pmc " var)
                        (prt var " = find_lex '" var "'")
                        (prt var "(0)")
                        var))
                   (t (let ((ret (apply super message args)))
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
                   (unless ($member var
                                    (collect-vars
                                     (funcall (caar args) :get :lambda-list)))
                     (set-lexical-var var (car args)))
                   self))
              (:pir
                 (let ((var (funcall self :get :var)))
                   (parrot-var var)))
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
                 (let ((var (parrot-var (funcall self :get :var))))
                   (prt ".local pmc " var)
                   (prt var " = find_lex '" var "'")
                   var))
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
                   (prt value " = get_dynamic_scope_value('"
                        (parrot-var var)
                        "', utf8:unicode:"
                        (prin1-to-string ($package-name ($symbol-package var)))
                        ", utf8:unicode:"
                        (prin1-to-string ($symbol-name var))
                        ")")
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
                   (prt (parrot-var (funcall self :get :var)) " = " value)
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
              (:pir
                 (let ((var (parrot-var (funcall self :get :var)))
                       (value (funcall (funcall self :get :form) :pir)))
                   (prt "store_lex '" var "', " value)
                   value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-dynamic-assignment (var form)
  (let ((super (make-assignment var form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let* ((var (funcall self :get :var))
                        (form (funcall self :get :form))
                        (value (funcall form :pir)))
                   (prt "set_dynamic_scope_value('"
                        (parrot-var var)
                        "', utf8:unicode:"
                        (prin1-to-string ($package-name ($symbol-package var)))
                        ", utf8:unicode:"
                        (prin1-to-string ($symbol-name var))
                        ", "
                        value
                        ")")
                   value))
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
                   (prt "eq_addr "
                        (prt-nil)
                        ", "
                        (funcall test :pir)
                        ", "
                        else-label)
                   (prt result " = " (funcall then :pir))
                   (prt "goto " end-label)
                   (prt-label else-label)
                   (prt result " = " (funcall else :pir))
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
                 (let ((first (funcall self :get :first))
                       (rest (funcall self :get :rest)))
                   (funcall first :pir)
                   (funcall rest :pir)))
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
                        (name ($gensym "let"))
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
                   (make-extracted-let
                    name
                    (funcall values :東京ミュウミュウ-metamorphose!
                             outers))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-lambda (name lambda-list body)
  (let ((super (make-program :name name :lambda-list lambda-list :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let* ((name (funcall self :get :name))
                        (lambda-list (funcall self :get :lambda-list))
                        (body (funcall self :get :body))
                        (outers (car args))
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
                   (funcall flat-function :set :body
                            (funcall body :東京ミュウミュウ-metamorphose!
                                     (cons flat-function outers)))
                   (make-extracted-lambda name)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-flet (lambdas body)
  (let ((super (make-program :lambdas lambdas :body body))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let ((lambdas (funcall self :get :lambdas))
                       (body  (funcall self :get :body))
                       (outers (car args)))
                   ($mapcar (lambda (x)
                              (funcall x :東京ミュウミュウ-metamorphose!
                                       outers))
                            lambdas)
                   (funcall body :東京ミュウミュウ-metamorphose! outers)))
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
                       (arguments (funcall self :get :arguments)))
                   (apply function :pir (funcall arguments :pir))))
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
                        (name ($gensym "lambda"))
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
                   (funcall flat-function :set :body
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
                       (rest  (funcall self :get :rest)))
                   (cons (funcall first :pir) (funcall rest :pir))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-no-argument ()
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
                       (sub (next-var))
                       (return-value (next-var)))
                   (prt ".const 'Sub' " sub " = " (parrot-sub-name symbol))
                   (prt sub " = newclosure " sub)
                   (prt return-value " = " sub "(" (join ", " args) ")")
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
                       (fun-var (next-var))
                       (return-value (next-var)))
                   (prt fun-var
                        " = get_hll_global [ "
                        (prin1-to-string ($package-name ($symbol-package symbol)))
                        " ], "
                        (prin1-to-string ($symbol-name symbol)))
                   (prt return-value
                        "  = "
                        fun-var
                        "("
                        (join ", " args)
                        ")")
                   return-value))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-change-package (name setq-form)
  (let ((super (make-program :name name :setq-form setq-form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュウミュウ-metamorphose!
                 (let ((setq-form (funcall self :get :setq-form))
                       (outers (car args)))
                   (funcall (car outers) :add :inner-functions
                            self)
                   (apply setq-form message args)))
              (:pir
                 (let ((name (funcall self :get :name)))
                   ;;(prt-in-namespace ($symbol-name (cadr name)))
                   (prt-in-namespace ($package-name (eval name)))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-eval-when (situations form raw-form)
  (let ((super (make-program :situations situations :form form
                             :raw-form raw-form))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:toplevelp t)
              (:東京ミュウミュウ-metamorphose!
                 (let* ((situations (funcall self :get :situations))
                        (form (funcall self :get :form))
                        (raw-form (funcall self :get :raw-form))
                        (modifiers nil))
                   (if (and (not *in-eval*)
                            ($member :compile-toplevel situations))
                       (eval raw-form))
                   (if ($member :load-toplevel situations)
                       (setq modifiers (cons ":load" modifiers)))
                   (if ($member :execute situations)
                       (setq modifiers (cons ":init" modifiers)))
                   (if modifiers
                       (let ((fun (make-flat-function ($gensym "eval-when")
                                                      nil
                                                      form
                                                      nil
                                                      nil
                                                      nil
                                                      (cons ":anon" modifiers))))
                         (funcall fun :set :body
                                  (funcall form
                                           :東京ミュウミュウ-metamorphose!
                                           (cons fun nil)))
                         fun)
                       (make-constant nil))))
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
                   (prt sym-var ".'specialize'()")
                   (prt "setattribute "
                        sym-var
                        ", 'value', "
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
                        (outers (car args)))
                   (if outers
                       (let* ((closure-name ($gensym ($symbol-name name)))
                              (closure (make-flat-function
                                        closure-name
                                        lambda-list
                                        (funcall
                                         body
                                         :東京ミュウミュウ-metamorphose!
                                         outers)
                                        nil nil outers nil))
                              (let-defun (make-let-defun name)))
                         (funcall (car outers) :add :inner-functions closure)
                         (funcall (car outers) :add :inner-functions let-defun)
                         (make-set-symbol-function name closure-name))
                       (let ((defun (make-flat-function
                                     name
                                     lambda-list
                                     nil
                                     nil
                                     nil
                                     outers
                                     nil)))
                         (funcall defun :set :body
                                  (funcall body
                                           :東京ミュウミュウ-metamorphose!
                                           (cons defun outers)))
                         defun))))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-let-defun (name)
  (let ((super (make-program :name name))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((name (funcall self :get :name)))
                   (prt-top ".sub " (parrot-sub-name name))
                   (prt ".param pmc x :slurpy")
                   (let ((f (next-var)))
                     (prt f
                          " = getattribute "
                          (prt-intern-symbol name)
                          ", 'function'")
                     (prt ".tailcall " f "(x :flat)"))
                   (prt-top ".end")
                   (new-line)))
              (t (let ((ret (apply super message args)))
                   (if (eq ret super) self ret))))))))

(defun make-set-symbol-function (name closure-name)
  (let ((super (make-program :name name :closure-name closure-name))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:pir
                 (let ((name (funcall self :get :name))
                       (closure-name (funcall self :get :closure-name))
                       (sub-var (next-var))
                       (closure-var (next-var)))
                   (prt ".const 'Sub' "
                        sub-var
                        " = "
                        (parrot-sub-name closure-name))
                   (prt closure-var " = newclosure " sub-var)
                   (prt "setattribute "
                        (prt-intern-symbol name)
                        ", 'function', "
                        closure-var)
                   closure-var))
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
                       (modifiers (join " " (funcall self :get :modifiers))))
                   (if outers
                       (prt-top ".sub "
                                (parrot-sub-name name)
                                " :outer("
                                (parrot-sub-name (funcall (car outers)
                                                          :get :name))
                                ") "
                                modifiers)
                       (prt-top ".sub " (parrot-sub-name name) modifiers))
                   (pir-lambda-list lambda-list)
                   (let ((dot-lex-vars ($mapcar (lambda (var)
                                                  (when (special-var-p var)
                                                    (prt-push-dynamic var)
                                                    var))
                                                arguments)))
                     ($mapcar (lambda (var)
                                (unless ($member var dot-lex-vars)
                                  (let ((var (parrot-var var)))
                                    (prt ".lex '" var "', " var))))
                              lexical-store))
                   (let ((ret (funcall body :pir)))
                     (prt ".return(" ret ")"))
                   (prt-top ".end")
                   (new-line)
                   ($mapcar (lambda (x)
                              (funcall x :pir))
                            inner-functions)))
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
                       (sub (next-var))
                       (result (next-var)))
                   (prt ".const 'Sub' " sub " = " (parrot-sub-name name))
                   (prt sub " = newclosure " sub)
                   (prt result " = " sub
                        "(" (join "," (funcall values :pir)) ")")
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
                   (prt ".const 'Sub' " var " = " (parrot-sub-name name))
                   ;; TODO newclosure の気持ちがわからない
                   (prt var " = newclosure " var)
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
                   (prt var
                        " = get_hll_global [ "
                        (prin1-to-string ($package-name ($symbol-package name)))
                        " ], "
                        (prin1-to-string ($symbol-name name)))
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

(defun next-var ()
  (setq *var-counter* (+ *var-counter* 1))
  (string+ "$P" (princ-to-string *var-counter*)))

(defun next-label (name)
  (setq *label-counter* (+ *label-counter* 1))
  (string+ "L_" name *label-counter*))

(defun parrot-tag (tag)
  (parrot-name tag "T_"))

(defun parrot-cont (x)
  (parrot-name x "c_"))

(defun parrot-var (lisp-var)
  (parrot-name lisp-var "v_"))


(defun parrot-name (lisp-var prefix)
  (let ((out prefix))
    ($map-string (lambda (c)
                   (if ($alpha-char-p c)
                       (setq out (string+ out c))
                       (setq out (string+ out (char-code (char c 0))))))
                 (prin1-to-string lisp-var))
    out))

(defun parrot-sub-name (symbol)
  (if ($symbol-package symbol)
      (prin1-to-string ($symbol-name symbol))
      (prin1-to-string (prin1-to-string symbol))))

(defun prt (&rest args)
  ($write-string "        " *pir-stream*)
  (apply #'prt-top args))

(defun prt-top (&rest args)
  ($mapcar (lambda (x)
             ($write-string x *pir-stream*))
           args)
  (new-line))

(defun prt-label (label)
  ($write-string label *pir-stream*)
  ($write-string ":" *pir-stream*)
  (new-line))

(defun prt-in-namespace (package)
  (prt-top ".namespace [ " (prin1-to-string package) " ]")
  (new-line))

(defun new-line ()
  ($terpri *pir-stream*))

(defun prt-nil ()
  (let ((var (next-var)))
    (prt var " = get_hll_global \"NIL\"")
    var))

(defun pir-constant (value)
  (if (atom value)
      (pir-atom value)
      (pir-cons value)))

(defun pir-cons (cons)
  (let ((var (next-var))
        (car (pir-constant (car cons)))
        (cdr (pir-constant (cdr cons))))
    (prt var " = cons(" car ", " cdr ")")
    var))

(defun pir-symbol (symbol)
  (prt-intern-symbol symbol))

(defun prt-intern-symbol (symbol)
  (let ((package-var (next-var))
        (package-name (prin1-to-string ($package-name
                                        ($symbol-package symbol))))
        (symbol-name (prin1-to-string ($symbol-name symbol)))
        (var (next-var)))
    (prt package-var " = find_package(utf8:unicode:" package-name ")")
    (prt var " = " package-var ".'intern'(utf8:unicode:" symbol-name ")")
    var))

(defun pir-atom (atom)
  (typecase atom
    (symbol
       (pir-symbol atom))
    (string
       (let ((var (next-var)))
         (prt var " = box utf8:unicode:" (prin1-to-string  atom))
         var))
    (character
       (let ((var (next-var)))
         (prt var " = new [\"COMMON-LISP\";\"CHARACTER\"]")
         (prt var " = " (prin1-to-string (char-code atom)))
         var))
    (t
       (let ((var (next-var)))
         (prt var " = box " (prin1-to-string atom))
         var))))

(defun set-lexical-var (var outers)
  (if (null outers)
      nil
      (if ($member var (collect-vars
                        (funcall (car outers) :get :lambda-list)))
          (let ((lexical-store (funcall (car outers) :get :lexical-store)))
            (unless ($member var lexical-store)
              (funcall (car outers) :add :lexical-store var)))
          (set-lexical-var var (cdr outers)))))

(defun collect-vars (x)
  (if (null x)
      nil
      (if ($member (car x) '(&rest &optional &key &allow-other-keys &aux))
          (collect-vars (cdr x))
          (cons (if (atom (car x))
                    (car x)
                    (caar x))
                (collect-vars (cdr x))))))

(defun list-to-arguments (list)
  (if list
      (make-arguments (car list) (list-to-arguments (cdr list)))
      (make-no-argument)))

(defun pir-lambda-list (lambda-list)
  (if (null lambda-list)
      nil
      (cond ((eq (car lambda-list) '&rest)
             (let ((var (parrot-var (cadr lambda-list))))
               (prt ".param pmc " var " :slurpy")
               (pir-lambda-list (cddr lambda-list))
               (prt var " = array_to_list(" var ")")))
            ((eq (car lambda-list) '&optional)
             (pir-lambda-list-optional (cdr lambda-list))
             (pir-lambda-list-default-value (cdr lambda-list)))
            (t
             (prt ".param pmc " (parrot-var (car lambda-list)))
             (pir-lambda-list (cdr lambda-list))))))

(defun suplied-p-var (arg)
  (if (and (consp arg)
           (caddr arg))
      (parrot-var (caddr arg))
      (string+ (parrot-var (if (atom arg) arg (car arg))) "_P")))

(defun pir-lambda-list-optional (lambda-list)
  (let* ((arg (if (atom (car lambda-list))
                  (cons (car lambda-list) nil)
                  (car lambda-list)))
         (var (parrot-var (car arg)))
         (supliedp (suplied-p-var arg)))
    (prt ".param pmc " var " :optional")
    (prt ".param int " supliedp " :opt_flag")))

(defun pir-lambda-list-default-value (lambda-list)
  (let* ((arg (if (atom (car lambda-list))
                  (cons (car lambda-list) nil)
                  (car lambda-list)))
         (var (parrot-var (car arg)))
         (default-value (cadr arg))
         (supliedp (suplied-p-var arg))
         (label (next-label "OPTIONAL")))
    (prt "if " supliedp " goto " label)
    ;; TODO ここの objectify は他の他の引数がバインドされた状態で評価される必要がある。
    ;; ここで objectify は遅すぎる。
    (prt var " = " (funcall (objectify default-value nil nil) :pir))
    (prt-label label)))

(defvar *info* nil)

(defun get-info (object key)
  ($assoc key (cdr ($assoc object *info*))))

(defun set-info (object key value)
  (let ((info ($assoc object *info*)))
    (if info
        (let ((key-value ($assoc key (cdr info))))
          (if key-value
              (setf (cdr key-value) value)
              (setf (cdr info) (acons key value (cdr info)))))
        (setf *info* (acons object (acons key value nil) *info*)))))

(defun special-var-p (symbol)
  (eq (cdr (get-info symbol :kind)) :special))

(defun macro-function-p (symbol)
  (cdr (get-info symbol :macro-function)))

(defun var-kind (var r)
  (if ($member var (car r))
      :local
      (labels ((f (x)
                 (if x
                     (if ($member var (car x))
                         :lexical
                         (f (cdr x)))
                     :dynamic)))
        (f (cdr r)))))

(defun prt-push-dynamic (symbol)
  (let ((var (parrot-var symbol)))
    (prt ".lex '" var "', " var)))

(defun extend-r (r vars)
  (cons vars r))

(defun extend-f (f flet-gensym)
  (if flet-gensym
      (extend-f (cons (car flet-gensym) f) (cdr flet-gensym))
      f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join (delimita list)
  (if list
      (let ((s (princ-to-string (car list))))
        ($mapcar (lambda (x)
                   (setq s (string+ s (string+ delimita (princ-to-string x)))))
                 (cdr list))
        s)
      ""))

(defun $mapcar1 (f list)
  (if list
      (cons (funcall f (car list))
            ($mapcar1 f (cdr list)))))

(defun $mapcar (f list &rest rest)
  (if list
      (cons (apply f (cons (car list) ($mapcar1 #'car rest)))
            (apply #'$mapcar f (cdr list) ($mapcar1 #'cdr rest)))))

(defun $map-string (f string)
  (if (= (length string) 0)
      nil
      (progn
        (funcall f (subseq string 0 1))
        ($map-string f (subseq string 1)))))

(defun $reverse (list)
  (%$reverse list nil))

(defun %$reverse (list acc)
  (if list
      (%$reverse (cdr list) (cons (car list) acc))
      acc))

(defun $list (&rest args)
  args)

(defun $consp (x)
  (if (atom x)
      nil
      t))

(defun $member (item list)
  (if list
      (if (eq item (car list))
          list
          ($member item (cdr list)))))

(defun $assoc (item alist)
  (if alist
      (if (eq item (caar alist))
          (car alist)
          ($assoc item (cdr alist)))))

(defun $alpha-char-p (c)
  (if (and (string<= "a" c)
           (string<= c "z"))
      t
      (if (and (string<= "A" c)
               (string<= c "Z"))
          t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pir-file-name (file)
  (string+ (subseq file 0 (- (length file) 4)) "pir"))

(defun parrot-compile-file (file)
  (let ((pir-file (pir-file-name file)))
    ;;(pbc-file (namestring (make-pathname :type "pbc" :defaults file))))
    (compile-lisp-to-pir file pir-file)
    ;;(compile-pir-to-pbc pir-file pbc-file)
    ))

(defun read-loop (in)
  (let ((form ($read in)))
    (when form
      (new-line)
      ($write-string "=head2" *pir-stream*)
      (new-line)
      ($write-string (prin1-to-string form) *pir-stream*)
      (new-line)
      ($write-string "=cut" *pir-stream*)
      (new-line)
      (let ((object (objectify form nil nil)))
        (if (funcall object :toplevelp)
            (setq object (funcall object :東京ミュウミュウ-metamorphose! nil))
            (let ((flat-function (make-flat-function
                                  ($gensym "toplevel")
                                  nil
                                  nil
                                  nil
                                  nil
                                  nil
                                  '(":anon" ":init" ":load"))))
              (setq object (funcall object
                                    :東京ミュウミュウ-metamorphose!
                                    ($list flat-function)))
              (funcall flat-function :set :body object)
              (setq object flat-function)))
        (funcall object :pir))
      (read-loop in))))

(defun compile-lisp-to-pir (lisp-file pir-file)
  (let ((*in-eval* nil)
        (*compile-toplevel* nil)
        (in (open-input-file lisp-file))
        (*pir-stream* (open-output-file pir-file)))
    (let ((*pir-stream* (make-broadcast-stream *pir-stream*
                                               *standard-output*)))
      (let ((*package* *package*))
        (put-common-header)
        (read-loop in)))
    ($close *pir-stream*)
    ($close in)))

(defun put-common-header ()
  (prt-top ".HLL \"chocolisp\"")
  (new-line))
