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
        (block
            (objectify-block (cadr form) (cddr form) r f))
        (catch
            ($error "not implemented."))
        (eval-when
            (objectify-eval-when (cadr form) (cddr form) r f))
        (flet
            (objectify-flet (cadr form) (cddr form) r f))
        (function
           (objectify-function (cadr form) r f))
        (go
           (objectify-go (cadr form) r))
        (if
            (objectify-if (cadr form) (caddr form) (cadddr form) r f))
        (labels
            (objectify-labels (cadr form) (cddr form) r f))
        (let
            (objectify-let (cadr form) (cddr form) r f))
        (let*
            (objectify-let* (cadr form) (cddr form) r f))
        (load-time-value
           ($error "not implemented."))
        (locally
            ($error "not implemented."))
        (macrolet
            ($error "not implemented."))
        (multiple-value-call
            ($error "not implemented."))
        (multiple-value-prog1
            ($error "not implemented."))
        (progn
          (objectify-progn (cdr form) r f))
        (progv
            ($error "not implemented."))
        (quote
           (objectify-quotation (cadr form)))
        (return-from
         (objectify-return-from (cadr form) (cddr form) r f))
        (setq
           ;; TODO 1 つだけじゃない
           (objectify-setq (cadr form) (caddr form) r f))
        (symbol-macrolet
            ($error "not implemented."))
        (tagbody
           (if (null (cdr form))
               (objectify-quotation nil)
               (objectify-tagbody (cdr form) r f)))
        (the
            (objectify-progn (cddr form) r f))
        (throw
            ($error "not implemented."))
        (unwind-protect
             ($error "not implemented."))
        ;; ここから下は special form ではない
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
        (parrot-code
           (make-parrot-code (cdr form)))
        (t
           (if (symbolp (car form))
               (if (macro-function (car form))
                   (objectify ($macroexpand form) r f)
                   (objectify-application (car form) (cdr form) r f))
               (objectify-application (car form) (cdr form) r f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; objectify
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
                              (objectify-lambda-list (cadr form) r f nil nil)
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
                                  (objectify-lambda-list lambda-list r new-f nil nil)
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
           (eq 'common-lisp:*package* symbol))
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
         (values ($mapcar #'cadr bindings)))
    (objectify (cons (cons 'lambda (cons vars body)) values) r f)))

(defun objectify-let* (bindings body r f)
  (if bindings
      (let* ((bind (let ((x (car bindings)))
                     (if (atom x) (cons x nil) x)))
             (var (car bind))
             (value (cadr bind)))
        (objectify (list 'let (list (list var value))
                         (cons 'let* (cons (cdr bindings)
                                           body)))
                   r f))
      (objectify (cons 'progn body) r f)))

(defun objectify-lambda (lambda-list body r f)
  (let ((lambda-list (objectify-lambda-list lambda-list r f nil nil)))
    (make-lambda ($gensym "lambda")
                 lambda-list
                 (objectify (cons 'progn body)
                            (extend-r r (collect-vars lambda-list))
                            f))))

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
  (let ((lambda-list (objectify-lambda-list lambda-list r f nil nil)))
    (make-defun name
                lambda-list
                (objectify (cons 'progn body) (extend-r r (collect-vars lambda-list)) f))))

(defun objectify-lambda-list (lambda-list r f keyword args)
  (if (null lambda-list)
      (make-lambda-list-end)
      (let ((car (car lambda-list)))
        (if (atom car)
            (if ($member car lambda-list-keywords)
                (objectify-lambda-list (cdr lambda-list) r f car args)
                (make-lambda-list
                 (cond ((eq keyword '&optional)
                        (make-optional-lambda-list car nil nil))
                       ((eq keyword '&key)
                        (make-key-lambda-list car nil nil))
                       ((eq keyword '&rest)
                        (make-rest-lambda-list car))
                       ((null keyword)
                        (make-required-lambda-list car)))
                 (objectify-lambda-list (cdr lambda-list) r f keyword (cons car args))))
            (make-lambda-list
             (apply (cond ((eq keyword '&optional) #'make-optional-lambda-list)
                          ((eq keyword '&key) #'make-key-lambda-list))
                    (car car)
                    (objectify (cons (list 'lambda args (cadr car)) args) (extend-r r args) f)
                    (caddr car)
                    nil)
             (objectify-lambda-list (cdr lambda-list) r f keyword (cons car args)))))))

(defun make-lambda-list (first rest)
  (let ((self (make-program :first first :rest rest)))
    (set-value self :pir
               (lambda (self)
                 (pir (get-value self :first))
                 (pir (get-value self :rest))))
    (set-value self :collect-vars
               (lambda (self)
                 (cons (get-value (get-value self :first) :var)
                       (call (get-value self :rest) :collect-vars))))
    (set-value self :pir-init-form
               (lambda (self)
                 (call (get-value self :first) :pir-init-form)
                 (call (get-value self :rest) :pir-init-form)))
    self))

(defun make-lambda-list-end ()
  (let ((self (make-program)))
    (set-value self :pir
               (lambda (self) self))
    (set-value self :collect-vars
               (lambda (self) self nil))
    (set-value self :pir-init-form
               (lambda (self) self))
    self))

(defun make-required-lambda-list (var)
  (let ((self (make-program :var var)))
    (set-value self :pir
               (lambda (self)
                 (prt ".param pmc " (parrot-var (get-value self :var)))))
    (set-value self :pir-init-form
               (lambda (self) self))
    self))

(defun make-init-form-lambda-list (var init-form svar)
  (let ((self (make-program :var var
                            :init-form (or init-form (make-constant nil))
                            :svar (suplied-p-var var svar))))
    (set-value self :pir-init-form
               (lambda (self)
                 (let ((label (next-label "INIT_FORM")))
                   (prt "if " (get-value self :svar) " goto " label)
                   (prt (parrot-var (get-value self :var)) " = "
                        (pir (get-value self :init-form)))
                   (prt-label label))))
    self))


(defun make-optional-lambda-list (var init-form svar)
  (let ((self (make-init-form-lambda-list var init-form svar)))
    (set-value self :pir
               (lambda (self)
                 (prt ".param pmc " (parrot-var (get-value self :var)) " :optional")
                 (prt ".param int " (get-value self :svar) " :opt_flag")))
    self))

(defun make-key-lambda-list (var init-form svar)
  (let ((self (make-init-form-lambda-list var init-form svar)))
    (set-value self :pir
               (lambda (self)
                 self
                 (prt ".param pmc key_args :slurpy")))
    (set-value self :pir-init-form
               (lambda (self)
                 (let ((key (get-value self :var))
                       (parrot-var (parrot-var (get-value self :var)))
                       (end-label (next-label "KEY_ARG_END")))
                   (prt ".local pmc " parrot-var)
                   (prt parrot-var " = getf(key_args, " (pir-symbol key) ")")
                   (prt "unless_null " parrot-var ", " end-label)
                   (prt parrot-var " = " (pir (get-value self :init-form)))
                   (prt-label end-label))))
    self))

(defun make-rest-lambda-list (var)
  (let ((self (make-program :var var)))
    (set-value self :pir
               (lambda (self)
                 (let ((var (parrot-var (get-value self :var))))
                   (prt ".param pmc " var " :slurpy"))))
    (set-value self :pir-init-form
               (lambda (self)
                 (let ((var (parrot-var (get-value self :var))))
                   (prt var " = array_to_list(" var ")"))))
    self))


(defun objectify-progn (body r f)
  (if (null body)
      (make-constant nil)
      (if (null (cdr body))
          (objectify (car body) r f)
          (make-progn (objectify (car body) r f)
                      (objectify (cons 'progn (cdr body)) r f)))))

(defun objectify-application (fun args r f)
  (let ((args (list-to-arguments ($mapcar (lambda (x) (objectify x r f)) args))))
    (if (symbolp fun)
        (if ($assoc fun f)
            (make-lexical-application (cdr ($assoc fun f)) args)
            (if (eq *package* ($symbol-package fun))
                (make-local-application fun args)
                (make-global-application fun args)))
        (if (and ($consp fun)
                 (eq (car fun) 'lambda))
            (let ((lambda-form (objectify-lambda (cadr fun) (cddr fun) r f)))
              (make-lambda-application lambda-form args))
            ($error (string+ fun " is not applicable."))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make
(defun get-value (hash key)
  ($gethash hash key))

(defun set-value (hash key value)
  ($sethash hash key value))

(defun add-value (hash key value)
  (set-value hash key (cons value (get-value hash key))))

(defun call (hash key &rest args)
  (apply (get-value hash key) hash args))

(defun make-object (&rest args)
  (let ((self ($make-hash-table)))
    (labels ((f (x)
               (when x
                 (set-value self (car x) (cadr x))
                 (f (cddr x)))))
      (f args))
    self))

(defun pir (self)
  (call self :pir))

(defun 東京ミュウミュウ-metamorphose! (self outer)
  (call self :sub outer))

(defun walk (self key &rest args)
  ($maphash (lambda (k v)
              (when ($hash-table-p v)
                (set-value self k (apply #'call v key args))))
            self)
  self)

(defun make-program (&rest args)
  (let ((self (apply #'make-object args)))
    (set-value self :sub
               (lambda (self outer)
                 (walk self :sub outer)))
    self))



(defun make-local-reference (var)
  (let ((self (make-program :var var)))
    (set-value self :sub
               (lambda (self outer)
                 (let ((var (get-value self :var)))
                   (unless ($member var
                                    (collect-vars
                                     (get-value outer :lambda-list)))
                     (set-lexical-var var outer))
                   self)))
    (set-value self :pir
               (lambda (self)
                 (let ((var (get-value self :var)))
                   (parrot-var var))))
    self))

(defun make-sub (&rest args)
  ;; name modifiers lambda-list lexical-store body outer inners
  (let ((self (apply #'make-program args)))
    (set-value self :toplevelp t)
    (set-value self :pir
               (lambda (self)
                 (let* ((*var-counter* 0)
                        (*label-counter* 0)
                        (name (get-value self :name))
                        (modifiers (join " " (get-value self :modifiers)))
                        (lambda-list (get-value self :lambda-list))
                        (arguments (collect-vars lambda-list))
                        (lexical-store (get-value self :lexical-store))
                        (outer (get-value self :outer)))
                   (prt-top ".sub "
                            (parrot-sub-name name)
                            (if outer
                                (string+ " :outer("
                                         (parrot-sub-name (get-value outer :name))
                                         ") ")
                                "")
                            modifiers)
                   (pir lambda-list)
                   (call lambda-list :pir-init-form)
                   (let ((lex-vars ($mapcar (lambda (var)
                                              (when (special-var-p var)
                                                (prt-push-dynamic var)
                                                var))
                                            arguments)))
                     ($mapcar (lambda (var)
                                (unless ($member var lex-vars)
                                  (let ((var (parrot-var var)))
                                    (prt ".lex '" var "', " var))))
                              lexical-store))
                   (prt ".return(" (pir (get-value self :body)) ")")
                   (prt-top ".end")
                   (new-line)
                   ($mapcar #'pir (get-value self :inners)))))
    self))

(defun make-parrot-code (form)
  (let ((self (make-program :form form)))
    (set-value self :pir
               (lambda (self)
                 (let ((form (get-value self :form))
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
                   var)))
    self))

(defun make-block (name type form)
  (let ((self (make-program :name name :type type :form form)))
    (set-value self :pir
               (lambda (self)
                 (let ((form (get-value self :form))
                       (handler (next-var))
                       (handler-label (next-label "BLOCK"))
                       (skip-label (next-label "BLOCK")))
                   (prt handler " = new 'ExceptionHandler'")
                   (prt "set_addr " handler ", " handler-label)
                   (prt handler ".'handle_types'(" (princ-to-string type) ")")
                   (prt "push_eh " handler)
                   (let ((ret (pir form))
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
                     ret))))
    self))

(defun make-return-from (name type result)
  (let ((self (make-program :name name :type type :result result)))
    (set-value self :pir
               (lambda (self)
                 (let ((type (get-value self :type))
                       (result (get-value self :result))
                       (ex-var (next-var))
                       (dummy-ret-var (next-var)))
                   (prt ex-var " = new 'Exception'")
                   (prt ex-var "['type'] = " (princ-to-string type))
                   (prt ex-var "['payload'] = " (pir result))
                   (prt "throw " ex-var)
                   dummy-ret-var)))
    self))

(defun make-tagbody (tags body)
  (let ((self (make-program :tags tags :body body)))
    (set-value self :sub
               (lambda (self outer)
                 (set-value self :body
                            ($mapcar (lambda (x)
                                       (東京ミュウミュウ-metamorphose! x outer))
                                     (get-value self :body)))
                 self))
    (set-value self :pir
               (lambda (self)
                 (let ((tags (get-value self :tags))
                       (body (get-value self :body)))
                   ($mapcar (lambda (x)
                              (let ((cont-var (parrot-cont x)))
                                (prt ".local pmc " cont-var)
                                (prt cont-var " = new 'Continuation'")
                                (prt "set_addr " cont-var ", " (parrot-tag x))
                                (prt ".lex '" cont-var "', " cont-var)))
                            tags)
                   ($mapcar #'pir body)
                   (pir (make-constant nil)))))
    self))

(defun make-tag (tag)
  (let ((self (make-program :tag tag)))
    (set-value self :pir
               (lambda (self)
                 (prt-label (parrot-tag (get-value self :tag)))))
    self))

(defun make-local-go (tag)
  (let ((self (make-program :tag tag)))
    (set-value self :pir
               (lambda (self)
                 (let ((tag (parrot-tag (get-value self :tag))))
                   (prt "goto " tag)
                   (next-var))))
    self))

(defun make-lexical-go (tag)
  (let ((self (make-program :tag tag)))
    (set-value self :pir
               (lambda (self)
                 (let ((var (parrot-cont (get-value self :tag))))
                   (prt ".local pmc " var)
                   (prt var " = find_lex '" var "'")
                   (prt var "(0)")
                   var)))
    self))

(defun make-reference (var)
  (make-program :var var))

(defun make-lexical-reference (var)
  (let ((self (make-reference var)))
    (set-value self :sub
               (lambda (self outer)
                 (let ((var (get-value self :var)))
                   (set-lexical-var var outer))
                 self))
    (set-value self :pir
               (lambda (self)
                 (let ((var (parrot-var (get-value self :var))))
                   (prt ".local pmc " var)
                   (prt var " = find_lex '" var "'")
                   var)))
    self))

(defun make-dynamic-reference (var)
  (let ((self (make-reference var)))
    (set-value self :pir
               (lambda (self)
                 (let ((var (get-value self :var))
                       (value (next-var)))
                   (prt value " = get_dynamic_scope_value('"
                        (parrot-var var)
                        "', utf8:unicode:"
                        (prin1-to-string ($package-name ($symbol-package var)))
                        ", utf8:unicode:"
                        (prin1-to-string ($symbol-name var))
                        ")")
                   value)))
    self))

(defun make-assignment (var form)
  (let ((self (make-program :var var :form form)))
    (set-value self :pir
               (lambda (self)
                 (let ((value (pir (get-value self :form))))
                   (prt (parrot-var (get-value self :var)) " = " value)
                   value)))
    self))

(defun make-local-assignment (var form)
  (make-assignment var form))

(defun make-lexical-assignment (var form)
  (let ((self (make-assignment var form)))
    (set-value self :pir
               (lambda (self)
                 (let ((var (parrot-var (get-value self :var)))
                       (value (pir (get-value self :form))))
                   (prt "store_lex '" var "', " value)
                   value)))
    self))

(defun make-dynamic-assignment (var form)
  (let ((self (make-assignment var form)))
    (set-value self :pir
               (lambda (self)
                 (let* ((var (get-value self :var))
                        (form (get-value self :form))
                        (value (pir form )))
                   (prt "set_dynamic_scope_value('"
                        (parrot-var var)
                        "', utf8:unicode:"
                        (prin1-to-string ($package-name ($symbol-package var)))
                        ", utf8:unicode:"
                        (prin1-to-string ($symbol-name var))
                        ", "
                        value
                        ")")
                   value)))
    self))

(defun make-constant (value)
  (let ((self (make-program :value value)))
    (set-value self :pir
               (lambda (self)
                 (pir-constant (get-value self :value))))
    self))

(defun make-if (test then else)
  (let ((self (make-program :test test :then then :else else)))
    (set-value self :pir
               (lambda (self)
                 (let ((test (get-value self :test))
                       (then (get-value self :then))
                       (else (get-value self :else))
                       (result (next-var))
                       (else-label (next-label "ELSE"))
                       (end-label (next-label "ENDIF")))
                   (prt "eq_addr " (prt-nil) ", " (pir test) ", " else-label)
                   (prt result " = " (pir then))
                   (prt "goto " end-label)
                   (prt-label else-label)
                   (prt result " = " (pir else))
                   (prt-label end-label)
                   result)))
    self))

(defun make-progn (first rest)
  (let ((self (make-program :first first :rest rest)))
    (set-value self :pir
               (lambda (self)
                 (let ((first (get-value self :first))
                       (rest (get-value self :rest)))
                   (pir first)
                   (pir rest))))
    self))

(defun make-lambda (name lambda-list body)
  (let ((self (make-program :name name :lambda-list lambda-list :body body)))
    (set-value self :sub
               (lambda (self outer)
                 (let* ((name (get-value self :name))
                        (lambda-list (get-value self :lambda-list))
                        (body (get-value self :body))
                        (sub (make-sub :name name
                                       :lambda-list lambda-list
                                       :outer outer)))
                   (add-value outer :inners sub)
                   (set-value sub :body (東京ミュウミュウ-metamorphose! body sub))
                   (make-extracted-lambda name))))
    self))

(defun make-flet (lambdas body)
  (let ((self (make-program :lambdas lambdas :body body)))
    (set-value self :sub
               (lambda (self outer)
                 (let ((lambdas (get-value self :lambdas))
                       (body  (get-value self :body)))
                   ($mapcar (lambda (x)
                              (東京ミュウミュウ-metamorphose! x outer))
                            lambdas)
                   (東京ミュウミュウ-metamorphose! body outer))))
    self))

(defun make-lexical-application (function args)
  (let ((self (make-program :function function :args args)))
    (set-value self :pir
               (lambda (self)
                 (let ((args (join ", " (pir (get-value self :args))))
                       (sub (next-var))
                       (return-value (next-var)))
                   (prt ".const 'Sub' " sub " = " (parrot-sub-name (get-value self :function)))
                   (prt sub " = newclosure " sub)
                   (prt return-value " = " sub "(" args ")")
                   return-value)))
    self))

(defun make-local-application (function args)
  (let ((self (make-program :function function :args args)))
    (set-value self :pir
               (lambda (self)
                 (let ((args (join ", " (pir (get-value self :args))))
                       (sub (parrot-sub-name (get-value self :function)))
                       (return-value (next-var)))
                   (prt return-value " = " sub "(" args ")")
                   return-value)))
    self))

(defun make-global-application (function args)
  (let ((self (make-program :function function :args args)))
    (set-value self :pir
               (lambda (self)
                 (let ((symbol (get-value self :function))
                       (args (join ", " (pir (get-value self :args))))
                       (fun-var (next-var))
                       (return-value (next-var)))
                   (prt fun-var
                        " = get_hll_global [ "
                        (prin1-to-string ($package-name ($symbol-package symbol)))
                        " ], binary:"
                        (prin1-to-string ($symbol-name symbol)))
                   (prt return-value "  = " fun-var "(" args ")")
                   return-value)))
    self))

(defun make-lambda-application (lambda args)
  (let ((self (make-program :lambda lambda :args args)))
    (set-value self :sub
               (lambda (self outer)
                 (let* ((lambda (get-value self :lambda))
                        (lambda-list (get-value lambda :lambda-list))
                        (body (get-value lambda :body))
                        (name ($gensym "lambda"))
                        (sub (make-sub :name name
                                       :lambda-list lambda-list
                                       :outer outer)))
                   (add-value outer :inners sub)
                   (set-value sub :body (東京ミュウミュウ-metamorphose! body sub))
                   (make-lexical-application name
                                             (東京ミュウミュウ-metamorphose!
                                              (get-value self :args) sub)))))
    self))


(defun make-arguments (first rest)
  (let ((self (make-program :first first :rest rest)))
    (set-value self :pir
               (lambda (self)
                 (let ((first (get-value self :first))
                       (rest  (get-value self :rest)))
                   (cons (pir first) (pir rest)))))
    self))

(defun make-no-argument ()
  (let ((self (make-program)))
    (set-value self :pir (lambda (self) self nil))
    self))

(defun make-change-package (name setq-form)
  (let ((self (make-program :name name :setq-form setq-form)))
    (set-value self :sub
               (lambda (self outer)
                 (let ((setq-form (get-value self :setq-form)))
                   (add-value outer :inners self)
                   (東京ミュウミュウ-metamorphose! setq-form outer))))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name)))
                   (prt-in-namespace ($package-name (eval name))))))
    self))

(defun make-eval-when (situations form raw-form)
  (let ((self (make-program :situations situations :form form
                            :raw-form raw-form)))
    (set-value self :toplevelp t)
    (set-value self :sub
               (lambda (self outer)
                 (let* ((situations (get-value self :situations))
                        (form (get-value self :form))
                        (raw-form (get-value self :raw-form))
                        (modifiers nil))
                   (if (and (not *in-eval*)
                            ($member :compile-toplevel situations))
                       (eval raw-form))
                   (if ($member :load-toplevel situations)
                       (setq modifiers (cons ":load" modifiers)))
                   (if ($member :execute situations)
                       (setq modifiers (cons ":init" modifiers)))
                   (if modifiers
                       (let ((sub (make-sub :name ($gensym "eval-when")
                                            :lambda-list (make-lambda-list-end)
                                            :outer outer
                                            :modifiers (cons ":anon" modifiers))))
                         (set-value sub :body (東京ミュウミュウ-metamorphose! form sub))
                         sub)
                       (make-constant nil)))))
    self))

(defun make-defvar (symbol form)
  (let ((self (make-program :symbol symbol :form form)))
    (set-value self :pir
               (lambda (self)
                 (let* ((symbol (get-value self :symbol))
                        (form   (get-value self :form))
                        (sym-var (prt-intern-symbol symbol)))
                   (prt sym-var ".'specialize'()")
                   (prt "setattribute "
                        sym-var
                        ", 'value', "
                        (pir form))
                   sym-var)))
    self))

(defun make-defun (name lambda-list body)
  (let ((self (make-program :name name :lambda-list lambda-list
                            :body body)))
    (set-value self :toplevelp t)
    (set-value self :sub
               (lambda (self outer)
                 (let* ((name (get-value self :name))
                        (lambda-list (get-value self :lambda-list))
                        (body (get-value self :body)))
                   (if outer
                       (let* ((closure-name ($gensym ($symbol-name name)))
                              (closure (make-sub :name closure-name
                                                 :lambda-list (make-lambda-list-end)
                                                 :outer outer))
                              (let-defun (make-let-defun name)))
                         (set-value closure :lambda-list
                                    (東京ミュウミュウ-metamorphose! lambda-list closure))
                         (set-value closure :body (東京ミュウミュウ-metamorphose! body closure))
                         (add-value outer :inners closure)
                         (add-value outer :inners let-defun)
                         (make-set-symbol-function name closure-name))
                       (let ((sub (make-sub :name name
                                            :lambda-list lambda-list
                                            :outr outer)))
                         (set-value sub :lambda-list
                                    (東京ミュウミュウ-metamorphose! lambda-list sub))
                         (set-value sub :body
                                    (東京ミュウミュウ-metamorphose! body sub))
                         sub)))))
    self))

(defun make-let-defun (name)
  (let ((self (make-program :name name)))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name)))
                   (prt-top ".sub " (parrot-sub-name name))
                   (prt ".param pmc x :slurpy")
                   (let ((f (next-var)))
                     (prt f
                          " = getattribute "
                          (prt-intern-symbol name)
                          ", 'function'")
                     (prt ".tailcall " f "(x :flat)"))
                   (prt-top ".end")
                   (new-line))))
    self))

(defun make-set-symbol-function (name closure-name)
  (let ((self (make-program :name name :closure-name closure-name)))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name))
                       (closure-name (get-value self :closure-name))
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
                   closure-var)))
    self))

(defun make-extracted-let (name values)
  (let ((self (make-program :name name :values values)))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name))
                       (values (get-value self :values))
                       (sub (next-var))
                       (result (next-var)))
                   (prt ".const 'Sub' " sub " = " (parrot-sub-name name))
                   (prt sub " = newclosure " sub)
                   (prt result " = " sub
                        "(" (join "," (funcall values :pir)) ")")
                   result)))
    self))

(defun make-extracted-lambda (name)
  (let ((self (make-program :name name)))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name))
                       (var (next-var)))
                   (prt ".const 'Sub' " var " = " (parrot-sub-name name))
                   ;; TODO newclosure の気持ちがわからない
                   (prt var " = newclosure " var)
                   var)))
    self))

(defun make-global-function-reference (name)
  (let ((self (make-program :name name)))
    (set-value self :pir
               (lambda (self)
                 (let ((name (get-value self :name))
                       (var (next-var)))
                   (prt var
                        " = get_hll_global [ "
                        (prin1-to-string ($package-name ($symbol-package name)))
                        " ], binary:"
                        (prin1-to-string ($symbol-name name)))
                   var)))
    self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-vars (vars)
  (if (null vars)
      nil
      (cons (cons (car vars) (cadr vars))
            (make-vars (cddr vars)))))

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

(defun set-lexical-var (var outer)
  (if outer
      (if ($member var (collect-vars
                        (get-value outer :lambda-list)))
          (let ((lexical-store (get-value outer :lexical-store)))
            (unless ($member var lexical-store)
              (add-value outer :lexical-store var)))
          (set-lexical-var var (get-value outer :outer)))))

(defun collect-vars (lambda-list)
  (call lambda-list :collect-vars))

(defun list-to-arguments (list)
  (if list
      (make-arguments (car list) (list-to-arguments (cdr list)))
      (make-no-argument)))

(defun suplied-p-var (var svar)
  (if svar
      (parrot-var svar)
      (string+ (parrot-var var) "_P")))

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
        (if (get-value object :toplevelp)
            (setq object (東京ミュウミュウ-metamorphose! object nil))
            (let ((sub (make-sub :name ($gensym "toplevel")
                                 :lambda-list (make-lambda-list-end)
                                 :modifiers '(":anon" ":init" ":load"))))
              (setq object (東京ミュウミュウ-metamorphose! object sub))
              (set-value sub :body object)
              (setq object sub)))
        (pir object))
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
