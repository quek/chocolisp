(in-package :chimacho)

(defmacro defmacro (name lambda-list &rest form)
  ;;(list 'eval-when '(:compile-toplevel :load-toplevel :execute)
  (list 'eval-when '(:load-toplevel :execute)
    (list '$set-attribute name "macro-function"
          (list 'lambda lambda-list
                (cons 'progn form)))))

(defmacro lambda (&rest form)
  (list 'function (cons 'lambda form)))

(defmacro in-package (name)
  (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
        (list 'setq '*package* (list 'find-package name))))

(defun $symbol-package (symbol)
  ($get-attribute symbol "package"))

(defun $symbol-name (symbol)
  ($get-attribute symbol "name"))

(defun $package-name (package)
  ($get-attribute package "name"))

(let ((counter 0))
  (defun $gensym (prefix)
    (setq counter (+ 1 counter))
    (make-symbol (string+ prefix counter))))

(defun $rplaca (cons x)
  ($set-attribute cons "car" x))

(defun $rplacd (cons x)
  ($set-attribute cons "cdr" x))

(defun $macroexpand (form)
  (if (eq (car form) 'defmacro)
      ;;(list 'eval-when '(:compile-toplevel :load-toplevel :execute)
      (list 'eval-when '(:load-toplevel :execute)
            (list '$set-attribute (list 'quote (cadr form)) "macro-function"
                  (cons 'lambda (cddr form))))
      (apply (macro-function (car form)) (cdr form))))


(in-package :common-lisp)

(defun list (&rest list)
  list)

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun %append2 (x y)
  (if x
      (cons (car x) (%append2 (cdr x) y))
      y))

(defun %append (first rest)
  (if rest
      (%append2 first (%append (car rest) (cdr rest)))
      first))

(defun append (&rest lists)
  (%append (car lists) (cdr lists)))

(defun not (x)
  (if x nil t))

;; 完全にダミー
(defun eval (form)
  (print "eval =>")
  (print form)
  (let* ((chimacho::*in-eval* t)
         (chimacho::*compile-toplevel* nil)
         (*package* *package*)
         (*pir-stream* (chimacho::$make-string-output-stream))
         (object (chimacho::objectify form nil nil)))
    (chimacho::prt-in-namespace (chimacho::$package-name *package*))
    (if (funcall object :toplevelp)
        (setq object (funcall object :東京ミュウミュウ-metamorphose! nil))
        (let ((flat-function (chimacho::make-flat-function
                              ($gensym "toplevel")
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
    (funcall object :pir)
    (chimacho::pir-eval (chimacho::$get-output-stream-string *pir-stream*))))
