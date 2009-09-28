(in-package :chimacho)

(defmacro defmacro (name lambda-list &rest form)
  (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
    (list '$set-attribute name "macro-function"
          (list 'lambda lambda-list
                (cons 'progn form)))))

(defmacro lambda (&rest form)
  (cons 'function form))

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
      (list 'eval-when '(:compile-toplevel :load-toplevel :execute)
            (list '$set-attribute (list 'quote (cadr form)) "macro-function"
                  (list 'lambda (caddr form)
                        (cons 'progn (cdddr form)))))
      (funcall (macro-function (car form)) form)))

;; 完全にダミー
(defun $eval (form)
  form)


(in-package :common-lisp)

(defun list (&rest list)
  list)

(defun acons (key datum alist)
  (cons (cons key datum) alist))
