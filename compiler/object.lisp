(defun make-vars (vars)
  (if (null vars)
      nil
      (cons (cons (car vars) (cadr vars))
            (make-var (cddr vars)))))

(defun walk (self vars message &rest args)
  (if (null vars)
      self
      (let ((key (caar vars))
            (val (cdar vars)))
        (when (functionp val)
          (funcall self :set key (apply val message args)))
        (walk self (cdr vars) args))))

(defun make-object (&rest vars)
  (let (self
        (vars (make-vars vars)))
    (setq self (lambda (message &rest args)
                 (case message
                   (:toplevelp
                      nil)
                   (:all-vars
                      vars)
                   (:get
                      (cdr (assoc (car args) vars)))
                   (:set
                      (let ((cons (assoc (car args) vars)))
                        (if cons
                            (rplacd cons (cadr args))))))))))

(defun make-program (&rest vars)
  (let ((super (apply #'make-object vars))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (:東京ミュミュ-metamorphose!
                      (walk self vars message args)
                      self)
                   (t
                      (apply super message args)))))))

(defun make-reference (var)
  (let ((super (make-program :var var))
        self)
    (setq self
          (lambda (message &rest args)
            (case message
              (:東京ミュミュ-metamorphose!
                 (if (member var
                             (funcall (car args) :get :arguments))
                     self
                     (progn
                       (make-lexical-var var))))
              (t (apply super message args)))))))

(defun make-lexical-var (var)
  (let ((super (make-program :var var))
        self)
    (setq self (lambda (message &rest args)
                 (case message
                   (t (apply super message args)))))))

(let ((x (make-program :name "foo" :args '(x y))))
  (list
   (funcall x :get :args)
   (funcall x :set :args '(bar foo))
   (funcall x :get :args nil)))
