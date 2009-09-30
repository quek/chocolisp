(in-package :chimacho)

(defmacro cond (&rest clouses)
  (if clouses
      `(if ,(caar clouses)
           (progn ,@(cdar clauses))
           (cond ,@(cdr clouses)))))

(defmacro case (keyform &rest cases)
  (let ((keysym ($gensym "KEY")))
    `(let ((,keysym ,keyform))
       (cond ,@($mapcar (lambda (x)
                          (if (eq t (car x))
                              `(t ,@(cdr x))
                              (if (atom (car x))
                                  `((eql ,keysym ',(car x))
                                    ,@(cdr x))
                                  `((or ,@($mapcar (lambda (x)
                                                     `(eql ,keysym ',x))
                                                   (car x)))
                                    ,@(cdr x)))))
                cases)))))
