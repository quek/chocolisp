(in-package :chimacho)

(defmacro when (test &rest body)
  `(if ,test
       (progn ,@body)))

(defmacro cond (&rest clouses)
  (if clouses
      `(if ,(caar clouses)
           (progn ,@(cdar clouses))
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

(defmacro typecase (keyform &rest cases)
  (let ((keysym ($gensym "KEY")))
    `(let ((,keysym ,keyform))
       (cond ,@($mapcar (lambda (x)
                          (if (eq t (car x))
                              `(t ,@(cdr x))
                              (if (atom (car x))
                                  `((typep ,keysym ',(car x))
                                    ,@(cdr x))
                                  `((or ,@($mapcar (lambda (x)
                                                     `(typep ,keysym ',x))
                                                   (car x)))
                                    ,@(cdr x)))))
                cases)))))

(defmacro loop (&rest form)
  (let ((tag ($gensym "LOOP")))
    `(block nil
      (tagbody
         ,tag
         (progn ,@form)
         (go ,tag)))))
