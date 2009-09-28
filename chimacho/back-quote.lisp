(in-package :chimacho)

(defun read-back-quote (in)
  (list 'back-quote (list 'quote ($read in))))

(defun read-comma (in)
  (list 'comma (list 'quote ($read in))))

(defun read-comma-at (in)
  (list 'comma-at (list 'quote ($read in))))

(defun back-quote (form)
  (%back-quote form 0 (lambda (x) x)))

(defun %back-quote (form level c)
  (if form
      (if (atom form)
          (funcall c (list 'quote form))
          (if (atom (car form))
              (%back-quote (cdr form) level
                           (lambda (x)
                             (funcall c
                                      (list 'cons (list 'quote (car form))
                                            x))))
              (%back-quote (cdr form) level
                           (lambda (x)
                             (funcall c
                                      (list 'append
                                            (%%back-quote (car form) level
                                                          (lambda (x)
                                                            x))
                                            x))))))
      (funcall c nil)))

(defun %%back-quote (form level c)
  (let ((car (car form)))
    (cond ((eq 'back-quote car)
           (%back-quote (cadr form) (+ 1 level) c))
          ((eq 'comma car)
           (if (= 0 level)
               (funcall c (list 'list (cadr form)))
               (funcall c (list 'list (list 'quote (cadr form))))))
          ((eq 'comma-at car)
           (if (= 0 level)
               (funcall c (cadr form))
               (funcall c (list 'quote (cdr form)))))
          (t
           (%back-quote (cdr form) level
                        (lambda (x)
                          (funcall c (list (list 'quote car) x))))))))
