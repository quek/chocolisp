(in-package :chimacho)

(defun read-back-quote (in)
  (list 'back-quote (list 'quote ($read in))))

(defun read-comma (in)
  (list 'comma (list 'quote ($read in))))

(defun read-comma-at (in)
  (list 'comma-at (list 'quote ($read in))))

(defun back-quote (form)
  (cond ((atom form)
         (list 'quote form))
        ((eq 'comma (car form))
         (cadr form))
        ((eq 'comma-at (car form))
         ($error "invalid ,@"))
        (t (%back-quote form 0))))

(defun %back-quote (form level)
  (if form
      (cond ((atom (car form))
             (list 'cons (list 'quote (car form))
                   (%back-quote (cdr form) level)))
            ((eq 'comma (caar form))
             (list 'cons (cadar form)
                   (%back-quote (cdr form) level)))
            ((eq 'comma-at (caar form))
             (list 'append (cadar form)
                   (%back-quote (cdr form) level)))
            (t
             (list 'cons (%back-quote (car form) level)
                   (%back-quote (cdr form) level))))))
