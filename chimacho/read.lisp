(in-package :chimacho)

(defun $read (in)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (if c
        (cond ((string= "'" c)
               ($read-char in)
               (cons 'quote ($read in)))
              ((string= "`" c)
               ($read-char in)
               (read-back-quote in))
              ((string= "," c)
               ($read-char in)
               (if (string= "@" ($peek-char in))
                   (progn ($read-char in)
                          (read-comma-at in))
                   (read-comma in)))
              ((string= "(" c)
                (read-list in))
              ((string= c ";")
               ($read-line in)
               ($read in))
              (t
               (read-atom in))))))

(defun skip-whitespace (in)
  (let ((c ($peek-char in)))
    (if (string<= c " ")
        (progn ($read-char in)
               (skip-whitespace in)))))

(defun read-list (in)
  ($read-char in)                        ; skip (
  (%read-list in nil))

(defun %read-list (in acc)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (cond ((string= c ";")
           ($read-line in)
           (%read-list in acc))
          ((string= c ")")
           ($read-char in)
           ($reverse acc))
          (t
           (%read-list in (cons ($read in) acc))))))

(defun read-atom (in)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (if (string= c "\"")
        (progn ($read-char in)
               (read-string in ""))
        (if (or (string< c "0")
                (string< "9" c))
            (read-symbol in "" "" nil t)
            (read-number in ($read-char in))))))

(defun read-string (in acc)
  (let ((c ($read-char in)))
    (if (string= c "\"")
        acc
        (if (string= c "\\")
            (read-string in (string+ acc ($read-char in)))
            (read-string in (string+ acc c))))))

(defun read-number (in acc)
  (let ((c ($peek-char in)))
    (if (or (null c)
            (string<= c " ")
            (string= c "(")
            (string= c ")")
            (string= c ";"))
        (string-to-number acc)
        (read-number in (string+ acc ($read-char in))))))

(defun read-symbol (in acc1 acc2 packagep exportp)
  (let ((c ($peek-char in)))
    (if (or (null c)
            (string<= c " ")
            (string= c "(")
            (string= c ")")
            (string= c ";"))
        (string-to-symbol acc1 acc2 packagep exportp)
        (let ((c ($read-char in)))
          (if (string= c ":")
              (if packagep
                  (read-symbol in acc1 acc2 packagep nil)
                  (read-symbol in acc1 acc2 t exportp))
              (if packagep
                  (read-symbol in acc1 (string+ acc2 c) packagep exportp)
                  (read-symbol in (string+ acc1 c) acc2 packagep exportp)))))))

(defun string-to-symbol (acc1 acc2 packagep exportp)
  (if packagep
      (if (string= acc1 "")
          ($intern acc2 (find-package "KEYWORD"))
          (if exportp
              ($find-export-symbol acc1 acc2)
              ($intern acc2 (find-package acc1))))
      ($intern acc1 *package*)))

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

;;(assert (equal 1 (back-quote 1)))
;;(assert (equal '(a) (back-quote (a))))
;;(assert (equal '(a b) (back-quote (a b))))
;;(assert (equal '(1 b) (let ((a 1)) (back-quote ((comma a) b)))))
;;(assert (equal '(a 2) (let ((b 2)) (back-quote (a (comma b))))))
;;(assert (equal '(1 (2 3)) (let ((a '(2 3))) (back-quote (1 (comma a))))))
;;(assert (equal '(1 (2 3) 4) (let ((a '(2 3))) (back-quote (1 (comma a) 4)))))
;;(assert (equal '(1 2 3) (let ((a '(2 3))) (back-quote (1 (comma-at a))))))
;;(assert (equal '(1 2 3 4) (let ((a '(2 3))) (back-quote (1 (comma-at a) 4)))))
