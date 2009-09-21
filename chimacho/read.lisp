(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *package* (find-package "CHIMACHO")))

(defun skip-whitespace (in)
  (let ((c (peek-char in)))
    (if (string< c " ")
        (progn (read-char in)
               (skip-whitespace in)))))

(defun read (in)
  (skip-whitespace in)
  (let ((c (peek-char in)))
    (if c
        (if (string= "(" c)
            (read-list in)
            (if (string= ";")
                (progn
                  (read-line in)
                  (read in))
                (read-atom))))))

(defun read-list (in)
  (read-char in)                        ; skip ;
  (%read-list in nil))

(defun %read-list (in acc)
  (skip-whitespace in)
  (let ((c (peek-char in)))
    (if (string= c ")")
        (reverse acc)
        (if (string= c "(")
            (%read-list in (cons (read-list in) acc))
            (if (string= c ";")
                (progn (read-line in)
                       (%read-list in acc))
                (%read-list in (cons (read-atom in) acc)))))))

(defun read-atom (in)
  (skip-whitespace in)
  (let ((c (peek-char in)))
    (if (string= c "\"")
        (read-string in)
        (read-number in))))

(defun read-string (in)
  (read-char in)                        ; skip "
  (%read-string in ""))

(defun %read-string (in acc)
  (let ((c (read-char in)))
    (if (string= c "\"")
        acc
        (if (string= c "\\")
            (%read-string in (string+ acc (read-char in)))
            (%read-string in (string+ acc c))))))

(defun read-atom (in)
  (let ((c (read-char in)))
    (if (string<= "0" c)
        (if (string<= c "9")
            (read-number in c)
            (read-symbol in c "" nil t))
        (read-symbol in c "" nil t))))

(defun read-number (in acc)
  (let ((c (read-char in)))
    (if c
        (if (string<= c " ")
            (string-to-number acc)
            (read-number in (string+ acc c)))
        (string-to-number acc))))

(defun read-symbol (in acc1 acc2 packagep exportp)
  (let ((c (read-char in)))
    (if c
        (if (string<= c " ")
            (string-to-symbol acc1 acc2 packagep exportp)
            (if (string= c ":")
                (if packagep
                    (read-symbol in acc1 acc2 packagep nil)
                    (read-symbol in acc1 acc2 t exportp))
                (if packagep
                    (read-symbol in acc1 (string+ acc2 c) packagep exportp)
                    (read-symbol in (string+ acc1 c) acc2 packagep exportp))))
        (string-to-symbol acc1 acc2 packagep exportp))))

(defun string-to-symbol (acc1 acc2 packagep exportp)
  (if packagep
      (if exportp
          (find-export-symbol acc1 acc2)
          (intern acc2 acc1))
      (intern acc1 *packgae*)))
