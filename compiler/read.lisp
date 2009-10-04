(in-package :chimacho)

(defun $read (in)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (if c
        (cond ((char= #\' c)
               ($read-char in)
               (list 'quote ($read in)))
              ((char= #\# c)
               ($read-char in)
               (read-dispatch in))
              ((char= #\` c)
               ($read-char in)
               (read-back-quote in))
              ((char= #\, c)
               ($read-char in)
               (if (char= #\@ ($peek-char in))
                   (progn ($read-char in)
                          (read-comma-at in))
                   (read-comma in)))
              ((char= #\( c)
                (read-list in))
              ((char= #\; c)
               ($read-line in)
               ($read in))
              (t
               (read-atom in))))))

(defun read-dispatch (in)
  (let ((c ($read-char in)))
    (cond ((char= #\' c)
           (list 'function ($read in)))
          ((char= #\\ c)
           ($read-char in))
          (t
           ($error (string+ "#" c " is unknown read dispatch macro."))))))

(defun skip-whitespace (in)
  (let ((c ($peek-char in)))
    (if (and c (char<= c #\ ))
        (progn ($read-char in)
               (skip-whitespace in)))))

(defun read-list (in)
  ($read-char in)                        ; skip (
  (%read-list in nil))

(defun %read-list (in acc)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (cond ((char= #\; c)
           ($read-line in)
           (%read-list in acc))
          ((char= #\) c)
           ($read-char in)
           ($reverse acc))
          (t
           (%read-list in (cons ($read in) acc))))))

(defun read-atom (in)
  (skip-whitespace in)
  (let ((c ($peek-char in)))
    (if (char= #\" c)
        (progn ($read-char in)
               (read-string in ""))
        (if (or (char< c #\0)
                (char< #\9 c))
            (read-symbol in "" "" nil t)
            (read-number in (string+ ($read-char in)))))))

(defun read-string (in acc)
  (let ((c ($read-char in)))
    (if (char= #\" c)
        acc
        (if (char= #\\ c)
            (read-string in (string+ acc ($read-char in)))
            (read-string in (string+ acc c))))))

(defun read-number (in acc)
  (let ((c ($peek-char in)))
    (if (or (null c)
            (char<= c #\ )
            (char= #\( c)
            (char= #\) c)
            (char= #\; c))
        (string-to-number acc)
        (read-number in (string+ acc ($read-char in))))))

(defun read-symbol (in acc1 acc2 packagep exportp)
  (let ((c ($peek-char in)))
    (if (or (null c)
            (char<= c #\ )
            (char= #\( c)
            (char= #\) c)
            (char= #\; c))
        (string-to-symbol acc1 acc2 packagep exportp)
        (let ((c ($read-char in)))
          (if (char= #\: c)
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
