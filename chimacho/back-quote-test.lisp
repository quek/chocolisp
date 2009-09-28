(in-package :chimacho)

(defmacro bq (form) (back-quote form))

(progn
(assert (equal 1 (bq 1)))
(assert (equal '(a) (bq (a))))
(assert (equal '(a b) (bq (a b))))
(assert (equal '(1 b) (let ((a 1)) (bq ((comma a) b)))))
(assert (equal '(a 2) (let ((b 2)) (bq (a (comma b))))))
(assert (equal '(1 (2 3)) (let ((a '(2 3))) (bq (1 (comma a))))))
(assert (equal '(1 (2 3) 4) (let ((a '(2 3))) (bq (1 (comma a) 4)))))
(assert (equal '(1 2 3) (let ((a '(2 3))) (bq (1 (comma-at a))))))
(assert (equal '(1 2 3 4) (let ((a '(2 3))) (bq (1 (comma-at a) 4)))))
(assert (equal '((1)) (bq ((1)))))
(assert (equal '((1)) (let ((a 1)) (bq (((comma a)))))))
)

(let ((a 1)
      (x 'y))
  (bq (let (((comma x) (comma a)))
        (comma x))))

(CONS 'LET (APPEND ('(COMMA X) (APPEND (LIST A) NIL)) (APPEND (LIST X) NIL)))

(CASE MESSAGE (GET X) (SET (SETQ X (CAR ARGS)))) 
(CONS (QUOTE LET) (APPEND ((QUOTE ((COMMA (QUOTE KEYSYM)) (COMMA (QUOTE KEYFORM)))) NIL) (APPEND ((QUOTE COND) (APPEND (QUOTE ($MAPCAR (LAMBDA (X) (IF (EQ T (CAR X)) (BACK-QUOTE (QUOTE (T (COMMA-AT (QUOTE (CDR X)))))) (IF (ATOM (CAR X)) (BACK-QUOTE (QUOTE ((EQL (COMMA (QUOTE KEYSYM)) (QUOTE COMMA (QUOTE (CAR X)))) (COMMA-AT (QUOTE (CDR X)))))) (BACK-QUOTE (QUOTE ((OR (COMMA-AT (QUOTE ($MAPCAR (LAMBDA (X) (BACK-QUOTE (QUOTE (EQL (COMMA (QUOTE KEYSYM)) (QUOTE COMMA (QUOTE X)))))) (CAR X))))) (COMMA-AT (QUOTE (CDR X))))))))) CASES)) NIL)) NIL))) 
