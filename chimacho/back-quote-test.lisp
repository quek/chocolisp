(in-package :chimacho)

(defmacro bq (form) (back-quote form))

(assert (equal 1 (bq 1)))
(assert (equal '(a) (bq (a))))
(assert (equal '(a b) (bq (a b))))
(assert (equal '(1 b) (let ((a 1)) (bq ((comma a) b)))))
(assert (equal '(a 2) (let ((b 2)) (bq (a (comma b))))))
(assert (equal '(1 (2 3)) (let ((a '(2 3))) (bq (1 (comma a))))))
(assert (equal '(1 (2 3) 4) (let ((a '(2 3))) (bq (1 (comma a) 4)))))
(assert (equal '(1 2 3) (let ((a '(2 3))) (bq (1 (comma-at a))))))
(assert (equal '(1 2 3 4) (let ((a '(2 3))) (bq (1 (comma-at a) 4)))))
