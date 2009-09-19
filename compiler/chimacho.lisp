(chimacho::in-package "CHIMACHO")

(defmacro unless (test form)
  (list 'if test (cons 'progn form)))

