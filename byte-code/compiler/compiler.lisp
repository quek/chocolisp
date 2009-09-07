#|
(in-package "a")
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (SETQ *PACKAGE* (SB-INT:FIND-UNDELETED-PACKAGE-OR-LOSE "a")))
とかはどうするんだ？
.namespace ["a"]
でいいのか。

トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。

(let ((x (defun f () 'foo))))
とかの場合は .const 'Sub' x = 'f' すればいいかな。
|#
(defun compile-toplevel (form)
  (if (atom form)
      (progn
        "トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。")
      (case (car form)
        (defun
            (compile-defun (cadr form) (cddr form)))
        (t
           "トップレベルでいろいろ問題ありそう。
全体を .sub .end で囲んで、中の defun 等を外出しにすればいいかも。"
           ()))))

(defun compile-form (form)
  (if (atom form)
      (if (symbolp form)
          (compile-reference form)
          (compile-quote form))
      (case (car form)
        (defun
            (compile-defun (cadr form) (cddr form)))
        (t))))

(defun compile-reference (variable)
  variable)

(defun compile-quote (value)
  value)

(defun compile-defun (name body)
  (format t ".sub '~a'~%" name)
  (compile-form body)
  (format t ".end~%"))

