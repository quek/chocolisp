(in-package :chimacho)

(defvar *standard-input*  (parrot-code "getstdin"))
(defvar *standard-output* (parrot-code "getstdout"))
(defvar *error-output*    (parrot-code "getstderr"))
