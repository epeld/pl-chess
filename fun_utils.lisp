(defpackage :fun-utils
  (:use common-lisp)
  (:export partial))

(in-package :fun-utils)

(defun partial
    (f &rest partial-args)
  (lambda (&rest rest)
    (apply f (append partial-args rest))))
