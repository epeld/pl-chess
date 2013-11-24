(defpackage control
  (:use :common-lisp)

  (:export :with-return
	   :ignore-conditions))

(in-package :control)

(defmacro with-return
    (var &body body)
  (let ((tag (gensym))
	(res (gensym)))
    `(labels ((,var (,res) (throw (quote ,tag) ,res)))
       (catch (quote ,tag)
	 ,@body))))

(defmacro ignore-conditions
    (conditions &body body)
  (labels ((empty-case (condition) (list condition () nil)))
    `(handler-case (progn ,@body)
      ,@(mapcar #'empty-case conditions))))
