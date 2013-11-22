(defpackage :fun-utils
  (:use common-lisp)
  (:export partial))

(in-package :fun-utils)

(defun partial
    (f &rest partial-args)
  (lambda (&rest rest)
    (apply f (append partial-args rest))))


(defun compose
    (&rest funcs)
  (let ((f (car funcs))
	(gs (cdr funcs)))
    (if (null gs)
	f
	(lambda (&rest args)
	  (let ((g (apply #'compose gs)))
	    (funcall f (apply g args)))))))

(let ((f (partial #'+ 3)))
  (funcall f 4))

(let ((f (partial #'* 3 4)))
  (funcall f 5))

(let ((f (compose (partial #'+ 3) (partial #'* 4))))
  (funcall f 5))
