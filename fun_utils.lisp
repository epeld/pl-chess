(defpackage :fun-utils
  (:use common-lisp)
  (:export partial
	   compose
	   applied))

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


(defun applied
    (f)
  (lambda
      (&args args)
    (apply f args)))


(defun flip-args
    (f)
  (lambda (a b)
    (funcall f b a)))

(let ((f (partial #'+ 3)))
  (funcall f 4))

(let ((f (partial #'* 3 4)))
  (funcall f 5))

(let ((f (compose (partial #'+ 3) (partial #'* 4))))
  (funcall f 5))
