(defpackage :seq-utils
  (:use :common-lisp
	:it.bese.FiveAM
	:fun-utils)
  (:export alist
	   take
	   group
	   assoc-value))

(in-package :seq-utils)

(defun take
    (n seq)
  (labels ((rec (n seq acc)
	     (if (or (null seq) (zerop n))
		 (values acc seq)
		 (rec (- n 1)
		      (cdr seq)
		      (cons (car seq) acc)))))

    (multiple-value-bind (v r)
	(rec n seq nil)
      (values (nreverse v) r))))


(defun group
    (n seq)
  (when (zerop n) (error 'zero-arg))
  (labels ((rec (n seq acc)
	     (if (null seq)
		 acc
		 (multiple-value-bind (g r)
		     (take n seq)
		   (rec n r (cons g acc))))))
    (nreverse (rec n seq nil))))


(defun aadd
    (pair alist)
  (acons (car pair)
	 (cdr pair)
	 alist))


(defun ->cons
    (two-list)
  (cons (nth 0 two-list)
	(nth 1 two-list)))


(defun assoc-value
    (&rest args)
  (apply (compose #'cdr #'assoc) args))

(defun alist
    (&rest args)
  (let ((pairs (group 2 args)))

    (pairlis (mapcar (partial #'nth 0) pairs)
	     (mapcar (partial #'nth 1) pairs))))


(test group
  "Test the group function"
  (is (equal '((1 2) (3 4) (5)) (group 2 '(1 2 3 4 5)))))

(explain! (run 'group))
