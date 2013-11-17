(defpackage seq-utils
  (:use :common-lisp :it.bese.FiveAM))

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

(group 2 (coerce "hej jag heter erik" 'list))
