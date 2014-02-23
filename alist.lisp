(in-package :peldan.util.alist)


(defun assoc-value
    (item alist)
  (cdr (assoc item alist)))


(defun nested-assoc
    (items alist)
  (let ((r (assoc (car items) alist)))
    (if (null (cdr items))
	r
	(nested-assoc (cdr items) (cdr r)))))


(defun nested-assoc-value
    (items alist)
  (cdr (nested-assoc items alist)))


(defmacro with-assocs (alist assocs &body forms)
  (let ((alist-sym (gensym)))
    (labels ((mk-form
		 (assoc)
	       (let ((kw (symbol-to-keyword assoc)))
		 `(,assoc (assoc-value ,kw ,alist-sym)))))

      `(let ((,alist-sym ,alist))
	 (let ,(mapcar #'mk-form assocs)
	   ,@forms)))))

#|
(macroexpand-1 (with-assocs (pairlis (list :one :two :three)
		       (list 1 2 3)) (three two) (+ three two three)))

(let ((mylist (pairlis (list :one :two :three)
		       (list 1 2 3))))
  (with-assocs mylist (two one)
    (+ one two)))
|#
