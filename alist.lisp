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
