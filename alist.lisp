(in-package :peldan.util.alist)


(defun assoc-value
    (item alist)
  (cdr (assoc item alist)))
