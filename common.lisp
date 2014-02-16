(in-package :peldan.util.common)

(defun dec (num)
  (- num 1))



(defmacro when-let (defs &body forms)
  (let ((last-let (car (car (last defs)))))
    `(let ,defs
       (when ,last-let
	 ,@forms))))
