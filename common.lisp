(in-package :peldan.util.common)

(defun dec (num)
  (- num 1))



(defmacro when-let (defs &body forms)
  (let ((last-let (car (car (last defs)))))
    `(let ,defs
       (when ,last-let
	 ,@forms))))


(defmacro thread-first (&rest forms)
  (let ((first (car forms))
	(second (car (nthcdr 1 forms)))
	(remainder (nthcdr 2 forms)))
    (let ((joined (insert first second 1)))
      (if (null remainder)
	  joined
	  `(thread-first ,joined ,@remainder)))))


(defmacro thread-last (&rest forms)
  (let ((first (car forms))
	(second (car (nthcdr 1 forms)))
	(remainder (nthcdr 2 forms)))
    (let ((joined (append second (list first))))
      (if (null remainder)
	  joined
	  `(thread-last ,joined ,@remainder)))))


(thread-last 3 (+ 4) (* (+ 2 1)))


(defun make-keyword (string)
  (values (intern (string-upcase string) "KEYWORD")))


(defun symbol-to-keyword (symbol)
  (make-keyword (symbol-name symbol)))
