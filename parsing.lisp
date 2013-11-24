(defpackage parsing

  (:use :common-lisp
	:fun-utils
	:seq-utils
	:parse-head)

  (:export :defparser
	   :defparsefunc
	   :run-clause
	   :with-parsing-environment
	   :head)

  (:shadow :head))


(in-package parsing)


(defun get-parser-function
    (entity)
  (let ((name (symbol-name entity)))
    (intern (concatenate 'string "PARSE-" name))))


(defun run-parser
    (head name &rest args)
  (apply (get-parser-function name) head args))


(defun run-clause
    (head clause)
  (cond ((consp clause)
	 (apply #'run-parser head clause))

	((symbolp clause)
	 (run-parser head clause))

	((null clause)
	 nil)

	(t
	 (error 'parse-error))))

(defmacro with-parsing-environment
    (var &body body)
  (let ((real-head (gensym))
	(res (gensym)))

    `(let* ((,real-head ,var)
	    (head (copy-list ,real-head))
	    (,res (progn ,@body)))

       (set-pos ,real-head
		(get-pos head))
       ,res)))


(defmacro defparsefunc
    (name args &body body)
  (let ((real-head (gensym)))

    `(defun ,name
	 (,real-head ,@args)
       (with-parsing-environment ,real-head
	 ,@body))))


(defun clause->alist
    (clause)
  (let ((pairs (cdr clause)))
    (apply #'alist pairs)))


(defmacro defparser
    (name &body body)
  (let ((parser-name (get-parser-function name))
	(clauses (mapcar #'car body))
	;(alists (mapcar #'clause->alist body))
	)
    `(defparsefunc ,parser-name
	 ()
       (mapcar (partial (applied #'run-clause) head)
	       (quote ,clauses)))))


(defparser move
    (piece :as :piece)
    ((maybe takes) :as :takes)
    ((maybe square) :as :source)
    (square :as :destination))

(macroexpand  (macroexpand-1 (macroexpand-1 (macroexpand-1 '(defparser move
							     (piece :as :piece)
							     ((maybe takes) :as :takes)
							     ((maybe square) :as :source)
							     (square :as :destination))))))
