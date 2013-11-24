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

	((null clause)
	 nil)

	((symbolp clause)
	 (run-parser head clause))

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


(defun clause->name
    (clause)
  (let ((alist (clause->alist clause)))
    (cdr (assoc :as alist))))


(defmacro defparser
    (name &body body)
  (let ((parser-name (get-parser-function name))
	(clauses (mapcar #'car body))
	(names (mapcar #'clause->name body))
	(run (gensym)))
    `(defparsefunc ,parser-name
	 ()
       (let ((,run (partial #'run-clause head)))
	 (pairlis (quote ,names) (mapcar ,run (quote ,clauses)))))))
