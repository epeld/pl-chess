(defpackage parsing
  (:use :common-lisp
	:parse-head)
  (:export defparser
	   defparsefunc
	   run-parser))

(in-package parsing)

(defun get-parser-function
    (entity)
  (let ((name (symbol-name entity)))
    (intern (concatenate 'string "PARSE-" name))))


(defun run-parser
    (head name &rest args)
  (apply (get-parser-function name) head args))


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


(defmacro defparser
    (name &body body)
  (let ((parser-name (get-parser-function name)))
    `(defparsefunc ,parser-name
	 ()
       (mapcar (partial (applied #'run-parser) head)
	       (quote ,body)))))


(defmacro defgrammar
    (name &body body)
  (let ((parsers (mapcar #'car body)))
    `(defparser ,name
       ,@parsers)))
