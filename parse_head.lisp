(defpackage parse-head
  (:use
   :common-lisp
   :it.bese.FiveAM
   :seq-utils
   :fun-utils))

(in-package parse-head)

(defun head
    (s)
  (cons s 0))


(defun forward
    (head &key (step 1))
  (incf (cdr head) step))


(defun peek
    (head &key (num 1))
  (let ((s (car head))
	(pos (cdr head)))
    (subseq s pos (+ pos num))))


(defun set-pos
    (head pos)
  (setf (cdr head) pos))


(defun get-pos
    (head)
  (cdr head))


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

; TODO write ignore-parse-errors or (ignore 'parse-error)
; TODO prettyprint parsing
; TODO has a bug. Head must be left in the position of succeeding perform-action
(defun parse-branch
    (head &body body)
  "Perform a branch, trying each action in turn until one succeeds"
  (catch 'branch-success
    (dolist (clause body)
      (handler-case (throw 'branch-success
		      (apply #'run-parser head clause))
	(parse-error ()
	  nil))
      (signal 'parse-error))))


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


(defgrammar move
    (piece :as :piece)
    ((maybe takes) :as :takes)
    ((maybe square) :as :source)
    (square) :as :destination)

(macroexpand  (macroexpand-1 (macroexpand-1 (macroexpand-1 '(defgrammar move
							     (piece)
							     ((maybe takes))
							     ((maybe square))
							     (square))))))
