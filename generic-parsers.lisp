(defpackage generic-parsers
  (:use :common-lisp
	:parsing
	:control)
  (:export parse-branch))

(in-package :generic-parsers)

; TODO prettyprint parsing
(defun parse-branch
    (head &rest body)
  "Perform a branch, trying each action in turn until one succeeds"
  (dolist (clause body)

    (when (null clause)
      (return-from parse-branch))

    (ignore-conditions (parse-error)
      (return-from parse-branch
	(apply #'run-parser head clause))))

  ; No branch succeeded
  (signal 'parse-error))

(defmacro defgrammar (&rest _))

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
