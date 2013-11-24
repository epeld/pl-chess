(defpackage generic-parsers
  (:use :common-lisp
	:parsing
	:control)
  (:export :parse-branch
	   :parse-char
	   :parse-maybe)
  (:import-from :parse-head
		:read-next))

(in-package :generic-parsers)


(defparsefunc parse-branch
    (&rest body)
  "Perform a branch, trying each action in turn until one succeeds"
  (dolist (clause body)

    (ignore-conditions (parse-error)
      (return-from parse-branch
	(run-clause head clause))))

  ; No branch succeeded
  (signal 'parse-error))

(defparsefunc parse-maybe
    (name)
  (parse-branch head name nil))


(defparsefunc parse-char
    (char-string)
  (let ((c (find (read-next head) char-string)))
    (unless c
      (signal 'parse-error))
    c))
