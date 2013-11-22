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


(defun parse-grammar-body
    (body)
  (mapcar #'(lambda (x) (alist (cons :action x))) body))


(defmacro defgrammar
    (name &rest body)
  )


(defun single-action-p
    (action)
  (not (consp action)))


(defun branchp
    (action)
  (and (consp action)
       (eq 'or (car action))))


(defun seqp
    (action)
  (and (consp action)
       (eq 'seq (car action))))


(defun compute-action-value
    (head action)
  (cond
    ((single-action-p action)
     (funcall action head))
    ((seqp action)
     (perform-sequential-actions head action))
    ((branchp action)
     (perform-branch head action))
    (t (error 'unknown-action))))


(defun perform-action
    (head action-alist)
  (let ((action (assoc-value :action action-alist)))
    (acons :value (compute-action-value head action)
	   action-alist)))


(defun perform-branch
    (head action-alists)
  "Perform a branch, trying each action in turn until one succeeds"
  (catch 'branch-success
    (dolist (action action-alists)
      (handler-case (throw 'branch-success
		      (perform-action (copy-list head) action))
	(parse-error ()
	  nil))
      (signal 'parse-error))))


(defun perform-sequential-actions
    (head action-alists)
  (mapcar (partial #'perform-action head)
	  action-alists))


(defmacro maybe
    (name)
  `(or ,name nil))

(defgrammar move
    (piece :as :piece)
    ((maybe takes) :as :takes)
    ((maybe square) :as :source)
    (square :as :destination))

(defgrammar takes
    (char #\x))
