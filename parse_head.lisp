(defpackage parse-head
  (:use :common-lisp :it.bese.FiveAM))

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

; TODO write functions (alist :key1 1 :key2 2 etc..)
; using group and pairlis
; write assoc-value (compose cdr assoc)
; compose

; TODO: destructure out the action
; add the result
; branch has :action '(alternative1 alternative2..)
(defun perform-action
    (head action-alist)
  (let ((action (cdr (assoc :action action-alist))))
    (if (consp action)
	todo
	(acons :value (funcall #'action head)
	       action-alist))))


(defun perform-actions
    (head action-alists)
  (labels ((rec (actions acc)
	     (let* ((action (car actions))
		    (rest (cdr actions))
		    (res (perform-action head rest)))
	       (if (null parts)
		   res
		   (cons res (perform-actions head rest))))))))

(defgrammar move
    (piece :as :piece)
    ((maybe takes) :as :takes)
    ((maybe square) :as :source)
    (square :as :destination))

(defgrammar takes
    (char #\x))
