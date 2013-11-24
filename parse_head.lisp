(defpackage parse-head
  (:use :common-lisp
	:it.bese.FiveAM
	:seq-utils
	:fun-utils)

  (:export :head
	   :forward
	   :peek
	   :get-pos
	   :set-pos
	   :read-next))

(in-package parse-head)

; TODO use abstractions for accessing head, pos

(defun head
    (s)
  (cons s 0))


(defun forward
    (head &key (step 1))
  (incf (cdr head) step))


(defun read-next
    (head)
  (unless (= (cdr head) (length (car head)))

    (let ((c (char (car head) (cdr head))))
      (forward head)
      c)))


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
