
(in-package :peldan.seq)

(defun take
    (n seq)
  (if (zerop n)
      nil
      (when seq
	(cons (car seq)
	      (take (- n 1) (cdr seq))))))


(def-suite seqs :description "Testing of the seq package")
(in-suite seqs)

(test take-0
  (let ((res (take 0 (list 1 2 3))))
    (is (eql nil res))))

(test take-n-less
  (let ((ls (list 1 2 3)))
    (is (eql 2 (length (take 2 ls))))))

(test take-n-more
  (let ((ls (list 1 2 3)))
    (is (eql 3 (length (take 5 ls))))))



(run! 'seqs)
