
(in-package :peldan.seq)

(defun take
    (n seq)
  (if (zerop n)
      nil
      (when seq
	(cons (car seq)
	      (take (- n 1) (cdr seq))))))


(defun drop
    (n seq)
  (if (zerop n)
      seq
      (drop (- n 1) (cdr seq))))

(defun prefixp
    (seq prefix-seq)
  (let ((prefix (take (length prefix-seq) seq)))
    (equal prefix prefix-seq)))


(def-suite seqs :description "Testing of the seq package")
(in-suite seqs)

(test prefix
  (is (prefixp (list 1 2 3 4 5) (list 1 2 3))))

(test drop
  (is (equal (list 4 5) (drop 3 (list 1 2 3 4 5)))))

(test drop-empty
  (is (equal (list 1 2 3 4 5) (drop 0 (list 1 2 3 4 5)))))

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
