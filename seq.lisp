(in-package :peldan.util.seq)


(defun split (seq &optional (on #\space))
  "Split a sequence on each occurence of 'on'."
  (labels ((rec (word seq acc)
	     (if seq
		 (let ((char (first seq))
		       (seq (rest seq)))
		   (if (eql char on)
		       (rec nil seq (cons (nreverse word) acc))
		       (rec (cons char word) seq acc)))
		 (nreverse (cons (nreverse word) acc)))))
    (rec nil seq nil)))


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

;; TODO Test.
(defun insert (e list index)
  (append (take index list) (cons e (drop index list))))


(defun prefixp
    (seq prefix-seq)
  (let ((prefix (take (length prefix-seq) seq)))
    (equal prefix prefix-seq)))


(defun replicate
    (val times)
  (if (zerop times)
      nil
      (cons val (replicate val (- times 1)))))


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


(test replicate
  (let ((res (replicate :test-val 13)))
    (is (eql 13 (length res)))))


(test split
  (let ((res (split (coerce "aaabbaba" 'list) #\b)))
    (is (= 4 (length res)))))

(test split-ending-in-space
  (let ((res (split (coerce "aaabbb" 'list) #\b)))
    (is (= 4 (length res)))))


(run! 'seqs)
