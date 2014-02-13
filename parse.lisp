(in-package :peldan.util.parse)

(defun number-char-p
    (c)
  (let ((num (- (char-int c) (char-int #\0))))
    (when (and (<= 0 num) (<= num 9))
      num)))
