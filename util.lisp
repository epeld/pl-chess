(in-package :peldan.util.parse)

(defun char-to-number
    (c)
  (- (char-int c) (char-int #\0)))
