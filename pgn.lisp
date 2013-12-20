
(in-package :peldan.pgn)

(defmacro alternatives
    (&rest forms)
  (let ((escape (gensym)))
    (labels ((make-case
	       (form)
	     `(handler-case (throw (quote ,escape) ,form) (error nil))))
      `(catch (quote ,escape)
	 ,@(mapcar #'make-case forms)
	 (error 'parse-failure)))))

; TODO write a macro transform-value that works on values


(defun run-parsers
    (seq &rest parsers)
    (labels ((rec
		 (seq parsers acc)
	       (if (null parsers)
		   (values seq (nreverse acc))
		   (let ((parser (car parsers))
			 (rest (cdr parsers)))
		     (multiple-value-bind (seq-new val) (funcall parser seq)
		       (rec seq-new rest (cons val acc)))))))
      (rec seq parsers nil))))


(defun parse-file
    (seq)
  (parse-char seq "abcdefgh"))


(defun parse-rank
    (seq)
  (parse-char seq "12345678"))


(defun parse-square
    (seq)
  (run-parsers parse-file parse-rank))


(defun parse-source-indicator
    (seq)
  (alternatives (parse-square seq)
		(parse-rank seq)
		(parse-file seq)))


(defun parse-pawn-move
    (seq)
  (multiple-value-bind (sq1 seq) (parse-source-indicator seq)
    sq1))


(defun parse-move
    (seq)
  (parse-pawn-move seq))
