
(in-package :peldan.pgn)

(defvar files (coerce "abcdefgh" 'list))
(defvar ranks (coerce "12345678" 'list))


(defun parse-file
    (text)
  (let ((c (car text))
	(rest (cdr text)))
    (unless (member c files)
      (error 'parse-error))
    (values rest c)))


(defun parse-rank
    (text)
  (let ((c (car text))
	(rest (cdr text)))
    (unless (member c ranks)
      (error 'parse-error))
    (values rest (number-char-p c))))


(defun parse-consecutive
    (text &rest parsers)
  (if parsers
    (multiple-value-bind (next r) (funcall (car parsers) text)
      (multiple-value-bind (pos rs) (apply #'parse-consecutive next (cdr parsers))
	(values pos (cons r rs))))
    (values text nil)))


(defun parse-square
    (text)
  (parse-consecutive text #'parse-file #'parse-rank))


(defun parse-move-type
    (text)
  (let ((c (car text))
	(rest (cdr text)))
    (if (eql c #\x) (values rest :takes) (values text :moves))))



(defun parse-square-indicator
    (text)
  (multiple-value-bind (pos r) (parse-square text)
    (values pos (acons :square r nil))))


(defun parse-file-indicator
    (text)
  (multiple-value-bind (pos r) (parse-file text)
    (values pos (acons :file r nil))))


(defun parse-rank-indicator
    (text)
  (multiple-value-bind (pos r) (parse-rank text)
    (values pos (acons :rank r nil))))



(defun parse-alternatives
    (text &rest alternatives)
  (if alternatives
    (handler-case (funcall (car alternatives) text)
      (error (_)
	(declare (ignore _))
	(apply #'parse-alternatives text (cdr alternatives))))
    (error 'parse-error)))

(defun parse-source-indicator
    (text)
  (parse-alternatives text
		      #'parse-square-indicator
		      #'parse-file-indicator
		      #'parse-rank-indicator))


(defun parse-long-pawn-move
    (text)
  (multiple-value-bind
	(pos r) (parse-consecutive text
				   #'parse-file-indicator
				   #'parse-move-type
				   #'parse-square)

    (values pos
	    (pairlis '(:source :move-type :destination)
		     r))))


(defun parse-short-pawn-move
    (text)
  (multiple-value-bind (pos r) (parse-square text)
    (values pos
	    (pairlis '(:destination :move-type) (list r :moves)))))



(defun parse-pawn-move
    (text)
  (multiple-value-bind
	(pos r) (parse-alternatives text
				    #'parse-long-pawn-move
				    #'parse-short-pawn-move)
    (values pos (acons :piece-type :pawn r))))


(defun parse-maybe
    (text parser)
  (parse-alternatives text
		      parser
		      (lambda (text) (values text nil))))


(defun parse-piece-type
    (text)
  (let ((c (car text))
	(rest (cdr text)))
    (unless c
      (error 'parse-error))
    (values rest
	    (case c
	      (#\N :knight)
	      (#\R :rook)
	      (#\K :king)
	      (#\Q :queen)
	      (#\B :bishop)
	      (t (error 'parse-error))))))

(defun parse-short-piece-move
    (text)
  (multiple-value-bind
	(pos r) (parse-consecutive text
				   #'parse-piece-type
				   #'parse-move-type
				   #'parse-square)
    (values pos
	    (pairlis '(:piece-type :move-type :destination) r))))



(defun parse-long-piece-move
    (text)
  (multiple-value-bind
	(pos r) (parse-consecutive text
				   #'parse-piece-type
				   #'parse-source-indicator
				   #'parse-move-type
				   #'parse-square)
    (values pos
	    (pairlis '(:piece-type :source :move-type :destination) r))))


(defun parse-piece-move
    (text)
  (parse-alternatives text
		      #'parse-long-piece-move
		      #'parse-short-piece-move))


(defun parse-string
    (text s)
  (unless (prefixp (coerce text 'list)
		   (coerce s 'list))
    (error 'parse-error))
  (values (drop (length s) text) s))

(defun parse-castles-queenside
    (text)
  (parse-string text "O-O-O")
  (acons :castles :queenside nil))

(defun parse-castles-kingside
    (text)
  (parse-string text "O-O")
  (acons :castles :kingside nil))

(defun parse-castles-move
    (text)
  (parse-alternatives text #'parse-castles-queenside #'parse-castles-kingside))


(defun parse-move
    (text)
  (parse-alternatives text
		      #'parse-pawn-move
		      #'parse-piece-move
		      #'parse-castles-move))



(def-suite pgn :description "Testing of the pgn package")
(in-suite pgn)

;; "Does not throw an error"
(defmacro works
    (&rest args)
  `(progn ,@args t))

(test parse-move
  (is (works (parse-move (coerce "O-O" 'list))))
  (is (works (parse-move (coerce "Rd2xd7" 'list))))
  (is (works (parse-move (coerce "exd7" 'list)))))


(test parse-rank
  (multiple-value-bind (_ r) (parse-rank (list #\3))
    (declare (ignore _))
    (is (eql 3 r))))

(test parse-rank-fail
  nil)

(test parse-source-indicator
  (multiple-value-bind (_ r) (parse-source-indicator (coerce "d4" 'list))
    (declare (ignore _))
    (is (not (null (assoc :square r))))))


(test parse-consecutive
  (multiple-value-bind (pos r) (parse-square (list #\e #\4))
    (is (not (null r)))
    (is (eql #\e (car r)))
    (is (eql 4 (nth 1 r)))
    (is (eql 2 (length r)))
    (is (eql 0 (length pos)))))

(defmacro parses-all
    (form)
  (let ((pos (gensym))
	(r (gensym)))
    `(multiple-value-bind (,pos ,r) ,form
       (is (null ,pos))
       ,r)))

(test parse-piece-move-long
  (let ((r (parses-all (parse-piece-move (coerce "Bd2xe3" 'list)))))
    (is (not (null r)))
    (is (eql :takes (assoc-value :move-type r)))
    (is (equal '(#\d 2)
	       (nested-assoc-value '(:source :square) r)))
    (is (equal '(#\e 3)
	       (assoc-value :destination r)))))

(test parse-piece-move
  (let ((r (parses-all (parse-piece-move (coerce "Ne4" 'list)))))
    (is (eql :knight (assoc-value :piece-type r)))
    (is (eql :moves (assoc-value :move-type r)))
    (is (equal '(#\e 4) (assoc-value :destination r)))))

(run! 'pgn)
