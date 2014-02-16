(in-package :peldan.fen)

(unless (constantp +example-fen+)
  (defconstant +example-fen+ "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"))

(unless (constantp +initial-fen+)
  (defconstant +initial-fen+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))


(defun char->piece-type (char)
  (case (char-upcase char)
    (#\P :pawn)
    (#\B :bishop)
    (#\N :knight)
    (#\R :rook)
    (#\K :king)
    (#\Q :queen)))


(defun char->color (char)
  (if (upper-case-p char) :white :black))


(defun piece-char-p
    (char)
  (when-let ((color (char->color char))
	     (piece-type (char->piece-type char)))
    (pairlis '(:piece-type :color)
	     `(,piece-type ,color))))


(defun blanks-char-p (char)
  (when-let ((num (number-char-p char)))
    (replicate nil num)))


(defun parse-board-char (char)
  (cond ((piece-char-p char))
	((blanks-char-p char))
	(t (error 'parse-error))))


(defun parse-board
    (board-part)
  (let ((board (list)))
    (dolist (char board-part)
      (unless (eql #\/ char)
	(setf board
	      (cons (parse-board-char char) board))))
    (nreverse board)))


(defun parse-turn
    (text)
  (cond
    ((equal '(#\w) text) :white)
    ((equal '(#\b) text) :black)
    (t (error 'parse-error))))


(defun parse-rights
    (input)
  (labels ((right (color side)
	     (pairlis '(:color :side) `(,color ,side)))
	   (parse-right (char)
	     (case char
	       (#\K (right :white :kingside))
	       (#\Q (right :white :queenside))
	       (#\k (right :black :kingside))
	       (#\q (right :black :queenside))
	       (t (error 'parse-error)))))

    (mapcar #'parse-right input)))


(defun parse-passant
    (text)
  (unless (equal '(#\-) text)
    (multiple-value-bind (_ square) (parse-square text)
      (declare (ignore _))
      square)))


(defun parse-number
    (input)
  (parse-integer (concatenate 'string input)))


(defun parse
    (fen)
  (let ((parts (split fen)))
    (unless (eql 6 (length parts))
      (error 'parse-error))
    (destructuring-bind (board turn rights passant half-move full-move) parts
	(pairlis (list :board :turn :rights :passant :half-move :full-move)
		 (list (parse-board board)
		       (parse-turn turn)
		       (parse-rights rights)
		       (parse-passant passant)
		       (parse-number half-move)
		       (parse-number full-move))))))

(parse (coerce +initial-fen+ 'list))
