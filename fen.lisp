(in-package :peldan.fen)

(defconstant +example-fen+ 'never-changing "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
(defconstant +initial-fen+ 'never-changing "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


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


(defun parse
    (fen)
  (let ((parts (split fen)))
    (unless (eql 6 (length parts))
      (error 'parse-error))
    (destructuring-bind (board turn rights passant half-move full-move) parts
	(append (parse-board board)
		(parse-turn turn)
		(parse-passant passant)
		(parse-half-move half-move)
		(parse-full-move full-move)))))
