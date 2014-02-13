(in-package :peldan.fen)

(defconstant example-fen "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
(defconstant initial-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


(defun parse-piece-type
    (char)
  (case (char-upcase char)
    (#\P :pawn)
    (#\B :bishop)
    (#\N :knight)
    (#\R :rook)
    (#\K :king)
    (#\Q :queen)
    (otherwise (error 'parse-error))))


(defun parse-piece
    (char)
  (let ((piece-type (parse-piece-type char))
	(color (if (upper-case-p char) :white :black)))
    (pairlis '(:piece-type :color)
	     '(parse-piece-type color))))


(defun parse-board
    (board-part)
  (let ((board (list)))
    (dolist (char board-part)
      (unless (eql #\/ char)
	;; TODO fix this. Should branch in a better way
	(if-let ((num (number-char-p char)))
	  (setf board (append (replicate nil num)))
	  (setf board (cons (parse-piece char) board)))))
    board))


(defun parse
    (fen)
  (let ((parts (split fen)))
    (unless (eql 6 (count parts))
      (error 'parse-error))
    (destructuring-bind ((board turn rights passant half-move full-move) parts)
	(append (parse-board board)
		(parse-turn turn)
		(parse-passant passant)
		(parse-half-move half-move)
		(parse-full-move full-move)))))
