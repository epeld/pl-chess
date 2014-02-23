(in-package :peldan.chess)


;; Needed facilities:
;; thread-first
;; thread-last
;; with-assocs (piece-type source ..)
;; destructuring with assocs in defun?

(defun make-square-move (pos from to)
  )



(defun make-castling-move (pos move)
  (let ((turn (assoc-value :turn pos))
	(side (assoc-value :castles move)))

    ;; TODO see idea in make-piece-move
    (thread-first (foldl #'make-square-move pos (castling-moves turn side))
		  (remove-castling-rights turn)
		  (update-move-counter)))


(defun find-move-candidate (pos move)
  (with-assocs move (destination piece-type source)
    (thread-last
     (find-pieces-reaching pos destination)
     (filter (match-piece-type-p piece-type))
     (filter (match-source-p source))
     (one-and-only))))

;; TODO
(defun make-piece-move (pos move)
  (with-assocs move (destination turn piece-type)
    (let ((candidate (find-move-candidate pos move)))

    ;; Idea: maybe these functions should all just return 'patches' to apply to pos
    (thread-first
     (make-square-move pos candidate destination)
     (remove-castling-rights turn (affected-castling-rights candidate destination))
     (update-move-counter (eql :pawn piece-type))
     (acons :passant (derive-passant pos candidate move)))


(defun make-pgn-move (pos move)
  (if (assoc :castles move)
      (make-castling-move pos move)
      (make-piece-move pos move)))
