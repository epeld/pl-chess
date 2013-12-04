(defpackage pgn
  (:use :common-lisp
	:parsing
	:generic-parsers))


(in-package :pgn)

(defparsefunc parse-takes
    ()
  (when (parse-char head "x")
    :takes))


(defparser piece
  ((char "BNKQR")))


(defparser square
  ((char "abcdefgh") :as :file)
  ((char "12345678") :as :rank))

(defparser piece-move
  (piece :as :piece)
  ((maybe takes) :as :takes)
  ((maybe square) :as :source)
  (square :as :destination))


(defparser move
  ((branch piece-move pawn-move) :as :move-type))


(parse-piece-move (parse-head:head "Ne4"))
