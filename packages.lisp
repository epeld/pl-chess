(defpackage :peldan.seq
  (:use :common-lisp
	:it.bese.FiveAM))

(defpackage :peldan.pgn
  (:use :common-lisp
	:peldan.util.parse
	:it.bese.FiveAM))

(defpackage :peldan.util.parse
  (:use :common-lisp)
  (:export char-to-number))
