(defpackage :peldan.util.common
  (:use :common-lisp)
  (:export dec))

(defpackage :peldan.util.seq
  (:use :common-lisp
	:peldan.util.common
	:it.bese.FiveAM)
  (:export take drop prefixp))

(defpackage :peldan.fen
  (:use :common-lisp
	:it.bese.FiveAM))

(defpackage :peldan.util.parse
  (:use :common-lisp)
  (:export number-char-p))

(defpackage :peldan.util.alist
  (:use :common-lisp)
  (:export assoc-value nested-assoc nested-assoc-value))

(defpackage :peldan.util.list
  (:use :common-lisp)
  (:export second third))

(defpackage :peldan.pgn
  (:use :common-lisp
	:peldan.util.parse
	:peldan.util.seq
	:peldan.util.alist
	:it.bese.FiveAM))
