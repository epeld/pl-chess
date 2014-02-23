(defpackage :peldan.util.seq
  (:use :common-lisp
	:it.bese.FiveAM)
  (:export take drop prefixp replicate split insert))

(defpackage :peldan.util.common
  (:use :common-lisp
	:peldan.util.seq)
  (:export dec when-let symbol-to-keyword))

(defpackage :peldan.util.parse
  (:use :common-lisp)
  (:export number-char-p))

(defpackage :peldan.util.alist
  (:use :common-lisp
	:peldan.util.common)
  (:export assoc-value nested-assoc nested-assoc-value))

(defpackage :peldan.util.list
  (:use :common-lisp)
  (:export second third))

(defpackage :peldan.pgn
  (:use :common-lisp
	:peldan.util.parse
	:peldan.util.seq
	:peldan.util.alist
	:it.bese.FiveAM)
  (:export parse-square))

(defpackage :peldan.fen
  (:use :common-lisp
	:peldan.util.common
	:peldan.util.parse
	:peldan.util.seq
	:peldan.pgn
	:it.bese.FiveAM))

(defpackage :peldan.chess
  (:use :common-lisp
	:peldan.util.common)
  )
