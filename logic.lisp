

(defun caro
    (p a)
  (fresh (d)
    (== (cons a d) p)))


(defun cdro
    (p a)
  (fresh (d)
    (== (cons d a) p)))


(defun nullo
    (p)
  (== nil p))


(defun eqo
    (a b)
  (== a b))


(defun membero
    (p a)
  (conde
    ((caro p a))
    ((fresh (d)
       (cdro p d)
       (membero d a)))))


(defun ino
    (m k v)
  (fresh (d)
    (== (acons k v d) m)))


(defun conso
    (a b r)
  (== (cons a b) r))


(defun appendo
    (x y r)
  (conde
    ((nullo x) (== r y))
    ((fresh (a b d)
       (caro x a)
       (cdro x b)
       (conso a d r)
       (appendo b y d)))))


(defun lengtho
    (p n)
  (== n (length p)))

(defun adjacentno
    (p a b n)
  (fresh (c d e f)
    (lengtho f n)
    (conde
      ((appendo d (cons b e) f)
       (appendo c (cons a f) p))

      ((appendo d (cons a e) f)
       (appendo c (cons b f) p)))))

(defun adjacento
    (p a b)
  (fresh (c d)
    (conde
      ((appendo c (cons a (cons b d)) p))
      ((appendo c (cons b (cons a d)) p)))))


(defun fileo
    (f)
  (membero (coerce "abcdefgh" 'list) f))


(defun ranko
    (r)
  (membero (list 1 2 3 4 5 6 7 8) r))


(defun adjacent-fileso
    (a b)
  (adjacento (coerce "abcdefgh" 'list) a b))


(defun adjacent-rankso
    (a b)
  (adjacento (list 1 2 3 4 5 6 7 8) a b))


(defun squareo
    (s)
  (fresh (f r)
    (== (cons f r) s)
    (fileo f)
    (ranko r)))


(defun cons-squareo
    (a b)
  (fresh (sq)
    (conso a b sq)
    (squareo sq)))


(defun pawn-move-shorto
    (s)
  (fresh (a b sq)
       (== (list a b) s) (cons-squareo a b)))


(defun pawn-take-shorto
    (s)
  (fresh (f r a b part1 part2)
    (appendo part1 part2 s)
    (== part2 (list #\x a b))
    (cons-squareo a b)
    (fileo f)
    (adjacent-fileso f a)
    (conde
      ((conso f r part1) (ranko r) (adjacent-rankso r b))
      ((conso f nil part1)))))


(defun pawn-moveo
    (s)
  (conde
    ((pawn-move-shorto s))
    ((pawn-take-shorto s))))
