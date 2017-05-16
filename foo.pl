
hello(X) :-
  new(P, picture),
  send(P, open),

  send(P, display, new(B, box(50,100))),
  send(P, display, new(B2, box(50,100))),
  send(P, display, new(B3, box(50,100))),

  new(S, spatial(@default, @default, @default, @default, w / 8 = w2, h / 8 = h2)),
  new(S2, spatial(xref = x, yref = y, xref = x - w, yref = y - h, w / 8 = w2, h / 8 = h2)),
  new(S3, spatial(xref = x, yref = y, xref = x - 2 * w, yref = y - 2 * h, w / 8 = w2, h / 8 = h2)),

  send(B, fill_pattern, colour(blue)),
  send(B2, fill_pattern, colour(red)),
  send(B3, fill_pattern, colour(orange)),

  new(_, constraint(P, B, S)),
  new(_, constraint(P, B2, S2)),
  new(_, constraint(P, B3, S3)),
  
      
  P = X.


hello2(X) :-
  new(P, picture),
  send(P, open),

  !,

  forall(between(0, 6, Row),
         (
           send(P, display, new(B, box(50,100))),

           new(S, spatial(@default, @default, xref = x - Row * w, yref = y - Row * h, w / 8 = w2, h / 8 = h2)),

           send(B, fill_pattern, colour(blue)),

           new(_, constraint(P, B, S)),
           format("Row is ~d\n", [Row])
         )
        ),

  % The last square..
  send(P, display, new(B, box(50, 200))),
  send(B, fill_pattern, colour(purple)),
  new(S, spatial(xref = x + w, yref = y + h, xref = x + w, yref = y + h, w / 8 = w2, h / 8 = h2)),
  new(_, constraint(P, B, S)),
         
         
  P = X.



board(X) :-
  new(P, picture),

  send(P, set(width := 500, height := 500)),
  send(P, open),

  get(P, area, BB),
  get(BB, width, BW),
  get(BB, height, BH),

  W is BW / 8,
  H is BH / 8,

  format("Looks like size will be ~d by ~d\n", [W, H]),

  forall(between(0,7, Row),
         forall(between(0,7, Col),
                (
                  make_square(Row, Col, W, H, Sq),
                  send(P, display, Sq)
                )
               )
        ),

  P = X.

make_square(Row, Col, W, H, Square) :-
  Ix is Row * 8 + Col,
  atom_number(A, Ix),
  
  new(D, device),
  send(D, display, new(B, box(W, H))),
  send(D, display, new(T, text(A))),

  % Prep the square appearence-wise
  square_color(Row, Col, Color),
  send(B, fill_pattern, colour(Color)),
  send(B, pen, 0),

  % Position the square
  X is Row * W,
  Y is Col * H,
  send(D, move, point(X, Y)),
  Square = D.

square_color(Row, Col, Color) :-
  Sum is Row + Col,
  1 is Sum mod 2 *-> Color = gray
  ; Color = white.
