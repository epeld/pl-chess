
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



hello3(X) :-
  new(P, picture),
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
                  send(P, display, new(B, box(W, H))),
                  X is Row * W,
                  Y is Col * H,
                  send(B, move, point(X, Y)),
                  send(B, fill_pattern, colour(orange))
                )
               )
        ),

  P = X.
