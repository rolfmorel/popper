cons1(A,B,C):-
    append([A],B,C).

cons2(A,B,C):-
    append([A],B,C).

succ(X, Y) :-
  Y is X + 1.

empty([]).

identical(X, X).
