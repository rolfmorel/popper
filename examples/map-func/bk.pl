cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).

cons1(A,B,C):-
    cons(A,B,C).
cons2(A,B,C):-
    cons(A,B,C).
succ(A,B):-
    B is A+1.