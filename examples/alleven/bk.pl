cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    C is A+B.
empty([]).
zero(0).
even(A):-
    0 is A mod 2.


dummy1(_,_):-
    false.
dummy2(_,_):-
    false.
dummy3(_,_):-
    false.

dummy1m(_):-
    false.
dummy2m(_):-
    false.
dummy3m(_):-
    false.