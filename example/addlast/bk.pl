tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    C is A+B.
empty([]).
zero(0).