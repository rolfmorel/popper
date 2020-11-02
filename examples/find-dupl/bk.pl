%% background knowledge
head([H|_],H).
tail([_|T],T).
mylast([A],A):-!.
mylast([_|T],A):-
    mylast(T,A).
element([X|_T],X).
element([_|T],X):-
    element(T,X).
mergesort([H|T],B):-
    msort([H|T],B).