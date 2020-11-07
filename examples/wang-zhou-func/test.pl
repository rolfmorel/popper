:- [exs].
:- [bk].
a:-
    forall(pos(f(A,B,C)), sum(A,B,C)).


%% sum(A,B,C):-
%%     zero(B),
%%     equal(A,C).

%% sum(A,B,C):-
%%     not_zero(B),
%%     and(A,B,D),
%%     shl(D,E),
%%     xor(A,B,F),
%%     sum(E,F,C).

sum(A,B,C) :- xor(B,A,C).
sum(A,B,C) :- and(A,B,D),shl(D,E),xor(A,B,F),sum(E,F,C).