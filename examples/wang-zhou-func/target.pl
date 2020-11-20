%% sum(A,B,C):-
%%     zero(B),
%%     equal(A,C).

%% sum(A,B,C):-
%%     not_zero(B),
%%     and(A,B,D),
%%     shl(D,E),
%%     xor(A,B,F),
%%     sum(E,F,B).