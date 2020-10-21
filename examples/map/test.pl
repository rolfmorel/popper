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

f(A,B):-empty_in(A),empty_out(B).
f(A,B):-tail(A,T1),f(T1,T2),head(A,H1),succ(H1,H2),cons(H2,T2,B).
=>
f(A,B):-empty_in(A),empty_out(B).
f(A,B):-cons(C,A,D),f(D,E),succ(C,F),cons(F,E,B).