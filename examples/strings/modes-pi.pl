max_vars(3).
max_body(2).
max_clauses(6).

modeh(f,2).
modeb(is_empty,1).
%% modeb(is_letter,1).
%% modeb(is_lowercase,1).
%% modeb(is_number,1).
modeb(is_space,1).
%% modeb(is_uppercase,1).
%% modeb(mk_lowercase,2).
%% modeb(mk_uppercase,2).
modeb(copyskip1,2).
modeb(skip1,2).
modeb(f,2).

direction(P,0,in):-
    pred(P,_).
direction(P,1,out):-
    pred(P,2).

:-
    multiclause(P,A),
    not recursive_clause(_,P,A).
:-
    num_clauses(P,N),
    N > 2.

{invented(inv1,2)}.
{invented(inv2,2)}.
%% {invented(inv3,2)}.

:-
    invented(P,A),
    head_literal(_,P,A,_),
    not recursive_clause(_,P,A).

:-
    body_literal(_,_,_,(1,_)).

:-
    body_literal(_,_,_,(V1,V2)),
    V1 != 0,
    V2 != 1,
    V2 != V1 + 1.

:-
    body_var(Clause,V0),
    #count{P,V1 : body_literal(Clause,P,2,(V0,V1))} > 1.


%% f(A,B):-copyskip(A,C),inv1(D,B).
%% inv1(A,B):-is_space(A),copyskip(A,B).
%% inv1(A,B):-skip(A,C),inv1(C,B).
%% inv2(A,B):-copyskip(A,B),empty(B).
%% inv2(A,B):-copyskip(A,C),inv2(C,B).

