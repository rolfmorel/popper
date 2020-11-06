max_vars(3).
max_body(2).
max_clauses(6).

modeh(f,2).
modeb(up,2).
modeb(down,2).
modeb(left,2).
modeb(right,2).

invented(inv1,2).
invented(inv2,2).
invented(inv3,2).
invented(inv4,2).
%% invented(inv5,2).

lower(f,inv1).
lower(inv1,inv2).
lower(inv2,inv3).
lower(inv3,inv4).
lower(inv4,inv5).

%% :-
    %% recursive_clause(_).

lower(A,B):-
    lower(A,C),
    lower(C,B).
modeh(P,A):-
    invented(P,A).
modeb(P,A):-
    invented(P,A).
pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).

meta_clause(Clause):-
    Clause > 0,
    invented(P,2),
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    P != R,
    P != Q,
    clause_size(Clause,2).

meta_clause(0):-
    not invented(P,2),
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    P != R,
    P != Q,
    clause_size(Clause,2).
:-
    clause(Clause),
    not meta_clause(Clause).