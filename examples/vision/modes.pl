max_vars(3).
max_body(2).
max_clauses(2).

modeh(f,2).
modeb(f,2).
modeb(draw1,2).
modeb(draw0,2).
modeb(move_right,2).
modeb(move_left,2).
modeb(move_up,2).
modeb(move_down,2).
modeb(at_top,1).
modeb(at_bottom,1).
modeb(at_left,1).
modeb(at_right,1).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
direction(P,0,in):-
    pred(P,_).
direction(P,1,out):-
    pred(P,2).
type(P,0,state):-
    pred(P,_).
type(P,1,state):-
    pred(P,2).

%% lower(f,inv1).
%% invented(inv1,2).
%% modeh(inv1,2).
%% modeb(inv1,2).
%% type(inv1,0,state).
%% type(inv1,1,state).
%% direction(inv1,0,in).
%% direction(inv1,1,out).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    clause_size(Clause,2).
:-
    clause(Clause),
    not meta_clause(Clause).
