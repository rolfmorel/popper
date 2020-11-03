max_vars(5).
max_body(5).
max_clauses(4).


modeh(f,2).
type(f,0,world).
type(f,1,world).
direction(f,0,in).
direction(f,1,out).

modeb(right,2).
type(right,0,world).
type(right,1,world).
direction(right,0,in).
direction(right,1,out).

modeb(left,2).
type(left,0,world).
type(left,1,world).
direction(left,0,in).
direction(left,1,out).

modeb(up,2).
type(up,0,world).
type(up,1,world).
direction(up,0,in).
direction(up,1,out).

modeb(down,2).
type(down,0,world).
type(down,1,world).
direction(down,0,in).
direction(down,1,out).

modeh(inv1,2).
modeb(inv1,2).
invented(inv1,2).
direction(inv1,0,in).
direction(inv1,1,out).

modeh(inv2,2).
modeb(inv2,2).
invented(inv2,2).
direction(inv2,0,in).
direction(inv2,1,out).

modeh(inv3,2).
modeb(inv3,2).
invented(inv3,2).
direction(inv3,0,in).
direction(inv3,1,out).

lower(f,inv1).
lower(f,inv2).
lower(f,inv3).
lower(inv1,inv2).
lower(inv1,inv3).
lower(inv1,inv2).
lower(inv1,inv3).
lower(inv2,inv3).

%% inv1(A,B):-inv2(A,B). allowed
%% inv1(A,B):-inv3(A,B). allowed
%% inv2(A,B):-inv1(A,B). not allowed
%% :-
%%     head_literal(Clause,P1,_,_),
%%     body_literal(Clause,P2,_,_),
%%     lower(P2,P1).
%% :-
    %% head_literal(_,P,A,_),
    %% invented(P,A),
    %% ADD ORDERING CONSTRAINT

%% :-
%%     head_literal(Clause,P,_,_),
%%     body_literal(Clause,P,_,_),
%%     invented(P,_).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V2)),
    body_literal(Clause,R,2,(V2,V1)),
    V0!=V1,V0!=V2,V1!=V2,
    clause_size(Clause,2).
:-
    clause(Clause),
    not meta_clause(Clause).