max_vars(3).
max_body(2).
max_clauses(2).

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

invented(inv1,2).
invented(inv2,2).
invented(inv3,2).
invented(inv4,2).
invented(inv5,2).

modeh(P,A):-
    invented(P,A).
modeb(P,A):-
    invented(P,A).
direction(P,0,in):-
    invented(P,2).
direction(P,1,out):-
    invented(P,2).

%% size(N):-

%% :-
    %% not clause(1).

lower(f,inv1).
lower(f,inv2).
lower(f,inv3).
lower(f,inv4).
lower(f,inv5).
lower(inv1,inv2).
lower(inv1,inv3).
lower(inv1,inv4).
lower(inv1,inv5).
lower(inv2,inv3).
lower(inv2,inv4).
lower(inv2,inv5).
lower(inv3,inv4).
lower(inv3,inv5).
lower(inv4,inv5).


%% TMP!!!
%% :-
%%     head_literal(Clause1,P,_,_),
%%     head_literal(Clause2,P,_,_),
%%     Clause1 != Clause2.

%% C1: FORCE ORDERING
%% inv2(A):- inv1(A)
%% DOES NOT SEEM TO DO ANYTHING
:-
    head_literal(Clause,Inv2,_,_),
    body_literal(Clause,Inv1,_,_),
    lower(Inv1,Inv2).

%% C2: USE INVENTED SYMBOLS IN ORDER
%% f(A):- inv2(A)
%% inv2(A):- q(A)
:-
    head_literal(_,Inv2,_,_),
    lower(Inv1,Inv2),
    not head_literal(_,Inv1,_,_).

%% C3: ORDER CLAUSES BY ORDERING
%% f(A):- inv1(A)
%% inv2(A):- q(A) (clause1)
%% inv1(A):- inv2(A) (clause2)
:-
    head_literal(Clause2,P1,_,_),
    head_literal(Clause1,P2,_,_),
    invented(P1,_),
    invented(P2,_),
    lower(P1,P2),
    Clause2 > Clause1.

%% PREVENT DUPLICATE INVENTED CLAUSES
%% inv1(A,B):-right(A,C),right(C,B).
%% inv2(A,B):-right(A,C),right(C,B).

single_head(P,A):-
    modeh(P,A),
    #count{Clause : head_literal(Clause,P,A,_)} == 1.


%% f(A,B):-inv1(A,C),inv2(C,B).
%% inv1(A,B):-right(A,C),right(C,B).
%% inv2(A,B):-right(A,C),right(C,B).
%% TODO: GENERALISE FOR MULTIPLE CLAUSES
:-
    clause_size(Clause1,N),
    clause_size(Clause2,N),
    Clause1 != Clause2,
    head_literal(Clause1,HeadPred1,A1,_),
    head_literal(Clause2,HeadPred2,A2,_),
    single_head(HeadPred1,A1),
    single_head(HeadPred2,A1),
    invented(HeadPred1,A1),
    invented(HeadPred2,A2),
    HeadPred1 != HeadPred2,
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V2)),
    body_literal(Clause,R,2,(V2,V1)),
    P != Q,
    P != R,
    V0!=V1,V0!=V2,V1!=V2,
    clause_size(Clause,2).
:-
    clause(Clause),
    not meta_clause(Clause).