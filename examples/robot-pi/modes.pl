max_vars(10).
max_body(10).
max_clauses(2).

modeh(f,2).
type(f,0,world).
type(f,1,world).
%% direction(f,0,in).
%% direction(f,1,out).

modeh(inv1,2).
modeb(inv1,2).
invented(inv1,2).

modeb(right,2).
type(right,0,world).
type(right,1,world).
%% direction(right,0,in).
%% direction(right,1,out).

%% :-
%%     not head_literal(_,P,A,_),
%%     invented(P,A).


%% :- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct