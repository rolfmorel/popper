%% max_vars(7).
%% max_body(7).
%% max_clauses(1).

max_vars(3).
max_body(3).
max_clauses(2).

modeh(f,2).
modeb(is_empty,1).
modeb(is_letter,1).
modeb(is_lowercase,1).
modeb(is_number,1).
modeb(is_space,1).
modeb(is_uppercase,1).
modeb(mk_lowercase,2).
modeb(mk_uppercase,2).
modeb(copyskip1,2).
modeb(skip1,2).
modeb(f,2).

direction(f,0,in).
direction(f,1,out).
direction(copyskip1,0,in).
direction(copyskip1,1,out).
direction(mk_uppercase,0,in).
direction(mk_uppercase,1,out).
direction(mk_lowercase,0,in).
direction(mk_lowercase,1,out).
direction(copy1,0,in).
direction(copy1,1,out).
direction(skip1,0,in).
direction(skip1,1,out).
direction(is_empty,0,in).
direction(is_letter,0,in).
direction(is_lowercase,0,in).
direction(is_uppercase,0,in).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
%% direction(P,0,in):-
    %% pred(P,_).
%% direction(P,1,out):-
    %% pred(P,2).
type(P,0,state):-
    pred(P,_).
type(P,1,state):-
    pred(P,2).


:- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct