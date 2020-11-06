%% max_vars(7).
%% max_body(7).
%% max_clauses(1).

max_vars(3).
max_body(2).
max_clauses(3).

modeh(f,2).
modeb(is_empty,1).
%% modeb(is_letter,1).
%% modeb(is_lowercase,1).
%% modeb(is_number,1).
%% modeb(is_space,1).
%% modeb(is_uppercase,1).
%% modeb(mk_lowercase,2).
%% modeb(mk_uppercase,2).
modeb(copyskip1,2).
%% modeb(copy1,2).
modeb(skip1,2).
modeb(f,2).

lower(f,inv1).
%% lower(inv1,inv2).
invented(inv1,2).
%% invented(inv2,2).

type(P,0,state):-
    pred(P,_).
type(P,1,state):-
    pred(P,2).

direction(P,0,in):-
    pred(P,_).
direction(P,1,out):-
    pred(P,2).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Enforce forward-chaining langauge bias %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct

%% precon pab :- qa, rab.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,1,(0,)),
    body_literal(Clause,R,2,(0,1)),
    P != R,
    clause_size(Clause,2).


%% postcon pab :- qab, rb.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,1)),
    body_literal(Clause,R,1,(1,)),
    P != Q,
    clause_size(Clause,2).

%% chain pab :- qac, rcb.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    P != R,
    P != Q,
    clause_size(Clause,2).

%% %% tailrec pab :- qac, pcb
meta_clause(Clause):-
    Clause > 0,
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,P,2,(2,1)),
    P != Q,
    clause_size(Clause,2).

%% meta_clause(0):-
%%     not invented(P,2),
%%     head_literal(Clause,P,2,(0,1)),
%%     body_literal(Clause,Q,2,(0,2)),
%%     body_literal(Clause,R,2,(2,1)),
%%     P != R,
%%     P != Q,
%%     clause_size(Clause,2).
%% :-
%%     clause(Clause),
%%     not meta_clause(Clause).