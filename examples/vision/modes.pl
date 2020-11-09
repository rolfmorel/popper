max_vars(3).
max_body(5).
max_clauses(1).

modeh(f,2).
modeb(f,2).
modeb(draw1,2).
modeb(draw0,2).
modeb(move_right,2).
modeb(move_left,2).
modeb(move_up,2).
modeb(move_down,2).
%% modeb(at_top,1).
%% modeb(at_bottom,1).
%% modeb(at_left,1).
%% modeb(at_right,1).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
direction(P,0,in):-
    pred(P,_).
direction(P,1,out):-
    pred(P,2).
%% type(P,0,state):-
%%     pred(P,_).
%% type(P,1,state):-
%%     pred(P,2).

%% lower(f,inv1).
%% invented(inv1,2).
%% modeh(inv1,2).
%% modeb(inv1,2).
%% type(inv1,0,state).
%% type(inv1,1,state).
%% direction(inv1,0,in).
%% direction(inv1,1,out).

%% # vision6(A,B):-at_right(A),draw1(A,B).
%% # vision6(A,B):-draw1(A,C),move_right(C,D),move_down(D,E),vision6(E,B).

%% %% P(A,B)<-Q(A,C),R(C,B).
%% meta_clause(Clause):-
%%     head_literal(Clause,P,2,(0,1)),
%%     body_literal(Clause,Q,2,(0,2)),
%%     body_literal(Clause,R,2,(2,1)),
%%     clause_size(Clause,2).
%% :-
%%     clause(Clause),
%%     not meta_clause(Clause).



%% :-
    %% clause(Clause,Var),
    %% #count{P,Vars : body_literal(Clause,P,2,Vars), var_member(Var,Vars)} > 2.
%% :-
    %% body_literal(Cl,_,_,(0,V)),
    %% V != 2.
%% :-
%%     body_literal(Cl,_,_,(V1,V2)),
%%     V1 != 0,
%%     V2 != 1,
%%     V2 != V1 + 1.
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct
%% :- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct



a:-
    body_literal(Cl,_,_,(0,V)),
    V != 2.

a:-
    body_literal(Cl,_,_,(1,_)).

a:-
    body_literal(Cl,_,_,(V1,V2)),
    V1 != 0,
    V2 != 1,
    V2 != V1 + 1.

%% #show tmp/1.
%% tmp((Clause,Var,N)):-
:-
    clause_var(Clause,Var),
    #count{P,Vars : body_literal(Clause,P,2,Vars),var_member(Var,Vars)} > 1.
%% :-
%%     clause_var(Clause,Var),
%%     #count{P,Vars : var_in_literal(Clause,P,Vars,Var), body_literal(Clause,P,_,Vars)} > 3.

a:-
    body_literal(Cl,P,_,(PV1,PV2)),
    body_literal(Cl,Q,_,(QV1,QV2)),
    P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct

a:-
    body_literal(Cl,P,_,(PV1,PV2)),
    body_literal(Cl,Q,_,(QV1,QV2)),
    P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct

a:-
    body_literal(Cl,P,_,(PV1,PV2)),
    body_literal(Cl,Q,_,(QV1,QV2)),
    PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct

a:-
    body_literal(Cl,P,_,(PV1,PV2)),
    body_literal(Cl,Q,_,(QV1,QV2)),
    PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct