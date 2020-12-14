%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################

#defined invented/2.
#defined inv_lower/2.

lower(P,Q):-
    modeh(P,_),
    invented(Q,_).
lower(P,Q):-
    inv_lower(P,Q).
lower(A,B):-
    lower(A,C),
    lower(C,B).

%% AN INVENTED SYMBOL MUST APPEAR IN THE HEAD OF A CLAUSE
:-
    invented(P,A),
    not head_literal(_,P,A,_).

appears_before(P,A):-
    invented(P,A),
    lower(Q,P),
    head_literal(C,Q,_,_),
    body_literal(C,P,_,_).

%% AN INVENTED SYMBOL MUST APPEAR IN THE BODY OF A CLAUSE
:-
    invented(P,A),
    not appears_before(P,A).

:-
    invented(P,_),
    inv_lower(Q,P),
    not invented(Q,_).

%% USE INVENTED SYMBOLS IN ORDER
:-
    invented(P,_),
    lower(Q,P),
    not head_literal(_,Q,_,_).

%% MUST HAVE NON-INVENTED TARGET PREDICATE
:-
    modeh(P,A),
    not head_literal(_,P,A,_).
%% :-
%%     #count{P,A : head_literal(_,P,A,_), not invented(P,A)} == 0.

%% FIRST CLAUSE CANNOT BE INVENTED
:-
    head_literal(0,P,A,_),
    invented(P,A).

%% ORDER CLAUSES BY ORDERING
%% f(A):- inv1(A)
%% inv2(A):- q(A) (C1)
%% inv1(A):- inv2(A) (C2)
:-
    C1 > 0,
    C2 > 0,
    head_literal(C2,P1,_,_),
    head_literal(C1,P2,_,_),
    lower(P1,P2),
    C2 > C1.

%% FORCE ORDERING
%% inv2(A):- inv1(A)
:-
    C > 0,
    head_literal(C,Inv2,_,_),
    body_literal(C,Inv1,_,_),
    lower(Inv1,Inv2).

%% USE INVENTED SYMBOLS IN ORDER
%% f(A):- inv2(A)
%% inv2(A):- q(A)
%% TODO: ENFORCE ONLY ON ONE DIRECTLY BELOW
:-
    invented(Inv2,_),
    invented(Inv1,_),
    head_literal(_,Inv2,_,_),
    lower(Inv1,Inv2),
    not head_literal(_,Inv1,_,_).

%% PREVENT DUPLICATE INVENTED CLAUSES
%% f(A,B):-inv1(A,C),inv2(C,B).
%% inv1(A,B):-right(A,C),right(C,B).
%% inv2(A,B):-right(A,C),right(C,B).
%% TODO: GENERALISE FOR MULTIPLE CLAUSES

:-
    C1 > 0,
    C2 > 0,
    C1 < C2,
    head_literal(C1,HeadPred1,A1,_),
    head_literal(C2,HeadPred2,A2,_),
    invented(HeadPred1,A1),
    invented(HeadPred2,A2),
    HeadPred1 != HeadPred2,
    %% not multiclause(HeadPred1,A1),
    %% not multiclause(HeadPred2,A2),
    body_literal(C2,P,_,Vars): body_literal(C1,P,_,Vars).

%% PREVENTS THIS:
%% p(A,B):-inv1(A,B).
%% inv1(A,B):-q(A,B).
%% TODO: DOUBLE CHECK!!
:-
    invented(P,A),
    clause_size(C,1),
    body_literal(C,P,A,_).

%% NO POINT INVENTING A SYMBOL IF IT ONLY HAS ONLY BODY LITERAL AND IS NOT A DISJUNCTION
%% f(A,B):-f1(A,C),f1(C,B).
%% f1(A,B):-right(A,B).
:-
    Clause > 0,
    invented(P,A),
    head_literal(Clause,P,A,_),
    clause_size(Clause,1),
    not multiclause(P,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INHERIT TYPE FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% X and Y should inherit the types of A and B respectively
var_type(C1,Var1,Type):-
    invented(P,A),
    C1 > 0,
    C1 != C2,
    head_literal(C1,P,A,Vars1),
    body_literal(C2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(C2,Var2,Type).

%% INHERIT TYPE FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% A and B should inherit the types of X and Y respectively
var_type(C2,Var2,Type):-
    invented(P,A),
    C1 > 0,
    C1 != C2,
    head_literal(C1,P,A,Vars1),
    body_literal(C2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(C1,Var1,Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INHERIT SAFETY FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% if A is safe then X is safe
%% safe_var(C1,Var1):-
%%     C1 > 0,
%%     invented(P,A),
%%     C1 != C2,
%%     head_literal(C1,P,A,Vars1),
%%     body_literal(C2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(C2,Var2).

%% INHERIT SAFETY FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% if Y is safe then B is safe
%% safe_var(C2,Var2):-
%%     C1 > 0,
%%     invented(P,A),
%%     C1 != C2,
%%     head_literal(C1,P,A,Vars1),
%%     body_literal(C2,P,A,Vars2),
%%     var_pos(Var1,Vars1,Pos),
%%     var_pos(Var2,Vars2,Pos),
%%     safe_var(C1,Var1).

%% INHERIT DIRECTION FROM BODY LITERALS
%% TODO: IMPROVE HORRIBLE HACK
%% direction(P1,Pos1,in):-
%%     invented(P1,A1),
%%     head_literal(Clause,P1,A1,Vars1),
%%     var_pos(Var,Vars1,Pos1),
%%     body_literal(Clause,P2,_,Vars2),
%%     var_pos(Var,Vars2,Pos2),
%%     direction(P2,Pos2,in),
%%     #count{P3,Vars3: body_literal(Clause,P3,_,Vars3),var_pos(Var,Vars3,Pos3),direction(P3,Pos2,out)} == 0.