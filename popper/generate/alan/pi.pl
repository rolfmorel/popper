%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################

#defined invented/2.

%% IF AN INVENTED SYMBOL IS IN THE HEAD OF A CLAUSE IT MUST ALSO APPEAR IN THE BODY OF A CLAUSE
:-
    invented(P,A),
    head_literal(_,P,A,_),
    not body_literal(_,P,A,_).

%% IF AN INVENTED SYMBOL IS IN THE BODY OF A CLAUSE THEN IT MUST ALSO APPEAR IN THE HEAD OF A CLAUSE
:-
    invented(P,A),
    body_literal(_,P,A,_),
    not head_literal(_,P,A,_).

%% PREVENT THE FIRST CLAUSE BEING INVENTED
:-
    not invented(P1,A1),
    invented(P2,A2),
    head_literal(C1,P1,A1,_),
    head_literal(C2,P2,A2,_),
    C2 < C1.

%% PREVENTS THIS:
%% p(A,B):-inv1(A,B).
%% inv1(A,B):-q(A,B).
%% TODO: DOUBLE CHECK!!
:-
    invented(P,A),
    clause_size(C,1),
    body_literal(C,P,A,_).

multiclause(P,A):-
    head_literal(Clause1,P,A,_),
    head_literal(Clause2,P,A,_),
    Clause1 != Clause2.

%% NO POINT INVENTING A SYMBOL IF IT ONLY HAS ONLY BODY LITERAL AND IS NOT A DISJUNCTION
%% f(A,B):-f1(A,C),f1(C,B).
%% f1(A,B):-right(A,B).
:-
    invented(P,A),
    head_literal(Clause,P,A,_),
    clause_size(Clause,1),
    not multiclause(P,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INHERIT TYPE FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% X and Y should inherit the types of A and B respectively
var_type(Clause1,Var1,Type):-
    invented(P,A),
    Clause1 > 0,
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(Clause2,Var2,Type).

%% INHERIT TYPE FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% A and B should inherit the types of X and Y respectively
var_type(Clause2,Var2,Type):-
    invented(P,A),
    Clause1 > 0,
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    var_type(Clause1,Var1,Type).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INHERIT SAFETY FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% if A is safe then X is safe
safe_var(Clause1,Var1):-
    invented(P,A),
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    safe_var(Clause2,Var2).

%% INHERIT SAFETY FROM CALLED PREDICATE
%% p(A,B):-inv1(A,B). (clause2)
%% inv1(X,Y):-q(X,Y). (clause1)
%% if Y is safe then B is safe
safe_var(Clause2,Var2):-
    invented(P,A),
    Clause1 != Clause2,
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    safe_var(Clause1,Var1).

%% INHERIT DIRECTION FROM BODY LITERALS
%% TODO: IMPROVE HORRIBLE HACK
direction(P1,Pos1,in):-
    invented(P1,A1),
    head_literal(Clause,P1,A1,Vars1),
    var_pos(Var,Vars1,Pos1),
    body_literal(Clause,P2,_,Vars2),
    var_pos(Var,Vars2,Pos2),
    direction(P2,Pos2,in),
    #count{P3,Vars3: body_literal(Clause,P3,_,Vars3),var_pos(Var,Vars3,Pos3),direction(P3,Pos2,out)} == 0.