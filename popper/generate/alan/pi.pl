%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################

#defined invented/2.

%% non_invented_head:-
    %% modeh(P,A),
    %% head_literal(P,A,_,_),
    %% not invented(P,A).

%% asda1(N):-
  %% #count{P,A : head_literal(_,P,A,_)} == N.
:-
  #count{P,A : head_literal(_,P,A,_), not invented(P,A)} == 0.
%% asda3(N):-
  %% #count{P,A : head_literal(_,P,A,_), invented(P,A)} == N.

%% tmp1(P/A):-
%%     head_literal(P,A,_,_),
%%     invented(P,A).

%% #show asda1/1.
%% #show asda2/1.
%% #show asda3/1.
%% #show tmp1/1.
%% #show invented/2.

:-
    not clause(1).

%% IF AN INVENTED SYMBOL IS IN THE HEAD OF A CLAUSE IT MUST ALSO APPEAR IN THE BODY OF A CLAUSE
:-
    head_literal(_,P,A,_),
    invented(P,A),
    not body_literal(_,P,A,_).

%% IF AN INVENTED SYMBOL IS IN THE BODY OF A CLAUSE THEN IT MUST ALSO APPEAR IN THE HEAD OF A CLAUSE
:-
    body_literal(_,P,A,_),
    invented(P,A),
    not head_literal(_,P,A,_).

%% PREVENT THE FIRST CLAUSE BEING INVENTED
:-
    head_literal(C1,P1,A1,_),
    head_literal(C2,P2,A2,_),
    not invented(P1,A1),
    invented(P2,A2),
    C2 < C1.

%% PREVENTS THIS:
%% p(A,B):-inv1(A,B).
%% inv1(A,B):-q(A,B).
%% TODO: DOUBLE CHECK!!
:-
    clause_size(C,1),
    body_literal(C,P,A,_),
    invented(P,A).

multiclause(P,A):-
    head_literal(Clause1,P,A,_),
    head_literal(Clause2,P,A,_),
    Clause1 != Clause2.

%% NO POINT INVENTING A SYMBOL IF IT ONLY HAS ONLY BODY LITERAL AND IS NOT A DISJUNCTION
%% f(A,B):-f1(A,C),f1(C,B).
%% f1(A,B):-right(A,B).
:-
    head_literal(Clause,P,A,_),
    invented(P,A),
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
    Clause1 > 0,
    Clause1 != Clause2,
    invented(P,A),
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
    Clause1 > 0,
    Clause1 != Clause2,
    invented(P,A),
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
    Clause1 != Clause2,
    invented(P,A),
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
    Clause1 != Clause2,
    invented(P,A),
    head_literal(Clause1,P,A,Vars1),
    body_literal(Clause2,P,A,Vars2),
    var_pos(Var1,Vars1,Pos),
    var_pos(Var2,Vars2,Pos),
    safe_var(Clause1,Var1).

