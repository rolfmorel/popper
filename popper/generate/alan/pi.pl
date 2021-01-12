%% ########################################
%% CLAUSES SPECIFIC TO PREDICATE INVENTION
%% ########################################

#defined invented/2.

index(P,0):-
    modeh(P,_).
index(inv1,1).
index(inv2,2).
index(inv3,3).
index(inv4,4).
index(inv5,5).

inv_lower(P,Q):-
    A > 0,
    A < B,
    index(P,A),
    index(Q,B).
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

%% AN INVENTED SYMBOL MUST APPEAR IN THE BODY OF A CLAUSE
appears_before(P,A):-
    invented(P,A),
    lower(Q,P),
    head_literal(C,Q,_,_),
    body_literal(C,P,_,_).

%% AN INVENTED SYMBOL MUST APPEAR IN THE BODY OF A CLAUSE
:-
    invented(P,A),
    not appears_before(P,A).

%% MUST INVENT IN ORDER
:-
    invented(P,_),
    inv_lower(Q,P),
    not invented(Q,_).

%% FORCE ORDERING
%% inv2(A):- inv1(A)
:-
    C > 0,
    head_literal(C,P,_,_),
    body_literal(C,Q,_,_),
    lower(Q,P).

%% USE INVENTED SYMBOLS IN ORDER
%% f(A):- inv2(A)
%% inv2(A):- q(A)
%% TODO: ENFORCE ONLY ON ONE DIRECTLY BELOW
%% :-
%%     invented(P,_),
%%     head_literal(_,P,_,_),
%%     inv_lower(Q,P),
%%     not head_literal(_,Q,_,_).

%% PREVENT DUPLICATE INVENTED CLAUSES
%% f(A,B):-inv1(A,C),inv2(C,B).
:-
    C1 > 0,
    C2 > 0,
    C1 < C2,
    lower(P,Q),
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    invented(P,_),
    invented(Q,_),
    same_body(C1,C2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% INHERIT TYPE FROM CALLING PREDICATE
%% p(A,B):-inv1(A,B). (C2)
%% inv1(X,Y):-q(X,Y). (C1)
%% X and Y should inherit the types of A and B respectively
var_type(C1,Var1,Type):-
    invented(P,A),
    C1 > C2,
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
    C1 > C2,
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


%% PRUNES SINGLE CLAUSE/LITERAL INVENTIONS
%% inv(A,B):-right(A,B).
:-
    invented(P,A),
    head_literal(Clause,P,A,_),
    clause_size(Clause,1),
    not multiclause(P,A).

%% PREVENTS SINGLE CLAUSE/LITERAL CALLS
%% f(A,B):-inv(A,B)
:-
    head_literal(C,P,Pa,_),
    invented(Q,Qa),
    body_literal(C,Q,Qa,_),
    clause_size(C,1),
    not multiclause(P,Pa).

only_once(P,A):-
    invented(P,A),
    head_literal(_,P,A,_),
    #count{C,Vars : body_literal(C,P,A,Vars)} == 1.

:-
    invented(P,A),
    head_literal(C1,P,A,_),
    not multiclause(P,A),
    only_once(P,A),
    C2 < C1,
    body_literal(C2,P,A,_),
    clause_size(C1,N1),
    clause_size(C2,N2),
    max_body(MaxN),
    N1 + N2 - 1 <= MaxN.




%% calls(C1,C2):-
%%     C2 > C1,
%%     body_literal(C1,P,A,_),
%%     head_literal(C2,P,A,_).

%% a:-
%%     calls(C1,C2),
%%     body_literal(C1,P,A,Vs),
%%     body_literal(C2,P,A,Vs).
%% :-
%%     a.


%% :-
    %% not head_literal(_,inv2,_,_).



%% ap(A) :- inv1(A),x8(A).
%% inv1(A) :- inv2(A),x8(A).
%% inv2(A) :- x4(A),x8(A).


%% f(A,B):- good(A), inv1(A,B).
%% inv1(A,B):- good(A),


%% ap(A) :- inv1(A),x8(A).



%% inv1(A) :- inv2(A),x8(A).
%% inv2(A) :- x4(A),x8(A).
%% =>
%% inv1(A) :- x4(A),x8(A),x8(A).



%% ==========

% f(A,B) :- inv1(A,C),right(C,D),right(D,B).
% inv1(A,B) :- right(A,C),right(C,B).
% =>
% f(A,B) :- right(A,Z),right(Z,C),right(C,D),right(D,B).

% f(A,B) :- right(A,C),inv1(C,D),right(D,B).
% inv1(X,Y) :- right(X,Z),right(Z,Y).
% =>
% f(A,B) :- right(A,C),right(C,Z),right(Z,D),right(D,B).

% f(A,B) :- right(A,C),right(C,D),inv1(D,B).
% inv1(X,Y) :- right(X,Z),right(Z,Y).
% =>
% f(A,B) :- right(A,C),right(C,D),right(D,Z),right(Z,B).

%% ==========

% f(A,B):-inv1(A,B),wants_coffee(B).
% inv1(A,B):-pour_coffee(A,B),wants_coffee(B),wants_tea(A).

f :- a, inv1.
inv1 :- b, c.

calls(C1,C2):-
    comp_literal(C1,C2,_,_,_).

comp_literal(C1,C2,P,A,Vs):-
    C1 < C2,
    body_literal(C1,P,A,Vs),
    head_literal(C2,P,A).
    %% what about vars?

unfolded(C,P,A,Vs):-
    body_literal(C,P,A,Vs),
    not comp_literal(C,_,P,A,Vs).
unfolded(C1,P,A,Vs):-
    calls(C1,C2),
    body_literal(C2,P,)





%% a:-
%%     calls(C1,C2),
%%     body_literal(C1,P,A,Vs),
%%     body_literal(C2,P,A,Vs).
%% :-
%%     a.