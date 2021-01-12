%% ALAN12
#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

#show head_literal/4.
#show pbody_literal/4.
#show nbody_literal/4.

#include "pi.pl".
#include "types.pl".
#include "recursion.pl".
#include "subsumption.pl".
#include "clauses.pl".
#include "direction.pl".
#include "bias.pl".
#include "vars.pl".
#include "ordering.pl".

head_aux(P,A):-
    modeh(P,A).
head_aux(P,A):-
    invented(P,A).

body_aux(P,A):-
    modeb(P,A).
body_aux(P,A):-
    invented(P,A).

{head_literal(C,P,A,Vars)}:-
    head_aux(P,A),
    index(P,I),
    C >= I,
    head_vars(A,Vars),
    max_clauses(N),
    C = 0..N-1.

{pbody_literal(C,P,A,Vars)}:-
    body_aux(P,A),
    vars(A,Vars),
    clause(C).

%% {nbody_literal(C,P,A,Vars)}:-
%%     body_aux(P,A),
%%     vars(A,Vars),
%%     clause(C).

:-
    pbody_literal(C,P,A,Vs),
    nbody_literal(C,P,A,Vs).

body_literal(C,P,A,Vars):-
    pbody_literal(C,P,A,Vars).
body_literal(C,P,A,Vars):-
    nbody_literal(C,P,A,Vars).

%% NO MORE THAN ONE HEAD LITERAL PER CLAUSE
:-
    clause(C),
    #count{P,A : head_literal(C,P,A,_)} > 1.

%% OBEY SIZE
:-
    clause(C),
    not clause_size(C,_).

%% CLAUSE IF THERE IS A HEAD LITERAL
clause(C):-
    head_literal(C,_,_,_).

%% COUNT BODY LITERALS
%% TODO: IMPROVE
clause_size(C,N):-
    clause(C),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(C,P,_,Vars)} == N.

%% COUNT CLAUSES
num_clauses(P,N):-
    head_literal(_,P,_,_),
    #count{C : head_literal(C,P,_,_)} == N.

literal(C,P,Vars):-
    head_literal(C,P,_,Vars).
literal(C,P,Vars):-
    body_literal(C,P,_,Vars).

%% ENSURE A CLAUSE
:-
    not clause(0).

%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(C,P,_,Vars),
    body_literal(C,P,_,Vars).

%% USE CLAUSES IN ORDER
:-
    clause(C),
    C > 1,
    not clause(C-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(C,Var),
    Var > 1,
    not clause_var(C,Var-1).

multiclause(P,A):-
    head_literal(_,P,A,_),
    not num_clauses(P,1).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
pred(P,A):-
    invented(P,A).

