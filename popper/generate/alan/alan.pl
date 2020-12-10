%% ALAN12
#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

#show head_literal/4.
#show body_literal/4.

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

%% GUESS HEAD LITERALS
{head_literal(Clause,P,A,Vars)}:-
    head_aux(P,A),
    head_vars(A,Vars),
    Clause = 0..N-1,
    max_clauses(N).

%% GUESS BODY LITERALS
{body_literal(Clause,P,A,Vars)}:-
    body_aux(P,A),
    vars(A,Vars),
    clause(Clause).

%% COUNT BODY LITERALS
clause_size(Clause,N):-
    clause(Clause),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P,Vars : body_literal(Clause,P,_,Vars)} = N.


:-
    clause(C),
    #count{P,A : head_literal(C,P,A,_)} != 1.

%% OBEY SIZE
:-
    clause(C),
    not clause_size(C,_).

%% THERE IS A CLAUSE IF THERE IS A HEAD LITERAL
clause(Clause):-
    head_literal(Clause,_,_,_).

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
    clause(Clause),
    Clause > 1,
    not clause(Clause-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause_var(Clause,Var),
    Var > 1,
    not clause_var(Clause,Var-1).

multiclause(P,A):-
    head_literal(C1,P,A,_),
    head_literal(C2,P,A,_),
    C1 < C2.

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
pred(P,A):-
    invented(P,A).