%% ALAN12
#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

#show head_literal/4.
#show body_literal/4.

#include "types.pl".
#include "recursion.pl".
#include "clauses.pl".
#include "direction.pl".
#include "bias.pl".
#include "vars.pl".

%% GUESS A SINGLE HEAD LITERAL
0 {head_literal(Clause,P,A,Vars) : modeh(P,A), head_vars(A,Vars)} 1:-
    Clause = 0..N-1,
    max_clauses(N).

%% %% GUESS AT LEAST 1 BUT AT MOST N BODY LITERALS
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).

%% THERE IS A CLAUSE IF WE HAVE GUESSED THE HEAD LITERAL
clause(Clause):-
    head_literal(Clause,_,_,_).

literal(Clause,P,Vars):-
    head_literal(Clause,P,_,Vars).
literal(Clause,P,Vars):-
    body_literal(Clause,P,_,Vars).

%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).

%% USE CLAUSES IN ORDER
:-
    clause(Clause),
    Clause > 0,
    not clause(Clause-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause(Clause),
    var_in_literal(Clause,_,_,Var),
    Var > 0,
    not var_in_literal(Clause,_,_,Var-1).

%% ENSURE A CLAUSE
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
%% :-
%%     size(N),
%%     #sum{Size+1 : clause_size(Clause,Size)} != N.