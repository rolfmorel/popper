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

possible_clause(0..N-1):-
    max_clauses(N).

%% GUESS A SINGLE HEAD LITERAL
0 {head_literal(Clause,P,A,Vars) : modeh(P,A), head_vars(A,Vars)} 1:-
    possible_clause(Clause).

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).

clause(Clause):-
    head_literal(Clause,_,_,_).

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

%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
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

literal(Clause,P,Vars):-
    head_literal(Clause,P,_,Vars).
literal(Clause,P,Vars):-
    body_literal(Clause,P,_,Vars).

clause_var(Clause,Var):-
    head_var(Clause,Var).
clause_var(Clause,Var):-
    body_var(Clause,Var).

head_var(Clause,Var):-
    head_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
body_var(Clause,Var):-
    body_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).

%% VAR IS IN VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% VAR IS IN A LITERAL
var_in_literal(Clause,P,Vars,Var):-
    literal(Clause,P,Vars),
    var_member(Var,Vars).