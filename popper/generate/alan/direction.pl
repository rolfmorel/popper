%% ENSURE INPUT VARS ARE GROUND
num_in_args(P,N):-
    direction(P,_,_),
    #count{Pos : direction(P,Pos,in)} = N.

%% VAR SAFE IF:
%% - VAR IS HEAD INPUT VAR
safe_var(Clause,Var):-
    head_literal(Clause,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction(P,Pos,in).

%% VAR SAFE IF:
%% - VAR IS IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Clause,Var):-
    num_in_args(P,0),
    body_literal(Clause,P,_,Vars),
    var_member(Var,Vars).

%% VAR SAFE IF:
%% - VAR IS IN SAFE LITERAL
safe_var(Clause,Var):-
    safe_literal(Clause,P,Vars),
    var_member(Var,Vars).

%% LITERAL WITH 1 INPUT VAR IS SAFE IF VAR IS SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,1),
    body_literal(Clause,P,_,Vars),
    var_pos(Var1,Vars,Pos),
    direction(P,Pos,in),
    safe_var(Clause,Var1).

%% LITERAL WITH 2 INPUT VARS IS SAFE IF BOTH VARS ARE SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,2),
    body_literal(Clause,P,_,Vars),
    var_pos(Var1,Vars,Pos1),
    var_pos(Var2,Vars,Pos2),
    direction(P,Pos1,in),
    direction(P,Pos2,in),
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
    safe_var(Clause,Var1),
    safe_var(Clause,Var2).

%% SAFE VARS
:-
    direction(_,_,_),
    var_in_literal(Clause,_,_,Var),
    not safe_var(Clause,Var).
