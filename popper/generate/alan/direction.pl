%%%%%%%%%%
%% ENSURES INPUT VARS ARE GROUND
%%%%%%%%%%

num_in_args(P,N):-
    direction(P,_,_),
    #count{Pos : direction(P,Pos,in)} == N.

%% VAR SAFE IF HEAD INPUT VAR
safe_var(Clause,Var):-
    head_literal(Clause,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction(P,Pos,in).

%% VAR SAFE IF IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Clause,Var):-
    num_in_args(P,0),
    body_literal(Clause,P,_,Vars),
    var_member(Var,Vars).

%% VAR SAFE IF IN SAFE LITERAL
safe_var(Clause,Var):-
    safe_literal(Clause,P,Vars),
    var_member(Var,Vars).

%% LITERAL WITH N INPUT VARS IS SAFE IF N VARS ARE SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,N),
    body_literal(Clause,P,_,Vars),
    #count{Var,Pos :
        var_pos(Var,Vars,Pos),
        direction(P,Pos,in),
        safe_var(Clause,Var)
    } == N.

%% SAFE VARS
:-
    direction(_,_,_), % guard for when no directions are given
    var_in_literal(Clause,_,_,Var),
    not safe_var(Clause,Var).