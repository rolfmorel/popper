%% ########################################
%% RECURSION
%% ########################################
non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive_clause(0).

recursive_clause(Clause):-
    Clause > 0,
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,_).

recursive_pred(P,A):-
    Clause > 0,
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,_).

has_base(P,A):-
    modeb(P,A),
    head_literal(Clause,P,A,_),
    not recursive_clause(Clause).

:-
    recursive_pred(P,A),
    not has_base(P,A).

%% DISALLOW TWO RECURSIVE CALLS
%% WHY DID WE ADD THIS??
:-
    Clause > 0,
    recursive_clause(Clause),
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,Vars1),
    body_literal(Clause,P,A,Vars2),
    Vars1 < Vars2.

%% PREVENT LEFT RECURSION
%% TODO: GENERALISE FOR ARITY > 3
:-
    Clause > 0,
    num_in_args(P,1),
    head_literal(Clause,P,A,Vars1),
    body_literal(Clause,P,A,Vars2),
    var_pos(Var,Vars1,Pos1),
    var_pos(Var,Vars2,Pos2),
    direction(P,Pos1,in),
    direction(P,Pos2,in).

%% TODO: REFACTOR
:-
    Clause > 0,
    num_in_args(P,2),
    %% get the head input vars
    head_literal(Clause,P,A,HeadVars),
    var_pos(HeadVar1,HeadVars,Pos1),
    var_pos(HeadVar1,HeadVars,Pos2),
    HeadPos1 != HeadPos2,
    direction(P,HeadPos1,in),
    direction(P,HeadPos2,in),
    %% get the body input vars
    body_literal(Clause,P,A,BodyVars),
    var_pos(HeadVar1,BodyVars,BodyPos1),
    var_pos(HeadVar2,BodyVars,BodyPos2),
    BodyPos1 != BodyPos2,
    direction(P,BodyPos1,in),
    direction(P,BodyPos2,in).
