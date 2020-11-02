%% ########################################
%% RECURSION
%% ########################################

non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

recursive:-
    recursive(Clause).

recursive(Clause):-
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,_).

%% IF RECURSIVE, THERE MUST BE A NON-RECURSIVE CLAUSE
has_base:-
    clause(Clause),
    not recursive(Clause).
:-
    recursive,
    not has_base.

%% DISALLOW TWO RECURSIVE CALLS
%% WHY DID WE ADD THIS??
:-
    recursive(C),
    head_literal(C,P,A,_),
    body_literal(C,P,A,V1),
    body_literal(C,P,A,V2),
    V1 != V2.

%% PREVENT LEFT RECURSION
%% TODO: GENERALISE FOR ARITY > 3
:-
    recursive(Clause),
    num_in_args(P,1),
    head_literal(Clause,P,A,Vars1),
    var_pos(Var,Vars1,Pos1),
    direction(P,Pos1,in),
    body_literal(Clause,P,A,Vars2),
    var_pos(Var,Vars2,Pos2),
    direction(P,Pos2,in).

:-
    recursive(Clause),
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
