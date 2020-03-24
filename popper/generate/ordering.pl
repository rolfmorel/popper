%% LITERAL ORDERING
%% ORDER BY THE FIRST ARGUMENT
:-
    body_literal(Clause,Literal1,_,_),
    body_literal(Clause,Literal2,_,_),
    Literal2 > Literal1,
    var(Clause,Literal1,0,V1),
    var(Clause,Literal2,0,V2),
    V1 > V2.

%% IF ALL PREVIOUS ARGUMENTS ARE THE SAME
%% THEN ORDER BY THIS ARGUMENT
:-
    body_literal(Clause,Literal1,_,_),
    body_literal(Clause,Literal2,_,_),
    Literal2 > Literal1,
    Pos > 0,
    #count{V :
        var(Clause,Literal1,K,V),
        var(Clause,Literal2,K,V),
        K = 0..Pos-1
    } == Pos,
    var(Clause,Literal1,Pos,V1),
    var(Clause,Literal2,Pos,V2),
    V1 > V2.

%% IF ALL ARGUMENTS ARE THE SAME THEN ORDER BY PREDICATE SYMBOL
:-
    body_literal(Clause,Literal1,P,A),
    body_literal(Clause,Literal2,Q,A),
    P > Q,
    Literal2 > Literal1,
    #count{V :
        var(Clause,Literal1,K,V),
        var(Clause,Literal2,K,V),
        K = 0..A-1
    } == A.



%% appears_before(Clause,Literal1,Var):-
%%     literal(Clause,Literal1,_,_),
%%     literal(Clause,Literal2,_,_),
%%     Literal2 < Literal1,
%%     var(Clause,Literal2,_,Var).

%% :-
%%     var(_,_,0,1).
%% :-
%%     var(Clause,Literal,0,Var),
%%     Literal > 0,
%%     not appears_before(Clause,Literal,Var).