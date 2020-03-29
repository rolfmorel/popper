%% %% PREVENT DUPLICATE LITERALS
%% %% CONTROL
:-
    literal(Clause,Literal1,P,A),
    literal(Clause,Literal2,P,A),
    Literal1 != Literal2,
    #count{Pos :
        var(Clause,Literal1,Pos,V),
        var(Clause,Literal2,Pos,V)
    } == A.

%% %% TREATMENT
%% different_literals(Clause,Literal1,Literal2):-
%%     body_literal(Clause,Literal1,P,_),
%%     body_literal(Clause,Literal2,Q,_),
%%     Literal1 != Literal2,
%%     P != Q.

%% different_literals(Clause,Literal1,Literal2):-
%%     body_literal(Clause,Literal1,_,A1),
%%     body_literal(Clause,Literal2,_,A2),
%%     A1 != A2.

%% different_literals(Clause,Literal1,Literal2):-
%%     body_literal(Clause,Literal1,_,_),
%%     body_literal(Clause,Literal2,_,_),
%%     Literal1 != Literal2,
%%     var(Clause,Literal1,Pos,Var1),
%%     var(Clause,Literal2,Pos,Var2),
%%     Var1 != Var2.

%% :-
%%     literal(Clause,Literal1,P,A),
%%     literal(Clause,Literal2,P,A),
%%     Literal1 != Literal2,
%%     not different_literals(Clause,Literal1,Literal2).