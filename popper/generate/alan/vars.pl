var(0..N-1):-
    max_vars(N).

%% mode(P,A):-
%%     modeh(P,A).
%% mode(P,A):-
%%     modeb(P,A).
%% max_arity(N):-
%%     #max{A :
%%         mode(_,A)
%%     } == N.

%% max_vars_(MaxVars):-
%%     max_vars(MV),
%%     max_body(M),
%%     max_arity(A),
%%     X = 1 + (M*A),
%%     #min{X, MV : } = MaxVars.


%% max_vars_(MaxVars):-
%%     max_vars(MV),
%%     max_body(M),
%%     max_arity(A),
%%     MaxVars = 1 + (M*A),
%%     MaxVars >= MV.

%% TODO: GENERALISE FOR ARITIES > 4
head_vars(1,(0,)).
head_vars(2,(0,1)).
head_vars(3,(0,1,2)).
head_vars(4,(0,1,2,3)).

%% POSSIBLE VARIABLE COMBINATIONS
%% TODO: GENERALISE FOR ARITIES > 4
vars(1,(Var1,)):-
    var(Var1).
vars(2,(Var1,Var2)):-
    var(Var1),
    var(Var2),
    Var1 != Var2.
vars(3,(Var1,Var2,Var3)):-
    vars(2,(Var1,Var2)),
    var(Var3),
    Var1 != Var3,
    Var2 != Var3.
vars(4,(Var1,Var2,Var3,Var4)):-
    vars(3,(Var1,Var2,Var3)),
    var(Var4),
    Var1 != Var4,
    Var2 != Var4,
    Var3 != Var4.

%% POSITION OF VAR IN VARS
%% TODO: GENERALISE FOR ARITIES > 3
var_pos(Var1,(Var1,),0):-
    vars(1,(Var1,)).
var_pos(Var1,(Var1,Var2),0):-
    vars(2,(Var1,Var2)).
var_pos(Var2,(Var1,Var2),1):-
    vars(2,(Var1,Var2)).
var_pos(Var1,(Var1,Var2,Var3),0):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var2,(Var1,Var2,Var3),1):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var3,(Var1,Var2,Var3),2):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var1,(Var1,Var2,Var3,Var4),0):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var2,(Var1,Var2,Var3,Var4),1):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var3,(Var1,Var2,Var3,Var4),2):-
    vars(4,(Var1,Var2,Var3,Var4)).
var_pos(Var4,(Var1,Var2,Var3,Var4),3):-
    vars(4,(Var1,Var2,Var3,Var4)).