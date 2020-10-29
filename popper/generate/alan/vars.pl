%% %% POSSIBLE VARIABLE COMBINATIONS
%% %% TODO: GENERALISE FOR ARITIES > 3
%% vars(1,(Var1,)):-
%%     need_arity(1),
%%     var(Var1).
%% vars(2,(Var1,Var2)):-
%%     need_arity(2),
%%     var(Var1),
%%     var(Var2),
%%     Var1 != Var2.
%% vars(3,(Var1,Var2,Var3)):-
%%     need_arity(3),
%%     var(Var1),
%%     var(Var2),
%%     var(Var3),
%%     Var1 != Var2,
%%     Var1 != Var3,
%%     Var2 != Var3.

%% %% POSITION OF VAR IN VARS
%% %% TODO: GENERALISE FOR ARITIES > 3
%% var_pos(Var1,(Var1,),0):-
%%     vars(1,(Var1,)).
%% var_pos(Var1,(Var1,Var2),0):-
%%     vars(2,(Var1,Var2)).
%% var_pos(Var2,(Var1,Var2),1):-
%%     vars(2,(Var1,Var2)).
%% var_pos(Var1,(Var1,Var2,Var3),0):-
%%     vars(3,(Var1,Var2,Var3)).
%% var_pos(Var2,(Var1,Var2,Var3),1):-
%%     vars(3,(Var1,Var2,Var3)).
%% var_pos(Var3,(Var1,Var2,Var3),2):-
%%     vars(3,(Var1,Var2,Var3)).


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