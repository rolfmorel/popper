#script (python)
import clingo
import itertools
def mkvar(var):
    return str(var)
def mkheadvars(arity):
    return tuple(str(x) for x in range(arity.number))
def lowervar(var):
    return str(var.number-1)
#end.

lower_var(@lowervar(V),@mkvar(V)):-
    V = 1..N,
    max_vars(N).
var(V):-
    lower_var(V,_).

clause_var(C,Var):-
    head_var(C,Var).
clause_var(C,Var):-
    body_var(C,Var).

head_var(C,Var):-
    head_literal(C,_,_,Vars),
    var_member(Var,Vars).
body_var(C,Var):-
    body_literal(C,_,_,Vars),
    var_member(Var,Vars).

var_member(Var,Vars):-
    var_pos(Var,Vars,_).

var_in_literal(C,P,Vars,Var):-
    literal(C,P,Vars),
    var_member(Var,Vars).

head_vars(A,@mkheadvars(A)):-
    modeh(_,A).
need_arity(A):-
    modeh(_,A).
need_arity(A):-
    modeb(_,A).

%% POSSIBLE VARIABLE COMBINATIONS
%% TODO: GENERALISE
vars(1,(Var1,)):-
    need_arity(1),
    var(Var1).
vars(2,(Var1,Var2)):-
    need_arity(2),
    var(Var1),
    var(Var2),
    Var1 != Var2.
vars(3,(Var1,Var2,Var3)):-
    need_arity(3),
    var(Var1),
    var(Var2),
    var(Var3),
    Var1 != Var2,
    Var1 != Var3,
    Var2 != Var3.
vars(4,(Var1,Var2,Var3,Var4)):-
    need_arity(4),
    var(Var1),
    var(Var2),
    var(Var3),
    var(Var4),
    Var1 != Var2,
    Var1 != Var3,
    Var1 != Var4,
    Var2 != Var3,
    Var2 != Var4,
    Var3 != Var4.

%% POSITION OF VAR IN VARS
%% TODO: GENERALISE
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