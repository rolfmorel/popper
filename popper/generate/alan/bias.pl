%% DATALOG
:-
    head_var(Clause,Var),
    not body_var(Clause,Var).

%% ELIMINATE SINGLETONS
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% %% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
:-
    body_var(Clause,Var),
    not head_connected(Clause,Var).


%% head_connected(Clause,Var):-
%%     head_var(Clause,Var).
%% head_connected(Clause,Var1):-
%%     %% Var1 > 0,
%%     head_connected(Clause,Var2),
%%     body_literal(Clause,_,_,Vars),
%%     var_member(Var1,Vars),
%%     var_member(Var2,Vars),
%%     Var1 != Var2.

%% %% MUST BE CONNECTED
%% :-
%%     %% Var > 0,
%%     body_var(Clause,Var),
%%     not head_connected(Clause,Var).


%% REMOVE REFLEXIVE
%% prevents: p(A):-q(A,B),q(B,A)
:-
    irreflexive(P,2),
    literal(Clause,P,(Var1,Var2)),
    literal(Clause,P,(Var2,Var1)).

%% FUNCTIONAL
%% prevents: p(A):-q(A,B),q(A,C)
:-
    functional(P,2),
    literal(Clause,P,(Var1,Var2)),
    literal(Clause,P,(Var1,Var3)),
    Var2 != Var3.

%% FUNCTIONAL FOR 3 - FIX
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.