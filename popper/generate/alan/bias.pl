%% DATALOG
:-
    head_var(C,Var),
    not body_var(C,Var).

%% ELIMINATE SINGLETONS
:-
    clause_var(C,Var),
    #count{P,Vars : var_in_literal(C,P,Vars,Var)} == 1.

%% MUST BE CONNECTED
head_connected(C,Var):-
    head_var(C,Var).
head_connected(C,Var1):-
    Var1 > 0,
    head_connected(C,Var2),
    body_literal(C,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2.
:-
    Var > 0,
    body_var(C,Var),
    not head_connected(C,Var).

%% IRREFLEXIVE
%% prevents: p(A):-q(A,B),q(B,A)
:-
    irreflexive(P,2),
    body_literal(C,P,2,Vars1),
    body_literal(C,P,2,Vars2),
    Vars1 = (Var1,Var2),
    Vars2 = (Var2,Var1),
    Vars1 < Vars2.

%% FUNCTIONAL
%% prevents: p(A):-q(A,B),q(A,C)
:-
    functional(P,2),
    body_literal(Clause,P,2,Vars1),
    body_literal(Clause,P,2,Vars2),
    Vars1 = (Var1,Var2),
    Vars2 = (Var1,Var3),
    Var2 != Var3,
    Vars1 < Vars2.

%% FUNCTIONAL FOR 3
%% TODO: GENERALISE AND REMOVE SYMMETRY
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.