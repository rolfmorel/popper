max_vars(15).
max_body(14).
max_clauses(2).

modeh(f,2).
modeb(right,2).
%% modeb(f,2).

direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).

:-
    not clause_size(C,1),
    body_literal(C,_,_,(0,B)),
    B != 2.

:-
    not clause_size(C,1),
    body_literal(C,_,_,(A,B)),
    A != 0,
    B != 1,
    B != A+1.

:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} != 2.