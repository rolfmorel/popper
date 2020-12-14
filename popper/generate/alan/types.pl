%% TYPE MATCHING
var_type(C,Var,Type):-
    var_in_literal(C,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(C,Var),
    #count{Type : var_type(C,Var,Type)} > 1.