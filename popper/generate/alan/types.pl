%% TYPE MATCHING
%% V2 AND V4 SEEM TO BE BEST

%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% %% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 :
%%         type(P,Pos1,Type1),
%%         var_pos(Var,Vars1,Pos1),
%%         var_in_literal(Clause,P,Vars1,Var)
%%     } > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% %% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

