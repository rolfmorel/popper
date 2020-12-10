%% %% ##################################################
%% %% SUBSUMPTION
%% %% ##################################################
%% :-
%%     Clause1 < Clause2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

same_head(C1,C2):-
    C1 != C2,
    head_literal(C1,HeadPred,A,HeadVars),
    head_literal(C2,HeadPred,A,HeadVars).

:-
    same_head(C1,C2),
    body_literal(C2,P,_,Vars): body_literal(C1,P,_,Vars).