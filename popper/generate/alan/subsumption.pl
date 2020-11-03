%% ##################################################
%% SUBSUMPTION
%% ##################################################
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    %% Clause1 != Clause2,
    Clause1 < Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).