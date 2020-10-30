%% ########################################
%% MULTIPLE CLAUSES
%% ########################################

multiclause:-
    clause(1).

%% SUBSUMPTION
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    N1 <= N2,
    Clause1 != Clause2, %% TODO - CAN WE CHANGE != TO < ?
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

%% TODO: ORDER RECURSIVE CLAUSES BY SIZE

%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #count{Var : body_literal(Clause,_,_,Vars), var_member(Var,Vars)} == N.

%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #count{Var : clause_var(Clause,Var)} == N.

%% %% %% ORDER CLAUSES BY NUMBER OF VARS
%% a:-
%%     clause_size(C1,N),
%%     clause_size(C2,N),
%%     C2 > C1,
%%     num_vars(C1,NumVars1),
%%     num_vars(C2,NumVars2),
%%     NumVars1 > NumVars2.