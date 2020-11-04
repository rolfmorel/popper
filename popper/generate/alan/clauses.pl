%% ORDER BY CLAUSE SIZE
%% p(A)<-q(A),r(A). (CLAUSE1)
%% p(A)<-s(A). (CLAUSE2)
%% V1
:-
    Clause2 > Clause1,
    not recursive_clause(Clause1,P,A),
    not recursive_clause(Clause2,P,A),
    head_literal(Clause1,P,A,Vars),
    head_literal(Clause2,P,A,Vars),
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    N1 > N2.

%% %% ORDER BY CLAUSE SIZE
%% %% V2
%% bigger_clause(Clause1,Clause2):-
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 > N2.
%% %% ORDER BY CLAUSE SIZE
%% :-
%%     Clause1 < Clause2,
%%     not recursive(Clause1),
%%     not recursive(Clause2),
%%     head_literal(Clause1,P,A,_),
%%     head_literal(Clause2,P,A,_),
%%     bigger_clause(Clause1,Clause2).

%% :-
%%     Clause2 > Clause1,
%%     recursive(Clause1),
%%     recursive(Clause2),
%%     head_literal(Clause1,P,A,_),
%%     head_literal(Clause2,P,A,_),
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 > N2.
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