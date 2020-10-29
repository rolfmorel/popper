%% ########################################
%% MULTIPLE CLAUSES
%% ########################################

multiclause:-
    clause(1).

%% %% REMOVE IDENTICAL CLAUSES
%% different(Clause1,Clause2):-
%%     head_literal(Clause1,P,_,_),
%%     head_literal(Clause2,Q,_,_),
%%     P != Q.
%% different(Clause1,Clause2):-
%%     head_literal(Clause1,_,_,Vars1),
%%     head_literal(Clause2,_,_,Vars2),
%%     Vars1 != Vars2.
%% different(Clause1,Clause2):-
%%     body_literal(Clause1,P,_,Vars),
%%     clause(Clause2),
%%     not body_literal(Clause2,P,_,Vars).

%% %% WEIRD: WHY IS THIS NEEDED?
%% different(Clause1,Clause2):-


%% REMOVE IDENTICAL CLAUSES
%% :-
%%     clause_size(Clause1,N),
%%     clause_size(Clause2,N),
%%     Clause1 != Clause2,
%%     head_literal(Clause1,P,A,PVars),
%%     head_literal(Clause2,P,A,PVars),
%%     body_literal(Clause1,Q,_,QVars) :  body_literal(Clause2,Q,_,QVars).

%% ENSURE CLAUSES ARE DIFFERENT
%% DUPLICATES THE SUBSUMPTION PART
%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     not different(Clause1,Clause2).

%% C1 SUBUMES C2 IF:
%% - C1 IS A SUBSET OF C2
%% subsumes(Clause1,Clause2):-
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     Clause1 != Clause2,
%%     #count{P, Vars : body_literal(Clause1,P,_,Vars), not body_literal(Clause2,P,_,Vars)} == 0.

%% DUPLICATES THE DIFFERENT PART
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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