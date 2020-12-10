%% ORDER BY CLAUSE SIZE
%% p(A)<-q(A),r(A). (C1)
%% p(A)<-s(A). (C2)
%% V1

bigger(C1,C2):-
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.
:-
    C2 > C1,
    not recursive_clause(C1,_,_),
    not recursive_clause(C2,_,_),
    same_head(C1,C2),
    bigger(C1,C2).
:-
    C2 > C1,
    recursive_clause(C1,_,_),
    recursive_clause(C2,_,_),
    same_head(C1,C2),
    bigger(C1,C2).


%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #sum{A,P : body_literal(Clause,P,A,_)} == N.

%% %% ORDER CLAUSES BY NUMBER OF VARS
%% :-
%%     C2 > C1,
%%     not recursive_clause(C1,P,A),
%%     not recursive_clause(C2,P,A),
%%     head_literal(C1,P,A,Vars),
%%     head_literal(C2,P,A,Vars),
%%     clause_size(C1,N),
%%     clause_size(C2,N),
%%     num_vars(C1,NumVars1),
%%     num_vars(C2,NumVars2),
%%     NumVars1 > NumVars2.