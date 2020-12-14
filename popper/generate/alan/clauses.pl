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
    C1 > 0,
    C2 > C1,
    recursive_clause(C1,_,_),
    recursive_clause(C2,_,_),
    same_head(C1,C2),
    bigger(C1,C2).