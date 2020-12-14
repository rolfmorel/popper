%% ORDER CLAUSES BY ORDERING
%% f(A):- inv1(A)
%% inv2(A):- q(A) (C1)
%% inv1(A):- inv2(A) (C2)
:-
    C1 > 0,
    C2 > 0,
    C2 > C1,
    head_literal(C1,P2,_,_),
    head_literal(C2,P1,_,_),
    lower(P1,P2).

before(C1,C2):-
    C1 < C2,
    head_literal(C1,P,_,_),
    head_literal(C2,Q,_,_),
    lower(P,Q).

before(C1,C2):-
    C1 < C2,
    head_literal(C1,P,_,_),
    head_literal(C2,P,_,_),
    not recursive_clause(C1,P,A),
    recursive_clause(C2,P,A).

count_lower(P,N):-
    head_literal(_,P,_,_),
    #count{Q : lower(Q,P)} == N.

min_clause(C,N+1):-
    C > 0,
    recursive_clause(C,P,A),
    count_lower(P,N).

min_clause(C,N):-
    head_literal(C,P,A,_),
    not recursive_clause(C,P,A),
    count_lower(P,N).