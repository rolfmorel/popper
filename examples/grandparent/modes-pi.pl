max_vars(3).
max_body(2).
max_clauses(4).

modeh(grandparent,2).
modeb(mother,2).
modeb(father,2).

modeh(inv1,2).
modeb(inv1,2).
invented(inv1,2).


%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V2)),
    body_literal(Clause,R,2,(V2,V1)),
    V0!=V1,V0!=V2,V1!=V2,
    clause_size(Clause,2).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V1)),
    V0!=V1,
    clause_size(Clause,1).
:-
    clause(Clause),
    not meta_clause(Clause).