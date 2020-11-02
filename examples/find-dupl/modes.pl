max_vars(3).
max_body(2).
max_clauses(4).


modeh(f,2).
%% modeb(mergesort,2).
modeb(tail,2).
modeb(head,2).
modeb(element,2).
%% modeb(mylast,2).
modeb(f,2).


type(f,0,list).
type(f,1,element).
type(tail,0,list).
type(tail,1,tail).
type(head,0,list).
type(head,1,element).
type(element,0,list).
type(element,1,element).


%% type(f,0,list).
%% type(f,1,element).
%% type(tail,0,list).
%% type(tail,1,tail).
%% type(head,0,list).
%% type(head,1,element).

direction(f,0,in).
direction(f,1,out).
direction(tail,0,in).
direction(tail,1,out).
direction(head,0,in).
direction(head,1,out).
direction(element,0,in).
direction(element,1,out).
%% type(head,0,list).
%% type(head,1,element).

modeh(inv1,2).
modeb(inv1,2).
invented(inv1,2).

%% %% metarules
%% metarule(dident, [P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
%% metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
%% metarule(tailrec, [P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).


%% P(A,B)<-Q(A,B),R(A,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V1)),
    body_literal(Clause,R,2,(V0,V1)),
    V0!=V1,
    clause_size(Clause,2).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(V0,V1)),
    body_literal(Clause,Q,2,(V0,V2)),
    body_literal(Clause,R,2,(V2,V1)),
    V0!=V1,V0!=V2,V1!=V2,
    clause_size(Clause,2).

:-
    clause(Clause),
    not meta_clause(Clause).