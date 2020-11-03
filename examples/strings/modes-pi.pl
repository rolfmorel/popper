%% max_vars(7).
%% max_body(7).
%% max_clauses(1).

max_vars(3).
max_body(2).
max_clauses(2).

modeh(f,2).
%% modeb(is_empty,1).
%% modeb(is_letter,1).
%% modeb(is_lowercase,1).
%% modeb(is_number,1).
%% modeb(is_space,1).
%% modeb(is_uppercase,1).
%% modeb(mk_lowercase,2).
%% modeb(mk_uppercase,2).
modeb(copyskip1,2).
%% modeb(copy1,2).
modeb(skip1,2).
modeb(f,2).

%% direction(f,0,in).
%% direction(f,1,out).
%% direction(copyskip1,0,in).
%% direction(copyskip1,1,out).
%% %% direction(mk_uppercase,0,in).
%% %% direction(mk_uppercase,1,out).
%% direction(copy1,0,in).
%% direction(copy1,1,out).
%% direction(skip1,0,in).
%% direction(skip1,1,out).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
%% direction(P,0,in):-
    %% pred(P,_).
%% direction(P,1,out):-
    %% pred(P,2).
type(P,0,state):-
    pred(P,_).
type(P,1,state):-
    pred(P,2).

lower(f,inv1).
invented(inv1,2).
modeh(inv1,2).
modeb(inv1,2).
type(inv1,0,state).
type(inv1,1,state).
%% direction(inv1,0,in).
%% direction(inv1,1,out).

%% P(A,B)<-Q(A,C),R(C,B).
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    clause_size(Clause,2).
:-
    clause(Clause),
    not meta_clause(Clause).