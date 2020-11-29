%% max_vars(7).
%% max_body(7).
%% max_clauses(1).

max_vars(3).
max_body(2).
max_clauses(3).

%% :-
    %% not clause(1).
%% :-
    %% #count{Clause : head_literal(Clause,f,2,_)} < 2.

%% :-
    %% recursive.

modeh(f,2).
modeb(is_empty,1).
modeb(is_letter,1).
modeb(is_lowercase,1).
modeb(is_number,1).
modeb(is_space,1).
%% modeb(is_uppercase,1).
%% modeb(mk_lowercase,2).
modeb(mk_uppercase,2).
modeb(copyskip1,2).
%% modeb(copy1,2).
modeb(skip1,2).
modeb(f,2).

lower(f,inv1).
lower(inv1,inv2).
%% lower(inv2,inv3).
%% lower(inv3,inv4).
invented(inv1,2).
%% invented(inv2,2).
%% invented(inv3,2).
%% invented(inv4,2).

%% type(P,0,state):-
%%     pred(P,_).
%% type(P,1,state):-
%%     pred(P,2).

direction(P,0,in):-
    pred(P,_).
direction(P,1,out):-
    pred(P,2).

%% precon pab :- qa, rab.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,1,(0,)),
    body_literal(Clause,R,2,(0,1)),
    P != R,
    not lower(Q,P),
    not lower(R,P),
    clause_size(Clause,2).

%% postcon pab :- qab, rb.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,1)),
    body_literal(Clause,R,1,(1,)),
    P != Q,
    not lower(Q,P),
    not lower(R,P),
    clause_size(Clause,2).

%% chain pab :- qac, rcb.
meta_clause(Clause):-
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,R,2,(2,1)),
    P != R,
    P != Q,
    not lower(Q,P),
    not lower(R,P),
    clause_size(Clause,2).

%% tailrec pab :- qac, pcb
meta_clause(Clause):-
    Clause > 0,
    head_literal(Clause,P,2,(0,1)),
    body_literal(Clause,Q,2,(0,2)),
    body_literal(Clause,P,2,(2,1)),
    P != Q,
    not lower(Q,P),
    clause_size(Clause,2).

:-
    clause(Clause),
    not meta_clause(Clause).

%% asda1:-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.

%% asda2:-
%%     size(N),
%%     #sum{K+1,Clause : clause_size(Clause,K)} != N.

%% size(10).

