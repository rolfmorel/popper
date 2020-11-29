max_vars(3).
max_body(3).
max_clauses(2).

%% EVEN
%% f(A):-zero(A).
%% f(A):-succ(B,A),succ(C,B),f(C).

% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).

modeh(f,1).
type(f,0,int).
direction(f,0,in).
modeb(f,1).

modeb(zero,1).
type(zero,0,int).
direction(zero,0,in).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,out).
direction(succ,1,in).
