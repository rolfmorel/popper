max_vars(5).
max_body(5).
max_clauses(3).

%% DROPLAST
%% f(A,B):-
%%     tail(A,C),
%%     empty(C),
%%     empty(B).
%% f(A,B):-
%%     tail(A,C),
%%     f(C,D),
%%     head(A,E),
%%     cons(E,D,B).

%% NEED TO EVENTUALLY ADD THIS CONSTRAINT TO THE MAIN ALAN ENCODING
%% PREVENT RECURSION IN THE FIRST CLAUSE
%% :-
%%     modeh(P,A),
%%     body_literal(0,_,P,A).

modeh(f,2).
type(f,0,world).
type(f,1,world).
direction(f,0,in).
direction(f,1,out).
%modeb(f,2).

modeb(up,2).
type(up,0,world).
type(up,1,world).
direction(up,0,in).
direction(up,1,out).

modeb(down,2).
type(down,0,world).
type(down,1,world).
direction(down,0,in).
direction(down,1,out).

modeb(left,2).
type(left,0,world).
type(left,1,world).
direction(left,0,in).
direction(left,1,out).

modeb(right,2).
type(right,0,world).
type(right,1,world).
direction(right,0,in).
direction(right,1,out).
