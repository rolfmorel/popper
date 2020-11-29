max_vars(5).
max_body(3).
max_clauses(2).

%% Sum the elements of the list. In Haskell: foldl (+) 0 
%%
%% f(A,N):-
%%     empty(A), 
%%     zero(N).
%% f(A,N):-
%%     cons(HA, BA, A),
%%     f(BA, N2),
%%     sum(N2, HA, N).

% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).

modeh(f,2).
type(f,0,list).
type(f,1,int).
direction(f,0,in).
direction(f,1,out).

modeb(f,2).
%type(f,0,list).
%type(f,1,int).
%direction(f,0,in).
%direction(f,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(zero,1).
type(zero,0,int).
%%%% used to be out as direction
direction(zero,0,out).

modeb(sum,3).
type(sum,0,int).
type(sum,1,int).
type(sum,2,int).
direction(sum,0,in).
direction(sum,1,in).
direction(sum,2,out).

modeb(cons,3).
type(cons,0,int).
type(cons,1,list).
type(cons,2,list).
direction(cons,0,out).
direction(cons,1,out).
direction(cons,2,in).

#show var/4.
#show literal/4.
