max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.
