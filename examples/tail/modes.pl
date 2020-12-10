max_vars(10).
max_body(9).
max_clauses(1).

modeh(f,2).
type(f,0,list).
type(f,1,element).
%% modeb(f,2).

modeb(tail,2).
type(tail,0,list).
type(tail,1,list).

modeb(head,2).
type(head,0,list).
type(head,1,element).

%% modeb(empty,1).
%% type(empty,0,list).

direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).