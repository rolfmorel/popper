%% max_vars(7).
%% max_body(5).
%% max_clauses(3).

max_vars(4).
max_body(5).
max_clauses(2).

%% max_vars(4).
%% max_body(5).
%% max_clauses(1).

modeh(f,1).
type(f,0,list).
direction(f,0,in).
modeb(f,1).

modeb(tail,2).
type(tail,0,list).
type(tail,1,list).
direction(tail,0,in).
direction(tail,1,out).

modeb(head,2).
type(head,0,list).
type(head,1,element).
direction(head,0,in).
direction(head,1,out).

%% works without this
modeb(last,2).
type(last,0,list).
type(last,1,element).
direction(last,0,in).
direction(last,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(zero,1).
type(zero,0,element).
direction(zero,0,in).

modeb(even,1).
type(even,0,element).
direction(even,0,in).
