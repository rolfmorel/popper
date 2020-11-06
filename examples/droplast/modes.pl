%% (base) ➜  droplast time popp exs.pl modes-fast.pl bk.pl --eval-timeout=0.01
%% f(A,B) :- tail(A,B).
%% f(A,B) :- head(A,D),tail(A,C),f(C,E),cons(D,E,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes-fast.pl bk.pl  8.27s user 0.08s system 99% cpu 8.357 total


max_vars(5).
max_body(5).
max_clauses(2).

modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).
modeb(f,2).

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


%% body_pred(tail, (list,list), (in,out)).
%% body_pred(head, (list,element), (in,out)).
%% body_pred(head, (list,element), (in,out)).


%% modeb(last,2).
%% type(last,0,list).
%% type(last,1,element).
%% direction(last,0,in).
%% direction(last,1,out).

%% modeb(length,2).
%% type(length,0,list).
%% type(length,1,int).
%% direction(length,0,in).
%% direction(length,1,out).

%% modeb(sum,3).
%% type(sum,0,int).
%% type(sum,1,int).
%% type(sum,2,int).
%% direction(sum,0,in).
%% direction(sum,1,in).
%% direction(sum,2,out).

modeb(cons,3).
type(cons,0,element).
type(cons,1,list).
type(cons,2,list).
direction(cons,0,in).
direction(cons,1,in).
direction(cons,2,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(zero,1).
type(zero,0,int).
direction(zero,0,in).