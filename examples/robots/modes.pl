%% (base) ➜  robots time popp exs.pl modes.pl bk.pl --eval-timeout=0.01
%% f(A,B) :- right(A,D),right(D,F),right(F,E),right(E,C),right(C,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl   23.76s user 0.07s system 99% cpu 23.943 total

max_vars(6).
max_body(6).
max_clauses(3).

modeh(f,2).
type(f,0,world).
type(f,1,world).
direction(f,0,in).
direction(f,1,out).

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