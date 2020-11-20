%% (base) ➜  robots popp exs.pl modes2.pl bk.pl
%% f(A,B) :- right(A,D),right(D,F),right(F,E),right(E,C),right(C,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes2.pl bk.pl  128.14s user 0.21s system 99% cpu 2:08.39 total

max_vars(7).
max_body(7).
max_clauses(3).

modeh(f,2).
modeb(up,2).
modeb(down,2).
modeb(left,2).
modeb(right,2).

pred(P,A):-
    modeh(P,A).
pred(P,A):-
    modeb(P,A).
direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).