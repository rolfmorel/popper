%% (base) ➜  robots popp exs.pl modes.pl bk.pl
%% f(A,B) :- right(A,C),right(C,D),right(D,F),right(F,E),right(E,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  81.60s user 0.17s system 99% cpu 1:21.78 total

max_vars(6).
max_body(6).
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