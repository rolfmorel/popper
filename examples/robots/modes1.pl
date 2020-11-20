%% (base) ➜  robots popp exs.pl modes1.pl bk.pl
%% f(A,B) :- right(A,E),right(E,D),right(D,C),right(C,F),right(F,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes1.pl bk.pl  29.03s user 0.09s system 99% cpu 29.148 total

max_vars(6).
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