%% (base) ➜  robots time popp exs.pl modes.pl bk.pl
%% f(A,B) :- right(A,F),right(F,C),right(C,E),right(E,D),right(D,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  19.61s user 0.06s system 99% cpu 19.667 total


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