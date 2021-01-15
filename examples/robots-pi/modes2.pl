max_vars(6).
max_body(3).
max_clauses(3).

modeh(f,2).
%% modeb(up,2).
%% modeb(down,2).
%% modeb(left,2).
modeb(right,2).

:-
    recursive.

{invented(inv1,2)}.
{invented(inv2,2)}.
{invented(inv3,2)}.
%% {invented(inv4,2)}.

direction(P,0,in):-
    pred(P,2).
direction(P,1,out):-
    pred(P,2).