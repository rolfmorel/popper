%% (base) ➜  map time popp exs.pl modes.pl bk.pl
%% f(A,B) :- empty(A),empty(B).
%% f(A,B) :- cons2(F,D,A),succ(F,E),f(D,C),cons1(E,C,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  7.66s user 0.09s system 100% cpu 7.749 total


max_vars(6).
max_body(4).
max_clauses(2).

:-
    not recursive.
:-
    not body_literal(0,empty,1,(0,)).
:-
    not body_literal(0,empty,1,(1,)).
%% :-
    %% body_literal(1,empty,1,_).

modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).
modeb(f,2).

%% modeb(head,2).
%% type(head,0,list).
%% type(head,1,element).
%% direction(head,0,in).
%% direction(head,1,out).

%% modeb(tail,2).
%% type(tail,0,list).
%% type(tail,1,list).
%% direction(tail,0,in).
%% direction(tail,1,out).

modeb(succ,2).
type(succ,0,element).
type(succ,1,element).
direction(succ,0,in).
direction(succ,1,out).


%% direction(cons1,(in,in,out)).
%% direction(cons2,(out,out,in)).

modeb(cons1,3).
type(cons1,0,element).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,in).
direction(cons1,1,in).
direction(cons1,2,out).

modeb(cons2,3).
type(cons2,0,element).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,out).
direction(cons2,1,out).
direction(cons2,2,in).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,out).