%% (base) ➜  map-func popp exs.pl modes1.pl bk.pl
%% f(A,B) :- empty(B),empty(A).
%% f(A,B) :- cons2(D,C,A),succ(D,F),f(C,E),cons1(F,E,B).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes1.pl bk.pl  113.61s user 0.65s system 100% cpu 1:54.26 total

max_vars(6).
max_body(4).
max_clauses(2).

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

same(cons1,cons2).

:-
    body_literal(C,P,_,(H1,_,L1)),
    body_literal(C,Q,_,(H2,_,L1)),
    H1 != H2,
    same(P,Q).
:-
    body_literal(C,P,_,(_,T1,L1)),
    body_literal(C,Q,_,(_,T2,L1)),
    T1 != T2,
    same(P,Q).

:-
    body_literal(C,P,_,(H,T,L1)),
    body_literal(C,Q,_,(H,T,L2)),
    L1 != L2,
    same(P,Q).

:-
    body_literal(C,P,_,(_,T,L)),
    body_literal(C,Q,_,(_,L,T)),
    same(P,Q).

:-
    body_literal(C,P,_,Vars),
    body_literal(C,Q,_,Vars),
    same(P,Q).