%% (base) ➜  map time popp ex.pl modes3.pl bk.pl --eval-timeout=0.01 --debug --clingo-args '-t 16'
%% python3 /Users/andrew/icloud/code/popper/popper.py ex.pl modes4.pl bk.pl      1731.01s user 3.39s system 1191% cpu 2:25.62 total

max_vars(6).
max_body(4).
max_clauses(2).

%% f(A,B) :- empty(A),empty(B).
%% f(A,B) :- cons(E,C,A),succ(E,F),f(C,D),cons(F,D,B).


:-
    body_literal(C,cons1,_,Vars),
    body_literal(C,cons2,_,Vars).

:-
    not recursive.

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