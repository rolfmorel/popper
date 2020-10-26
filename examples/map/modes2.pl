max_vars(6).
max_body(6).
max_clauses(2).


%% f(A,B):-empty(A),empty(B).
%% f(A,B):-tail(A,T1),f(T1,T2),head(A,H1),succ(H1,H2),cons(H2,T2,B).
%% =>
%% f(A,B):-empty(A),empty(B).
%% f(A,B):-cons(C,A,D),f(D,E),succ(C,F),cons(F,E,B).

:-
    not body_literal(0,empty,1,(0,)).
:-
    not body_literal(0,empty,1,(1,)).

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