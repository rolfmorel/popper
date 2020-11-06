%% (base) ➜  wang-zhou time popp exs.pl modes.pl bk.pl
%% sum(A,B,C) :- is_zero(B),equal(A,C).
%% sum(A,B,C) :- and(A,B,F),shl(F,D),xor(B,A,E),sum(E,D,C).
%% python3 /Users/andrew/icloud/code/popper/popper.py exs.pl modes.pl bk.pl  115.03s user 0.26s system 99% cpu 1:55.41 total


%% POPPER SETTINGS HARDCODED TO THE PERFECT VALUES
max_vars(6).
max_body(4).
max_clauses(2).

%% HEAD DECLARATION
modeh(sum,3).
%% BODY DECLARATIONS
modeb(sum,3).
modeb(and,3).
%% modeb(or,3).
modeb(xor,3).
%% modeb(xnor,3).
%% modeb(neg,2).
modeb(shl,2).
modeb(is_zero,1).
%% modeb(is_not_zero,1).
modeb(equal,2).

%% ARGUMENT DIRECTIONS FOR THE BK
direction(sum,0,in).
direction(sum,1,in).
direction(sum,2,out).

direction(and,0,in).
direction(and,1,in).
direction(and,2,out).

direction(or,0,in).
direction(or,1,in).
direction(or,2,out).

direction(xor,0,in).
direction(xor,1,in).
direction(xor,2,out).

direction(neg,0,in).
direction(neg,1,out).

direction(shl,0,in).
direction(shl,1,out).

direction(equal,0,in).
direction(equal,1,out).

direction(is_zero,0,in).
direction(is_not_zero,0,in).

%% HACKS TO MAKE LEARNING TRACTABLE

%% HACK 1: ENFORCE A CONTRAINT SO THAT A BK PRED SYMBOL MAY APPEAR AT MOST ONCE IN A CLAUSE
only_once(shl).
only_once(and).
only_once(or).
only_once(xor).
only_once(equal).
only_once(sum).
:-
    only_once(P),
    body_literal(C,P,_,Vars1),
    body_literal(C,P,_,Vars2),
    Vars1 != Vars2.

%% HACK(s) 2:
%% FIRST CLAUSE MUST CONTAIN THE BODY LITERAL is_zero(B)
:-
    not body_literal(0,is_zero,_,(1,)).
%% FIRST CLAUSE MUST CONTAIN THE BODY LITERAL equal(A,C)
:-
    not body_literal(0,equal,_,(0,2)).
%% SECOND CLAUSE MUST CONTAIN THE BODY LITERAL equal(_)
:-
    body_literal(1,equal,_,_).
%% SECOND CLAUSE MUST CONTAIN THE BODY LITERAL sum(_,_,C)
:-
    not body_literal(1,sum,_,(_,_,2)).

