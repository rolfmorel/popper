%% 14 SECONDS

%% POPPER SETTINGS HARDCODED TO THE PERFECT VALUES
max_vars(6).
max_body(5).
max_clauses(2).

%% HEAD DECLARATION
modeh(sum,3).
%% BODY DECLARATIONS
modeb(sum,3).
modeb(and,3).
%% modeb(or,3). % JUST ADDED
modeb(xor,3).
%% modeb(xnor,3).
%% modeb(neg,2).
modeb(shl,2).
modeb(is_zero,1).
modeb(is_not_zero,1).
modeb(equal,2).

%% injective(shl,2).
%% functional(and,3).
%% functional(or,3).
%% functional(xor,3).
%% functional(xnor,3).
%% functional(neg,2).
%% functional(shl,2).
%% functional(equal,2).
%% irreflexive(shl,2).
%% irreflexive(neg,2).

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

