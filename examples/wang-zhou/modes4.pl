%% COMPETE
%% 1CPU: 190 MINS
%% 2CPUS: 57 MINS
%% 3CPUS: 80 MINS
%% 4CPUS: 39 MINS
%% 5CPUS: 20 MINS
%% 6CPUS: 18 MINS
%% 7CPUS: 11 MINS
%% 8CPUS: 13 MINS
%% 9CPUS: 15 MINS
%% 10CPUS: 9 MINS
%% 14CPUS: 7 MINS

%% SPLIT
%% 1CPU: 190 MINS
%% 2CPUS: ?? MINS
%% 3CPUS: ?? MINS
%% 4CPUS: ?? MINS
%% 5CPUS: ?? MINS
%% 6CPUS: ?? MINS
%% 7CPUS: ?? MINS
%% 8CPUS: ?? MINS
%% 9CPUS: 8 MINS
%% 10CPUS: 12 MINS
%% 11CPUS: 10 MINS
%% 14CPUS: 9 MINS
max_vars(6).
max_body(5).
max_clauses(2).

modeh(sum,3).
modeb(sum,3).
modeb(and,3).
%% modeb(or,3).
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
only_once(xor).
only_once(equal).
only_once(sum).
:-
    only_once(P),
    body_literal(C,P,_,Vars1),
    body_literal(C,P,_,Vars2),
    Vars1 != Vars2.

%% HACK(s) 2:
%% SECOND CLAUSE CANNOT CONTAIN THE BODY LITERAL equal(_)
:-
    body_literal(1,equal,_,_).
%% SECOND CLAUSE MUST CONTAIN THE BODY LITERAL sum(_,_,C)
:-
    not body_literal(1,sum,_,(_,_,2)).