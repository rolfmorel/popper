#include "unfold.pl".

%% f :- inv1,inv1.
%% inv1 :- r,r.
%% =>
%% f :- r,r,r,r.

%% n= 6 -> 4
%% f :- inv1,inv1,
%% inv1 :- r,r.


%% n=7 k=6
%% f :- inv1,inv1,inv1.
%% inv1 :- r,r.

%% n=7 k=6
%% f :- inv1,inv1,
%% inv1 :- r,r,r.

%% --
%% n=8 k=8
%% f :- inv1,inv1,inv1,inv1.
%% inv1 :- r,r.

%% n=8 k=8
%% f :- inv1,inv1,
%% inv1 :- r,r,r,r.

%% n=8 k=9
%% f :- inv1,inv1,inv1.
%% inv1 :- r,r,r

head_literal(0,f,1,(0,)).
body_literal(0,inv1,1,(0,)).
body_literal(0,inv2,1,(0,)).
body_literal(0,b,1,(0,)).

head_literal(1,inv1,1,(0,)).
body_literal(1,b,1,(0,)).
body_literal(1,c,1,(0,)).

head_literal(2,inv2,1,(0,)).
body_literal(2,d,1,(0,)).
body_literal(2,e,1,(0,)).