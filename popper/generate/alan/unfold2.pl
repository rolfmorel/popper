#include "unfold.pl".

%% f :- inv1,inv2.
%% inv1 :- b, c.
%% inv2 :- d, e.

head_literal(0,f,1,(0,)).
body_literal(0,inv1,1,(0,)).
body_literal(0,inv2,1,(0,)).

head_literal(1,inv1,1,(0,)).
body_literal(1,b,1,(0,)).
body_literal(1,c,1,(0,)).

head_literal(2,inv2,1,(0,)).
body_literal(2,d,1,(0,)).
body_literal(2,e,1,(0,)).