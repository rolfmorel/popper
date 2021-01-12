#include "unfold.pl".

%% f :- a, inv1.
%% inv1 :- b, c.

head_literal(0,f,1,(0,)).
body_literal(0,a,1,(0,)).
body_literal(0,inv1,1,(0,)).

head_literal(1,inv1,1,(0,)).
body_literal(1,b,1,(0,)).
body_literal(1,c,1,(0,)).