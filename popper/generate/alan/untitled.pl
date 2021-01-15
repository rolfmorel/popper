%% f :- a, inv1.
%% inv1 :- b, c.

head_literal(0,f,1,(0,)).
body_literal(0,a,1,(0,)).
body_literal(0,inv1,1,(0,)).

head_literal(1,inv1,1,(0,)).
body_literal(1,b,1,(0,)).
body_literal(1,c,1,(0,)).

calls(C1,C2):-
    comp_literal(C1,C2,_,_,_).

comp_literal(C1,C2,P,A,Vs):-
    C1 < C2,
    body_literal(C1,P,A,Vs),
    head_literal(C2,P,A).
    %% what about vars?

unfolded(C,P,A,Vs):-
    body_literal(C,P,A,Vs),
    not comp_literal(C,_,P,A,Vs).
unfolded(C1,P,A,Vs):-
    calls(C1,C2),
    body_literal(C2,P,A,Vs),
    not comp_literal(C2,_,P,A,Vs).