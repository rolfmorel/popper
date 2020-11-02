:-['bk'].
%% f(A,B) :- right(A,C),right(C,B).
%% f(A,B) :- inv1(A,C),inv1(C,B).
%% inv1(A,B) :- right(A,C),right(C,B).

f(A,B) :- right(A,C),right(C,B).
f(A,B) :- inv1(A,C),right(C,B).
f(A,B) :- inv1(A,C),inv1(C,B).
inv1(A,B) :- right(A,C),right(C,B).


%% a(X:-
%% pos(f(w(0,0),w(8,0))).