:-['bk'].

f(A,B) :- right(A,C),inv1(C,B).
inv1(A,B) :- right(A,C),inv1(C,B).
inv1(A,B) :- right(A,C),right(C,B).
