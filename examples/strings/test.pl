f(A,B) :- skip1(A,C),copyskip1(C,B).
f(A,B) :- copyskip1(C,B),f(A,C).
