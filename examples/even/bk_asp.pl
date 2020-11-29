max_int(100).
zero(0).

succ(X, X+1):-zero(X).
succ(X+1, Y+1):-succ(X,Y),max_int(Z),X<Z,Y<Z.
