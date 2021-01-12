clause(C):-
    head_literal(C,_,_,_).

clause_size(C,N):-
    clause(C),
    #count{P,Vars : body_literal(C,P,_,Vars)} == N.

prog_size(N):-
    #sum{K+1,Clause : clause_size(Clause,K)} == N.

unfolded_size(N):-
    #count{C,P,Vars : lit(C,P,_,Vars)} == N.

#show prog_size/1.
#show unfolded_size/1.

calls(C1,C2):-
    comp_literal(C1,C2,_,_,_).

comp_literal(C1,C2,P,A,Vs):-
    C1 < C2,
    body_literal(C1,P,A,Vs),
    head_literal(C2,P,A,Vs).
    %% what about vars?

unfolded_body(C,P,A,Vs):-
    body_literal(C,P,A,Vs),
    not comp_literal(C,_,P,A,Vs).
unfolded_body(C1,P,A,Vs):-
    calls(C1,C2),
    unfolded_body(C2,P,A,Vs),
    not comp_literal(C2,_,P,A,Vs).

redundant(C):-
    calls(_,C).



hlit(C,P,A,Vs):-
    head_literal(C,P,A,Vs),
    not redundant(C).

blit(C,P,A,Vs):-
    unfolded_body(C,P,A,Vs),
    not redundant(C).

lit(C,P,A,Vs):-
    hlit(C,P,A,Vs).
lit(C,P,A,Vs):-
    blit(C,P,A,Vs).

#show hlit/4.
#show blit/4.