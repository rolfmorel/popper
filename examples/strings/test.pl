:-['bk'].


%% f(A,B) :- skip1(A,C),inv1(C,B).
%% inv1(A,B) :- copyskip1(A,B),is_empty(B).
%% inv1(A,B) :- copyskip1(A,C),inv1(C,B).

f(A,B) :- mk_uppercase(A,C),skip1(C,B).

a:-
    %% S1 = s()
    f(s(['@', 'b', 'o', 'b'],X),s(_,[])),
    writeln(X).

%% ['b','o','b']