%% CHECK FOR SUBSUMPION BETWEEN CLAUSES
duplicate_literals(Clause1,Literal1,Clause2,Literal2):-
    Literal1 > 0,
    Literal2 > 0,
    literal(Clause1,Literal1,P,A),
    literal(Clause2,Literal2,P,A),
    Clause1 != Clause2,
    #count{Pos :
        var(Clause1,Literal1,Pos,V),
        var(Clause2,Literal2,Pos,V)
    } == A.

:-
    clause(Clause1),
    clause(Clause2),
    Clause1 < Clause2,
    #count{Literal1 : body_literal(Clause1,Literal1,P,A)} == Size,
    #count{Literal1 :
        body_literal(Clause1,Literal1,P,A),
        body_literal(Clause2,Literal2,P,A),
        duplicate_literals(Clause1,Literal1,Clause2,Literal2)
    } == Size.

%% #show literal/4.
%% #show var/4.
%% #show clause/1.


%% %% ORDER CLAUSES BY SIZE
size(Clause,N):-
    literal(Clause,N,_,_),
    not literal(Clause,N+1,_,_).

:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    size(Clause1,N1),
    size(Clause2,N2),
    N1 > N2.

different(Clause1,Clause2):-
    var(Clause1,Literal,Pos,Var1),
    var(Clause2,Literal,Pos,Var2),
    Var1 != Var2,
    Clause1 != Clause2.

:-
    clause(Clause1),
    clause(Clause2),
    Clause1 != Clause2,
    not different(Clause1,Clause2).

%% ORDER CLAUSES BY THEIR FIRST ARGUMENT OF THE FIRST Literal
:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    size(Clause1,N),
    size(Clause2,N),
    Literal = 1..Max,
    max_body(Max),
    literal(Clause1,Literal,_,_),
    literal(Clause2,Literal,_,_),
    var(Clause1,Literal,0,V1),
    var(Clause2,Literal,0,V2),
    V1 > V2.

:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    size(Clause1,N),
    size(Clause2,N),
    literal(Clause1,Literal,_,_),
    literal(Clause2,Literal,_,_),
    Literal = 1..Max,
    max_body(Max),
    Pos > 0,
    #count{V :
        var(Clause1,Literal,K,V),
        var(Clause2,Literal,K,V),
        K = 0..Pos-1
    } == Pos,
    var(Clause1,Literal,Pos,V1),
    var(Clause2,Literal,Pos,V2),
    %% V2 < V1.
    V1 > V2.

:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    size(Clause1,N),
    size(Clause2,N),
    Literal = 1..Max,
    max_body(Max),
    literal(Clause1,Literal,P,1),
    literal(Clause2,Literal,Q,1),
    var(Clause1,Literal,0,V0),
    var(Clause2,Literal,0,V0),
    P > Q.


%% IF ALL ARGUMENTS ARE THE SAME THEN ORDER BY PREDICATE SYMBOL
:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    size(Clause1,N),
    size(Clause2,N),
    Literal = 1..Max,
    max_body(Max),
    literal(Clause1,Literal,P,A),
    literal(Clause2,Literal,Q,A),
    P > Q,
    #count{V :
        var(Clause1,Literal,K,V),
        var(Clause2,Literal,K,V),
        K = 0..A-1
    } == A.