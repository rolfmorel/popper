#include "duplicates.pl".
#include "ordering.pl".
#include "clauses.pl".
#include "modes.pl".

%% CLAUSE 0
clause(0).

%% GUESS OTHER CLAUSES
{clause(0..N-1)}:-
    max_clauses(N).

%% GUESS CLAUSES IN ORDER
:-
    clause(Clause1),
    Clause1 > 1,
    Clause2 = Clause1-1,
    not clause(Clause2).

%% HEAD LITERALS
literal(Clause,0,P,A):-
    clause(Clause),
    modeh(P,A).

%% %% GUESS BODY LITERAL
{literal(Clause,Literal,P,A)}:-
    clause(Clause),
    Literal = 1..N,
    max_body(N),
    modeb(P,A).

body_literal(Clause,Literal,P,A):-
    literal(Clause,Literal,P,A),
    Literal > 0.

%% MUST HAVE A LITERAL AT POS 1
:-
    clause(Clause),
    not literal(Clause,1,_,_).

%% GUESS LITERALS IN ORDER
:-
    body_literal(Clause,Literal1,_,_),
    Literal2 = Literal1-1,
    not literal(Clause,Literal2,_,_).

%% PREVENT MULTIPLE LITERALS AT SAME INDEX
:-
    literal(Clause,Literal,P,_),
    literal(Clause,Literal,Q,_),
    P != Q.

:-
    %% this duplicates the above constraint
    literal(Clause,Literal,_,A1),
    literal(Clause,Literal,_,A2),
    A1 != A2.

%% HEAD VARS
var(Clause,0,V,V):-
    literal(Clause,0,P,A),
    V = 0..A,
    V < A.

%% GUESS BODY VARS
{var(Clause,Literal,Pos,Var)}:-
    body_literal(Clause,Literal,_,A),
    Pos = 0..A-1,
    Var = 0..N-1,
    max_vars(N).

%% MUST USE VARS IN ORDER
:-
    var(Clause,_,_,V1),
    V1 > 0,
    V2 = V1-1,
    not var(Clause,_,_,V2).

%% PREVENT MULTIPLE VARS IN THE SAME INDEX AND POSITION
:-
    var(Clause,Literal,Pos,V1),
    var(Clause,Literal,Pos,V2),
    V1 != V2.

%% PREVENT SAME VAR IN THE SAME LITERAL
:-
    var(Clause,Literal,Pos1,V),
    var(Clause,Literal,Pos2,V),
    Pos1 != Pos2.

%% ENSURE CORRECT NUMBER OF VARS
%% CONTROL
%% a:-
%%     literal(Clause,Literal,_,A),
%%     #count{Pos,Var :
%%         var(Clause,Literal,Pos,Var)
%%     } != A.
%% TREATMENT SEEMS TO REDUCE SOLVING TIME BY 1/2
:-
    literal(Clause,Literal,_,A),
    Pos = 0..A-1,
    not var(Clause,Literal,Pos,_).


%% MUST BE DATALOG
%% CONTROL
%% a:-
%%     var(Clause,0,_,V),
%%     #count{Literal :
%%         var(Clause,Literal,_,V),
%%         Literal > 0
%%     } == 0.
%% TREATMENT
var_in_body(Clause,Var):-
    var(Clause,Literal,_Pos,Var),
    Literal > 0.
:-
    var(Clause,0,_Pos,Var),
    not var_in_body(Clause,Var).

%% TYPE MATCHING
:-
    var(Clause,Literal1,Pos1,V),
    var(Clause,Literal2,Pos2,V),
    literal(Clause,Literal1,P,_),
    literal(Clause,Literal2,Q,_),
    type(P,Pos1,Type1),
    type(Q,Pos2,Type2),
    Type1 != Type2.

%% PREVENT SINGLETONS
%% CONTROL
:-
    var(Clause,_,_,Var),
    #count{Literal,Pos : var(Clause,Literal,Pos,Var)} == 1.

%% %% TREATMENT
%% not_singleton(Clause,Var):-
%%     var(Clause,Literal1,Pos,Var),
%%     var(Clause,Literal2,Pos,Var),
%%     Literal1 != Literal2.
%% not_singleton(Clause,Var):-
%%     var(Clause,Literal,Pos1,Var),
%%     var(Clause,Literal,Pos2,Var),
%%     Pos1 != Pos2.
%% a:-
%%     var(Clause,_,_,Var),
%%     not not_singleton(Clause,Var).

%% %% IF WE ADD A LITERAL THEN IT MUST USE AN EXISTING VARIABLE
:-
    body_literal(Clause,Literal1,_,_),
    #count{V :
        var(Clause,Literal1,_,V),
        var(Clause,Literal2,_,V),
        literal(Clause,Literal2,_,_),
        Literal2 < Literal1
    } == 0.



%% TODO: REMOVE REFLEXIVE

%% %% REMOVE IRREFLEXIVE - TODO GENERALISE
%% happy(A):-add1(A,B),rich(B),add1(B,A) <- prevent the second add1/2 literal

:-
    irreflexive(P),
    literal(Clause,Literal1,P,2),
    literal(Clause,Literal2,P,2),
    Literal1 != Literal2,
    var(Clause,Literal1,Pos1,V1),
    var(Clause,Literal1,Pos2,V2),
    var(Clause,Literal2,Pos1,V2),
    var(Clause,Literal2,Pos2,V1).

%% %% FUNCTIONAL - TODO GENERALISE
%% happy(A):-add1(A,B),add1(A,C),equal(B,C) <- must have that consta
:-
    functional(P),
    literal(Clause,Index1,P,2),
    var(Clause,Index1,0,V1),
    var(Clause,Index1,1,V2),
    literal(Clause,Index2,P,2),
    Index2 != Index1,
    var(Clause,Index2,0,V1),
    var(Clause,Index2,1,V3),
    V2 != V3.
