%% ALAN10

#show head_literal/4.
#show body_literal/4.

possible_clause(0..N-1):-
    max_clauses(N).

var(0..N-1):-
    max_vars(N).

clause(Clause):-
    head_literal(Clause,_,_,_).

%% USE CLAUSES IN ORDER
:-
    clause(Clause),
    Clause > 0,
    not clause(Clause-1).

%% USE VARS IN ORDER IN A CLAUSE
:-
    clause(Clause),
    var_in_literal(Clause,_,_,Var),
    Var > 0,
    not var_in_literal(Clause,_,_,Var-1).

%% GUESS A SINGLE HEAD LITERAL
0 {head_literal(Clause,P,1,(0,)) : modeh(P,1)} 1:-
    possible_clause(Clause).

0 {head_literal(Clause,P,2,(0,1)) : modeh(P,2)} 1:-
    possible_clause(Clause).

0 {head_literal(Clause,P,3,(0,1,2)) : modeh(P,3)} 1:-
    possible_clause(Clause).

%% GUESS AT LEAST 1 BUT AT MOST N BODY LITERALS
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).

%% ENSURE A CLAUSE
:-
    not clause(0).

%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).

%% OBEY PROGRAM SIZE
:-
    size(N),
    #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.

literal(Clause,P,Vars):-
    head_literal(Clause,P,_,Vars).
literal(Clause,P,Vars):-
    body_literal(Clause,P,_,Vars).

%% PREVENTS UNEEDED GROUNDING
%% MICRO OPTIMISATION
need_arity(A):-
    modeh(_,A).
need_arity(A):-
    modeb(_,A).

%% POSSIBLE VARIABLE COMBINATIONS
%% TODO: GENERALISE FOR ARITIES > 3
vars(1,(Var1,)):-
    need_arity(1),
    var(Var1).
vars(2,(Var1,Var2)):-
    need_arity(2),
    var(Var1),
    var(Var2),
    Var1 != Var2.
vars(3,(Var1,Var2,Var3)):-
    need_arity(3),
    var(Var1),
    var(Var2),
    var(Var3),
    Var1 != Var2,
    Var1 != Var3,
    Var2 != Var3.


clause_var(Clause,Var):-
    head_var(Clause,Var).
clause_var(Clause,Var):-
    body_var(Clause,Var).

head_var(Clause,Var):-
    head_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).
body_var(Clause,Var):-
    body_literal(Clause,_P,_A,Vars),
    var_member(Var,Vars).

%% DATALOG
:-
    head_var(Clause,Var),
    not body_var(Clause,Var).

%% VAR IS IN VARS
var_member(Var,Vars):-
    var_pos(Var,Vars,_).

%% POSITION OF VAR IN VARS
%% TODO: GENERALISE FOR ARITIES > 3
var_pos(Var1,(Var1,),0):-
    vars(1,(Var1,)).

var_pos(Var1,(Var1,Var2),0):-
    vars(2,(Var1,Var2)).
var_pos(Var2,(Var1,Var2),1):-
    vars(2,(Var1,Var2)).

var_pos(Var1,(Var1,Var2,Var3),0):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var2,(Var1,Var2,Var3),1):-
    vars(3,(Var1,Var2,Var3)).
var_pos(Var3,(Var1,Var2,Var3),2):-
    vars(3,(Var1,Var2,Var3)).

%% VAR IS IN A LITERAL
var_in_literal(Clause,P,Vars,Var):-
    literal(Clause,P,Vars),
    var_member(Var,Vars).

%% ELIMINATE SINGLETONS
%% MUCH FASTER THAN ALTERNATIVES (see below)
%% I DO NOT KNOW WHY
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
:-
    var_in_literal(Clause,P,Vars1,Var),
    var_in_literal(Clause,Q,Vars2,Var),
    var_pos(Var,Vars1,Pos1),
    var_pos(Var,Vars2,Pos2),
    type(P,Pos1,Type1),
    type(Q,Pos2,Type2),
    Type1 != Type2.

%% %% TYPE MATCHING V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} != 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars).

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% %% MUST BE CONNECTED
:-
    body_var(Clause,Var),
    not head_connected(Clause,Var).

%% ENSURE INPUT VARS ARE GROUND
num_in_args(P,N):-
    direction(P,_,_),
    #count{Pos : direction(P,Pos,in)} = N.

%% VAR SAFE IF:
%% - VAR IS HEAD INPUT VAR
safe_var(Clause,Var):-
    head_literal(Clause,P,_,Vars),
    var_pos(Var,Vars,Pos),
    direction(P,Pos,in).

%% VAR SAFE IF:
%% - VAR IS IN A LITERAL THAT ONLY HAS OUT VARS
safe_var(Clause,Var):-
    num_in_args(P,0),
    body_literal(Clause,P,_,Vars),
    var_member(Var,Vars).

%% VAR SAFE IF:
%% - VAR IS IN SAFE LITERAL
safe_var(Clause,Var):-
    safe_literal(Clause,P,Vars),
    var_member(Var,Vars).

%% LITERAL WITH 1 INPUT VAR IS SAFE IF VAR IS SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,1),
    body_literal(Clause,P,_,Vars),
    var_pos(Var1,Vars,Pos),
    direction(P,Pos,in),
    safe_var(Clause,Var1).

%% LITERAL WITH 2 INPUT VARS IS SAFE IF BOTH VARS ARE SAFE
safe_literal(Clause,P,Vars):-
    num_in_args(P,2),
    body_literal(Clause,P,_,Vars),
    var_pos(Var1,Vars,Pos1),
    var_pos(Var2,Vars,Pos2),
    direction(P,Pos1,in),
    direction(P,Pos2,in),
    Pos1 != Pos2,
    safe_var(Clause,Var1),
    safe_var(Clause,Var2).

%% SAFE VARS
:-
    direction(_,_,_),
    var_in_literal(Clause,_,_,Var),
    not safe_var(Clause,Var).

%% REMOVE REFLEXIVE
%% prevents: p(A):-q(A,B),q(B,A)
:-
    irreflexive(P,2),
    literal(Clause,P,(Var1,Var2)),
    literal(Clause,P,(Var2,Var1)).

%% FUNCTIONAL
%% prevents: p(A):-q(A,B),q(A,C)
:-
    functional(P,2),
    literal(Clause,P,(Var1,Var2)),
    literal(Clause,P,(Var1,Var3)),
    Var2 != Var3.

%% ########################################
%% RECURSION
%% ########################################

recursive:-
    recursive(Clause).

recursive(Clause):-
    head_literal(Clause,P,A,_),
    body_literal(Clause,P,A,_).

%% IF RECURSIVE, THERE MUST BE A NON-RECURSIVE CLAUSE
has_base:-
    clause(Clause),
    not recursive(Clause).
:-
    recursive,
    not has_base.

%% DISALLOW TWO RECURSIVE CALLS
:-
    recursive(C),
    head_literal(C,P,A,_),
    body_literal(C,P,A,V1),
    body_literal(C,P,A,V2),
    V1 != V2.

%% THIS VERSION IS SOMETIMES FASTER
%% :-
%%     #count{Clause : clause(Clause)} = N,
%%     #count{Clause : recursive(Clause)} = N.

%% PREVENT LEFT RECURSION
%% TODO: GENERALISE FOR ARITY > 3
:-
    recursive(Clause),
    num_in_args(P,1),
    head_literal(Clause,P,A,Vars1),
    var_pos(Var,Vars1,Pos1),
    direction(P,Pos1,in),
    body_literal(Clause,P,A,Vars2),
    var_pos(Var,Vars2,Pos2),
    direction(P,Pos2,in).

:-
    recursive(Clause),
    num_in_args(P,2),
    %% get the head input vars
    head_literal(Clause,P,A,HeadVars),
    var_pos(HeadVar1,HeadVars,Pos1),
    var_pos(HeadVar1,HeadVars,Pos2),
    HeadPos1 != HeadPos2,
    direction(P,HeadPos1,in),
    direction(P,HeadPos2,in),
    %% get the body input vars
    body_literal(Clause,P,A,BodyVars),
    var_pos(HeadVar1,BodyVars,BodyPos1),
    var_pos(HeadVar2,BodyVars,BodyPos2),
    BodyPos1 != BodyPos2,
    direction(P,BodyPos1,in),
    direction(P,BodyPos2,in).

%% ########################################
%% MULTIPLE CLAUSES
%% ########################################

multiclause:-
    clause(1).

%% %% REMOVE IDENTICAL CLAUSES
%% different(Clause1,Clause2):-
%%     head_literal(Clause1,P,_,_),
%%     head_literal(Clause2,Q,_,_),
%%     P != Q.
%% different(Clause1,Clause2):-
%%     head_literal(Clause1,_,_,Vars1),
%%     head_literal(Clause2,_,_,Vars2),
%%     Vars1 != Vars2.
%% different(Clause1,Clause2):-
%%     body_literal(Clause1,P,_,Vars),
%%     clause(Clause2),
%%     not body_literal(Clause2,P,_,Vars).

%% %% WEIRD: WHY IS THIS NEEDED?
%% different(Clause1,Clause2):-


%% REMOVE IDENTICAL CLAUSES
%% :-
%%     clause_size(Clause1,N),
%%     clause_size(Clause2,N),
%%     Clause1 != Clause2,
%%     head_literal(Clause1,P,A,PVars),
%%     head_literal(Clause2,P,A,PVars),
%%     body_literal(Clause1,Q,_,QVars) :  body_literal(Clause2,Q,_,QVars).

%% ENSURE CLAUSES ARE DIFFERENT
%% DUPLICATES THE SUBSUMPTION PART
%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     not different(Clause1,Clause2).

%% C1 SUBUMES C2 IF:
%% - C1 IS A SUBSET OF C2
%% subsumes(Clause1,Clause2):-
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     Clause1 != Clause2,
%%     #count{P, Vars : body_literal(Clause1,P,_,Vars), not body_literal(Clause2,P,_,Vars)} == 0.

%% DUPLICATES THE DIFFERENT PART
subsumes(Clause1,Clause2):-
    Clause1 != Clause2,
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

:-
    clause(Clause1),
    clause(Clause2),
    Clause1 != Clause2,
    subsumes(Clause1,Clause2).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% a:-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause2 > Clause1,
%%     N1 > N2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2).

%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #count{Var : body_literal(Clause,_,_,Vars), var_member(Var,Vars)} == N.

%% num_vars(Clause,N):-
%%     max_vars(MaxN),
%%     N <= MaxN,
%%     clause(Clause),
%%     #count{Var : clause_var(Clause,Var)} == N.

%% %% %% ORDER CLAUSES BY NUMBER OF VARS
%% a:-
%%     clause_size(C1,N),
%%     clause_size(C2,N),
%%     C2 > C1,
%%     num_vars(C1,NumVars1),
%%     num_vars(C2,NumVars2),
%%     NumVars1 > NumVars2.