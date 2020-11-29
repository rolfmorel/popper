% WARN: NEW INSTANCE
% WARN: ADDING following fragment with name 'alan'
%% ALAN11

#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

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

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).
%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).
%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
:-
    size(N),
    #sum{Size+1 : clause_size(Clause,Size)} != N.

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
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
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
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2. % simply reduces grounding

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
%% CAN WE PUSH THIS TO THE GUESS?
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
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
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
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.


%% ########################################
%% RECURSION
%% ########################################


non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

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
%% WHY DID WE ADD THIS??
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
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
% WARN: ADDING following fragment with name 'modes_file'
max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.

% WARN: ADDING following fragment with name 'program_size'
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
  size(n),
  #count{Clause,P,Vars : literal(Clause,P,Vars)} != n.

% WARN: GROUNDING parts '[('alan', []), ('modes_file', [])]'
% WARN: NEW INSTANCE
% WARN: ADDING following fragment with name 'alan'
%% ALAN11

#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

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

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).
%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).
%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
:-
    size(N),
    #sum{Size+1 : clause_size(Clause,Size)} != N.

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
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
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
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2. % simply reduces grounding

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
%% CAN WE PUSH THIS TO THE GUESS?
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
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
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
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.


%% ########################################
%% RECURSION
%% ########################################


non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

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
%% WHY DID WE ADD THIS??
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
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
% WARN: ADDING following fragment with name 'modes_file'
max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.

% WARN: ADDING following fragment with name 'program_size'
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
  size(n),
  #count{Clause,P,Vars : literal(Clause,P,Vars)} != n.

% WARN: GROUNDING parts '[('alan', []), ('modes_file', [])]'
% WARN: GROUNDING parts '[('program_size', [1])]'
% WARN: Atom size(1) is assigned/made True
% WARN: Atom size(1) is released/made False
% WARN: GROUNDING parts '[('program_size', [2])]'
% WARN: Atom size(2) is assigned/made True
% WARN: NEW INSTANCE
% WARN: ADDING following fragment with name 'alan'
%% ALAN11

#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

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

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).
%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).
%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
:-
    size(N),
    #sum{Size+1 : clause_size(Clause,Size)} != N.

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
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
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
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2. % simply reduces grounding

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
%% CAN WE PUSH THIS TO THE GUESS?
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
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
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
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.


%% ########################################
%% RECURSION
%% ########################################


non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

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
%% WHY DID WE ADD THIS??
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
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
% WARN: ADDING following fragment with name 'modes_file'
max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.

% WARN: ADDING following fragment with name 'program_size'
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
  size(n),
  #count{Clause,P,Vars : literal(Clause,P,Vars)} != n.

% WARN: GROUNDING parts '[('alan', []), ('modes_file', [])]'
% WARN: GROUNDING parts '[('program_size', [1])]'
% WARN: Atom size(1) is assigned/made True
% WARN: Atom size(1) is released/made False
% WARN: GROUNDING parts '[('program_size', [2])]'
% WARN: Atom size(2) is assigned/made True
% WARN: NEW INSTANCE
% WARN: ADDING following fragment with name 'alan'
%% ALAN11

#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

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

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).
%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).
%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
:-
    size(N),
    #sum{Size+1 : clause_size(Clause,Size)} != N.

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
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
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
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2. % simply reduces grounding

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
%% CAN WE PUSH THIS TO THE GUESS?
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
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
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
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.


%% ########################################
%% RECURSION
%% ########################################


non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

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
%% WHY DID WE ADD THIS??
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
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
% WARN: ADDING following fragment with name 'modes_file'
max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.

% WARN: ADDING following fragment with name 'program_size'
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
  size(n),
  #count{Clause,P,Vars : literal(Clause,P,Vars)} != n.

% WARN: GROUNDING parts '[('alan', []), ('modes_file', [])]'
% WARN: GROUNDING parts '[('program_size', [1])]'
% WARN: Atom size(1) is assigned/made True
% WARN: Atom size(1) is released/made False
% WARN: GROUNDING parts '[('program_size', [2])]'
% WARN: Atom size(2) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1identicalV0V1'
included_clause_fV0V1identicalV0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV1)),CV1!=CV0.
% WARN: ADDING following fragment with name 'specialisation0'
:-included_clause_fV0V1identicalV0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination1'
:-included_clause_fV0V1identicalV0V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation2'
:-included_clause_fV0V1identicalV0V1(C0),clause_size(C0,1).
% WARN: GROUNDING parts '[('fV0V1identicalV0V1', []), ('specialisation0', []), ('elimination1', []), ('generalisation2', [])]'
% WARN: Atom size(2) is released/made False
% WARN: GROUNDING parts '[('program_size', [3])]'
% WARN: Atom size(3) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V0'
included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation3'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination4'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V0', []), ('specialisation3', []), ('elimination4', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V1'
included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation5'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination6'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V1', []), ('specialisation5', []), ('elimination6', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V1cons1V2V1V0'
included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation7'
:-included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination8'
:-included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V1cons1V2V1V0', []), ('specialisation7', []), ('elimination8', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons1V3V2V0'
included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation9'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination10'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons1V3V2V0', []), ('specialisation9', []), ('elimination10', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation11'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination12'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation13'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),clause_size(C0,2).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V1', []), ('specialisation11', []), ('elimination12', []), ('generalisation13', [])]'
% WARN: ADDING following fragment with name 'fV0V1identicalV0V2identicalV2V1'
included_clause_fV0V1identicalV0V2identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation14'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination15'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation16'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),clause_size(C0,2).
% WARN: GROUNDING parts '[('fV0V1identicalV0V2identicalV2V1', []), ('specialisation14', []), ('elimination15', []), ('generalisation16', [])]'
% WARN: Atom size(3) is released/made False
% WARN: GROUNDING parts '[('program_size', [4])]'
% WARN: Atom size(4) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V3V0V1succV3V2'
included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV0,CV1)),body_literal(C,succ,2,(CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation17'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination18'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V3V0V1succV3V2', []), ('specialisation17', []), ('elimination18', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation19'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination20'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3', []), ('specialisation19', []), ('elimination20', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation21'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination22'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3', []), ('specialisation21', []), ('elimination22', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation23'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination24'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0', []), ('specialisation23', []), ('elimination24', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1'
included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation25'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination26'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1', []), ('specialisation25', []), ('elimination26', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1'
included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation27'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination28'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1', []), ('specialisation27', []), ('elimination28', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation29'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination30'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3', []), ('specialisation29', []), ('elimination30', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation31'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination32'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1', []), ('specialisation31', []), ('elimination32', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation33'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination34'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3', []), ('specialisation33', []), ('elimination34', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation35'
:-included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination36'
:-included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3', []), ('specialisation35', []), ('elimination36', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation37'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination38'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3', []), ('specialisation37', []), ('elimination38', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation39'
:-included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination40'
:-included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3', []), ('specialisation39', []), ('elimination40', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1'
included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation41'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination42'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1', []), ('specialisation41', []), ('elimination42', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1'
included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation43'
:-included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination44'
:-included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1', []), ('specialisation43', []), ('elimination44', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation45'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination46'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3', []), ('specialisation45', []), ('elimination46', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons1V4V3V0succV4V2'
included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,succ,2,(CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation47'
:-included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination48'
:-included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons1V4V3V0succV4V2', []), ('specialisation47', []), ('elimination48', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V2V3V1succV4V2'
included_clause_fV0V1cons1V4V3V0cons2V2V3V1succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,succ,2,(CV4,CV2)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation49'
:-included_clause_fV0V1cons1V4V3V0cons2V2V3V1succV4V2(C0),C0 < 1,not clause(1).
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V2V3V1succV4V2', []), ('specialisation49', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V4V1V3succV4V2'
included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV3)),body_literal(C,succ,2,(CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation50'
:-included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination51'
:-included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V4V1V3succV4V2', []), ('specialisation50', []), ('elimination51', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1'
included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation52'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination53'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1', []), ('specialisation52', []), ('elimination53', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation54'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination55'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1', []), ('specialisation54', []), ('elimination55', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4'
included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation56'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination57'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),separable.
% WARN: ADDING following fragment with name 'generalisation58'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4', []), ('specialisation56', []), ('elimination57', []), ('generalisation58', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4'
included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation59'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination60'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4', []), ('specialisation59', []), ('elimination60', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V3V1V0succV2V3'
included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,succ,2,(CV2,CV3)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation61'
:-included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination62'
:-included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V3V1V0succV2V3', []), ('specialisation61', []), ('elimination62', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation63'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination64'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3', []), ('specialisation63', []), ('elimination64', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation65'
:-included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination66'
:-included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3', []), ('specialisation65', []), ('elimination66', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation67'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination68'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1', []), ('specialisation67', []), ('elimination68', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation69'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination70'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3', []), ('specialisation69', []), ('elimination70', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1emptyV3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation71'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination72'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1emptyV3', []), ('specialisation71', []), ('elimination72', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation73'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination74'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1', []), ('specialisation73', []), ('elimination74', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation75'
:-included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination76'
:-included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1', []), ('specialisation75', []), ('elimination76', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation77'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination78'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3', []), ('specialisation77', []), ('elimination78', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation79'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination80'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1', []), ('specialisation79', []), ('elimination80', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,identical,2,(CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation81'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination82'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0', []), ('specialisation81', []), ('elimination82', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons2,3,(CV2,CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation83'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination84'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0', []), ('specialisation83', []), ('elimination84', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1'
included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation85'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination86'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1', []), ('specialisation85', []), ('elimination86', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1'
included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,identical,2,(CV3,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation87'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination88'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1', []), ('specialisation87', []), ('elimination88', [])]'
% WARN: ADDING following fragment with name 'fV0V1identicalV0V3identicalV2V1identicalV3V2'
included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation89'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination90'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),separable.
% WARN: ADDING following fragment with name 'generalisation91'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1identicalV0V3identicalV2V1identicalV3V2', []), ('specialisation89', []), ('elimination90', []), ('generalisation91', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3'
included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation92'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination93'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3', []), ('specialisation92', []), ('elimination93', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3'
included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation94'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination95'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3', []), ('specialisation94', []), ('elimination95', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,identical,2,(CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation96'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination97'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation98'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1', []), ('specialisation96', []), ('elimination97', []), ('generalisation98', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2'
included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation99'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination100'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2', []), ('specialisation99', []), ('elimination100', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2'
included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation101'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination102'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),separable.
% WARN: ADDING following fragment with name 'generalisation103'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2', []), ('specialisation101', []), ('elimination102', []), ('generalisation103', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons1,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation104'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination105'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1', []), ('specialisation104', []), ('elimination105', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation106'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination107'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0', []), ('specialisation106', []), ('elimination107', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation108'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination109'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0', []), ('specialisation108', []), ('elimination109', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation110'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination111'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2', []), ('specialisation110', []), ('elimination111', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation112'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination113'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1', []), ('specialisation112', []), ('elimination113', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation114'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination115'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2', []), ('specialisation114', []), ('elimination115', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2'
included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation116'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination117'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2', []), ('specialisation116', []), ('elimination117', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation118'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination119'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1', []), ('specialisation118', []), ('elimination119', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation120'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination121'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1', []), ('specialisation120', []), ('elimination121', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation122'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination123'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1', []), ('specialisation122', []), ('elimination123', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation124'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination125'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1', []), ('specialisation124', []), ('elimination125', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4'
included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation126'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination127'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4', []), ('specialisation126', []), ('elimination127', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation128'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination129'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2', []), ('specialisation128', []), ('elimination129', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation130'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination131'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0', []), ('specialisation130', []), ('elimination131', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation132'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination133'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2', []), ('specialisation132', []), ('elimination133', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV0,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation134'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination135'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2', []), ('specialisation134', []), ('elimination135', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0emptyV2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,empty,1,(CV2,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation136'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination137'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0emptyV2', []), ('specialisation136', []), ('elimination137', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1emptyV2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,empty,1,(CV2,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation138'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination139'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1emptyV2', []), ('specialisation138', []), ('elimination139', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation140'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination141'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0', []), ('specialisation140', []), ('elimination141', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation142'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination143'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2', []), ('specialisation142', []), ('elimination143', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation144'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination145'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2', []), ('specialisation144', []), ('elimination145', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation146'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination147'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2', []), ('specialisation146', []), ('elimination147', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1'
included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation148'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination149'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1', []), ('specialisation148', []), ('elimination149', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation150'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination151'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1', []), ('specialisation150', []), ('elimination151', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation152'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination153'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2', []), ('specialisation152', []), ('elimination153', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation154'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination155'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2', []), ('specialisation154', []), ('elimination155', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation156'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination157'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2', []), ('specialisation156', []), ('elimination157', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation158'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination159'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2', []), ('specialisation158', []), ('elimination159', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation160'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination161'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2', []), ('specialisation160', []), ('elimination161', [])]'
% WARN: NEW INSTANCE
% WARN: ADDING following fragment with name 'alan'
%% ALAN11

#defined functional/2.
#defined irreflexive/2.
#defined direction/3.
#defined type/3.
#defined size/1.

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

%% GUESS 1 > K <= NAT LEAST 1 BUT AT MOST N BODY LITERALS
%% V1
1 {body_literal(Clause,P,A,Vars) : modeb(P,A), vars(A,Vars)} N:-
    clause(Clause),
    max_body(N).
%% HEAD LITERAL CANNOT BE IN THE BODY
:-
    head_literal(Clause,P,_,Vars),
    body_literal(Clause,P,_,Vars).
%% V2
%% 1 {body_literal(Clause,P,A,Vars) :
%%         modeb(P,A),
%%         vars(A,Vars),
%%         not head_literal(Clause,P,_,Vars)} N:-
%%     clause(Clause),
%%     max_body(N).

%% CAN WE PUSH THIS TO THE GUESS?
%% head_connected

%% ENSURE A CLAUSE
%% TODO
%% V0
%% clause(0).
%% V1
:-
    not clause(0).

%% size(5).
%% TODO
%% CAN WE REPLACE THIS WITH A COUNT OF BODY LITERALS?
%% OBEY PROGRAM SIZE
%% SIZE V1
%% :-
%%     size(N),
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.
%% SIZE V2
:-
    size(N),
    #sum{Size+1 : clause_size(Clause,Size)} != N.

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
    %% body_var(Clause,Var). <- we could add this literal which enforces a datalog constraint
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
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
%% TYPES V1
%% :-
%%     var_in_literal(Clause,P,Vars1,Var),
%%     var_in_literal(Clause,Q,Vars2,Var),
%%     var_pos(Var,Vars1,Pos1),
%%     var_pos(Var,Vars2,Pos2),
%%     type(P,Pos1,Type1),
%%     type(Q,Pos2,Type2),
%%     Type1 != Type2.

%% TYPES V2
%% :-
%%     clause_var(Clause,Var),
%%     #count{Type1 : type(P,Pos1,Type1), var_pos(Var,Vars1,Pos1), var_in_literal(Clause,P,Vars1,Var)} > 1.

%% TYPES V3
%% var_type(Clause,Var,Type):-
%%     var_in_literal(Clause,P,Vars,Var),
%%     var_pos(Var,Vars,Pos),
%%     type(P,Pos,Type).
%% :-
%%     clause(Clause),
%%     var_type(Clause,Var,T1),
%%     var_type(Clause,Var,T2),
%%     T1 < T2.

%% TYPES V4
var_type(Clause,Var,Type):-
    var_in_literal(Clause,P,Vars,Var),
    var_pos(Var,Vars,Pos),
    type(P,Pos,Type).
:-
    clause_var(Clause,Var),
    #count{Type : var_type(Clause,Var,Type)} > 1.

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    Var1 != Var2. % simply reduces grounding

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% MUST BE CONNECTED
%% CAN WE PUSH THIS TO THE GUESS?
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
    %% Pos1 != Pos2,
    %% TODO CHECK IT BREAKS SYMMETRY
    Pos1 < Pos2,
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
:-
    functional(P,3),
    direction(P,0,in),
    direction(P,1,in),
    direction(P,2,out),
    literal(Clause,P,(Var1,Var2,Var3)),
    literal(Clause,P,(Var1,Var2,Var4)),
    Var3 != Var3.


%% ########################################
%% RECURSION
%% ########################################


non_separable:-
    head_literal(_,P,A,_),
    body_literal(_,P,A,_).

separable:-
    not non_separable.

:-
    recursive(0).

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
%% WHY DID WE ADD THIS??
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
%% subsumes(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% :-
%%     clause(Clause1),
%%     clause(Clause2),
%%     Clause1 != Clause2,
%%     subsumes(Clause1,Clause2).

%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% %% ORDER CLAUSES BY SIZE
%% THIS SLOWS THINGS DOWN TERRIBLY
clause_size(Clause1,N):-
    clause(Clause1),
    max_body(MaxN),
    N > 0,
    N <= MaxN,
    #count{P1,Vars1 : body_literal(Clause1,P1,_,Vars1)} = N.

%% ORDER BY CLAUSE SIZE
:-
    C2 > C1,
    not recursive(C1),
    not recursive(C2),
    clause_size(C1,N1),
    clause_size(C2,N2),
    N1 > N2.

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
% WARN: ADDING following fragment with name 'modes_file'
max_vars(6).
max_body(4).
max_clauses(2).

%% Add 1 to each element. In Haskell: map (+1)
%%
%% f(X, Y):- 
%%     empty(X),
%%     identical(X, Y).
%%
%% f(X, Y) :-
%%     cons1(HX, BX, X),
%%     f(BX, BY),
%%     succ(HX, HY), 
%%     cons2(HY, BY, Y).


% Prevent recursion in first clause.
:-
    modeh(P,A),
    body_literal(0,_,P,A).


modeh(f,2).
type(f,0,list).
type(f,1,list).
direction(f,0,in).
direction(f,1,out).

modeb(cons1,3).
type(cons1,0,int).
type(cons1,1,list).
type(cons1,2,list).
direction(cons1,0,out).
direction(cons1,1,out).
direction(cons1,2,in).

modeb(cons2,3).
type(cons2,0,int).
type(cons2,1,list).
type(cons2,2,list).
direction(cons2,0,in).
direction(cons2,1,in).
direction(cons2,2,out).

modeb(succ,2).
type(succ,0,int).
type(succ,1,int).
direction(succ,0,in).
direction(succ,1,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(identical,2).
type(identical,0,list).
type(identical,1,list).
direction(identical,0,in).
direction(identical,1,out).

#show var/4.
#show literal/4.

% WARN: ADDING following fragment with name 'program_size'
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
  size(n),
  #count{Clause,P,Vars : literal(Clause,P,Vars)} != n.

% WARN: GROUNDING parts '[('alan', []), ('modes_file', [])]'
% WARN: GROUNDING parts '[('program_size', [1])]'
% WARN: Atom size(1) is assigned/made True
% WARN: Atom size(1) is released/made False
% WARN: GROUNDING parts '[('program_size', [2])]'
% WARN: Atom size(2) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1identicalV0V1'
included_clause_fV0V1identicalV0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV1)),CV1!=CV0.
% WARN: ADDING following fragment with name 'specialisation0'
:-included_clause_fV0V1identicalV0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination1'
:-included_clause_fV0V1identicalV0V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation2'
:-included_clause_fV0V1identicalV0V1(C0),clause_size(C0,1).
% WARN: GROUNDING parts '[('fV0V1identicalV0V1', []), ('specialisation0', []), ('elimination1', []), ('generalisation2', [])]'
% WARN: Atom size(2) is released/made False
% WARN: GROUNDING parts '[('program_size', [3])]'
% WARN: Atom size(3) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V0'
included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation3'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination4'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V0', []), ('specialisation3', []), ('elimination4', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V1'
included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation5'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination6'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V1', []), ('specialisation5', []), ('elimination6', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V1cons1V2V1V0'
included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation7'
:-included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination8'
:-included_clause_fV0V1cons1V2V0V1cons1V2V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V1cons1V2V1V0', []), ('specialisation7', []), ('elimination8', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons1V3V2V0'
included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation9'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination10'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons1V3V2V0', []), ('specialisation9', []), ('elimination10', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation11'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination12'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation13'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V1(C0),clause_size(C0,2).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V1', []), ('specialisation11', []), ('elimination12', []), ('generalisation13', [])]'
% WARN: ADDING following fragment with name 'fV0V1identicalV0V2identicalV2V1'
included_clause_fV0V1identicalV0V2identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1.
% WARN: ADDING following fragment with name 'specialisation14'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination15'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation16'
:-included_clause_fV0V1identicalV0V2identicalV2V1(C0),clause_size(C0,2).
% WARN: GROUNDING parts '[('fV0V1identicalV0V2identicalV2V1', []), ('specialisation14', []), ('elimination15', []), ('generalisation16', [])]'
% WARN: Atom size(3) is released/made False
% WARN: GROUNDING parts '[('program_size', [4])]'
% WARN: Atom size(4) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V3V0V1succV3V2'
included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV0,CV1)),body_literal(C,succ,2,(CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation17'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination18'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V3V0V1succV3V2', []), ('specialisation17', []), ('elimination18', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation19'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination20'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0identicalV1V3', []), ('specialisation19', []), ('elimination20', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation21'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination22'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons2V2V0V3', []), ('specialisation21', []), ('elimination22', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation23'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination24'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V0', []), ('specialisation23', []), ('elimination24', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1'
included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,identical,2,(CV3,CV1)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation25'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination26'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V3V0identicalV3V1', []), ('specialisation25', []), ('elimination26', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1'
included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV1)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation27'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination28'
:-included_clause_fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V3V0cons2V2V0V1', []), ('specialisation27', []), ('elimination28', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation29'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination30'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons2V2V1V3', []), ('specialisation29', []), ('elimination30', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation31'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination32'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0cons1V2V3V1', []), ('specialisation31', []), ('elimination32', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation33'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination34'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V0identicalV0V3', []), ('specialisation33', []), ('elimination34', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation35'
:-included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination36'
:-included_clause_fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons2V2V3V1identicalV0V3', []), ('specialisation35', []), ('elimination36', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation37'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination38'
:-included_clause_fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons1V2V1V3identicalV0V3', []), ('specialisation37', []), ('elimination38', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3'
included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV0,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation39'
:-included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination40'
:-included_clause_fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V0V3cons2V2V0V1identicalV0V3', []), ('specialisation39', []), ('elimination40', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1'
included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation41'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination42'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V3identicalV3V1', []), ('specialisation41', []), ('elimination42', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1'
included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation43'
:-included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination44'
:-included_clause_fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons2V2V0V3identicalV3V1', []), ('specialisation43', []), ('elimination44', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation45'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination46'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V0V3', []), ('specialisation45', []), ('elimination46', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons1V4V3V0succV4V2'
included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,succ,2,(CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation47'
:-included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination48'
:-included_clause_fV0V1cons1V2V1V3cons1V4V3V0succV4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons1V4V3V0succV4V2', []), ('specialisation47', []), ('elimination48', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V2V3V1succV4V2'
included_clause_fV0V1cons1V4V3V0cons2V2V3V1succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,succ,2,(CV4,CV2)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation49'
:-included_clause_fV0V1cons1V4V3V0cons2V2V3V1succV4V2(C0),C0 < 1,not clause(1).
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V2V3V1succV4V2', []), ('specialisation49', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V4V1V3succV4V2'
included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV3)),body_literal(C,succ,2,(CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation50'
:-included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination51'
:-included_clause_fV0V1cons1V2V3V0cons1V4V1V3succV4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V4V1V3succV4V2', []), ('specialisation50', []), ('elimination51', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1'
included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation52'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination53'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons1V3V4V0identicalV2V1', []), ('specialisation52', []), ('elimination53', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation54'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination55'
:-included_clause_fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons1V3V4V0cons2V3V2V1', []), ('specialisation54', []), ('elimination55', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4'
included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation56'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination57'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),separable.
% WARN: ADDING following fragment with name 'generalisation58'
:-included_clause_fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V4cons2V3V2V1identicalV0V4', []), ('specialisation56', []), ('elimination57', []), ('generalisation58', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4'
included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation59'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination60'
:-included_clause_fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons1V3V2V4identicalV0V4', []), ('specialisation59', []), ('elimination60', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V3V1V0succV2V3'
included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,succ,2,(CV2,CV3)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation61'
:-included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination62'
:-included_clause_fV0V1cons1V2V1V0cons2V3V1V0succV2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V3V1V0succV2V3', []), ('specialisation61', []), ('elimination62', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation63'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination64'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV0V3', []), ('specialisation63', []), ('elimination64', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3'
included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation65'
:-included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination66'
:-included_clause_fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons1V2V3V1identicalV0V3', []), ('specialisation65', []), ('elimination66', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons2,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation67'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination68'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V1', []), ('specialisation67', []), ('elimination68', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation69'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination70'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV1V3', []), ('specialisation69', []), ('elimination70', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1emptyV3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation71'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination72'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1emptyV3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1emptyV3', []), ('specialisation71', []), ('elimination72', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation73'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination74'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV3V1', []), ('specialisation73', []), ('elimination74', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation75'
:-included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination76'
:-included_clause_fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V1V3cons1V2V3V1', []), ('specialisation75', []), ('elimination76', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons2,3,(CV2,CV1,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation77'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination78'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V1V3', []), ('specialisation77', []), ('elimination78', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1'
included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation79'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination80'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V0cons1V2V3V1', []), ('specialisation79', []), ('elimination80', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,identical,2,(CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation81'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination82'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1identicalV3V0', []), ('specialisation81', []), ('elimination82', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0'
included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV2,CV3,CV1)),body_literal(C,cons2,3,(CV2,CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation83'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination84'
:-included_clause_fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V2V3V1cons2V2V3V0', []), ('specialisation83', []), ('elimination84', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1'
included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation85'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination86'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V2V3V1cons2V2V0V1', []), ('specialisation85', []), ('elimination86', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1'
included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV0)),body_literal(C,identical,2,(CV3,CV1)),body_literal(C,cons1,3,(CV2,CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation87'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination88'
:-included_clause_fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V3V0cons1V2V3V1identicalV3V1', []), ('specialisation87', []), ('elimination88', [])]'
% WARN: ADDING following fragment with name 'fV0V1identicalV0V3identicalV2V1identicalV3V2'
included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation89'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination90'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),separable.
% WARN: ADDING following fragment with name 'generalisation91'
:-included_clause_fV0V1identicalV0V3identicalV2V1identicalV3V2(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1identicalV0V3identicalV2V1identicalV3V2', []), ('specialisation89', []), ('elimination90', []), ('generalisation91', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3'
included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation92'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination93'
:-included_clause_fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V0V3identicalV0V3', []), ('specialisation92', []), ('elimination93', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3'
included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation94'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination95'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons2V2V0V3identicalV0V3', []), ('specialisation94', []), ('elimination95', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,identical,2,(CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation96'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination97'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),separable.
% WARN: ADDING following fragment with name 'generalisation98'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V4identicalV4V1', []), ('specialisation96', []), ('elimination97', []), ('generalisation98', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2'
included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation99'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination100'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V4V2V1cons2V4V3V2', []), ('specialisation99', []), ('elimination100', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2'
included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation101'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination102'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),separable.
% WARN: ADDING following fragment with name 'generalisation103'
:-included_clause_fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2(C0),clause_size(C0,3).
% WARN: GROUNDING parts '[('fV0V1cons1V4V3V0cons2V4V2V1identicalV3V2', []), ('specialisation101', []), ('elimination102', []), ('generalisation103', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons1,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation104'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination105'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V1cons1V3V2V0identicalV2V1', []), ('specialisation104', []), ('elimination105', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation106'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination107'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV2V0', []), ('specialisation106', []), ('elimination107', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation108'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination109'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV2V0', []), ('specialisation108', []), ('elimination109', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation110'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination111'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V1V2', []), ('specialisation110', []), ('elimination111', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation112'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination113'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V1V2identicalV2V1', []), ('specialisation112', []), ('elimination113', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation114'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination115'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V1V2', []), ('specialisation114', []), ('elimination115', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2'
included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation116'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination117'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V0cons1V3V4V2', []), ('specialisation116', []), ('elimination117', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation118'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination119'
:-included_clause_fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V1V0identicalV2V1', []), ('specialisation118', []), ('elimination119', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation120'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination121'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV2V1', []), ('specialisation120', []), ('elimination121', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation122'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination123'
:-included_clause_fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V2V0identicalV2V1', []), ('specialisation122', []), ('elimination123', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation124'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination125'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV2V1', []), ('specialisation124', []), ('elimination125', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4'
included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV2,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation126'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination127'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V0identicalV2V4', []), ('specialisation126', []), ('elimination127', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation128'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination129'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV1V2', []), ('specialisation128', []), ('elimination129', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation130'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination131'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V2V0', []), ('specialisation130', []), ('elimination131', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation132'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination133'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons2V3V0V2', []), ('specialisation132', []), ('elimination133', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation134'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination135'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0identicalV0V2', []), ('specialisation134', []), ('elimination135', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0emptyV2'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,empty,1,(CV2,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation136'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination137'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0emptyV2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0emptyV2', []), ('specialisation136', []), ('elimination137', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1emptyV2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,empty,1,(CV2,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation138'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination139'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1emptyV2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1emptyV2', []), ('specialisation138', []), ('elimination139', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation140'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination141'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V2V0', []), ('specialisation140', []), ('elimination141', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation142'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination143'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1cons2V3V0V2', []), ('specialisation142', []), ('elimination143', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons2,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation144'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination145'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV0V2', []), ('specialisation144', []), ('elimination145', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2'
included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,identical,2,(CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation146'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination147'
:-included_clause_fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons2V3V0V1identicalV1V2', []), ('specialisation146', []), ('elimination147', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1'
included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation148'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination149'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V1V2cons2V3V2V1', []), ('specialisation148', []), ('elimination149', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation150'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination151'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V1', []), ('specialisation150', []), ('elimination151', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation152'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination153'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V1identicalV0V2', []), ('specialisation152', []), ('elimination153', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation154'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination155'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V1identicalV1V2', []), ('specialisation154', []), ('elimination155', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation156'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination157'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V2V1identicalV0V2', []), ('specialisation156', []), ('elimination157', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation158'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination159'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0cons2V4V3V2', []), ('specialisation158', []), ('elimination159', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation160'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination161'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V1V0identicalV0V2', []), ('specialisation160', []), ('elimination161', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V3identicalV3V1'
included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,identical,2,(CV3,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation162'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination163'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V3identicalV3V1', []), ('specialisation162', []), ('elimination163', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,identical,2,(CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation164'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination165'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2identicalV1V2', []), ('specialisation164', []), ('elimination165', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V0identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation166'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination167'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V0identicalV1V2', []), ('specialisation166', []), ('elimination167', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V2V0V1succV3V2'
included_clause_fV0V1cons1V3V1V0cons2V2V0V1succV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,succ,2,(CV3,CV2)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation168'
:-included_clause_fV0V1cons1V3V1V0cons2V2V0V1succV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination169'
:-included_clause_fV0V1cons1V3V1V0cons2V2V0V1succV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V2V0V1succV3V2', []), ('specialisation168', []), ('elimination169', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V3cons2V2V0V1identicalV0V3'
included_clause_fV0V1cons1V2V1V3cons2V2V0V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV2,CV1,CV3)),body_literal(C,cons2,3,(CV2,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation170'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination171'
:-included_clause_fV0V1cons1V2V1V3cons2V2V0V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V3cons2V2V0V1identicalV0V3', []), ('specialisation170', []), ('elimination171', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V1V2cons2V3V1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation172'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination173'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V1V2cons2V3V1V2', []), ('specialisation172', []), ('elimination173', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V1V2identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V1V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation174'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination175'
:-included_clause_fV0V1cons1V3V1V2cons2V3V1V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V1V2identicalV0V2', []), ('specialisation174', []), ('elimination175', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V1cons1V3V1V2identicalV0V2'
included_clause_fV0V1cons1V3V0V1cons1V3V1V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons1,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation176'
:-included_clause_fV0V1cons1V3V0V1cons1V3V1V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination177'
:-included_clause_fV0V1cons1V3V0V1cons1V3V1V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V1cons1V3V1V2identicalV0V2', []), ('specialisation176', []), ('elimination177', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V1V2identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation178'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination179'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V1V2identicalV0V2', []), ('specialisation178', []), ('elimination179', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V2cons2V3V2V0identicalV0V2'
included_clause_fV0V1cons1V3V1V2cons2V3V2V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation180'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination181'
:-included_clause_fV0V1cons1V3V1V2cons2V3V2V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V2cons2V3V2V0identicalV0V2', []), ('specialisation180', []), ('elimination181', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V1V2cons2V3V0V2'
included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation182'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination183'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V1V2cons2V3V0V2', []), ('specialisation182', []), ('elimination183', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V1V2identicalV1V2'
included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,identical,2,(CV1,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation184'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination185'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2identicalV1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V1V2identicalV1V2', []), ('specialisation184', []), ('elimination185', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V1V2identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons2V3V1V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation186'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination187'
:-included_clause_fV0V1cons1V3V1V0cons2V3V1V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V1V2identicalV0V2', []), ('specialisation186', []), ('elimination187', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V2V0identicalV0V2'
included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation188'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination189'
:-included_clause_fV0V1cons1V3V1V0cons2V3V2V0identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V2V0identicalV0V2', []), ('specialisation188', []), ('elimination189', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2identicalV2V0'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,identical,2,(CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation190'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination191'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2identicalV2V0', []), ('specialisation190', []), ('elimination191', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2cons2V3V1V2'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation192'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination193'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2cons2V3V1V2', []), ('specialisation192', []), ('elimination193', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V0'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation194'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination195'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2cons2V3V2V0', []), ('specialisation194', []), ('elimination195', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons2V3V0V2emptyV2'
included_clause_fV0V1cons1V3V1V0cons2V3V0V2emptyV2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,empty,1,(CV2,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation196'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2emptyV2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination197'
:-included_clause_fV0V1cons1V3V1V0cons2V3V0V2emptyV2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons2V3V0V2emptyV2', []), ('specialisation196', []), ('elimination197', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0succV2V3succV3V2'
included_clause_fV0V1cons1V3V1V0succV2V3succV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,succ,2,(CV3,CV2)),body_literal(C,succ,2,(CV2,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation198'
:-included_clause_fV0V1cons1V3V1V0succV2V3succV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination199'
:-included_clause_fV0V1cons1V3V1V0succV2V3succV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0succV2V3succV3V2', []), ('specialisation198', []), ('elimination199', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V3V1V0succV2V3'
included_clause_fV0V1cons1V2V1V0cons1V3V1V0succV2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,succ,2,(CV2,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation200'
:-included_clause_fV0V1cons1V2V1V0cons1V3V1V0succV2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination201'
:-included_clause_fV0V1cons1V2V1V0cons1V3V1V0succV2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V3V1V0succV2V3', []), ('specialisation200', []), ('elimination201', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons1V3V0V1succV2V3'
included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,succ,2,(CV2,CV3)),body_literal(C,cons1,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation202'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination203'
:-included_clause_fV0V1cons1V2V1V0cons1V3V0V1succV2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons1V3V0V1succV2V3', []), ('specialisation202', []), ('elimination203', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V3identicalV1V3'
included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons2,3,(CV2,CV1,CV3)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation204'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination205'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V3identicalV1V3', []), ('specialisation204', []), ('elimination205', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V3cons2V2V3V0'
included_clause_fV0V1cons1V2V1V0cons2V2V1V3cons2V2V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,cons2,3,(CV2,CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation206'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3cons2V2V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination207'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3cons2V2V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V3cons2V2V3V0', []), ('specialisation206', []), ('elimination207', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V3emptyV3'
included_clause_fV0V1cons1V2V1V0cons2V2V1V3emptyV3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation208'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3emptyV3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination209'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3emptyV3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V3emptyV3', []), ('specialisation208', []), ('elimination209', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V2V1V0cons2V2V1V3identicalV3V0'
included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV2,CV1,CV0)),body_literal(C,cons2,3,(CV2,CV1,CV3)),body_literal(C,identical,2,(CV3,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2.
% WARN: ADDING following fragment with name 'specialisation210'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination211'
:-included_clause_fV0V1cons1V2V1V0cons2V2V1V3identicalV3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V2V1V0cons2V2V1V3identicalV3V0', []), ('specialisation210', []), ('elimination211', [])]'
% WARN: Atom size(4) is released/made False
% WARN: GROUNDING parts '[('program_size', [5])]'
% WARN: Atom size(5) is assigned/made True
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V2V0cons2V3V2V1cons2V4V0V1'
included_clause_fV0V1cons1V3V2V1cons1V4V2V0cons2V3V2V1cons2V4V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV2,CV0)),body_literal(C,cons2,3,(CV4,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation212'
:-included_clause_fV0V1cons1V3V2V1cons1V4V2V0cons2V3V2V1cons2V4V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination213'
:-included_clause_fV0V1cons1V3V2V1cons1V4V2V0cons2V3V2V1cons2V4V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V2V0cons2V3V2V1cons2V4V0V1', []), ('specialisation212', []), ('elimination213', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation214'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination215'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V0cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V0cons2V3V2V1', []), ('specialisation214', []), ('elimination215', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V1cons2V3V2V1'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V1cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation216'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V1cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination217'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V1cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons1V4V2V1cons2V3V2V1', []), ('specialisation216', []), ('elimination217', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V0V2cons1V4V1V0cons2V3V2V1'
included_clause_fV0V1cons1V3V2V1cons1V4V0V2cons1V4V1V0cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV0,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation218'
:-included_clause_fV0V1cons1V3V2V1cons1V4V0V2cons1V4V1V0cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination219'
:-included_clause_fV0V1cons1V3V2V1cons1V4V0V2cons1V4V1V0cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V0V2cons1V4V1V0cons2V3V2V1', []), ('specialisation218', []), ('elimination219', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV3V4'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV3V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,succ,2,(CV3,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation220'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV3V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination221'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV3V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV3V4', []), ('specialisation220', []), ('elimination221', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV4V3'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV4V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,succ,2,(CV4,CV3)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation222'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV4V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination223'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV4V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1succV4V3', []), ('specialisation222', []), ('elimination223', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons1V4V1V2cons2V3V2V1'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V1V2cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation224'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V1V2cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination225'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons1V4V1V2cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons1V4V1V2cons2V3V2V1', []), ('specialisation224', []), ('elimination225', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons2V3V2V1identicalV4V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons2V3V2V1identicalV4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,identical,2,(CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation226'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons2V3V2V1identicalV4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination227'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons2V3V2V1identicalV4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons2V3V2V1identicalV4V1', []), ('specialisation226', []), ('elimination227', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V1cons2V3V2V1identicalV0V4'
included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons2V3V2V1identicalV0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation228'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons2V3V2V1identicalV0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination229'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons2V3V2V1identicalV0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V1cons2V3V2V1identicalV0V4', []), ('specialisation228', []), ('elimination229', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V1'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation230'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination231'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V1', []), ('specialisation230', []), ('elimination231', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V1V2'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation232'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination233'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V1V2', []), ('specialisation232', []), ('elimination233', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V0'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV4,CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation234'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination235'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V2V0', []), ('specialisation234', []), ('elimination235', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V0V2'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV4,CV0,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation236'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination237'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0cons2V3V2V1cons2V4V0V2', []), ('specialisation236', []), ('elimination237', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV4V3'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV4V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,succ,2,(CV4,CV3)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,identical,2,(CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation238'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV4V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination239'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV4V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV4V3', []), ('specialisation238', []), ('elimination239', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV3V4'
included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV3V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,identical,2,(CV2,CV0)),body_literal(C,succ,2,(CV3,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation240'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV3V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination241'
:-included_clause_fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV3V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V4V1V0identicalV2V0succV3V4', []), ('specialisation240', []), ('elimination241', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0identicalV2V0identicalV4V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0identicalV2V0identicalV4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,identical,2,(CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,identical,2,(CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation242'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0identicalV2V0identicalV4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination243'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0identicalV2V0identicalV4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0identicalV2V0identicalV4V1', []), ('specialisation242', []), ('elimination243', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V1identicalV0V4identicalV2V0'
included_clause_fV0V1cons1V3V1V4cons1V3V2V1identicalV0V4identicalV2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,identical,2,(CV2,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation244'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1identicalV0V4identicalV2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination245'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1identicalV0V4identicalV2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V1identicalV0V4identicalV2V0', []), ('specialisation244', []), ('elimination245', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons1V5V2V4cons2V5V0V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V2V4cons2V5V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation246'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V2V4cons2V5V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination247'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V2V4cons2V5V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons1V5V2V4cons2V5V0V1', []), ('specialisation246', []), ('elimination247', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V4V0cons2V3V4V2cons2V5V0V1'
included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V4V2cons2V5V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation248'
:-included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V4V2cons2V5V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination249'
:-included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V4V2cons2V5V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V4V0cons2V3V4V2cons2V5V0V1', []), ('specialisation248', []), ('elimination249', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V4V0cons2V5V0V1'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V4V0cons2V5V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation250'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V4V0cons2V5V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination251'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V4V0cons2V5V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V4V0cons2V5V0V1', []), ('specialisation250', []), ('elimination251', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V4V0cons2V5V0V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V4V0cons2V5V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation252'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V4V0cons2V5V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination253'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V4V0cons2V5V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V4V0cons2V5V0V1', []), ('specialisation252', []), ('elimination253', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V4V0cons2V3V2V4cons2V5V0V1'
included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V2V4cons2V5V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation254'
:-included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V2V4cons2V5V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination255'
:-included_clause_fV0V1cons1V3V2V1cons1V5V4V0cons2V3V2V4cons2V5V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V4V0cons2V3V2V4cons2V5V0V1', []), ('specialisation254', []), ('elimination255', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V1V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV5,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation256'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination257'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V1V4', []), ('specialisation256', []), ('elimination257', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V1V4cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V1V4cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation258'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V1V4cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination259'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V1V4cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V1V4cons2V3V2V4', []), ('specialisation258', []), ('elimination259', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV4)),body_literal(C,cons2,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation260'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination261'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V4V2', []), ('specialisation260', []), ('elimination261', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation262'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination263'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V1V4cons2V5V2V4', []), ('specialisation262', []), ('elimination263', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V1V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV4)),body_literal(C,cons1,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation264'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination265'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V1V4', []), ('specialisation264', []), ('elimination265', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V1V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation266'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination267'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V1V4', []), ('specialisation266', []), ('elimination267', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V0V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation268'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination269'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V0V4', []), ('specialisation268', []), ('elimination269', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V0V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons2,3,(CV5,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation270'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination271'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V0V4', []), ('specialisation270', []), ('elimination271', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V0V4'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation272'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination273'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V0V4', []), ('specialisation272', []), ('elimination273', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V0V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation274'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination275'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V0V4', []), ('specialisation274', []), ('elimination275', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation276'
:-included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination277'
:-included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons2V5V2V4', []), ('specialisation276', []), ('elimination277', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons1V5V4V2'
included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation278'
:-included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination279'
:-included_clause_fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V4cons1V3V2V1cons1V5V1V0cons1V5V4V2', []), ('specialisation278', []), ('elimination279', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation280'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination281'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V4V2', []), ('specialisation280', []), ('elimination281', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V1'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation282'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination283'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V1', []), ('specialisation282', []), ('elimination283', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation284'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination285'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V1cons2V3V2V4', []), ('specialisation284', []), ('elimination285', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV4,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation286'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination287'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V1', []), ('specialisation286', []), ('elimination287', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V0'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons2,3,(CV3,CV4,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation288'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination289'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V0', []), ('specialisation288', []), ('elimination289', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V0cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV4,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation290'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination291'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V0cons2V5V2V4', []), ('specialisation290', []), ('elimination291', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V1V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation292'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination293'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V1V4', []), ('specialisation292', []), ('elimination293', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V1V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation294'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination295'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V1V4', []), ('specialisation294', []), ('elimination295', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V1V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation296'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination297'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V1V4', []), ('specialisation296', []), ('elimination297', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V1V4'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V1V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV1,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation298'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V1V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination299'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V1V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V1V4', []), ('specialisation298', []), ('elimination299', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation300'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination301'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V2V4', []), ('specialisation300', []), ('elimination301', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons1V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons1V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation302'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons1V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination303'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons1V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons1V5V2V4', []), ('specialisation302', []), ('elimination303', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V0V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV0,CV4)),body_literal(C,cons1,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation304'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination305'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V0V4', []), ('specialisation304', []), ('elimination305', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation306'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination307'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V2V4', []), ('specialisation306', []), ('elimination307', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation308'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination309'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V2V4cons2V3V2V4', []), ('specialisation308', []), ('elimination309', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation310'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination311'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V2V4', []), ('specialisation310', []), ('elimination311', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation312'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination313'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V1cons1V5V1V0cons2V5V2V4', []), ('specialisation312', []), ('elimination313', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation314'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination315'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V2V4', []), ('specialisation314', []), ('elimination315', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation316'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination317'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons2V5V2V4', []), ('specialisation316', []), ('elimination317', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V1cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V1cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation318'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V1cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination319'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V1cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V1cons2V5V2V4', []), ('specialisation318', []), ('elimination319', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation320'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination321'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V2V4', []), ('specialisation320', []), ('elimination321', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation322'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination323'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V2V4', []), ('specialisation322', []), ('elimination323', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation324'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination325'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V4V2cons2V5V2V4', []), ('specialisation324', []), ('elimination325', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V2V4'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV5,CV2,CV4)),body_literal(C,cons1,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation326'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination327'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons2V5V2V4', []), ('specialisation326', []), ('elimination327', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V0V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V0V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV0,CV4)),body_literal(C,cons1,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation328'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V0V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination329'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V0V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V0V4', []), ('specialisation328', []), ('elimination329', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons1V5V4V2'
included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation330'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination331'
:-included_clause_fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons1V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V4cons1V3V2V1cons1V5V1V0cons1V5V4V2', []), ('specialisation330', []), ('elimination331', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V1'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons2,3,(CV3,CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation332'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination333'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V1', []), ('specialisation332', []), ('elimination333', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation334'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination335'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V2V4', []), ('specialisation334', []), ('elimination335', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation336'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination337'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V2', []), ('specialisation336', []), ('elimination337', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation338'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination339'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V2cons2V3V4V2', []), ('specialisation338', []), ('elimination339', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation340'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination341'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V2', []), ('specialisation340', []), ('elimination341', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation342'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination343'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons1V5V4V2', []), ('specialisation342', []), ('elimination343', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V0V4cons1V5V1V0'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V0V4cons1V5V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons1,3,(CV5,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation344'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V0V4cons1V5V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination345'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V0V4cons1V5V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V0V4cons1V5V1V0', []), ('specialisation344', []), ('elimination345', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V0V4cons1V5V1V0cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V0V4cons1V5V1V0cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV0,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation346'
:-included_clause_fV0V1cons1V3V2V1cons1V5V0V4cons1V5V1V0cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination347'
:-included_clause_fV0V1cons1V3V2V1cons1V5V0V4cons1V5V1V0cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V0V4cons1V5V1V0cons2V3V2V4', []), ('specialisation346', []), ('elimination347', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV0,CV4)),body_literal(C,cons2,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation348'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination349'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V0V4cons2V5V4V2', []), ('specialisation348', []), ('elimination349', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V1'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation350'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination351'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V1', []), ('specialisation350', []), ('elimination351', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V1'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons2,3,(CV5,CV4,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation352'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination353'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V1', []), ('specialisation352', []), ('elimination353', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V0'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation354'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination355'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons1V5V4V0', []), ('specialisation354', []), ('elimination355', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V2V4'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V2V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation356'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V2V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination357'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V2V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V2V4', []), ('specialisation356', []), ('elimination357', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V0'
included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV4)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation358'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination359'
:-included_clause_fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V2V4cons1V5V1V0cons1V5V4V0', []), ('specialisation358', []), ('elimination359', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation360'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination361'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons1V5V4V0cons2V3V4V2', []), ('specialisation360', []), ('elimination361', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons2,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation362'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination363'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V2', []), ('specialisation362', []), ('elimination363', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV4,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation364'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination365'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V2', []), ('specialisation364', []), ('elimination365', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V4V2'
included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V4V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV4,CV2)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation366'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V4V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination367'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V4V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V0cons1V5V1V0cons2V5V4V2', []), ('specialisation366', []), ('elimination367', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V0'
included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV4,CV2)),body_literal(C,cons2,3,(CV5,CV4,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation368'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination369'
:-included_clause_fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V3V4V2cons1V5V1V0cons2V5V4V0', []), ('specialisation368', []), ('elimination369', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V0'
included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV4)),body_literal(C,cons2,3,(CV5,CV4,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation370'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination371'
:-included_clause_fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V1cons1V5V1V0cons2V3V2V4cons2V5V4V0', []), ('specialisation370', []), ('elimination371', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V1V3'
included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV1,CV3)),body_literal(C,cons1,3,(CV4,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation372'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination373'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V1V3', []), ('specialisation372', []), ('elimination373', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons2V5V3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons2V5V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons2,3,(CV5,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation374'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons2V5V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination375'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons2V5V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons2V5V3V2', []), ('specialisation374', []), ('elimination375', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons1V5V2V3'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons1V5V2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation376'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons1V5V2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination377'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons1V5V2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0cons1V5V1V0cons1V5V2V3', []), ('specialisation376', []), ('elimination377', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V0'
included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation378'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination379'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V0', []), ('specialisation378', []), ('elimination379', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V1'
included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons1,3,(CV5,CV3,CV1)),body_literal(C,cons1,3,(CV4,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation380'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination381'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons1V5V3V1', []), ('specialisation380', []), ('elimination381', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V0V3'
included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV1,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV3)),body_literal(C,cons1,3,(CV4,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation382'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination383'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V3cons1V5V1V0cons2V5V0V3', []), ('specialisation382', []), ('elimination383', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V0cons1V4V1V2cons2V4V0V3cons2V4V3V2'
included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V0V3cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons2,3,(CV4,CV0,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation384'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V0V3cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination385'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V0V3cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V0cons1V4V1V2cons2V4V0V3cons2V4V3V2', []), ('specialisation384', []), ('elimination385', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V0V3cons2V4V3V2identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V0V3cons2V4V3V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV0,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation386'
:-included_clause_fV0V1cons1V4V1V2cons2V4V0V3cons2V4V3V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination387'
:-included_clause_fV0V1cons1V4V1V2cons2V4V0V3cons2V4V3V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V0V3cons2V4V3V2identicalV0V2', []), ('specialisation386', []), ('elimination387', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V3V2identicalV0V3identicalV3V2'
included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V3identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation388'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V3identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination389'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V3identicalV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V3V2identicalV0V3identicalV3V2', []), ('specialisation388', []), ('elimination389', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V0V2'
included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation390'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination391'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V0V2', []), ('specialisation390', []), ('elimination391', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V3V2'
included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons2,3,(CV5,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation392'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination393'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2cons2V5V3V2', []), ('specialisation392', []), ('elimination393', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V3cons1V5V3V0cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V5V2V3cons1V5V3V0cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons1,3,(CV5,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation394'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V3cons1V5V3V0cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination395'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V3cons1V5V3V0cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V3cons1V5V3V0cons2V4V3V2', []), ('specialisation394', []), ('elimination395', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V0cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V0cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation396'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V0cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination397'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V0cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V0cons2V4V3V2', []), ('specialisation396', []), ('elimination397', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2succV5V4'
included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2succV5V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,succ,2,(CV5,CV4)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation398'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2succV5V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination399'
:-included_clause_fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2succV5V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V3V0cons2V4V3V2succV5V4', []), ('specialisation398', []), ('elimination399', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V1V3'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV5,CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation400'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination401'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V1V3', []), ('specialisation400', []), ('elimination401', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V0V3cons1V4V1V2cons2V4V3V2identicalV0V3'
included_clause_fV0V1cons1V4V0V3cons1V4V1V2cons2V4V3V2identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV4,CV0,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation402'
:-included_clause_fV0V1cons1V4V0V3cons1V4V1V2cons2V4V3V2identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination403'
:-included_clause_fV0V1cons1V4V0V3cons1V4V1V2cons2V4V3V2identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V0V3cons1V4V1V2cons2V4V3V2identicalV0V3', []), ('specialisation402', []), ('elimination403', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V1V3cons2V4V3V2identicalV0V3'
included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons2V4V3V2identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation404'
:-included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons2V4V3V2identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination405'
:-included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons2V4V3V2identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V1V3cons2V4V3V2identicalV0V3', []), ('specialisation404', []), ('elimination405', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV0V3'
included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation406'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination407'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV0V3', []), ('specialisation406', []), ('elimination407', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV0V3'
included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation408'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination409'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV0V3', []), ('specialisation408', []), ('elimination409', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2identicalV5V3'
included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2identicalV5V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV5,CV0)),body_literal(C,identical,2,(CV5,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation410'
:-included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2identicalV5V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination411'
:-included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2identicalV5V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2identicalV5V3', []), ('specialisation410', []), ('elimination411', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V0cons1V4V1V2cons1V4V3V1cons2V4V3V2'
included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons1V4V3V1cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV3,CV1)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation412'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons1V4V3V1cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination413'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons1V4V3V1cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V0cons1V4V1V2cons1V4V3V1cons2V4V3V2', []), ('specialisation412', []), ('elimination413', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV1V3'
included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation414'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination415'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V0cons1V4V1V2cons2V4V3V2identicalV1V3', []), ('specialisation414', []), ('elimination415', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V0cons1V4V1V2cons2V4V1V3cons2V4V3V2'
included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V1V3cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV0)),body_literal(C,cons2,3,(CV4,CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation416'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V1V3cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination417'
:-included_clause_fV0V1cons1V4V1V0cons1V4V1V2cons2V4V1V3cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V0cons1V4V1V2cons2V4V1V3cons2V4V3V2', []), ('specialisation416', []), ('elimination417', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V2cons2V4V3V2identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V2cons2V4V3V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV3,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation418'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V2cons2V4V3V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination419'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V2cons2V4V3V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V2cons2V4V3V2identicalV0V2', []), ('specialisation418', []), ('elimination419', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V5cons1V4V5V0cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons1V4V5V0cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV5,CV0)),body_literal(C,cons1,3,(CV4,CV3,CV5)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation420'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons1V4V5V0cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination421'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons1V4V5V0cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V5cons1V4V5V0cons2V4V3V2', []), ('specialisation420', []), ('elimination421', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V1V3cons2V4V3V2identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V1V3cons2V4V3V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation422'
:-included_clause_fV0V1cons1V4V1V2cons2V4V1V3cons2V4V3V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination423'
:-included_clause_fV0V1cons1V4V1V2cons2V4V1V3cons2V4V3V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V1V3cons2V4V3V2identicalV0V2', []), ('specialisation422', []), ('elimination423', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV1V3'
included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV1V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,identical,2,(CV1,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation424'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV1V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination425'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV1V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV1V3', []), ('specialisation424', []), ('elimination425', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V1cons2V4V3V2identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V1cons2V4V3V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV3,CV1)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation426'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V1cons2V4V3V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination427'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V1cons2V4V3V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V1cons2V4V3V2identicalV0V2', []), ('specialisation426', []), ('elimination427', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V2V3cons2V4V3V2identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V2V3cons2V4V3V2identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation428'
:-included_clause_fV0V1cons1V4V1V2cons2V4V2V3cons2V4V3V2identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination429'
:-included_clause_fV0V1cons1V4V1V2cons2V4V2V3cons2V4V3V2identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V2V3cons2V4V3V2identicalV0V2', []), ('specialisation428', []), ('elimination429', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV2V3'
included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,identical,2,(CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation430'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination431'
:-included_clause_fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V3V2identicalV0V2identicalV2V3', []), ('specialisation430', []), ('elimination431', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3'
included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV5,CV0)),body_literal(C,cons2,3,(CV4,CV5,CV3)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation432'
:-included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination433'
:-included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3(C0),separable.
% WARN: ADDING following fragment with name 'generalisation434'
:-included_clause_fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3(C0),clause_size(C0,4).
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V5V0cons2V4V3V2cons2V4V5V3', []), ('specialisation432', []), ('elimination433', []), ('generalisation434', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V0V3'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons2,3,(CV5,CV0,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation435'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination436'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V0V3', []), ('specialisation435', []), ('elimination436', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V1cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V1cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV5,CV3,CV1)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation437'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V1cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination438'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V1cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V1cons2V4V3V2', []), ('specialisation437', []), ('elimination438', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V2cons2V4V3V2'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V2cons2V4V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV5,CV3,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation439'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V2cons2V4V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination440'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V2cons2V4V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons1V5V3V2cons2V4V3V2', []), ('specialisation439', []), ('elimination440', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V2V3'
included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V2V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV2,CV0)),body_literal(C,cons2,3,(CV5,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV3,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation441'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V2V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination442'
:-included_clause_fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V2V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V5V2V0cons2V4V3V2cons2V5V2V3', []), ('specialisation441', []), ('elimination442', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V5cons2V4V3V2identicalV0V5'
included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons2V4V3V2identicalV0V5(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV5)),body_literal(C,cons1,3,(CV4,CV3,CV5)),body_literal(C,cons2,3,(CV4,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation443'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons2V4V3V2identicalV0V5(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination444'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V5cons2V4V3V2identicalV0V5(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V5cons2V4V3V2identicalV0V5', []), ('specialisation443', []), ('elimination444', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V1V3emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V1V3emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV1,CV3)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation445'
:-included_clause_fV0V1cons1V4V1V2cons2V4V1V3emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination446'
:-included_clause_fV0V1cons1V4V1V2cons2V4V1V3emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V1V3emptyV3identicalV0V2', []), ('specialisation445', []), ('elimination446', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,empty,1,(CV3,)),body_literal(C,cons2,3,(CV4,CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation447'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination448'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3(C0),separable.
% WARN: ADDING following fragment with name 'generalisation449'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3(C0),clause_size(C0,4).
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0cons2V4V0V2emptyV3', []), ('specialisation447', []), ('elimination448', []), ('generalisation449', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V0emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V0emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation450'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination451'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V0emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V0emptyV3identicalV0V2', []), ('specialisation450', []), ('elimination451', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V2emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V2emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV3,CV2)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation452'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V2emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination453'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V2emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V2emptyV3identicalV0V2', []), ('specialisation452', []), ('elimination453', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V2V3emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V2V3emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV2,CV3)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation454'
:-included_clause_fV0V1cons1V4V1V2cons2V4V2V3emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination455'
:-included_clause_fV0V1cons1V4V1V2cons2V4V2V3emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V2V3emptyV3identicalV0V2', []), ('specialisation454', []), ('elimination455', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons2V4V0V3emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons2V4V0V3emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV4,CV0,CV3)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation456'
:-included_clause_fV0V1cons1V4V1V2cons2V4V0V3emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination457'
:-included_clause_fV0V1cons1V4V1V2cons2V4V0V3emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons2V4V0V3emptyV3identicalV0V2', []), ('specialisation456', []), ('elimination457', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V3V1emptyV3identicalV0V2'
included_clause_fV0V1cons1V4V1V2cons1V4V3V1emptyV3identicalV0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV3,CV1)),body_literal(C,empty,1,(CV3,)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation458'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V1emptyV3identicalV0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination459'
:-included_clause_fV0V1cons1V4V1V2cons1V4V3V1emptyV3identicalV0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V3V1emptyV3identicalV0V2', []), ('specialisation458', []), ('elimination459', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V1cons1V4V3V0identicalV3V1'
included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V4V3V0identicalV3V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV4,CV3,CV0)),body_literal(C,identical,2,(CV3,CV1)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation460'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V4V3V0identicalV3V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination461'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V4V3V0identicalV3V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V1cons1V4V3V0identicalV3V1', []), ('specialisation460', []), ('elimination461', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V1cons1V5V2V3cons1V5V3V0'
included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V2V3cons1V5V3V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons1,3,(CV5,CV2,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation462'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V2V3cons1V5V3V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination463'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V2V3cons1V5V3V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V1cons1V5V2V3cons1V5V3V0', []), ('specialisation462', []), ('elimination463', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V1cons1V5V3V0cons2V5V3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V3V0cons2V5V3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV5,CV3,CV0)),body_literal(C,cons2,3,(CV5,CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3,CV5!=CV0,CV5!=CV1,CV5!=CV2,CV5!=CV3,CV5!=CV4.
% WARN: ADDING following fragment with name 'specialisation464'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V3V0cons2V5V3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination465'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1cons1V5V3V0cons2V5V3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V1cons1V5V3V0cons2V5V3V2', []), ('specialisation464', []), ('elimination465', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V2V1identicalV0V3identicalV3V2'
included_clause_fV0V1cons1V4V1V2cons1V4V2V1identicalV0V3identicalV3V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,identical,2,(CV3,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation466'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1identicalV0V3identicalV3V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination467'
:-included_clause_fV0V1cons1V4V1V2cons1V4V2V1identicalV0V3identicalV3V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V2V1identicalV0V3identicalV3V2', []), ('specialisation466', []), ('elimination467', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V4V1V2cons1V4V1V3cons1V4V2V1identicalV0V3'
included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons1V4V2V1identicalV0V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,identical,2,(CV0,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV3)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation468'
:-included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons1V4V2V1identicalV0V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination469'
:-included_clause_fV0V1cons1V4V1V2cons1V4V1V3cons1V4V2V1identicalV0V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V4V1V2cons1V4V1V3cons1V4V2V1identicalV0V3', []), ('specialisation468', []), ('elimination469', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V2cons1V3V1V0cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V0V2cons1V3V1V0cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV3,CV0,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation470'
:-included_clause_fV0V1cons1V3V0V2cons1V3V1V0cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination471'
:-included_clause_fV0V1cons1V3V0V2cons1V3V1V0cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V2cons1V3V1V0cons1V4V1V2cons1V4V2V1', []), ('specialisation470', []), ('elimination471', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V0cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation472'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination473'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V0cons1V4V1V2cons1V4V2V1', []), ('specialisation472', []), ('elimination473', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V0V2'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation474'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination475'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V0V2', []), ('specialisation474', []), ('elimination475', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V0'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation476'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination477'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V0', []), ('specialisation476', []), ('elimination477', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V2V1cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V1V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation478'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination479'
:-included_clause_fV0V1cons1V3V1V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V2V1cons1V4V1V2cons1V4V2V1', []), ('specialisation478', []), ('elimination479', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V3V1V2cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV3,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation480'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination481'
:-included_clause_fV0V1cons1V3V1V0cons1V3V1V2cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V3V1V2cons1V4V1V2cons1V4V2V1', []), ('specialisation480', []), ('elimination481', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV3V4'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV3V4(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,succ,2,(CV3,CV4)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation482'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV3V4(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination483'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV3V4(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV3V4', []), ('specialisation482', []), ('elimination483', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV4V3'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV4V3(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,succ,2,(CV4,CV3)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation484'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV4V3(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination485'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV4V3(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1succV4V3', []), ('specialisation484', []), ('elimination485', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V1V2'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons2,3,(CV3,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation486'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination487'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V1V2', []), ('specialisation486', []), ('elimination487', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V1'
included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV1,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV2,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation488'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination489'
:-included_clause_fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V1V0cons1V4V1V2cons1V4V2V1cons2V3V2V1', []), ('specialisation488', []), ('elimination489', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V2cons1V3V2V0cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V0V2cons1V3V2V0cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation490'
:-included_clause_fV0V1cons1V3V0V2cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination491'
:-included_clause_fV0V1cons1V3V0V2cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V2cons1V3V2V0cons1V4V1V2cons1V4V2V1', []), ('specialisation490', []), ('elimination491', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V2'
included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV2)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation492'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination493'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V2', []), ('specialisation492', []), ('elimination493', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V2V0'
included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation494'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination495'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V2V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V2V0', []), ('specialisation494', []), ('elimination495', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV0,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation496'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination497'
:-included_clause_fV0V1cons1V3V0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1', []), ('specialisation496', []), ('elimination497', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V1'
included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons2,3,(CV3,CV0,CV1)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation498'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination499'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V0V1', []), ('specialisation498', []), ('elimination499', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V0'
included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V0(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons2,3,(CV3,CV1,CV0)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation500'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V0(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination501'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V0(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V0', []), ('specialisation500', []), ('elimination501', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V3V2V1cons1V4V1V2cons1V4V2V1'
included_clause_fV0V1cons1V3V2V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation502'
:-included_clause_fV0V1cons1V3V2V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination503'
:-included_clause_fV0V1cons1V3V2V0cons1V3V2V1cons1V4V1V2cons1V4V2V1(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V3V2V1cons1V4V1V2cons1V4V2V1', []), ('specialisation502', []), ('elimination503', [])]'
% WARN: ADDING following fragment with name 'fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V2'
included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C):-head_literal(C,f,2,(CV0,CV1)),body_literal(C,cons1,3,(CV3,CV2,CV0)),body_literal(C,cons1,3,(CV4,CV1,CV2)),body_literal(C,cons2,3,(CV3,CV1,CV2)),body_literal(C,cons1,3,(CV4,CV2,CV1)),CV1!=CV0,CV2!=CV0,CV2!=CV1,CV3!=CV0,CV3!=CV1,CV3!=CV2,CV4!=CV0,CV4!=CV1,CV4!=CV2,CV4!=CV3.
% WARN: ADDING following fragment with name 'specialisation504'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C0),C0 < 1,not clause(1).
% WARN: ADDING following fragment with name 'elimination505'
:-included_clause_fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V2(C0),separable.
% WARN: GROUNDING parts '[('fV0V1cons1V3V2V0cons1V4V1V2cons1V4V2V1cons2V3V1V2', []), ('specialisation504', []), ('elimination505', [])]'
