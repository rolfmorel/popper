#show head_literal/4.
#show body_literal/4.

%% NEED AT LEAST ONE CLAUSE
clause(0).

%% GUESS OTHER CLAUSES
{clause(1..N-1)}:-
    max_clauses(N).

%% NEED AT LEAST THE NUMBER OF VARS AS THE MODEH
%% THIS IS A SENSIBLE ASSUMPTION
var(0..A-1):-
    modeh(_,A).

%% %% %% GUESS EXTRA VARS
{var(A..N-1)}:-
    modeh(_,A),
    max_vars(N).

%% HARDCODE HEAD LITERALS
%% TODO: GENERALISE IN FUTURE WHEN THERE MAY BE MULTIPLE MODEH DECLARATIONS
head_literal(Clause,P,1,(0,)):-
    clause(Clause),
    modeh(P,1).

head_literal(Clause,P,2,(0,1)):-
    clause(Clause),
    modeh(P,2).

head_literal(Clause,P,3,(0,1,2)):-
    clause(Clause),
    modeh(P,3).

%% GUESS BODY LITERALS
{body_literal(Clause,P,A,Vars)}:-
    clause(Clause),
    modeb(P,A),
    vars(A,Vars).

literal(Clause,P,Vars):-
    head_literal(Clause,P,_,Vars).
literal(Clause,P,Vars):-
    body_literal(Clause,P,_,Vars).

num_body_literals(Clause,Size):-
    %% max_body(N),
    %% Size <= N,
    clause(Clause),
    #count{P,Vars : body_literal(Clause,P,_,Vars)} = Size.

same_size(Clause1,Clause2):-
    num_body_literals(Clause1,N),
    num_body_literals(Clause2,N).

%% PREVENTS UNEEDED GROUNDING
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
    var(Var3).

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

%% DENOTES WHETHER VAR IS IN VARS
%% TODO: GENERALISE FOR ARITIES > 3
var_member(Var1,(Var1,)):-
    vars(1,(Var1,)).

var_member(Var1,(Var1,Var2)):-
    vars(2,(Var1,Var2)).
var_member(Var2,(Var1,Var2)):-
    vars(2,(Var1,Var2)).

var_member(Var1,(Var1,Var2,Var3)):-
    vars(3,(Var1,Var2,Var3)).
var_member(Var2,(Var1,Var2,Var3)):-
    vars(3,(Var1,Var2,Var3)).
var_member(Var3,(Var1,Var2,Var3)):-
    vars(3,(Var1,Var2,Var3)).

var_in_literal(Clause,P,Vars,Var):-
    literal(Clause,P,Vars),
    var_member(Var,Vars).

%% DENOTES POSITION OF VAR IN VARS
%% TODO: GENERALISE FOR ARITIES > 3
var_pos(Clause,P,0,Var):-
    literal(Clause,P,(Var)).

var_pos(Clause,P,0,Var):-
    literal(Clause,P,(Var,_)).
var_pos(Clause,P,1,Var):-
    literal(Clause,P,(_,Var)).

var_pos(Clause,P,0,Var):-
    literal(Clause,P,(Var,_,_)).
var_pos(Clause,P,1,Var):-
    literal(Clause,P,(_,Var,_)).
var_pos(Clause,P,2,Var):-
    literal(Clause,P,(_,_,Var)).

%% TWO VARS CO-APPEAR IN A BODY LITERAL
share_literal(Clause,Var1,Var2):-
    body_literal(Clause,_,_,Vars),
    var_member(Var1,Vars),
    var_member(Var2,Vars),
    head_connected(Clause,Var2).

%% A VAR IS CONNECTED TO THE HEAD
head_connected(Clause,Var):-
    head_var(Clause,Var).
head_connected(Clause,Var1):-
    body_var(Clause,Var1),
    head_connected(Clause,Var2),
    share_literal(Clause,Var1,Var2).

%% USED TO ENFORCE SAFE ARGUMENTS
%% I.E. MAKES SURE THAT CERTAN ARGUMENTS ARE GROUND
%% HEAD INPUT VARS ARE SAFE
safe_var(Clause,Var):-
    head_literal(Clause,P,_A,Vars),
    var_member(Var,Vars),
    var_pos(Clause,P,Pos,Var),
    direction(P,Pos,in).

safe_var(Clause,Var):-
    body_literal(Clause,P,A,Vars),
    var_member(Var,Vars),
    var_pos(Clause,P,Pos,Var),
    direction(P,Pos,out),
    safe_literal(Clause,P,Vars).

safe_literal(Clause,P,(Var1,Var2)):-
    var_pos(Clause,P,Pos1,Var1),
    var_pos(Clause,P,Pos2,Var2),
    direction(P,Pos1,in),
    direction(P,Pos2,out),
    safe_var(Clause,Var1).

%% USED TO REMOVE DUPLCATE CLAUSES WHEN INDUCING MULTIPLE CLAUSE
different(Clause1,Clause2):-
    Clause1 != Clause2,
    head_literal(Clause1,P,_,_),
    head_literal(Clause2,Q,_,_),
    P != Q.
different(Clause1,Clause2):-
    Clause1 != Clause2,
    head_literal(Clause1,_,_,Vars1),
    head_literal(Clause2,_,_,Vars2),
    Vars1 != Vars2.
different(Clause1,Clause2):-
    Clause1 != Clause2,
    body_literal(Clause1,P,_,Vars),
    clause(Clause2),
    not body_literal(Clause2,P,_,Vars).

%% WEIRD: WHY IS THIS NEEDED?
different(Clause1,Clause2):-
    different(Clause2,Clause1).

%% C1 SUBUMES C2 IF C1 IS A SUBSET OF C2
subsumes(Clause1,Clause2):-
    %% num_body_literals(Clause1,N1),
    %% num_body_literals(Clause2,N2),
    %% N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    Clause1 != Clause2,
    #count{P, Vars : body_literal(Clause1,P,_,Vars), not body_literal(Clause2,P,_,Vars)} == 0.

num_vars(Clause,N):-
    %% max_vars(MaxN),
    %% N <= MaxN,
    clause(Clause),
    #count{Var : body_literal(Clause,_,_,Vars), var_member(Var,Vars)} == N.

same_num_vars(Clause1,Clause2):-
    num_vars(Clause1,N),
    num_vars(Clause2,N).

%% CONSTRAINTS

%% GUESS CLAUSES IN ORDER
:-
    clause(Clause),
    Clause > 0,
    not clause(Clause-1).

%% VARS MUST BE USED
:-
    var(Var),
    not clause_var(_,Var).

%% GUESS VARS IN ORDER
:-
    var(Var),
    Var > 0,
    not var(Var-1).

%% MUST USE VARS IN ORDER
:-
    clause(Clause),
    var_in_literal(Clause,_,_,Var),
    Var > 0,
    not var_in_literal(Clause,_,_,Var-1).

%% MUST HAVE HEAD LITERAL
:-
    clause(Clause),
    not head_literal(Clause,_,_,_).

%% CLAUSE MUST HAVE A BODY LITERAL
:-
    clause(Clause),
    not body_literal(Clause,_,_,_).

%% MAX BODY LITERALS
:-
    num_body_literals(_,N),
    max_body(Max),
    N > Max.

%% MUST BE DATALOG
:-
    head_var(Clause,Var),
    not body_var(Clause,Var).

%% MUST BE CONNECTED
:-
    body_var(Clause,Var),
    not head_connected(Clause,Var).

%% ELIMINATE SINGLETONS
:-
    clause_var(Clause,Var),
    #count{P,Vars : var_in_literal(Clause,P,Vars,Var)} == 1.

%% TYPE MATCHING
:-
    var_pos(Clause,P,Pos1,Var),
    var_pos(Clause,Q,Pos2,Var),
    type(P,Pos1,Type1),
    type(Q,Pos2,Type2),
    Type1 != Type2.

%% VAR ARGUMENT DIRECTIONS
:-
    var_in_literal(Clause,_,_,Var),
    not safe_var(Clause,Var).

%% program_size(Size):-
%%     #count{Clause,P,Vars : literal(Clause,P,Vars)} == Size.
%% :-
%%     size(N),
%%     not program_size(N).

% NB: constraint gets added by popper
%:-
%    size(N),
%    #count{Clause,P,Vars : literal(Clause,P,Vars)} != N.

%% REMOVE REFLEXIVE
%% prevents: p(A):-q(A,B),q(B,A)
:-
    irreflexive(P,A),
    body_literal(Clause,P,2,(Var1,Var2)),
    body_literal(Clause,P,2,(Var2,Var1)).

%% FUNCTIONAL
%% prevents: p(A):-q(A,B),q(A,C)
:-
    functional(P,2),
    body_literal(Clause,P,2,(Var1,Var2)),
    body_literal(Clause,P,2,(Var1,Var3)),
    Var2 != Var3.

%% MAKE SURE CLAUSES ARE DIFFERENT
%% THIS DUPLICATES THE SUBSUMPTION PART
:-
    clause(Clause1),
    clause(Clause2),
    Clause1 != Clause2,
    not different(Clause1,Clause2).

%% #show subsumes/2.
%% PREVENT SUBSUMPTION REDUNDANT CLAUSES
:-
    clause(Clause1),
    clause(Clause2),
    subsumes(Clause1,Clause2).

%% ORDER CLAUSES BY SIZE
:-
    clause(Clause1),
    clause(Clause2),
    Clause2 > Clause1,
    num_body_literals(Clause1,N1),
    num_body_literals(Clause2,N2),
    N1 > N2.

%% ORDER CLAUSES BY NUMBER OF VARS
:-
    same_size(Clause1,Clause2),
    Clause2 > Clause1,
    num_vars(Clause1,NumVars1),
    num_vars(Clause2,NumVars2),
    NumVars1 > NumVars2.

%% :-
%%     same_size(Clause1,Clause2),
%%     same_num_vars(Clause1,Clause2),
%%     Clause2 > Clause1,
%%     num_body_literals(Clause1,1),
%%     body_literal(Clause1,P,_,_),
%%     body_literal(Clause2,Q,_,_),
%%     P > Q.
