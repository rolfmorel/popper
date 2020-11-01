%% ##################################################
%% SUBSUMPTION
%% ##################################################

%% OLD!!!!
%% SUBSUMPTION V2
:-
    clause_size(Clause1,N1),
    clause_size(Clause2,N2),
    Clause1 != Clause2,
    N1 <= N2,
    head_literal(Clause1,HeadPred,_,HeadVars),
    head_literal(Clause2,HeadPred,_,HeadVars),
    body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).


%% %% SUBSUMPTION V1
%% :-
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2,
%%     Clause1 != Clause2, %% TODO - CAN WE CHANGE != TO < ?
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).


%% %% SUBSUMPTION V2
%% leq_size(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2.
%% :-
%%     leq_size(Clause1,Clause2),
%%     Clause1 != Clause2, %% TODO - CAN WE CHANGE != TO < ?
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% SUBSUMPTION V3
%% leq_size(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     clause_size(Clause1,N1),
%%     clause_size(Clause2,N2),
%%     N1 <= N2.
%% same_head(Clause1,Clause2):-
%%     Clause1 != Clause2,
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars).
%% :-
%%     leq_size(Clause1,Clause2),
%%     Clause1 != Clause2, %% TODO - CAN WE CHANGE != TO < ?
%%     same_head(Clause1,Clause2),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% SUBSUMPTION V4
%% same_head(Clause1,Clause2):
%%     head_literal(Clause1,HeadPred,_,HeadVars),
%%     head_literal(Clause2,HeadPred,_,HeadVars),
%%     Clause1 != Clause2.
%% :-
%%     Clause1 != Clause2,
%%     same_head(Clause1,Clause2),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).

%% SUBSUMPTION V5
%% p(A,B)<-Q(A,B).
%% p(A,B)<-r(A,B),r(B).
%% :-
%%     Clause1 < Clause2,
%%     same_head(Clause1,Clause2),
%%     body_literal(Clause2,P,_,Vars): body_literal(Clause1,P,_,Vars).
