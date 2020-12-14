%% %% ##################################################
%% %% SUBSUMPTION
%% %% ##################################################
same_head(C1,C2):-
    C1 != C2,
    head_literal(C1,P,A,Vars),
    head_literal(C2,P,A,Vars).

same_body(C1,C2):-
    C1 != C2,
    clause(C1),
    clause(C2),
    body_literal(C2,P,_,Vars): body_literal(C1,P,_,Vars).

%% %% c*c*n^2 in the number of body literals
%% different_body(C1,C2):-
%%     C1 != C2,
%%     clause(C1),
%%     clause(C2),
%%     body_literal(C1,P,_,Vars),
%%     not body_literal(C2,P,_,Vars).

%% same_body(C1,C2):-
%%     C1 != C2,
%%     clause(C1),
%%     clause(C2),
%%     not different_body(C1,C2).

:-
    C1 < C2,
    same_head(C1,C2),
    same_body(C1,C2).