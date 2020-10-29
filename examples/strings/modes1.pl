max_body(2).
max_clauses(20).
max_vars(3).
modeh(f,2).
modeb(copyskip1,2).

direction(copyskip1,0,in).
direction(copyskip1,1,out).
direction(f,0,in).
direction(f,1,out).

type(copyskip1,0,paired_strs).
type(copyskip1,1,paired_strs).
type(f,0,paired_strs).
type(f,1,paired_strs).


% Enforce forward-chaining langauge bias %
%% %% direction(not_empty,0,in).
%% %% direction(not_letter,0,in).
%% %% direction(not_lowercase,0,in).
%% %% direction(not_number,0,in).
%% %% direction(not_space,0,in).
%% %% direction(not_uppercase,0,in).
%% %% modeb(not_empty,1).
%% %% modeb(not_letter,1).
%% %% modeb(not_lowercase,1).
%% %% modeb(not_number,1).
%% %% modeb(not_space,1).
%% %% modeb(not_uppercase,1).
%% %% type(not_empty,0,paired_strs).
%% %% type(not_letter,0,paired_strs).
%% %% type(not_lowercase,0,paired_strs).
%% %% type(not_number,0,paired_strs).
%% %% type(not_space,0,paired_strs).
%% %% type(not_uppercase,0,paired_strs).
%% direction(copy1,0,in).
%% direction(copy1,1,out).
%% direction(is_empty,0,in).
%% direction(is_letter,0,in).
%% direction(is_lowercase,0,in).
%% direction(is_number,0,in).
%% direction(is_space,0,in).
%% direction(is_uppercase,0,in).
%% direction(mk_lowercase,0,in).
%% direction(mk_lowercase,1,out).
%% direction(mk_uppercase,0,in).
%% direction(mk_uppercase,1,out).
%% direction(skip1,0,in).
%% direction(skip1,1,out).
%% modeb(copy1,2).
%% modeb(f,2).
%% modeb(is_empty,1).
%% modeb(is_letter,1).
%% modeb(is_lowercase,1).
%% modeb(is_number,1).
%% modeb(is_space,1).
%% modeb(is_uppercase,1).
%% modeb(mk_lowercase,2).
%% modeb(mk_uppercase,2).
%% modeb(skip1,2).
%% type(copy1,0,paired_strs).
%% type(copy1,1,paired_strs).
%% type(is_empty,0,paired_strs).
%% type(is_letter,0,paired_strs).
%% type(is_lowercase,0,paired_strs).
%% type(is_number,0,paired_strs).
%% type(is_space,0,paired_strs).
%% type(is_uppercase,0,paired_strs).
%% type(mk_lowercase,0,paired_strs).
%% type(mk_lowercase,1,paired_strs).
%% type(mk_uppercase,0,paired_strs).
%% type(mk_uppercase,1,paired_strs).
%% type(skip1,0,paired_strs).
%% type(skip1,1,paired_strs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- body_literal(Cl,_,_,(V1,V2)),V1!=0,V2 != 1,V2 != V1 + 1.
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV1=QV1. % given distinct body lits (diff preds), first args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),P!=Q,PV2=QV2. % given distinct body lits (diff preds), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV1!=QV1,PV2=QV2. % given distinct body lits (diff first arg), second args must be distinct
:- body_literal(Cl,P,_,(PV1,PV2)),body_literal(Cl,Q,_,(QV1,QV2)),PV2!=QV2,PV1=QV1. % given distinct body lits (diff second arg), first args must be distinct