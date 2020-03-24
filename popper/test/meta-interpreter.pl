:- use_module(library(pairs)).

:- dynamic program/2.


prove([],[success([])]).
prove([Atom|Atoms],Result) :-
    prove_aux(Atom,AuxResult),
    (AuxResult = [success(ClIds1)] -> 
        prove(Atoms,AtomsResult),
	(AtomsResult = [success(ClIds2)] -> 
            append(ClIds1,ClIds2,ClIds),Result = [success(ClIds)] ;
            Result = AtomsResult) ;
        Result = AuxResult).


prove_aux_nondet(Atom,Result) :-  
    program([Atom|Body]),
    prove(Body,Result),
    (Result = [success(_)] -> ! ; true).


prove_aux((_,_,Pred,Args),Result) :-
    findall((ClId,LitId,Pred,Args)-AuxResult,
    	    prove_aux_nondet((ClId,LitId,Pred,Args),AuxResult),
            AtomResults),
    (AtomResults = [] -> 
         false ;
	 (member((ClId,_,_,_)-[success(ClIds)],AtomResults) -> 
	     Result = [success([ClId|ClIds])] ;
	     pairs_values(AtomResults,Results),
             flatten(Results,Result))),!.


prove_aux(Atom,Result) :-
    Atom = (ClId,LitId,Pred,Args),
    Goal =.. [Pred|Args],
    ((predicate_property(Goal,defined),call(Goal)) *->
        Result = [success([])] ;
        Result = [failure(ClId,LitId,Pred,Args)]).
