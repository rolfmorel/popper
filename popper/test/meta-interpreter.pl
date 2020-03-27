:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(apply)).

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
    %%write("prove_aux_nondet1: "),
    %%writeln(Atom),
    program([Atom|Body]),
    %%write("prove_aux_nondet2: "),
    %%writeln(Body),
    prove(Body,Result),
    (Result = [success(_)] -> ! ; true).


atom_result_to_failure((ClauseId,LiteralId,(_,_,RecD,Pred,Args))-_,failure(ClauseId,LiteralId,RecD,Pred,Args)):-
    %%write("atom_result_to_failure: "),
    %%writeln(failure(ClauseId,LiteralId,RecD,Pred,Args)),
    true.


select_result(AtomResults,_,[success([(ClId,RecD)|ClIds])]):-
    %%write("select_result1a: "),
    %%writeln(AtomResults),
    member((_,_,(ClId,_,RecD,_,_))-[success(ClIds)],AtomResults),!,
    %%write("select_result1b: "),
    %%writeln((ClId,_,RecD,_,_)),
    true.
select_result(AtomResults,0,Result):-
    pairs_values(AtomResults,Results),
    flatten(Results,Result),!.
select_result(AtomResults,RecDepth,Result):-
    RecDepth > 0,
    % For each clause, all the results must be failures; collapse the failures to a single failure
    maplist(atom_result_to_failure,AtomResults,LitFailures),
    %%write("select_result2: "),
    %%writeln(LitFailures),
    list_to_set(LitFailures,Result),!.


prove_aux((ClauseId,LiteralId,RecDepth,Pred,Args),Result) :-
    %%write("prove_aux-findall1: "),
    %%writeln((ClauseId,LiteralId,RecDepth,Pred,Args)),
    RecD is RecDepth + 1,
    findall((ClauseId,LiteralId,(ClId,LitId,RecDepth,Pred,Args))-AuxResult,
    	    prove_aux_nondet((ClId,LitId,RecD,Pred,Args),AuxResult),
            AtomResults),
    %%write("prove_aux-findall2: "),
    %%writeln(AtomResults),
    (AtomResults = [] -> 
	false ;
	select_result(AtomResults,RecDepth,Result)),!.


prove_aux(Atom,Result) :-
    %%write("prove_aux-call1: "),
    %%writeln(Atom),
    Atom = (ClId,LitId,RecDepth,Pred,Args),
    Goal =.. [Pred|Args],
    ((predicate_property(Goal,defined),call(Goal)) *->
        Result = [success([])] ;
        Result = [failure(ClId,LitId,RecDepth,Pred,Args)]),
    %%write("prove_aux-call2: "),
    %%writeln(Result),
    true.
