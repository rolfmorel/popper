:- use_module(library(pairs)).

:- dynamic program/2.

prove([],[success([])]).
prove([Atom|Atoms],Result) :-
    write("zero: "),
    writeln(Atom),
    prove_aux(Atom,AuxResult),
    (AuxResult = [success(ClIds1)] ->
    write("alpha: "),
    writeln(ClIds1),
    (prove(Atoms,AtomsResult),writeln(AtomsResult),(AtomsResult = [success(ClIds2)] -> append(ClIds1,ClIds2,ClIds),Result = [success(ClIds)]; Result=AtomsResult));
        Result = AuxResult).

prove_aux(Atom,Result) :-
    Atom = (_,_,Pred,Args),
    write("one: "),
    writeln(Atom),
    findall((ClId,LitId,Pred,Args)-AuxResult,
            (program([(ClId,LitId,Pred,Args)|Body]),prove(Body,AuxResult)),
            AtomResults), % NOTE: this WILL try all clauses, even if an early one succeeds. The old MI did not do this.
    write("beta: "),
    write(Atom),
    writeln(AtomResults),
    (AtomResults = [] -> 
         false;
	 %(last(AtomResults,(ClId,_,_,_)-[success(ClIds)]) -> 
	 (member((ClId,_,_,_)-[success(ClIds)],AtomResults) -> 
	     write("gamma: "),writeln(ClId),Result = [success([ClId|ClIds])];
	     pairs_values(AtomResults,Results),
             flatten(Results,Result))),!.

prove_aux(Atom,Result) :-
    Atom = (ClId,LitId,Pred,Args),
    Goal =.. [Pred|Args],
    ((predicate_property(Goal,defined),call(Goal)) *->
        Result = [success([])];
        Result = [failure(ClId,LitId,Pred,Args)]).
