import copy

from sys import stderr

import pyswip

from ..util import SUCCESS, FAILURE


def query_prolog(prolog, example):
    assignments = list(prolog.query(f"{example} =.. [Pred|Args],prove([(_,_,0,Pred,Args)],Result)."))
    assert (len(assignments) == 1) # NB: invariant of a (properly functioning) meta-interpreter
    return assignments[0]['Result']


def meta_interpret(prolog, program, example):
    result = query_prolog(prolog, example)

    if len(result) == 1 and str(result[0].name) == 'success':
        success = result[0]
        clause_id_rec_depth_pairs = map(lambda functor: functor.args, success.args[0])
        responsible_clause_ids = set(map(lambda pair: pair[0], clause_id_rec_depth_pairs))
        responsible_clauses = list(map(lambda idx_cl : idx_cl[1], 
                                  filter(lambda idx_cl: idx_cl[0] in responsible_clause_ids,
                                  enumerate(program))))
        return SUCCESS, responsible_clauses
    else:
        assert (len(result) == len(program))
        body_gen_clauses = []
        for failure in result:
            assert str(failure.name) == 'failure'

            clause_id, literal_id, _, _, args = failure.args
            non_failing_part_clause = program[clause_id][:literal_id]
            failing_literal = copy.deepcopy(program[clause_id][literal_id])
            for arg_id, arg in enumerate(args):
                if type(arg) == pyswip.Variable: 
                    # this argument was not at fault in causing no groundings to exist
                    # this fact can be used when generating constraints
                    failing_literal[3][arg_id] = None
            body_gen_clauses += [non_failing_part_clause + [failing_literal]]

        return FAILURE, body_gen_clauses 
