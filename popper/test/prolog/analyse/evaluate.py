import math
import ast
from collections import defaultdict, namedtuple
from functools import partial 
from itertools import chain
import pyswip

from popper.util import Result
from popper.representation import program_to_code
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin


def maximum_of_seen_literals(seen_lits):
    max_seen_lits = dict()
    for cl_id, lit_id in seen_lits:
        max_seen_lits[cl_id] = max(max_seen_lits.get(cl_id, -1), lit_id)
    return max_seen_lits


def seen_literals_to_subprogram(program, max_seen_lits):
    subprog = []
    for cl_id in sorted(max_seen_lits.keys()):
        lit_id = max_seen_lits[cl_id]
        clause_id, head, body = program[cl_id]
        subprog.append((clause_id, head, body[:lit_id]))

    return tuple(subprog)


class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.context.add_child('evaluate')
        self.context.evaluate.add_child('instrumented')
        self.context.evaluate.instrumented.add_child('query')
        self.context.evaluate.instrumented.query['timeouts'] = 0

        self.seen_lits_of_subprogs = set() # size < max num of ordered subprogs (i.e. < product(#lit(cl) for cl in program))
        foreign_func = partial(__class__.ffi_share, self)
        pyswip.registerForeign(foreign_func, name='ffi_share', arity=1) # NB: not thread-safe


    # NB: Special method. Used as a FFI call from SWI
    def ffi_share(self, seen_literals):
        seen_lits = ((pair_cl_id_lit_id.args[0], pair_cl_id_lit_id.args[1])
                     for pair_cl_id_lit_id in seen_literals)
        max_seen_lits = maximum_of_seen_literals(seen_lits)
        sorted_seen_lits = ((cl_id, max_seen_lits[cl_id])
                            for cl_id in sorted(max_seen_lits.keys()))

        self.seen_lits_of_subprogs.add(tuple(sorted_seen_lits))


    def evaluate(self, program, example):
        with self.context.evaluate.instrumented:
            successful_subprogs, failing_subprogs = set(), set()

            # FIXME: HACK!!!
            example = example[:-1] + ",EmptyLits,SeenLits)"
            instrumented_example_call = "(empty_assoc(EmptyLits)," \
                                       f"{example}," \
                                        "assoc_to_keys(SeenLits,SuccesLits))"
            with self.context.evaluate.instrumented.query:
                self.seen_lits_of_subprogs  = set()
                res, assignments = self.query(instrumented_example_call)

            if res is None:
                # TODO: determine which subprogs will not timeout and return them
                # maybe keep track of how often a subprog gets asserted.
                # if once it is presumably safe, if many it presumably is not
                self.context.evaluate.instrumented.query['timeouts'] += 1
                failing_subprogs = { tuple(program) }
            elif res:
                assert len(assignments) == 1, "must be exactly one SeenLits"
                seen_literals = assignments[0]['SuccesLits']
                seen_lits = list(tuple(pair.args) for pair in seen_literals)

                max_seen_lits = maximum_of_seen_literals(seen_lits)

                assert all(lit_id == len(program[cl_id][2])
                           for cl_id, lit_id in max_seen_lits.items()), "successful subprograms use entire clauses"

                subprog = seen_literals_to_subprogram(program, max_seen_lits)
                successful_subprogs.add(subprog)
            else:
                # TODO: filter out subprogs we know will succeed
                # that is, subprograms who have a SLD-branch which is
                # strictly contained in the SLD-branch of another subprogram.

                for max_seen_lits in self.seen_lits_of_subprogs:
                    subprog = seen_literals_to_subprogram(program, dict(max_seen_lits))
                    failing_subprogs.add(subprog)

            return res, successful_subprogs, failing_subprogs
