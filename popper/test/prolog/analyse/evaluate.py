from collections import defaultdict
from functools import partial
from itertools import chain

from popper.util import Result
from popper.representation import EvalAtom 
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin


class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def instrumented_evaluate(self, program, example):
        # FIXME: HACK!!!
        example = example[:-1] + ",[])"

        self.query(example)

        trace = list(self.query("trace(ClId,LitId,Pred,Args,Path,Success)"))
        self.prolog.retractall("trace(_,_,_,_,_,_)")

        return convert_instrumentation_to_execution_forest(trace)


    def evaluate(self, program, example):
        result, exe_forest = self.instrumented_evaluate(program, example)

        if result:
            subprogs = extract_succeeding_sub_programs(program, exe_forest)
        else:
            subprogs = extract_failing_sub_programs(program, exe_forest)
            subprogs = chain([program], filter(lambda subprog: subprog != program, subprogs)) # FIXME: does not clear duplicates

        return result, subprogs


def convert_instrumentation_to_execution_forest(trace):
    result = Result.Failure

    path_to_eval_atom = dict()
    path_edges = defaultdict(set)

    for traced_atom in trace:
        cl_id, lit_id = traced_atom['ClId'], traced_atom['LitId']
        pred, grounding = traced_atom['Pred'], traced_atom['Args']
        path, success = traced_atom['Path'], (traced_atom['Success'] == 'true')
        mode = None

        full_path = tuple(map(tuple,[[cl_id, lit_id]] + path))
        path_to_eval_atom[full_path] = EvalAtom(pred, grounding, success)

        if lit_id == 0 and path == [] and success:
            result = Result.Success

        if lit_id >= 1:
            if lit_id == 1 and path != []:
                parent_full_path = tuple(map(tuple,path))
            else:
                parent_full_path = tuple(map(tuple,[[cl_id, lit_id - 1]] + path))
            path_edges[parent_full_path].add(full_path)

    inverted_path_edges = defaultdict(set)
    for origin, dests in path_edges.items():
        for dest in dests:
            inverted_path_edges[dest].add(origin)

    return result, (path_to_eval_atom, path_edges, inverted_path_edges)
