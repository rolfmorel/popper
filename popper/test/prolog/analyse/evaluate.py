import math
import ast
from collections import defaultdict, namedtuple
from functools import partial

from popper.util import Result
from popper.representation import EvalAtom 
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin

# instrument program with in_path's and out_path's
# upon reaching the end of a body set out_path to in_path with current cl_id prepended.
# upon a failure branch ipc write the in_path with additionally (cl_id, lit_id) prepended.
# upon a success branch the out path with will contain all clauses were necessary for deriving it
# For a failure trace, retest whether the subprogram fails for the example/all examples.
# For a success trace, no need to retest subprogram for this example

class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.context.add_child('evaluate')
        self.context.evaluate['timeouts'] = 0
        self.context.evaluate.add_child('instrumented')
        self.context.evaluate.instrumented.add_child('query')
        self.context.evaluate.instrumented.add_child('obtain')
        self.context.evaluate.instrumented.add_child('retract')
        self.context.evaluate.instrumented.add_child('conversion')

    @staticmethod
    def subprogram_from_path(program, path):
        max_lit_id_per_clause = {}
        for cl_id, lit_id in path:
            old_max_lit_id = max_lit_id_per_clause.get(cl_id, -math.inf)
            max_lit_id_per_clause[cl_id] = max(old_max_lit_id, lit_id)

        subprog = []
        for (cl_id, head, body) in program:
            if cl_id in max_lit_id_per_clause:
                body = body[:max_lit_id_per_clause[cl_id]]
                subprog.append((cl_id, head, body))
        return tuple(subprog)

    def extract_subprogram_from_traces(self, program):
        def convert_path(str_path):
            path = str_path[2:-2].split("),(")
            if path == ['']:
                path = []
            else:
                path = list(map(lambda y: [int(y[0]),int(y[1])], map(lambda x: x.split(','), path)))
            return path

        for line in self.ipc_file:
            line = line[:-1]
            #self.DBG_PRINT(f"IPC line '{line}'")
            try:
                if line.startswith('succ'):
                    path = convert_path(line.split("|")[1])
                    yield Result.Success, __class__.subprogram_from_path(program, path)
                elif line.startswith('fail'):
                    pred, grounding, str_path = line.split("|")[1:]
                    path = convert_path(str_path)
                    yield Result.Failure, __class__.subprogram_from_path(program, path)
                else:
                    raise ValueError("unknown IPC prefix '{line}")
            except ValueError:
                self.DBG_PRINT(f"IPC line '{line}' malformed")
                #break # timeouts might cause incomplete lines to be written/read
        self.ipc_file.truncate(0)
        self.ipc_file.seek(0)


    def evaluate(self, program, example):
    #def instrumented_evaluate(self, program, example):
        with self.context.evaluate.instrumented:
            # FIXME: HACK!!!
            example = example[:-1] + ",[],_)"

            #self.DBG_PRINT("before query")
            with self.context.evaluate.instrumented.query:
                self.query("seek(ipc,0,bof,_)") # start writing at the beginning of the truncated file
                res, _ = self.query(example)
                self.query("flush_output(ipc)")
            #self.DBG_PRINT(f"result: {res}")
            if res is None:
                #self.DBG_PRINT("query result is None")
                self.context.evaluate['timeouts'] += 1
                return None, set(), { tuple(program) }
            #self.DBG_PRINT("after query & before obtaining trace")

            with self.context.evaluate.instrumented.conversion:
                overall_result = None
                success_subprogs, failure_subprogs = set(), set()

                for result, subprog in self.extract_subprogram_from_traces(program):
                    if overall_result is None:
                        overall_result = result
                    if result:
                        overall_result = result
                        success_subprogs.add(subprog)
                    if not result:
                        failure_subprogs.add(subprog)

            if overall_result:
                success_subprogs.add(tuple(program))
            else:
                failure_subprogs.add(tuple(program))

            return overall_result, success_subprogs, failure_subprogs


#def convert_instrumentation_to_execution_forest(trace):
#    result = Result.Failure
#
#    path_to_eval_atom = dict()
#    path_edges = defaultdict(set)
#
#    for traced_atom in trace:
#        cl_id, lit_id = traced_atom[0], traced_atom[1]
#        pred, grounding = traced_atom[2], traced_atom[3]
#        path, success = traced_atom[4], traced_atom[5]
#
#        full_path = tuple(map(tuple,[[cl_id, lit_id]] + path))
#        path_to_eval_atom[full_path] = EvalAtom(pred, grounding, success)
#
#        if lit_id == 0 and path == [] and success:
#            result = Result.Success
#
#        if lit_id >= 1:
#            if lit_id == 1 and path != []:
#                parent_full_path = tuple(map(tuple,path))
#            else:
#                parent_full_path = tuple(map(tuple,[[cl_id, lit_id - 1]] + path))
#            path_edges[parent_full_path].add(full_path)
#
#    inverted_path_edges = defaultdict(set)
#    for origin, dests in path_edges.items():
#        for dest in dests:
#            inverted_path_edges[dest].add(origin)
#
#    return result, (path_to_eval_atom, path_edges, inverted_path_edges)
