import ast
from collections import defaultdict
from functools import partial

from popper.util import Result
from popper.representation import EvalAtom 
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin


class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.context.add_child('evaluate')
        self.context.evaluate.add_child('instrumented')
        self.context.evaluate.instrumented.add_child('query')
        self.context.evaluate.instrumented.add_child('obtain')
        self.context.evaluate.instrumented.add_child('retract')
        self.context.evaluate.instrumented.add_child('conversion')


    def obtain_trace(self):
        for line in self.ipc_file:
            cl_id, lit_id, pred, grounding, path, success = line.split("|")
            cl_id, lit_id = int(cl_id), int(lit_id)
            path = ast.literal_eval(path) #NB: this could be expensive...
            success = success == 'true\n'
            yield (cl_id, lit_id, pred, grounding, path, success)
        self.ipc_file.truncate(0)
        self.ipc_file.seek(0)


    def instrumented_evaluate(self, program, example):
        with self.context.evaluate.instrumented:
            # FIXME: HACK!!!
            example = example[:-1] + ",[])"

            #self.DBG_PRINT("before query")
            with self.context.evaluate.instrumented.query:
                self.query("seek(ipc,0,bof,_)") # start writing at the beginning of the truncated file
                res, _ = self.query(example)
                self.query("flush_output(ipc)")
            #self.DBG_PRINT(f"result: {res}")
            if res is None:
                #self.DBG_PRINT("after query (& giving up)")
                return None, None
            #self.DBG_PRINT("after query & before obtaining trace")

            with self.context.evaluate.instrumented.obtain:
                #trace = list(self.prolog.query("trace(ClId,LitId,Pred,Args,Path,Success)"))
                trace = list(self.obtain_trace())
                #self.DBG_PRINT("trace", trace)
                #self.DBG_PRINT(f"after obtaining trace {len(trace)} & before retractall")
            #with self.context.evaluate.instrumented.retract:
            #    self.prolog.retractall("trace(_,_,_,_,_,_)")
            #self.DBG_PRINT("after retractall & before exe forest")

            with self.context.evaluate.instrumented.conversion:
                out = convert_instrumentation_to_execution_forest(trace)

            #self.DBG_PRINT("after exe forest")
            return out


    def evaluate(self, program, example):
        with self.context.evaluate:
            result, exe_forest = self.instrumented_evaluate(program, example)

            if result is None: # FIXME: Upon a timeout just give up. Obtaining a trace is too expensive, for some reason...
                return None, set((program,))
            elif result:
                subprogs = extract_succeeding_sub_programs(program, exe_forest)
            else:
                subprogs = extract_failing_sub_programs(program, exe_forest)

            return result, set(subprogs)


def convert_instrumentation_to_execution_forest(trace):
    result = Result.Failure

    path_to_eval_atom = dict()
    path_edges = defaultdict(set)

    for traced_atom in trace:
        cl_id, lit_id = traced_atom[0], traced_atom[1]
        pred, grounding = traced_atom[2], traced_atom[3]
        path, success = traced_atom[4], traced_atom[5]

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
