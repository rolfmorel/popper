import ast
from collections import defaultdict, namedtuple
from functools import partial

from popper.util import Result
from popper.representation import EvalAtom 
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin

SucClause = namedtuple("SucClause", "cl_id path")
FailLit = namedtuple("FailLit", "cl_id lit_id pred grounding path")

# instrument program with in_path's and out_path's
# upon reaching the end of a body set out_path to in_path with current cl_id prepended.
# upon a failure branch ipc write the in_path with additionally (cl_id, lit_id) prepended.
# upon a success branch the out path with will contain all clauses were necessary for deriving it
# For a failure trace, retest whether the subprogram fails for the example/all examples.
# For a success trace, no need to retest subprogram for this example

def trace_to_abstract_sld(trace, program):
    abstract_sld = { cl_id : (None, {})) for cl_id in range(len(program)) } # per clause the subtrees and clause success or failure

    def ensure_subtrees(remaining_path, tree):
        cl_id, lit_id = remaining_path[0]

        if cl_id not in tree:
            tree[cl_id] = (None, {})
        recurse_lits = tree[cl_id][1]
        if lit_id not in recurse_lits:
            recurse_lits[lit_id] = {}

        if len(remaining_path) > 1:
            return add_subtrees(remaining_path[1:], recurse_lits[lit_id])
        return recurse_lits[lit_id]

    for datum in trace:
        subtree = ensure_subtrees(reversed(datum.path))
        if isinstance(datum, SucClause):
            if datum.cl_id in subtree:
                subtree[cl_id][0] = True
            else:
                subtree[cl_id] = (True, {})
        elif isinstance(datum, FailLit):
            subtree[cl_id][1][lit_id] = (False, datum.grounding)
        else:
            raise ValueError(f"wtf is {datum}")
    return abstract_sld


def abstract_sld_to_failing_sub_programs(abstract_sld_tree, program):
    failing_sub_programs = []

    def trace_to_sub_program(trace):
        pass

    def walk_depth_first(tree, trace):
        if 
        for cl_id in sorted(tree.keys()):
            body = tree[cl_id]
            success_marker = body[0]
            if not success_marker:



            if success_marker:
                trace_ = trace | {(cl_id, True)}

            recurse_lits, success_marker = tree[cl_id]
            for lit_id in sorted(recurse_lits.keys()):
                sub_tree = recurse_lits[lit_id]
                trace 

        for cl_id, body in tree.items():
            for 

    for


    pass


def abstract_sld_to_succeeding_sub_programs(abstract_sld_tree, program):
    def collect_clauses(cl_set, recurse_lits):
        for sub_tree in recurse_lits.values():
            for cl_id, (recurse_lits, success_marker) in sub_tree.items():
                if success_marker[0]:
                    cl_set.add(cl_id)
                collect_clauses(cl_set, sub_tree)

    responsible_clauses = {}
    for cl_id, body in abstract_sld_tree.items():
        if body[0]: # check the success marker of the clause
            responsible_clauses[cl_id] = { cl_id }
            collect_clauses(responsible_clauses[cl_id], sld)

    succeeding_sub_programs = []
    for root_cl_id, resp_cls in responsible_clauses.items():
        sub_prog = []
        for cl_id in resp_cls:
            sub_prog.append(program[cl_id])
        succeeding_sub_programs.append(sub_prog)
    return succeeding_sub_programs


class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.context.add_child('evaluate')
        self.context.evaluate.add_child('instrumented')
        self.context.evaluate.instrumented.add_child('query')
        self.context.evaluate.instrumented.add_child('obtain')
        self.context.evaluate.instrumented.add_child('retract')
        self.context.evaluate.instrumented.add_child('conversion')

    def obtain_trace(self, program):
        def convert_path(str_path):
            path = str_path[2:-2].split("],[")
            if path == ['']:
                path = []
            else:
                path = list(map(lambda y: [int(y[0]),int(y[1])], map(lambda x: x.split(','), path)))
            return path

        for line in self.ipc_file:
            try:
                if line.startswith('succ'):
                    cl_id, path = line.split("|")[1:]
                    yield SucClause(int(cl_id), convert_path(path))
                elif line.startswith('fail'):
                    cl_id, lit_id, pred, grounding, path = line.split("|")[1:]
                    yield FailLit(int(cl_id), int(lit_id), pred, grounding, convert_path(path))
                else:
                    raise ValueError("unknown IPC prefix")
            except ValueError:
                self.DBG_PRINT(f"IPC line '{line}' malformed")
                break # timeouts might cause incomplete lines to be written/read
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
                trace = self.obtain_trace()
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
