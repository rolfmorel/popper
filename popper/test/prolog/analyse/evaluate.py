import math
import ast
from collections import defaultdict, namedtuple
from functools import partial 
from itertools import chain
import pyswip

from popper.util import Result
#from popper.representation import EvalAtom 
from popper.representation import program_to_code
from popper.representation.analyse.execution_forest import extract_succeeding_sub_programs, extract_failing_sub_programs

from popper.test.prolog.evaluate import EvaluateMixin as PrologEvaluateMixin


sld_stack = [] # will grow one-to-one with prolog's SLD-stack
cur_lits = dict() # < max num of body literals in program
subprogs = set() # < max num of ordered subprogs (i.e. < product(#lit(cl) for cl in program))

def enter_cl(cl_idx, rec_cl_idx, rec_lit_idx):
    rec_idx = (rec_cl_idx, rec_lit_idx)
    already_visited = rec_idx in cur_lits # O(1)
    
    if rec_idx != (-1,-1): # if different from top-level dummy value
        cur_lits.add(rec_idx) # O(1)
    else:
        already_visited = True # hack
    #print(('enter', (cl_idx, rec_idx, already_visited)))
    sld_stack.append(('enter', (cl_idx, rec_idx, already_visited))) # O(1)
pyswip.registerForeign(enter_cl)

def exit_cl(cl_idx, lit_idx):
    clause_successfull = lit_idx == -1
    if clause_successfull:
        lit_idx = math.inf

    tag, _ = sld_stack[-1] # O(1)
    if tag == 'successes':
        _, success_set = sld_stack.pop() # O(1)
        if clause_successfull:
            success_set.add(cl_idx) # O(1)
    elif clause_successfull:
        success_set = set((cl_idx,))

    tag, (cl_enter_idx, rec_idx, already_visited) = sld_stack.pop() # O(1)
    assert tag == 'enter'
    #print(('exit', (cl_idx, lit_idx, rec_idx, already_visited)))
    assert cl_idx == cl_enter_idx

    if clause_successfull:
        if len(sld_stack) > 0 and sld_stack[-1][0] == 'successes':
            sld_stack[-1][1].update(success_set)
        else:
            sld_stack.append(('successes', success_set)) # O(1)
    else:
        ### START 
        subprog = dict()
        for cl, lit in chain(cur_lits, ((cl_idx, lit_idx),)):
            furthest_lit_idx = subprog.get(cl, 0) # O(1)
            if lit > furthest_lit_idx:
                subprog[cl] = lit
        subprogs.add(tuple(subprog.items())) 
        ### END ==> O(#distinct recursive lits in branch)

    if already_visited is False:
        cur_lits.remove(rec_idx) # O(1)
pyswip.registerForeign(exit_cl)

class EvaluateMixin(PrologEvaluateMixin):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.context.add_child('evaluate')
        self.context.evaluate.add_child('instrumented')
        self.context.evaluate.instrumented.add_child('query')
        self.context.evaluate.instrumented.query['timeouts'] = 0
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
        global subprogs, sld_stack, cur_lits
        with self.context.evaluate.instrumented:
            # FIXME: HACK!!!
            example = example[:-1] + ",(-1,-1))"

            #self.DBG_PRINT("before query")
            with self.context.evaluate.instrumented.query:
                subprogs = set()
                sld_stack = []
                cur_lits = set()

                res, _ = self.query(example)

            if res != None:
                assert res.value == (sld_stack != []), (res, sld_stack)

            successful_subprogs, failing_subprogs = set(), set()
            if res: # case there was a succesfull SLD branch
                assert len(sld_stack) == 1
                _, succeeding_cl_ids = sld_stack[-1]
                succeeding_clauses = []
                for cl_idx, clause in enumerate(program):
                    if cl_idx in succeeding_cl_ids:
                        succeeding_clauses.append(clause)
                successful_subprogs = set((program,
                                          tuple(sorted(succeeding_clauses))))
            else: # only came across failing SLD branches
                failing_subprogs.add(program)
                for subprog_identifier in subprogs:
                    subprog = []
                    for cl_idx, lit_idx in subprog_identifier:
                        prog_cl_idx, prog_cl_head, prog_cl_body = program[cl_idx]
                        lit_idx = min(lit_idx, len(prog_cl_body)) # for case lit_idx == math.inf
                        subprog_cl_body = prog_cl_body[:lit_idx] # '- 1' because of index for head
                        subprog.append((prog_cl_idx, prog_cl_head, subprog_cl_body))
                    failing_subprogs.add(tuple(sorted(subprog)))

            #print("SUCCESS:", res)
            #print("stack:", sld_stack)
            #print("subprogs:", subprogs)
            #print("SUCCESSFUL SUBPROGS:", [program_to_code(p) for p in successful_subprogs])
            #print("FAILING SUBPROGS:", [program_to_code(p) for p in failing_subprogs])

            if res is None:
                self.context.evaluate.instrumented.query['timeouts'] += 1
                return None, set(), { tuple(program) }

            return res, successful_subprogs, failing_subprogs


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
