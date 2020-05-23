import copy 
from collections import defaultdict

from popper.util import Result
from popper.representation import program_to_ordered_program

def arguments_to_prolog(arguments):
    args = []
    for arg in arguments:
        if type(arg) == int:
            args += [chr(ord('A') + arg)]
        else:
            args += arg
    return args


def atom_to_prolog(atom):
    pred = atom.predicate
    args = ','.join(arguments_to_prolog(atom.arguments))
    return f"{pred}({args})"


class AnalyseMixin(object):
    EXAMPLE = ['f(A,B) :- { tail(A,B) }.',
 'f(A,B) :- { f(C,D),tail(D,B),tail(A,C),tail(B,C) }.']
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def _assert_prog(self, prog):
        program = self.program_to_asserting_prolog(program_to_ordered_program(prog))
        for clause in program:
            self.prolog.assertz(clause)


    def _retract(self):
        args = ','.join(['_'] * (self.modeh.arity + 1))
        self.prolog.retractall(f"{self.modeh.predicate}({args})")


    def literal_to_asserting_prolog(self, cl_id, lit_id, atom):
        if atom.predicate == self.modeh.predicate:
            pred = atom.predicate
            args = ','.join(arguments_to_prolog(atom.arguments))

            atom_ = f"{pred}({args},[{cl_id}|Path])"
        else:
            atom_ = atom_to_prolog(atom)
        pred = atom.predicate
        args = ','.join(arguments_to_prolog(atom.arguments))
        return f"({atom_} *-> assertz(et({cl_id},{lit_id},{pred},({args}),[{cl_id}|Path],true)) ; \
(assertz(et({cl_id},{lit_id},{pred},({args}),[{cl_id}|Path],false)),false))"


    def program_to_asserting_prolog(self, program):
        prolog_program = []
        for clause in program:
            cl_id, head, body = clause
            head_args = ','.join(arguments_to_prolog(head.arguments))

            head_lit = f"{head.predicate}({head_args},Path)"
            body_lits = []
            for idx, atom in enumerate(body):
                body_lits += [self.literal_to_asserting_prolog(cl_id, idx + 1, atom)]
            body_lits += [f"assertz(et({cl_id},0,{head.predicate},({head_args}),[{cl_id}|Path],true))",
                          "(Path = [] -> !,fail ; true)"]
            prolog_program.append(f"{head_lit} :- {','.join(body_lits)}")
        return prolog_program


    def instrumented_evaluate(self, example):
        self.prolog.retractall("et(_,_,_,_,_,_)")

        # HACK!!!
        example = example[:-1] + ",[])"

        self.query(example)

        return list(self.prolog.query("et(ClId,LitId,Pred,Args,Path,Success)"))


    @staticmethod
    def project_down(program, exe_forest):
        # for a pos example, can prune all specialisations of all returned subprograms
        # for a neg example, can determine the responsible clauses by checking if subprogram i has complete clause i
        #     in which case all complete clauses in subprogram i are responsible and hence generalisations of subprogram i can be pruned

        exe_trees = defaultdict(list)
        for atom in exe_forest:
            if atom['Success'] == 'true':
                exe_trees[atom['Path'][0]].append(atom)

        subprogs = []
        for cl_id in range(len(program)):
            tree = exe_trees[cl_id]
            seen = {(cl_id, 0)} if tree != [] else {}
            for atom in tree:
                seen.add((atom['ClId'], atom['LitId']))

            subprog = copy.deepcopy(program)
            for cl_id in reversed(range(len(program))):
                clause_body = subprog[cl_id][2]
                if (cl_id, 0) not in seen:
                    subprog.pop(cl_id)
                    continue
                for lit_id in reversed(range(1, len(clause_body) + 1)):
                    if (cl_id, lit_id) not in seen and (cl_id, lit_id - 1) not in seen:
                        clause_body.pop(lit_id - 1)
                if subprog[cl_id] == []:
                    subprog.pop(cl_id)
            subprogs.append(subprog)
        return subprogs


