import os
from popper.representation import program_to_ordered_program, clause_to_code

import pyswip


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


class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        self.context.configure.add_child('assert_')
        self.context.configure.assert_.add_child('instrumented')
        self.context.configure.add_child('retract')
        super().__init__(*args, **kwargs)
        

    def assert_program(self, program, basic=None):
        if basic is True:
            with self.context.configure.assert_:
                for clause in program:
                    self.prolog.assertz(clause_to_code(clause))
        else:
            with self.context.configure.assert_.instrumented:
                clauses = self.program_to_asserting_prolog(program)
                for clause in clauses:
                    print('clause', clause)
                    self.prolog.assertz(clause)


    def retract(self):
        with self.context.configure.retract:
            args = ','.join(['_'] * (self.modeh.arity))
            self.prolog.retractall(f"{self.modeh.predicate}({args})")
            args = ','.join(['_'] * (self.modeh.arity + 1))
            self.prolog.retractall(f"{self.modeh.predicate}({args})")


    def program_to_asserting_prolog(self, program):
        prolog_program = []
        for clause in program:
            cl_id, head, body = clause
            head_args = ','.join(arguments_to_prolog(head.arguments))

            head_lit = f"{head.predicate}({head_args},(RecClIdx,RecLitIdx))"

            body_lits = []
            for idx, atom in enumerate(body):
                atom = self.literal_to_asserting_prolog(cl_id, idx + 1, atom)
                body_lits.append(atom)

            # NB: cut on empty path causes execution of later clauses, even when earlier clause is successful
#            suffix = (f"OutPath = [({cl_id},{len(body)})|{path}]" + "," +  f"format(ipc, 'blah|~w|~w~n', [InPath,OutPath])," +
#                      f"(InPath = [] -> !,format(ipc, 'succ|~w~n', [OutPath]),false)")
#            prolog_program.append(f"{head_lit} :- {','.join(body_lits)},{suffix}") 
            prefix = f"enter_cl({cl_id},RecClIdx,RecLitIdx),LitIdx=idx(0),("
            suffix = f"-> exit_cl({cl_id},-1) ; (LitIdx=idx(N),exit_cl({cl_id},N),false))"
            prolog_program.append(f"{head_lit} :- {prefix}({','.join(body_lits)}){suffix}") 
        return prolog_program


    def literal_to_asserting_prolog(self, cl_id, lit_id, atom):
        if atom.predicate == self.modeh.predicate:
            pred = atom.predicate
            args = ','.join(arguments_to_prolog(atom.arguments))
            atom_ = f"{pred}({args},({cl_id},{lit_id}))"
        else:
            atom_ = atom_to_prolog(atom)
        return (f"({atom_}*->true;" + 
                 f"(LitIdx=idx(N),M is max({lit_id},N),nb_setarg(1,LitIdx,M),fail))")
#    def literal_to_asserting_prolog_old(self, cl_id, lit_id, atom):
#        if atom.predicate == self.modeh.predicate:
#            pred = atom.predicate
#            args = ','.join(arguments_to_prolog_old(atom.arguments))
#
#            atom_ = f"{pred}({args},[[{cl_id},{lit_id}]|Path])"
#        else:
#            atom_ = atom_to_prolog(atom)
#        pred = atom.predicate
#        args = ','.join(arguments_to_prolog(atom.arguments))
#        return f"({atom_} *-> format(ipc, '~d|~d|~w|~w|~w|~w~n', [{cl_id},{lit_id},{pred},[{args}],Path,true]) ; \
#(format(ipc, '~d|~d|~w|~w|~w|~w~n', [{cl_id},{lit_id},{pred},[{args}],Path,false]),false))"
#
#
#    def program_to_asserting_prolog_old(self, program):
#        prolog_program = []
#        for clause in program:
#            cl_id, head, body = clause
#            head_args = ','.join(arguments_to_prolog(head.arguments))
#
#            head_lit = f"{head.predicate}({head_args},Path)"
#            body_lits = []
#            for idx, atom in enumerate(body):
#                body_lits += [self.literal_to_asserting_prolog(cl_id, idx + 1, atom)]
#
#            assert_prefix = f"format(ipc, '~d|~d|~w|~w|~w|~w~n', [{cl_id},0,{head.predicate},[{head_args}],Path"
#            success_assert = assert_prefix + ",true])"
#            failure_assert = assert_prefix + ",false])"
#
#            body = f"({','.join(body_lits)}) *-> {success_assert} ; {failure_assert},false" 
#            prolog_program.append(f"{head_lit} :- {body},(Path = [] -> !,false)") # NB: cut on empty path causes execution of later clauses, even when early clause is successful
#        return prolog_program
