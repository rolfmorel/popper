import os
from popper.representation import program_to_ordered_program, clause_to_code


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

        prefix = '/dev/shm'
        if not os.path.exists(prefix):
            prefix = '/tmp'

        self.ipc_filename = f"{prefix}/popper_x-{os.getpid()}"
        self.ipc_file = open(self.ipc_filename, 'w+')
        self.ipc_file.truncate() # make sure is empty, even if name was stale
        success = list(self.prolog.query(f'open("{self.ipc_filename}", write, _, [alias(ipc)])'))
        assert success == [{}], success


    def assert_program(self, program, basic=None):
        if basic is True:
            with self.context.configure.assert_:
                for clause in program:
                    self.prolog.assertz(clause_to_code(clause))
        else:
            with self.context.configure.assert_.instrumented:
                clauses = self.program_to_asserting_prolog(program)
                for clause in clauses:
                    self.prolog.assertz(clause)


    def retract(self):
        with self.context.configure.retract:
            args = ','.join(['_'] * (self.modeh.arity))
            self.prolog.retractall(f"{self.modeh.predicate}({args})")
            args = ','.join(['_'] * (self.modeh.arity + 2))
            self.prolog.retractall(f"{self.modeh.predicate}({args})")

    def program_to_asserting_prolog(self, program):
        prolog_program = []
        for clause in program:
            cl_id, head, body = clause
            head_args = ','.join(arguments_to_prolog(head.arguments))

            path = 'InPath'
            head_lit = f"{head.predicate}({head_args},{path},OutPath)"

            body_lits = []
            for idx, atom in enumerate(body):
                path, atom = self.literal_to_asserting_prolog(cl_id, idx + 1, atom, path)
                body_lits.append(atom)

            # NB: cut on empty path causes execution of later clauses, even when earlier clause is successful
#            suffix = (f"OutPath = [({cl_id},{len(body)})|{path}]" + "," +  f"format(ipc, 'blah|~w|~w~n', [InPath,OutPath])," +
#                      f"(InPath = [] -> !,format(ipc, 'succ|~w~n', [OutPath]),false)")
#            prolog_program.append(f"{head_lit} :- {','.join(body_lits)},{suffix}") 
            suffix = (f"OutPath = [({cl_id},{len(body)})|{path}]" + "," +
                      f"(InPath = [] -> !,format(ipc, 'succ|~w~n', [OutPath]),false ; true)")
            prolog_program.append(f"{head_lit} :- {','.join(body_lits)},{suffix}") 
        return prolog_program

    def literal_to_asserting_prolog(self, cl_id, lit_id, atom, in_path):
        cur_path = f"[({cl_id},{lit_id})|{in_path}]"
        if atom.predicate == self.modeh.predicate:
            pred = atom.predicate
            args = ','.join(arguments_to_prolog(atom.arguments))

            out_path = f"Path{lit_id}"
            atom_ = f"{pred}({args},{cur_path},{out_path})"
        else:
            atom_ = atom_to_prolog(atom)
            out_path = in_path
        pred = atom.predicate
        args = ','.join(arguments_to_prolog(atom.arguments))
        return out_path, \
               (f"({atom_} *-> true ;" + 
                f"(format(ipc, 'fail|~w|~w|~w~n', [{pred},[{args}],{cur_path}]),false))")

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
