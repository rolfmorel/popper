from popper.representation import clause_to_code


class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        self.context.configure.add_child('assert_')
        self.context.configure.add_child('retract')
        super().__init__(*args, **kwargs)
        self.current_clauses = set()


    def assert_program(self, program, basic=None):
        assert basic in (True, None)
        with self.context.configure.assert_:
            for clause in program:
                self.prolog.assertz(clause_to_code(clause))
                self.current_clauses.add(clause)


    def retract(self):
        with self.context.configure.retract:
            head_lits = set(head for _, head, _ in self.current_clauses)
            for head_lit in head_lits:
                args = ','.join(['_'] * head_lit.arity)
                self.prolog.retractall(f"{head_lit.predicate}({args})")
                # for asserting programs
                self.prolog.retractall(f"{head_lit.predicate}({args},_)")
            self.current_clauses = set()
