from popper.representation import clause_to_code


class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        self.context.configure.add_child('assert_')
        self.context.configure.add_child('retract')
        super().__init__(*args, **kwargs)


    def assert_ordered_program(self, program):
        with self.context.configure.assert_:
            for clause in program:
                self.prolog.assertz(clause_to_code(clause))
    assert_program = assert_ordered_program


    def retract_program_clauses(self):
        with self.context.configure.retract:
            args = ','.join(['_'] * self.modeh.arity)
            self.prolog.retractall(f"{self.modeh.predicate}({args})")
            # for asserting programs
            self.prolog.retractall(f"{self.modeh.predicate}({args},_)")
