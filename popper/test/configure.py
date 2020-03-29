from ..representation import clause_to_code

class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        super().__init__(*args, **kwargs)


    def assert_program(self, program):
        with self.context.configure:
            for clause in program:
                self.prolog.assertz(clause_to_code(clause))


    def retract_program_clauses(self):
        with self.context.configure:
            args = ','.join(['_'] * self.modeh.arity)
            self.prolog.retractall(f"{self.modeh.predicate}({args})")
