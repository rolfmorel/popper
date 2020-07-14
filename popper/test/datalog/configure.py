from popper.representation import program_to_code


class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        self.__prog_count = 0
        super().__init__(*args, **kwargs)

    def assert_program(self, program, basic=None, code=False):
        assert basic in (True, None)
        with self.context.configure:
            code_program = program
            if not code:
                code_program = '\n'.join(program_to_code(program))

            self.__prog_count += 1
            prog_name = f"program{self.__prog_count}"

            self.clingo_ctl.add(prog_name, [], code_program)
            self.clingo_ctl.ground([(prog_name, [])])

            with self.clingo_ctl.solve(yield_=True) as handle:
                # None indicates no model could be found (could try with more allowed literals)
                model = next(handle) # NB: as long as we are doing datalog there is only going to be one model (at most)
                self.atoms = model.symbols(atoms=True)
                self.atom_strs = set(map(str, self.atoms))

    def retract(self):
        with self.context.configure:
            self.reset()
            self.setup()
