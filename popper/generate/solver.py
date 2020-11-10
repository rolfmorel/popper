import uuid
from sys import stderr

from popper.representation import Atom


class SolverMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('solver')
        self.context.solver.add_child('setting_size')
        self.context.solver.add_child('adding')
        self.context.solver.add_child('grounding')
        self.context.solver.add_child('solving')
        self.context.solver.add_child('conversion')
        super().__init__(*args, **kwargs)

    def set_program_size(self, size):
        with self.context.solver.setting_size:
            for atom, truthiness in self.solver.assigned.items():
                if atom.predicate == 'size' and truthiness:
                    self.solver.release(atom)
            # NB: might well attempt to reground the old parts of base as well
            with self.context.solver.grounding:
                self.solver.ground(program_size=[size])

            self.solver.assign(Atom('size', (size,)), True)

    def get_program(self):
        with self.context.solver:
            model = self.solver.get_model()
            if model:
                with self.context.solver.conversion:
                    return self.model_to_program(model)
            return model
