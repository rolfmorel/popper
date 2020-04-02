from abc import ABC, abstractmethod

import clingo

from .setup import SetupMixin
from .solver import SolverMixin
from .representation import RepresentationMixin
from ..util.debug import DebugMixin

from ..representation import ModeDecleration
from ..util import TimeAccumulatingContext


class GenerateInterface(ABC):
    @abstractmethod
    def setup(self, *args, **kwargs): pass

    @abstractmethod
    def set_program_size(self, *args, **kwargs): pass

    @abstractmethod
    def get_program(self, *args, **kwargs): pass

    @abstractmethod
    def impose_constraints(self, *args, **kwargs): pass 


class Generate(SetupMixin,RepresentationMixin,SolverMixin,DebugMixin,GenerateInterface):
    def __init__(self, mode_file=None, max_literals=20, no_pruning=False, ground=False,
                 context=TimeAccumulatingContext(), debug=False):
        self.context = context
        super().__init__(debug=debug)

        with context:
            self.max_literals = max_literals
            self.no_pruning = no_pruning
            self.ground = ground

            self.clingo_ctl = clingo.Control()
            self.setup(mode_file) # from SetupMixin

            with open(mode_file) as f:
                modes_code = f.read()
                self.modeh = ModeDecleration.from_modeh(modes_code)
                self.modebs = ModeDecleration.from_modebs(modes_code)
                self.predicate_to_modeb = dict((m.predicate, m) for m in self.modebs)
