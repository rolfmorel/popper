from abc import ABC, abstractmethod

import clingo

from .setup import SetupMixin
from .solver import SolverMixin

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


class Generate(SetupMixin,SolverMixin,GenerateInterface):
    def __init__(self, mode_file=None, max_literals=20, no_pruning=False, ground=False,
                 context=TimeAccumulatingContext(), debug=False):
        self.context = context
        super().__init__()

        with context:
            self.max_literals = max_literals
            self.no_pruning = no_pruning
            self.ground = ground
            self.debug = debug

            self.clingo_ctl = clingo.Control()
            self.setup(mode_file) # from SetupMixin

            self.num_literals = 1
