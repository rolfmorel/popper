from abc import ABC, abstractmethod

from pyswip import Prolog

from .setup import SetupMixin
from .configure import ConfigureMixin
from .evaluate import EvaluateMixin

from ..util import TimeAccumulatingContext


class TestInterface(ABC):
    @abstractmethod
    def setup(self, *args, **kwargs): pass

    @abstractmethod
    def assert_program(self, *args, **kwargs): pass

    @abstractmethod
    def evaluate(self, *args, **kwargs): pass

    @abstractmethod
    def retract_program_clauses(self, *args, **kwargs): pass


class Test(SetupMixin,ConfigureMixin,EvaluateMixin,TestInterface):
    def __init__(self, modeh, bk_file=None,
                 pos_exs=None, neg_exs=None, eval_timeout=None,
                 context=TimeAccumulatingContext(), debug=False):
        self.context = context
        super().__init__()

        with context:
            self.modeh = modeh

            self.pos_examples = pos_exs
            self.neg_examples = neg_exs
            self.eval_timeout = eval_timeout
            self.debug = debug

            self.prolog = Prolog()

            self.setup(bk_file) # from SetupMixin
