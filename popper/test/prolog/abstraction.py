from abc import ABC, abstractmethod

from pyswip import Prolog

from ..abstraction import TestInterface

from .setup import SetupMixin
from .configure import ConfigureMixin
from .evaluate import EvaluateMixin
#from .analyse import AnalyseMixin

from popper.util.debug import DebugMixin
from popper.util import TimeAccumulatingContext


class Test(SetupMixin,ConfigureMixin,EvaluateMixin,DebugMixin,TestInterface):
    analyses = False
    minimal_testing = None # set by __init__

    def __init__(self, modeh, bk_file=None,
                 pos_exs=None, neg_exs=None, eval_timeout=None, minimal_testing=True,
                 context=TimeAccumulatingContext(), debug=False):
        with context:
            self.context = context
            self.prolog = Prolog()

        super(self.__class__, self).__init__(debug=debug)

        with context:
            self.modeh = modeh

            self.pos_examples = pos_exs
            self.neg_examples = neg_exs
            self.eval_timeout = eval_timeout
            self.minimal_testing = minimal_testing

            self.setup(bk_file) # from SetupMixin

            self.program_outcomes = dict() # keep track of already tested programs (and their outcomes)
