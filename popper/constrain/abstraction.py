from abc import ABC, abstractmethod

from .derive import DeriveMixin
from .banish import BanishMixin
#from .elimination import EliminationMixin
from .specialization import SpecializationMixin
from .generalization import GeneralizationMixin

from ..util import TimeAccumulatingContext
from ..util.debug import DebugMixin


class ConstrainInterface(ABC):
    @abstractmethod
    def derive_constraints(self, *args, **kwargs): pass

    @abstractmethod
    def generalization_constraint(self, *args, **kwargs): pass

    @abstractmethod
    def specialization_constraint(self, *args, **kwargs): pass

    @abstractmethod
    def elimination_constraint(self, *args, **kwargs): pass

    @abstractmethod
    def banish_constraint(self, *args, **kwargs): pass


class Constrain(DeriveMixin,GeneralizationMixin,SpecializationMixin,
                BanishMixin,DebugMixin,ConstrainInterface):
    def __init__(self, modeh, num_pos_examples, num_neg_examples, 
                 ground=False, no_pruning=False, context=TimeAccumulatingContext(), debug=False):
        self.context = context
        super().__init__(debug=debug)

        self.modeh = modeh
        self.num_pos_examples = num_pos_examples
        self.num_neg_examples = num_neg_examples

        self.ground = ground
        self.no_pruning = no_pruning

#        self.exact_clause_handles = set() # string identifiers of constraints for clauses who occur exactly
        self.included_clause_handles = set() # string identifiers of constraints for clauses who are a subset of a clause
