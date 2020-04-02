from abc import ABC, abstractmethod

from .derive import DeriveMixin
from .banish import BanishMixin
from .elimination import EliminationMixin
from .specialization import SpecializationMixin
from .generalization import GeneralizationMixin
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
                EliminationMixin,BanishMixin,DebugMixin,ConstrainInterface):
    def __init__(self, modeh, ground=False, no_pruning=False, debug=False):
        super().__init__(debug=debug)

        self.modeh = modeh

        self.ground = ground
        self.no_pruning = no_pruning
