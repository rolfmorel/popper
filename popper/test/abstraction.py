from abc import ABC, abstractmethod


class TestInterface(ABC):
    @property
    @classmethod
    @abstractmethod
    def analyses(): pass

    @abstractmethod
    def setup(self, *args, **kwargs): pass

    @abstractmethod
    def assert_program(self, *args, **kwargs): pass

    @abstractmethod
    def assert_ordered_program(self, *args, **kwargs): pass

    @abstractmethod
    def evaluate(self, *args, **kwargs): pass

    @abstractmethod
    def retract_program_clauses(self, *args, **kwargs): pass
