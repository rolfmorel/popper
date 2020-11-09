from abc import ABC, abstractmethod
from contextlib import contextmanager


class TestInterface(ABC):
    @property
    @classmethod
    @abstractmethod
    def analyses(): pass

    @property
    @abstractmethod
    def minimal_testing(): pass

    @abstractmethod
    def setup(self, *args, **kwargs): pass

    @abstractmethod
    def query(self, *args, **kwargs): pass

    @abstractmethod
    def evaluate(self, *args, **kwargs): pass

    @contextmanager
    def using(self, program, *args, **kwargs):
        try:
            self.assert_program(program, *args, **kwargs)
            yield
        finally:
            self.retract()

    @abstractmethod
    def assert_program(self, *args, **kwargs): pass

    @abstractmethod
    def retract(self, *args, **kwargs): pass
