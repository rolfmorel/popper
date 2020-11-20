from abc import ABC, abstractmethod

from pyswip import Prolog

from popper.test.abstraction import TestInterface
from popper.test.prolog.setup import SetupMixin
from popper.test.prolog import Test as PrologTest

from popper.util.debug import DebugMixin

from .configure import ConfigureMixin
from .evaluate import EvaluateMixin

class Test(SetupMixin,ConfigureMixin,EvaluateMixin,DebugMixin,TestInterface):
    analyses = True
    minimal_testing = None

    def __init__(self, *args, **kwargs):
        PrologTest.__init__(self, *args, **kwargs)
