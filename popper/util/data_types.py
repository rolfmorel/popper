from enum import Enum


class Result(Enum):
    Success = True
    Failure = False


class Outcome(Enum):
    None_ = 'none'
    Some = 'some'
    All = 'all'
