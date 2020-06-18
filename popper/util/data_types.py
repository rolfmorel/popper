from enum import Enum


class Result(Enum):
    Success = True
    Failure = False

    def __bool__(self):
        return self.value


class Outcome(Enum):
    None_ = 'none'
    Some = 'some'
    All = 'all'
