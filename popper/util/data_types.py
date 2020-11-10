from enum import Enum
from functools import reduce


class Result(Enum):
    Success = True
    Failure = False

    def __bool__(self):
        return self.value

    def __or__(self, other):
        return __class__(self.value | other.value)

    def __and__(self, other):
        return __class__(self.value & other.value)


class Outcome(Enum):
    None_ = 'none'
    Some = 'some'
    All = 'all'

    @staticmethod
    def from_results(results):
        one_success = reduce(operator.or_, results, Result.Failure)
        all_success = reduce(operator.and_, results, Result.Success)

        if all_success:
            return Outcome.All
        if one_success:
            return Outcome.Some
        return Outcome.None_
