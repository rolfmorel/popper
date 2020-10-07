from enum import Enum


class ConstraintType(Enum):
    Specialisation = 'specialisation'
    Generalisation = 'generalisation'
    Elimination = 'elimination'
    Banish = 'banish'


class RuleType(Enum):
    InclusionRule = 'inclusion_rule'
