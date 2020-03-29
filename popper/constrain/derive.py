from ..representation import is_program_recursive
from ..util import Outcome


None_ = Outcome.None_
Some = Outcome.Some
All = Outcome.All


class DeriveMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def derive_constraints(self, program, pos_outcome, neg_outcome):
        if neg_outcome == All:
            # we do not distinguish between entailing some or all of the negative examples
            neg_outcome = Some  
        if is_program_recursive(program):
            return self.recursive_constraints(program, pos_outcome, neg_outcome)
        return self.non_recursive_constraints(program, pos_outcome, neg_outcome)


    def non_recursive_constraints(self, program, pos_outcome, neg_outcome):
        if (pos_outcome, neg_outcome) == (All, None_):
            return [] # program was unfalsifiable
        if (pos_outcome, neg_outcome) == (All, Some):
            return [self.generalization_constraint(program)]
        if (pos_outcome, neg_outcome) == (Some, None_):
            return [self.specialization_constraint(program)]
        if (pos_outcome, neg_outcome) == (Some, Some):
            return [self.specialization_constraint(program),
                    self.generalization_constraint(program)]
        if (pos_outcome, neg_outcome) == (None_, None_):
            return [self.elimination_constraint(program)]
        if (pos_outcome, neg_outcome) == (None_, Some):
            return [self.elimination_constraint(program),
                    self.generalization_constraint(program)]


    def recursive_constraints(self, program, pos_outcome, neg_outcome):
        if (pos_outcome, neg_outcome) == (All, None_):
            return [] # program was unfalsifiable
        if (pos_outcome, neg_outcome) == (All, Some):
            return [self.generalization_constraint(program)]
        if (pos_outcome, neg_outcome) == (Some, None_):
            return [self.specialization_constraint(program)]
        if (pos_outcome, neg_outcome) == (Some, Some):
            return [self.specialization_constraint(program),
                    self.generalization_constraint(program)]
        if (pos_outcome, neg_outcome) == (None_, None_):
            return [self.specialization_constraint(program)]
        if (pos_outcome, neg_outcome) == (None_, Some):
            return [self.specialization_constraint(program),
                    self.generalization_constraint(program)]
