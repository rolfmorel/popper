from itertools import chain

from .data_types import ConstraintType

from ..representation import is_recursive_program
from ..util import Outcome


None_ = Outcome.None_
Some = Outcome.Some
All = Outcome.All

Spec = ConstraintType.Specialisation
Gen = ConstraintType.Generalisation
Elim = ConstraintType.Elimination
Banish = ConstraintType.Banish


class DeriveMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('derive')
        super().__init__(*args, **kwargs)
        self.id = 0


    def from_type(self, constraint_type, program):
        with self.context.derive:
            if constraint_type == Gen: return self.generalization_constraint(program)
            if constraint_type == Spec: return self.specialization_constraint(program)
            if constraint_type == Elim: return self.elimination_constraint(program)
            if constraint_type == Banish: return self.banish_constraint(program)
            assert False, "do not recognize '{constraint_type}' as a constraint type"


    def derive(self, program, pos_outcome, neg_outcome):
        with self.context.derive:
            if neg_outcome == All: 
                neg_outcome = Some  # we do not distinguish between entailing some or all of the negative examples

            inclusion_rules = self.derive_inclusion_rules(program)

            if (pos_outcome, neg_outcome) == (All, None_) or self.no_pruning:
                # program was unfalsifiable, in which we can only prune it ...
                # or we are instructed to prune no additional programs
                constraints = [(Banish, self.banish_constraint(program))]
            elif (pos_outcome, neg_outcome) == (All, Some):
                constraints = [(Gen, self.generalization_constraint(program))]
            elif (pos_outcome, neg_outcome) == (Some, None_):
                constraints = [(Spec, self.specialization_constraint(program))]
            elif (pos_outcome, neg_outcome) == (Some, Some):
                constraints = [(Spec, self.specialization_constraint(program)),
                               (Gen, self.generalization_constraint(program))]
            else: 
                assert pos_outcome == None_

                inclusion_rules = chain(self.derive_inclusion_rules(program),
                                        (self.program_inclusion_rule(program),))

                constraints = [(Spec, self.specialization_constraint(program))] + \
                              [(Elim, constraint) for constraint in self.elimination_constraint(program)]

                if neg_outcome == Some:
                    constraints += [(Gen, self.generalization_constraint(program))]

            named_constraints = []
            for type_, constraint in constraints:
                name = f"{type_.value}{self.id}"
                self.id += 1
                named_constraints.append((type_, name, constraint))

            return chain(inclusion_rules, named_constraints)


#    def separable_constraints(self, program, pos_outcome, neg_outcome):
#        if (pos_outcome, neg_outcome) == (All, Some):
#            return [(Gen, self.generalization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (Some, None_):
#            return [(Spec, self.specialization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (Some, Some):
#            return [(Spec, self.specialization_constraint(program)),
#                    (Gen, self.generalization_constraint(program))]
#        # TODO: the elim constraints can be generalised so as to account the program being separable from other clauses
#        if (pos_outcome, neg_outcome) == (None_, None_):
#            return [(Spec, self.specialization_constraint(program))] + \
#                   [(Elim, self.elimination_constraint([clause])) for clause in program]
#        if (pos_outcome, neg_outcome) == (None_, Some):
#            return [(Spec, self.specialization_constraint(program))] + \
#                   [(Elim, self.elimination_constraint([clause])) for clause in program] + \
#                   [(Gen, self.generalization_constraint(program))]
#
#
#    def general_constraints(self, program, pos_outcome, neg_outcome):
#        if (pos_outcome, neg_outcome) == (All, Some):
#            return [(Gen, self.generalization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (Some, None_):
#            return [(Spec, self.specialization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (Some, Some):
#            return [(Spec, self.specialization_constraint(program)),
#                    (Gen, self.generalization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (None_, None_):
#            return [(Spec, self.specialization_constraint(program))]
#        if (pos_outcome, neg_outcome) == (None_, Some):
#            return [(Spec, self.specialization_constraint(program)),
#                    (Gen, self.generalization_constraint(program))]
