from .common import clause_identifier, clause_to_asp_literals, \
                    asp_literals_for_distinct_clause_variables
from .data_types import ConstraintType, RuleType

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
        super().__init__(*args, **kwargs)


    def inclusion_rule(self, clause):
        cl_handle = clause_identifier(clause)
        cl_id = str(clause[0]) if self.ground else "C"

        asp_lits = clause_to_asp_literals(clause, self.ground, cl_id=cl_id)
        if not self.ground:
            asp_lits += asp_literals_for_distinct_clause_variables(clause, cl_id=cl_id)

        return cl_handle, f"included_clause_{cl_handle}({cl_id}):-" + ",".join(asp_lits) + "."

    
    def derive_inclusion_rules(self, program):
        for clause in program:
            cl_handle, rule = self.inclusion_rule(clause)
            if cl_handle not in self.included_clause_handles:
                self.included_clause_handles.add(cl_handle)
                yield (RuleType.InclusionRule, rule)


    def derive_constraints(self, program, pos_outcome, neg_outcome):
        if (pos_outcome, neg_outcome) == (All, None_):
            return [] # program was unfalsifiable

        if self.no_pruning:
            return [(Banish, self.banish_constraint(program))]

        if neg_outcome == All:
            # we do not distinguish between entailing some or all of the negative examples
            neg_outcome = Some  

        if is_recursive_program(program): # FIXME: make this a check for separability
            constraints = self.general_constraints(program, pos_outcome, neg_outcome)
        else:
            constraints = self.separable_constraints(program, pos_outcome, neg_outcome)

        return list(self.derive_inclusion_rules(program)) + constraints


    def separable_constraints(self, program, pos_outcome, neg_outcome):
        if (pos_outcome, neg_outcome) == (All, Some):
            return [(Gen, self.generalization_constraint(program))]
        if (pos_outcome, neg_outcome) == (Some, None_):
            return [(Spec, self.specialization_constraint(program))]
        if (pos_outcome, neg_outcome) == (Some, Some):
            return [(Spec, self.specialization_constraint(program)),
                    (Gen, self.generalization_constraint(program))]
        # TODO: the elim constraints can be generalised so as to account the program being separable from other clauses
        if (pos_outcome, neg_outcome) == (None_, None_):
            return [(Spec, self.specialization_constraint(program))] + \
                   [(Elim, self.elimination_constraint([clause])) for clause in program]
        if (pos_outcome, neg_outcome) == (None_, Some):
            return [(Spec, self.specialization_constraint(program))] + \
                   [(Elim, self.elimination_constraint([clause])) for clause in program] + \
                   [(Gen, self.generalization_constraint(program))]


    def general_constraints(self, program, pos_outcome, neg_outcome):
        if (pos_outcome, neg_outcome) == (All, Some):
            return [(Gen, self.generalization_constraint(program))]
        if (pos_outcome, neg_outcome) == (Some, None_):
            return [(Spec, self.specialization_constraint(program))]
        if (pos_outcome, neg_outcome) == (Some, Some):
            return [(Spec, self.specialization_constraint(program)),
                    (Gen, self.generalization_constraint(program))]
        if (pos_outcome, neg_outcome) == (None_, None_):
            return [(Spec, self.specialization_constraint(program))]
        if (pos_outcome, neg_outcome) == (None_, Some):
            return [(Spec, self.specialization_constraint(program)),
                    (Gen, self.generalization_constraint(program))]
