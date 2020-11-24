from itertools import chain

from .data_types import ConstraintType, RuleType

from ..util import Outcome


None_ = Outcome.None_
Some = Outcome.Some
All = Outcome.All

Spec = ConstraintType.Specialisation
Gen = ConstraintType.Generalisation
Elim = ConstraintType.Elimination
Banish = ConstraintType.Banish


class DeriveMixin(object):
    outcomes_to_constraints = {
            (All, None_)   : frozenset((Banish,)),  # unfalsifiable yet undesired, hence only prune this program
            (All, Some)    : frozenset((Gen,)),
            (Some, None_)  : frozenset((Spec,)),
            (Some, Some)   : frozenset((Spec,Gen)),
            (None_, None_) : frozenset((Spec,Elim)),
            (None_, Some)  : frozenset((Spec,Elim,Gen)),
    }


    def __init__(self, *args, **kwargs):
        self.context.add_child('derive')
        super().__init__(*args, **kwargs)
        self.id = 0
        self.rule_to_id = dict()


    def derive_constraint_types(self, program, pos_outcome, neg_outcome):
        with self.context.derive:
            if self.no_pruning:
                # Same as unfalsifiable case: do not prune additional programs
                pos_outcome, neg_outcome = All, None_

            if neg_outcome == All: 
                # we don't distinguish between entailing some or all negative examples
                neg_outcome = Some  

            return __class__.outcomes_to_constraints[(pos_outcome, neg_outcome)]


    def derive_inclusion_rules(self, program, constraint_types):
        with self.context.derive:
            if Spec in constraint_types or Gen in constraint_types:
                for clause in program:
                    cl_handle, rule = self.clause_inclusion_rule(clause)
                    if cl_handle not in self.included_clause_handles:
                        self.included_clause_handles.add(cl_handle)
                        yield (RuleType.InclusionRule, cl_handle, rule)
            if Elim in constraint_types:
                yield (RuleType.InclusionRule, *self.program_inclusion_rule(program))


    def constraints_from_type(self, program, constraint_type):
        if constraint_type == Gen: return [self.generalization_constraint(program)]
        if constraint_type == Spec: return [self.specialization_constraint(program)]
        if constraint_type == Elim: return list(self.elimination_constraint(program))
        if constraint_type == Banish: return [self.banish_constraint(program)]
        assert False, "do not recognize '{constraint_type}' as a constraint type"


    def derive_constraints(self, program, constraint_types):
        with self.context.derive:
            named_constraints = []
            for constraint_type in constraint_types:
                for constraint in self.constraints_from_type(program, constraint_type):
                    if constraint in self.rule_to_id:
                        name = self.rule_to_id[constraint]
                    else:
                        name = f"{constraint_type.value}{self.id}"
                        self.id += 1
                        self.rule_to_id[constraint] = name
                    named_constraints.append((constraint_type, name, constraint))

            return named_constraints
