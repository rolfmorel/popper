from itertools import chain

from .common import clause_to_asp_literals, \
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


class DeriveImposeMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('derive')
        self.context.add_child('impose')
        self.context.impose.add_child('adding')
        self.context.impose.add_child('grounding')
        super().__init__(*args, **kwargs)
        self.id = 0


    def clause_identifier(self, clause, ground=None):
        if ground == None:
            ground = self.ground

        def atom_to_ident(atom):
            vars_ = ((f"{var}" if ground else f"V{var}")
                     for var in atom.arguments)
            return f"{atom.predicate}" + "".join(vars_)
            
        cl_id, head, body = clause
        return "".join(map(atom_to_ident, [head] + sorted(body))) + \
               (f"_cl{cl_id}" if ground else "")


    def inclusion_rule(self, clause):
        cl_handle = self.clause_identifier(clause)
        cl_id = str(clause[0]) if self.ground else "C"

        asp_lits = clause_to_asp_literals(clause, self.ground, cl_id=cl_id)
        if not self.ground:
            asp_lits += asp_literals_for_distinct_clause_variables(clause, cl_id=cl_id)

        return cl_handle, f"included_clause_{cl_handle}({cl_id}):-" + ",".join(asp_lits) + "."

    
    def derive_inclusion_rules(self, program):
        for clause in program:
            cl_handle, rule = self.inclusion_rule(clause)
            yield (RuleType.InclusionRule, cl_handle, rule)


    def derive(self, program, pos_outcome, neg_outcome):
        with self.context.derive:
            if (pos_outcome, neg_outcome) == (All, None_):
                return [] # program was unfalsifiable

            if neg_outcome == All:
                # we do not distinguish between entailing some or all of the negative examples
                neg_outcome = Some  

            if self.no_pruning:
                constraints = [(Banish, self.banish_constraint(program))]
            else:
                if is_recursive_program(program): # FIXME: make this a check for separability
                    constraints = self.general_constraints(program, pos_outcome, neg_outcome)
                else:
                    constraints = self.separable_constraints(program, pos_outcome, neg_outcome)

            named_constraints = []
            for type_, constraint in constraints:
                name = f"{type_.value}{self.id}"
                self.id += 1
                named_constraints.append((type_, name, constraint))

            return chain(self.derive_inclusion_rules(program), named_constraints)


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


    def impose(self, named_constraints):
        with self.context.impose:
            names = []
            for name, constraint in named_constraints:
                if name not in self.solver.added:
                    with self.context.impose.adding:
                        self.solver.add(constraint, name=name)
                    names.append(name)
            with self.context.impose.grounding:
                self.solver.ground(*names)
