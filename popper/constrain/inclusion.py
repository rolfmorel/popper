from .common import clause_to_asp_literals, \
                    asp_literals_for_distinct_clause_variables

from .data_types import RuleType


class InclusionMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


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


    def program_identifier(self, program, ground=None):
        return '_'.join(sorted(self.clause_identifier(cl) for cl in program))


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
            if cl_handle not in self.included_clause_handles:
                self.included_clause_handles.add(cl_handle)
                yield (RuleType.InclusionRule, cl_handle, rule)
