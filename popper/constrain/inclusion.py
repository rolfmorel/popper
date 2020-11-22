from .common import clause_to_asp_literals, asp_literals_for_distinct_clauses, \
                    asp_literals_for_distinct_clause_variables


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
        return '__'.join(sorted(self.clause_identifier(cl) for cl in program))


    def clause_inclusion_rule(self, clause):
        cl_handle = self.clause_identifier(clause)
        cl_id = str(clause[0]) if self.ground else "C"

        asp_lits = clause_to_asp_literals(clause, self.ground, cl_id=cl_id)

        if not self.ground:
            asp_lits += asp_literals_for_distinct_clause_variables(clause, cl_id=cl_id)

        return cl_handle, f"included_clause_{cl_handle}({cl_id}):-" + ",".join(asp_lits) + "."


    def program_inclusion_rule(self, program):
        program_handle = self.program_identifier(program)

        asp_lits = []
        for i, cl_handle in enumerate(self.clause_identifier(cl) for cl in program):
            clause_var = f"C{i}" if not self.ground else str(i)
            asp_lits.append(f"included_clause_{cl_handle}({clause_var})")

        if not self.ground:
            asp_lits += asp_literals_for_distinct_clauses(program)

        return cl_handle, f"included_program_{program_handle}:-" + ",".join(asp_lits) + "."
