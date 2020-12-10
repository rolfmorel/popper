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
            
        return "".join(map(atom_to_ident,
                           [clause.head] + sorted(clause.body)))


    def program_identifier(self, program, ground=None):
        return 'prog_' + '_'.join(sorted(self.clause_identifier(cl) for cl in program))


    def clause_inclusion_rule(self, clause):
        cl_handle = self.clause_identifier(clause)
        cl_id = str(clause.num) if self.ground else "C"

        asp_lits = clause_to_asp_literals(clause, self.ground, cl_id=cl_id)

        if not self.ground:
            asp_lits.append(f"{cl_id}>={clause.min_num}")
            asp_lits += asp_literals_for_distinct_clause_variables(clause, cl_id=cl_id)

        return cl_handle, f"included_clause({cl_handle},{cl_id}):-" + ",".join(asp_lits) + "."


    def program_inclusion_rule(self, program):
        program_handle = self.program_identifier(program)

        asp_lits = []
        for cl in program:
            cl_handle = self.clause_identifier(cl)
            clause_var = f"C{cl.num}" if not self.ground else str(cl.num)
            asp_lits.append(f"included_clause({cl_handle},{clause_var})")

        if not self.ground:
            for cl_num1, cl_nums in program.before.items():
                for cl_num2 in cl_nums:
                    asp_lits.append(f"C{cl_num1}<C{cl_num2}")

            asp_lits += asp_literals_for_distinct_clauses(program)

        return program_handle, f"included_program({program_handle}):-" + ",".join(asp_lits) + "."
