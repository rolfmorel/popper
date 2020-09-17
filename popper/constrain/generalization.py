from .common import clause_to_asp_literals, asp_literals_for_distinct_clause_variables, \
                    asp_literals_for_distinct_variables, clause_identifier


class GeneralizationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def exact_clause_constraint(self, clause):
        constraint = None
        cl_handle = clause_identifier(clause)
        if cl_handle not in self.exact_clause_handles:
            cl_id = str(clause[0]) if self.ground else ""
            body = clause[2]

            asp_lits = clause_to_asp_literals(clause, self.ground, cl_id=cl_id)
            asp_lits.append(f"clause_size({cl_id},{len(body)})")
            if not self.ground:
                asp_lits += asp_literals_for_distinct_clause_variables(clause, cl_id=cl_id)
            # TODO: assert equality with known variables of the head

            constraint = f"exact_clause_{cl_handle}:-" + ",".join(asp_lits) + "."
        return cl_handle, constraint


    def generalization_constraint(self, program):
        constraints = []
        gen_lits = []

        for clause in program:
            cl_handle, constraint = self.exact_clause_constraint(clause)
            if constraint: 
                # clause was not encountered before in a generalization constraint
                constraints.append(constraint)
                self.exact_clause_handles.add(cl_handle)
            gen_lits += f"exact_clause_{cl_handle}"

        return constraints + [":-" +  ",".join(gen_lits) + "."]

#    def generalization_constraint(self, program):
#        gen_lits = []
#        for clause in program:
#            gen_lits += clause_to_asp_literals(clause, self.ground)
#            cl_id, _, body = clause
#            cl_id = f"C{cl_id}" if not self.ground else str(cl_id)
#            gen_lits += [f"clause_size({cl_id},{len(body)})"]
#        if not self.ground:
#            # TODO: asserting distinct clauses should not be necessary. Including these atoms increases grounging
#            gen_lits += asp_literals_for_distinct_clauses(program)
#            gen_lits += asp_literals_for_distinct_variables(program)
#        return ":-" +  ",".join(gen_lits) + "."
