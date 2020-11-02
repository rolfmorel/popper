from .common import asp_literals_for_distinct_variables, asp_literals_for_distinct_clauses, clause_identifier


class GeneralizationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def generalization_constraint(self, program):
        gen_lits = []

        for clause in program:
            cl_id = str(clause[0]) if self.ground else f"C{clause[0]}"
            body = clause[2]

            cl_handle = clause_identifier(clause)
            gen_lits += [f"included_clause_{cl_handle}({cl_id})",
                         f"clause_size({cl_id},{len(body)})"]

        if not self.ground:
            gen_lits += asp_literals_for_distinct_clauses(program)

        return ":-" +  ",".join(gen_lits) + "."
