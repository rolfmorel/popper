from .common import clause_to_asp_literals, asp_literals_for_distinct_clauses, \
                    asp_literals_for_distinct_variables


class GeneralizationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def generalization_constraint(self, program):
        gen_lits = []
        for clause in program:
            gen_lits += clause_to_asp_literals(clause, self.ground)
            cl_id, _, body = clause
            cl_id = f"ClId{cl_id}" if not self.ground else str(cl_id)
            gen_lits += [f"clause_size({cl_id},{len(body)})"]
        if not self.ground:
            gen_lits += asp_literals_for_distinct_clauses(program)
            gen_lits += asp_literals_for_distinct_variables(program)
        return ":-" +  ",".join(gen_lits) + "."
