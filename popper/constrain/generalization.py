from .common import asp_literals_for_distinct_variables, asp_literals_for_distinct_clauses


class GeneralizationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def generalization_constraint(self, program):
        gen_lits = []

        for clause in program:
            cl_id = str(clause.num) if self.ground else f"C{clause.num}"

            cl_handle = self.clause_identifier(clause)
            gen_lits += [f"included_clause_{cl_handle}({cl_id})",
                         f"clause_size({cl_id},{len(clause.body)})"]

        if not self.ground:
            for cl_num1, cl_nums in program.before.items():
                for cl_num2 in cl_nums:
                    gen_lits.append(f"C{cl_num1}<C{cl_num2}")

            for clause in program:
                gen_lits.append(f"C{clause.num}>={clause.min_num}")

            gen_lits += asp_literals_for_distinct_clauses(program)

        return ":-" +  ",".join(gen_lits) + "."
