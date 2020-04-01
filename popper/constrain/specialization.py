from .common import clause_to_asp_literals, asp_literals_for_distinct_clauses


class SpecializationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def specialization_constraint(self, program):
        spec_lits = []
        for clause in program:
            spec_lits += clause_to_asp_literals(clause, self.ground)
        if not self.ground:
            spec_lits += asp_literals_for_distinct_clauses(program)

        spec_lits.append(f"not clause({len(program)})")
        return ":- " + ",".join(spec_lits) + "."
