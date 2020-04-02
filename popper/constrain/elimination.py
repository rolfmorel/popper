from .common import clause_to_asp_literals, asp_literals_for_distinct_clauses


class EliminationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        
    def elimination_constraint(self, program):
        elim_lits = []
        for clause in program:
            elim_lits += clause_to_asp_literals(clause, self.ground)
        if not self.ground:
            elim_lits += asp_literals_for_distinct_clauses(program)

        elim_lits.append("not recursive")
        return ":- " + ",".join(elim_lits) + "."
