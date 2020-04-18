from .common import clause_to_asp_literals, asp_literals_for_distinct_clauses, \
                    asp_literals_for_distinct_variables


class SpecializationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
#TODO:
#Popper's specialisation constraints are quite weak. Given
#    h1 = {last(A,B):- head(A,B). last(A,B):- sumlist(A,B).}.
#Popper does not prune
#    h2 = {last(A,B):- head(A,B),sumlist(A,B). }
#Also, given
#    h3 = {last(A,B):- head(A,B).}
#Popper's specialisation constraint does not prune
#    h4 = {last(A,B):- head(A,B),sumlist(A,B). last(A,B):- head(A,B),member(B,A).}.

    def specialization_constraint(self, program):
        spec_lits = []
        for clause in program:
            spec_lits += clause_to_asp_literals(clause, self.ground)
        if not self.ground:
            spec_lits += asp_literals_for_distinct_clauses(program)
            spec_lits += asp_literals_for_distinct_variables(program)

        spec_lits.append(f"not clause({len(program)})")
        return ":-" + ",".join(spec_lits) + "."
