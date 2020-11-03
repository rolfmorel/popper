from .common import asp_literals_for_distinct_clauses


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


    def specialization_constraint(self, program, elimination=False):
        spec_lits = []

        for clause in program:
            cl_id = str(clause[0]) if self.ground else f"C{clause[0]}"
            cl_handle = self.clause_identifier(clause)
            spec_lits.append(f"included_clause_{cl_handle}({cl_id})")
            if not elimination:
                spec_lits.append(f"{cl_id} < {len(program)}")

        spec_lits.append(f"not clause({len(program)})" if not elimination else "separable")
        if not self.ground:
            spec_lits += asp_literals_for_distinct_clauses(program)

        return ":-" +  ",".join(spec_lits) + "."


    def elimination_constraint(self, program):
        return self.specialization_constraint(program, elimination=True)
