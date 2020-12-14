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


    def specialization_constraint(self, program):
        program_ident = self.program_identifier(program)

        return f":-included_program({program_ident}),not clause({len(program)})."
