from .common import var_gen, atom_to_asp_literals


class SpecializationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def specialization_constraint(self, program):
        spec_literals = self.specialization_literals(program)[0]
        spec_literals.append(f"not clause({len(program)})")
        return ":- " + ",".join(spec_literals) + "."


    def specialization_literals(self, program):
        clause_var_gen, literal_var_gen = var_gen('ClId'), var_gen('LitId')
        clause_ids = []
        spec_literals = []
        for clause in program:
            head, body = clause[0], clause[1:]
            if not self.ground:
                clause_var = next(clause_var_gen)
                clause_ids.append(clause_var)
            else:
                clause_ids.append(str(head[0])) # the head's clause_id

            lits = atom_to_asp_literals(head, not self.ground and clause_var)
            spec_literals += lits

            for atom in body:
                if not self.ground:
                    lit_var = next(literal_var_gen)
                lits = atom_to_asp_literals(atom, not self.ground and clause_var,
                                            not self.ground and lit_var)
                spec_literals += lits
                if not self.ground:
                    spec_literals += [f"{lit_var} > 0"]
        return spec_literals, clause_ids
