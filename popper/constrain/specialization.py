from .common import var_gen, atom_to_asp_literals


def specialization_constraint(program, ground=True):
    clause_id_var_gen = var_gen('ClId')
    lit_id_var_gen = var_gen('LitId')

    spec_constraint = []
    for clause in program:
        head, body = clause[0], clause[1:]
        clause_var = next(clause_id_var_gen)

        lits = atom_to_asp_literals(head, not ground and clause_var)
        spec_constraint += lits

        for atom in body:
            lit_var = next(lit_id_var_gen)
            lits = atom_to_asp_literals(atom, not ground and clause_var,
                                        not ground and lit_var)
            spec_constraint += lits
            if not ground:
                spec_constraint += [f"{lit_var} > 0"]
    spec_constraint.append(f"not clause({len(program)})")
    return ":- " + ",".join(spec_constraint) + "."
