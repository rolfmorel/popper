from .common import atom_to_asp_literals


def elimination_constraint(program):
    elim_constraint = []
    for cl_id, clause in enumerate(program):
        for atom in clause:
            elim_constaint += atom_to_asp_literals(atom)
        num_lits = len(clause)
        elim_constraint.append(f"not literal({clause_id},{num_lits},_,_)")
    elim_constraint.append(f"not clause({len(program)})")
    return ":- " + ",".join(spec_constraint) + "."
