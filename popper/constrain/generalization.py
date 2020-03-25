from .common import var_gen, atom_to_asp_literals


def generalization_constraint(program):
    clause_id_var_gen = var_gen('ClId')

    gen_constraint = []
    for cl_id, clause in enumerate(program):
        clause_var = next(clause_id_var_gen)
        for atom in clause:
            gen_constraint += atom_to_asp_literals(atom, clause_var)
        gen_constraint.append(f"size({clause_var},{len(clause)-1})")
    return ":-" +  ",".join(gen_constraint) + "."
