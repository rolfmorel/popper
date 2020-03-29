from .common import var_gen, atom_to_asp_literals


class GeneralizationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def generalization_constraint(self, program):
        # TODO: abstract over the clause ids (and literal ids?)
        # NB: depending on the ordering constraints, the above might not be necessary
        clause_var_gen = var_gen('cl_id')
        gen_literals = []
        for cl_id, clause in enumerate(program):
            clause_var = next(clause_var_gen)
            for atom in clause:
                gen_literals += atom_to_asp_literals(atom, clause_var)
            gen_literals.append(f"size({clause_var},{len(clause)-1})")
        return ":-" +  ",".join(gen_literals) + "."
