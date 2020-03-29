from .common import var_gen, atom_to_asp_literals


class EliminationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        
    def elimination_constraint(self, program):
        elim_lits, cl_vars = self.specialization_literals(program)
        clause_restr = ",".join("Clause != " + cl_var for cl_var in cl_vars)
        elim_lits.append("not recursive")
        #elim_lits.append(f"#count{{ Clause,Literal : \
#body_literal(Clause,Literal,{self.modeh.predicate},_),{clause_restr} }} == 0")
        return ":-" + ",".join(elim_lits) + "."
