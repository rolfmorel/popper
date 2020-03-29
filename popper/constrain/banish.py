from .common import atom_to_asp_literals


class BanishMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    @staticmethod
    def banish_constraint(program):
        banish_lits = []
        for cl_id, clause in enumerate(program):
            for atom in clause:
                banish_lits += atom_to_asp_literals(atom)
            num_lits = len(clause)
            banish_lits.append(f"not literal({cl_id},{num_lits},_,_)")
        banish_lits.append(f"not clause({len(program)})")
        return ":- " + ",".join(banish_lits) + "."
