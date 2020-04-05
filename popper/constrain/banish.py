from .common import clause_to_asp_literals


class BanishMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    @staticmethod
    def banish_constraint(program):
        banish_lits = []
        for clause in program:
            cl_id, _, body = clause
            banish_lits += clause_to_asp_literals(clause, ground=True, cl_id=f"Cl{cl_id}")
            banish_lits += [f"clause_size({cl_id},{len(body)})"]
        banish_lits += [f"not clause({len(program)})"]
        return ":-" + ",".join(banish_lits) + "."
