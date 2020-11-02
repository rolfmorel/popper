from .common import clause_to_asp_literals


class BanishMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def banish_constraint(self, program):
        banish_lits = []

        for clause in program:
            cl_id, _, body = clause
            cl_handle = self.clause_identifier(clause)
            banish_lits.append(f"included_clause_{cl_handle}({cl_id})")
            banish_lits += [f"clause_size({cl_id},{len(body)})"]
        banish_lits += [f"not clause({len(program)})"]
        return ":-" + ",".join(banish_lits) + "."
