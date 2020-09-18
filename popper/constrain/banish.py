from .common import clause_to_asp_literals


class BanishMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def banish_constraint(self, program):
        constraints = []
        banish_lits = []

        for clause in program:
            cl_id, _, body = clause
            cl_handle, constraint = self.included_clause_constraint(clause) # from CommonMixin
            if constraint: 
                # clause was not encountered before
                constraints.append(constraint)
                self.included_clause_handles.add(cl_handle)

            banish_lits.append(f"included_clause_{cl_handle}({cl_id})")
            banish_lits += [f"clause_size({cl_id},{len(body)})"]
        banish_lits += [f"not clause({len(program)})"]
        return constraints + [":-" + ",".join(banish_lits) + "."]
