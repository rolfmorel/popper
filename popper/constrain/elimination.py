from collections import OrderedDict, defaultdict

from .common import asp_literals_for_distinct_clauses


class EliminationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


    def elimination_constraint(self, program):
        preds_num_clauses = defaultdict(int)
        preds_num_recursive_clauses = defaultdict(int)
        for clause in program:
            _, head, body = clause
            preds_num_clauses[head.predicate] += 1
            if clause.is_recursive():
                preds_num_recursive_clauses[head.predicate] += 1

        recursively_called = set()
        while True:
            something_added = False
            for clause in program:
                _, head, body = clause
                recursive = clause.is_recursive()
                for body_pred in (blit.predicate for blit in body):
                    if body_pred not in preds_num_clauses: continue
                    if (body_pred != head.predicate and recursive) \
                            or head.predicate in recursively_called:
                        something_added |= not body_pred in recursively_called
                        recursively_called.add(body_pred)
            if not something_added: break

        program_ident = self.program_identifier(program)

        for pred in preds_num_clauses.keys() - recursively_called:
            elim_lits = []
            for other_pred, num_clauses in preds_num_clauses.items():
                if other_pred == pred: continue
                elim_lits.append(f"num_clauses({other_pred},{num_clauses})")
            num_recursive = preds_num_recursive_clauses[pred]
            elim_lits.append(f"num_recursive({pred},{num_recursive})")

            yield f":-included_program({program_ident})," + ','.join(elim_lits) + '.'
