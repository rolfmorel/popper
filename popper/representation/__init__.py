from functools import reduce

from .data_types import Atom, EvalAtom, ModeDeclaration, ArgumentMode, VAR_ANY


#EXAMPLE_PROGRAM = [(0, Atom('f',mode,[0,1]),{Atom('tail',mode,[0,2]),Atom('head',mode,[2,1])})],
#                   (1, Atom('f',mode,[0,1]),{Atom('reverse',mode,[0,2]),Atom('sumlist',mode,[2,1])})],

#                   ]
# where mode is an appropriate mode for each atom


def is_recursive_clause(clause):
    _, head, body = clause
    for literal in body:
        if literal.predicate == head.predicate:
            return True
    return False


def is_recursive_program(program):
    return reduce(lambda x, y: x or is_recursive_clause(y), program, False)


def program_to_ordered_program(program):
    def selection_closure(head_pred, grounded_vars, literals):
        if len(literals) == 0: return []

        rec_lits, nonrec_lits = [], []
        for lit in literals:
            if lit.inputs.issubset(grounded_vars):
                if lit.predicate == head_pred:
                    rec_lits.append(lit)
                else:
                    nonrec_lits.append(lit)

        selected_lit = next(iter(nonrec_lits + rec_lits), None)
        if selected_lit == None:
            raise ValueError(f"literals {literals} could not be grounded")
        return [selected_lit] + \
               selection_closure(head_pred, grounded_vars.union(selected_lit.outputs),
                                 literals.difference({selected_lit}))
    ordered_clauses = []
    for clause in program:
        cl_id, head, body = clause
        ordered_clauses.append((cl_id, head,
                tuple(selection_closure(head.predicate, head.inputs, body.copy()))))
    return ordered_clauses


def clause_to_code(clause):
    _, head, body = clause
    head_, body_ = str(head.to_code()), map(lambda a: a.to_code(), body)
    if type(body) == set:
        return f"{head_} :- {{ {','.join(body_)} }}"
    return f"{head_} :- {','.join(body_)}"


def program_to_code(program):
    code_program = []
    for clause in program:
        code_program.append(clause_to_code(clause) + '.')
    return code_program
