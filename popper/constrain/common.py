from ..representation import VAR_ANY


def head_atom_to_asp_literal(cl_id, atom, ground=False):
    return "head_" + _atom_to_asp_literal(cl_id, atom, ground)


def body_atom_to_asp_literal(cl_id, atom, ground=False):
    return "body_" + _atom_to_asp_literal(cl_id, atom, ground)


def _atom_to_asp_literal(cl_id, atom, ground):
    pred, arity, args = atom.predicate, atom.arity, atom.arguments

    if not ground:
        args = tuple(map(lambda arg: "_" if arg == VAR_ANY else f"{cl_id}V{arg}",
                         args))
    else:
        args = tuple(map(str,args))
    if len(args) == 1: asp_vars = f"({args[0]},)"
    else: asp_vars = "(" + ",".join(args) + ")"
    return f"literal({cl_id},{pred},{arity},{asp_vars})"


def clause_to_asp_literals(clause, ground=False):
    cl_id, head, body = clause
    cl_id = f"ClId{cl_id}" if not ground else str(cl_id)
    lits = [head_atom_to_asp_literal(cl_id, head, ground)]
    lits += map(lambda atom: body_atom_to_asp_literal(cl_id, atom, ground),
                body)
    return lits


def asp_literals_for_distinct_clauses(program):
    lits = []
    for clause1 in program:
        for clause2 in program:
            # (in)equality constraints are symmetric, hence no need to consider both orderings
            if clause2 == clause1: break 
            cl1_id, cl2_id = f"ClId{clause1[0]}", f"ClId{clause2[0]}"
            # ensure clauses do not overlap (smaller grounding as well)
            lits += [cl1_id + "!=" + cl2_id] 
    return lits
