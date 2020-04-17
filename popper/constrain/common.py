from ..representation import VAR_ANY


def head_atom_to_asp_literal(cl_id, atom, ground=False):
    return "head_" + _atom_to_asp_literal(cl_id, atom, ground)


def body_atom_to_asp_literal(cl_id, atom, ground=False):
    return "body_" + _atom_to_asp_literal(cl_id, atom, ground)


def _atom_to_asp_literal(cl_id, atom, ground):
    pred, arity, args = atom.predicate, atom.arity, atom.arguments

    if not ground:
        #TODO: generating variable for the head's inputs only increaes grounding. Get rid of vars for these
        args = tuple(map(lambda arg: "_" if arg == VAR_ANY else f"{cl_id}V{arg}",
                         args))
    else:
        args = tuple(map(str,args))
    if len(args) == 1: asp_vars = f"({args[0]},)"
    else: asp_vars = "(" + ",".join(args) + ")"
    return f"literal({cl_id},{pred},{arity},{asp_vars})"


def clause_to_asp_literals(clause, ground=False, cl_id=None):
    cl_id_, head, body = clause
    if cl_id == None:
        cl_id = f"ClId{cl_id_}" if not ground else str(cl_id_)
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


def asp_literals_for_distinct_variables(program):
    lits = []
    for clause in program:
        cl_id, head, body = clause
        clause_vars = set(var for atom in ([head] + list(body)) for var in atom.arguments)
        for var1 in clause_vars:
            for var2 in clause_vars:
                # TODO: if we assert inequality by <, that should (soundly) shrink the grounding a bit
                # (in)equality constraints are symmetric, hence no need to consider both orderings
                if var1 == var2: break
                # ensure clauses do not overlap (smaller grounding as well)
                lits += [f"ClId{cl_id}V{var1}" + "!=" + f"ClId{cl_id}V{var2}"]
    return lits
