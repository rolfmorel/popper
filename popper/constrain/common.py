VAR_ANY = None # identifier for when the particular variable does not matter


def atom_to_asp_literals(atom, clid=None, litid=None):
    (atom_clid, atom_litid, pred, args) = atom
    clid, litid = clid or atom_clid, litid or atom_litid
    lit_constraint = [f"literal({clid},{litid},{pred},{len(args)})"]
    for var_pos, var_id in enumerate(args):
        if var_id == VAR_ANY:
            var_id = '_'
        lit_constraint.append(f"var({clid},{litid},{var_pos},{var_id})")
    return lit_constraint


def var_gen(varname='V', count=0):
    while True:
        yield varname + str(count)
        count += 1
