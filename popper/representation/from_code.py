from .data_types import ModeDeclaration, Atom

EVS_MODES = """\
modeh(evs,1). 
direction(evs,0,in). 
modeb(empty,1). 
direction(empty,0,in). 
modeb(head,2). 
direction(head,0,in). 
direction(head,1,out). 
modeb(even,1). 
direction(even,0,in). 
modeb(odd,1). 
direction(odd,0,in). 
modeb(tail,2). 
direction(tail,0,in). 
direction(tail,1,out). 
modeb(evs,1). 
direction(evs,0,in). 
"""
EVS_CODE = """\
evs(A) :- empty(A).
evs(A) :- head(A,B),even(B),tail(A,C),head(C,D),odd(D),tail(C,E),evs(E).
"""


# assumes definite program as input
def from_strs(modes_str, program_str):
    head_mode = ModeDeclaration.from_modeh(modes_str)
    body_modes = ModeDeclaration.from_modebs(modes_str)

    return from_code((head_mode, body_modes), program_str)

def from_code(modes, program_str):
    head_mode, body_modes = modes

    lines = filter(lambda l: l != '', program_str.split('\n'))
    clauses = []
    for clause_id, line in enumerate(lines):
        assert line[-1] == '.'
        head, _, body = line[:-2].partition(':-')
        head = head.strip()
        body = body.strip().split('),')

        def parse_atom_str(atom):
            pred, _, args = atom.partition('(')
            return pred, [ord(arg_str) - ord('A') for arg_str in args.split(',')]

        head_pred, head_args = parse_atom_str(head[:-1])
        head_atom = Atom(head_pred, head_mode, head_args)

        body_atoms = []
        for atom in body:
            body_pred, body_args = parse_atom_str(atom)
            body_mode = next(filter(
                lambda m: m.predicate == body_pred and m.arity == len(body_args),
                body_modes))
            body_atoms.append(Atom(body_pred, body_mode, body_args))
        clauses.append((clause_id, head_atom, tuple(body_atoms)))

    return clauses

