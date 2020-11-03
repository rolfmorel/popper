from collections import defaultdict

from ..representation import Atom, ModeDeclaration, ArgumentMode


PRINT_ATOMS = ['head_literal', 'body_literal']
PRINT_ATOMS = []


class RepresentationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def model_to_program(self, model):
        clause_id_to_head = {}
        clause_id_to_body = defaultdict(set)
        directions = defaultdict(lambda: defaultdict(lambda: ArgumentMode.Unknown))

        for atom in model:
            if atom.name in PRINT_ATOMS:
                print(atom)
            if atom.name == "direction":
                pred_name = atom.arguments[0].name
                arg_idx = atom.arguments[1].number
                arg_dir_str = atom.arguments[2].name

                if arg_dir_str == 'in':
                    arg_dir = ArgumentMode.Input
                elif arg_dir_str == 'out':
                    arg_dir = ArgumentMode.Output
                else:
                    raise Exception(f"Unrecogniced argument direction '{arg_dir_str}'")

                directions[pred_name][arg_idx] = arg_dir
            if atom.name == "head_literal":
                clause_id = atom.arguments[0].number
                predicate = atom.arguments[1].name
                arguments = tuple(map(lambda arg: arg.number,
                                  atom.arguments[3].arguments))

                head_atom = Atom(predicate, None, arguments)
                clause_id_to_head[clause_id] = head_atom
            if atom.name == "body_literal":
                clause_id = atom.arguments[0].number
                predicate = atom.arguments[1].name
                arguments = tuple(map(lambda arg: arg.number,
                                  atom.arguments[3].arguments))

                body_atom = Atom(predicate, None, arguments)
                clause_id_to_body[clause_id].add(body_atom)

        # now that all directions have been seen, set the modes
        for cl_id in clause_id_to_head.keys():
            atom = clause_id_to_head[cl_id]
            dirs = (directions[atom.predicate][i] for i in range(atom.arity))
            mode = ModeDeclaration(atom.predicate, tuple(dirs))
            clause_id_to_head[cl_id] = Atom(atom.predicate, mode, atom.arguments)
        for cl_id, body in clause_id_to_body.items():
            body_with_dirs = set()
            for atom in body:
                dirs = (directions[atom.predicate][i] for i in range(atom.arity))
                mode = ModeDeclaration(atom.predicate, tuple(dirs))
                body_with_dirs.add(Atom(atom.predicate, mode, atom.arguments))
            clause_id_to_body[cl_id] = body_with_dirs

        return tuple(map(lambda clause_key: (clause_key,
                                             clause_id_to_head[clause_key],
                                             clause_id_to_body[clause_key]),
                         sorted(clause_id_to_head.keys())))
