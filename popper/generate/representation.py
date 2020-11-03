from collections import defaultdict

from ..representation import Atom, ModeDeclaration


PRINT_ATOMS = ['head_literal', 'body_literal']
PRINT_ATOMS = []


class RepresentationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def model_to_program(self, model):
        clause_id_to_head = {}
        clause_id_to_body = defaultdict(set)
        for atom in model:
            if atom.name in PRINT_ATOMS:
                print(atom)
            if atom.name == "head_literal":
                clause_id = atom.arguments[0].number
                predicate = atom.arguments[1].name
                arguments = tuple(map(lambda arg: arg.number,
                                  atom.arguments[3].arguments))

                if self.modeh.predicate == predicate:
                    mode = self.modeh
                else:
                    mode = ModeDeclaration.from_arity(predicate, len(arguments))

                head_atom = Atom(predicate, mode, arguments)
                clause_id_to_head[clause_id] = head_atom
            if atom.name == "body_literal":
                clause_id = atom.arguments[0].number
                predicate = atom.arguments[1].name
                arguments = tuple(map(lambda arg: arg.number,
                                  atom.arguments[3].arguments))

                mode = self.predicate_to_modeb.get(predicate, None)
                if not mode:
                    mode = ModeDeclaration.from_arity(predicate, len(arguments))

                body_atom = Atom(predicate, mode, arguments)
                clause_id_to_body[clause_id].add(body_atom)
        return tuple(map(lambda clause_key: (clause_key,
                     clause_id_to_head[clause_key],
                     clause_id_to_body[clause_key]),
                        sorted(clause_id_to_head.keys())))
