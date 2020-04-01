from collections import defaultdict

from ..representation import Atom


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

                head_atom = Atom(predicate, self.modeh, arguments)
                clause_id_to_head[clause_id] = head_atom
            if atom.name == "body_literal":
                clause_id = atom.arguments[0].number
                predicate = atom.arguments[1].name
                arguments = tuple(map(lambda arg: arg.number,
                                  atom.arguments[3].arguments))

                modeb = self.predicate_to_modeb[predicate]
                body_atom = Atom(predicate, modeb, arguments)
                clause_id_to_body[clause_id].add(body_atom)
        return list(map(lambda clause_key: (clause_key,
                     clause_id_to_head[clause_key],
                     clause_id_to_body[clause_key]),
                        sorted(clause_id_to_head.keys())))
