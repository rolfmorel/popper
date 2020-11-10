from collections import defaultdict

from ..representation import ProgramAtom


class RepresentationMixin(object):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def model_to_program(self, model):
        clause_id_to_head = {}
        clause_id_to_body = defaultdict(set)
        for atom in model:
            if atom.name not in ('head_literal', 'body_literal'):
                continue

            clause_id = atom.arguments[0].number
            predicate = atom.arguments[1].name
            arguments = tuple(map(lambda arg: arg.number,
                              atom.arguments[3].arguments))
            if atom.name == "head_literal":
                head_atom = ProgramAtom(predicate, arguments, self.modeh)
                clause_id_to_head[clause_id] = head_atom
            if atom.name == "body_literal":
                modeb = self.predicate_to_modeb[predicate]
                body_atom = ProgramAtom(predicate, arguments, modeb)
                clause_id_to_body[clause_id].add(body_atom)
        return tuple(map(lambda clause_key: (clause_key,
                                             clause_id_to_head[clause_key],
                                             clause_id_to_body[clause_key]),
                         sorted(clause_id_to_head.keys())))
