from collections import namedtuple, defaultdict
from functools import reduce
from itertools import chain


class Var(namedtuple('Var', ('clause', 'names'))):
    def __eq__(self, other):
        return isinstance(other, __class__) and super().__eq__(other)

    def __hash__(self):
        return hash((self.clause, self.names))



def program_to_dependency_forest(program):
    nodes = dict()
    outgoing = defaultdict(set)
    incoming = defaultdict(set)

    for clause in program:
        forwards = defaultdict(set)
        backwards = defaultdict(set)

        clause_id, head, body = clause
        nodes[(clause_id, 0)] = head
        for lit_id, atom in enumerate(body, start=1):
            nodes[(clause_id, lit_id)] = atom

        for head_input in head.inputs:
            forwards[(clause_id, 0)].add(Var(clause_id, head_input))
            backwards[Var(clause_id, head_input)].add((clause_id, 0))

            for lit_id, atom in enumerate(body, start=1):
                if head_input in atom.inputs:
                    forwards[Var(clause_id, head_input)].add((clause_id, lit_id))
                    backwards[(clause_id, lit_id)].add(Var(clause_id, head_input))

        body_outputs = set(reduce(set.union, map(lambda a: a.outputs, body), set())) - head.inputs
        for body_output in body_outputs:
            for lit_id, atom in enumerate(body, start=1):
                if body_output in atom.outputs:
                    forwards[(clause_id, lit_id)].add(Var(clause_id, body_output))
                    backwards[Var(clause_id, body_output)].add((clause_id, lit_id))
            for lit_id, atom in enumerate(body, start=1):
                if body_output in atom.inputs:
                    forwards[Var(clause_id, body_output)].add((clause_id, lit_id))
                    backwards[(clause_id, lit_id)].add(Var(clause_id, body_output))

        vars = list(head.inputs) + list(body_outputs)
        collated = set()
        for var1 in vars:
            if var1 in collated: continue
            names = [var1]
            for var2 in filter(lambda v2: v2 != var1, vars):
                if forwards[(clause_id, var1)] == forwards[(clause_id,var2)] and \
                   backwards[(clause_id, var1)] == backwards[(clause_id, var2)]:
                       names.append(var2)
                       collated.add(var2)
            nodes[Var(clause_id, tuple(names))] = ()

            outgoing[Var(clause_id, tuple(names))] = set(n for n in forwards[Var(clause_id, var1)] if not isinstance(n, Var)) 
            for atom_pos in outgoing[Var(clause_id, tuple(names))]:
                incoming[atom_pos].add(Var(clause_id, tuple(names)))

            incoming[Var(clause_id, tuple(names))] = set(n for n in backwards[Var(clause_id, var1)] if not isinstance(n, Var)) 
            for atom_pos in incoming[Var(clause_id, tuple(names))]:
                outgoing[atom_pos].add(Var(clause_id, tuple(names)))

    return nodes, outgoing, incoming


def to_dep_exe_forest(df, ef):
    dep_nodes, dep_outgoing, dep_incoming = df
    exe_nodes, exe_outgoing, exe_incoming = ef
    nodes = exe_nodes.copy()
    outgoing = defaultdict(set)
    incoming = defaultdict(set)

    for atom_full_path, exe_node in exe_nodes.items():
        atom_pos, path = atom_full_path[0], atom_full_path[1:]
        in_vars = dep_incoming[atom_pos]
        out_vars = dep_outgoing[atom_pos]

        for var in in_vars:
            nodes[(path, var)] = ()
            incoming[atom_full_path].add((path, var))
            outgoing[(path, var)].add(atom_full_path)

        for var in out_vars:
            nodes[(path, var)] = ()
            if atom_full_path[0][1] == 0 and len(atom_full_path) > 1:
                # atom is the head of a recursive call
                atom_full_path = atom_full_path[1:]
            outgoing[atom_full_path].add((path, var))
            incoming[(path, var)].add(atom_full_path)

    return nodes, outgoing, incoming

