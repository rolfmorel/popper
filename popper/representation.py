import re

from enum import Enum
from collections import namedtuple

from functools import reduce


#EXAMPLE_PROGRAM = [(0, Atom('f',mode,[0,1]),{Atom('tail',mode,[0,2]),Atom('head',mode,[2,1])})],
#                   (1, Atom('f',mode,[0,1]),{Atom('reverse',mode,[0,2]),Atom('sumlist',mode,[2,1])})],

#                   ]
# where mode is an appropriate mode for each atom


VAR_ANY = None # identifier for when the particular variable does not matter


class Atom(namedtuple('Atom', ['predicate', 'mode', 'arguments'])):
    def __str__(self):
        args = map(lambda arg: f"V{arg}", self.arguments)
        return f"{self.predicate}({','.join(args)})"

    def to_code(self):
        args = map(lambda arg: chr(ord('A') + arg) if type(arg) == int else arg,
                   self.arguments)
        return f"{self.predicate}({','.join(args)})"

    def __repr__(self): # strictly speaking wrong, but more useful for debugging
        mode_args = (f"{m.value}V{arg}" for arg, m in zip(self.arguments, self.mode.arguments))
        return f"{self.predicate}({','.join(mode_args)})"

    def split_arguments(self):
        ins, outs, unks = set(), set(), set()
        
        for idx, (mode, arg) in enumerate(zip(self.mode.arguments, self.arguments)):
            if mode == ArgumentMode.Input: ins.add((idx,arg))
            if mode == ArgumentMode.Output: outs.add((idx,arg))
            if mode == ArgumentMode.Unknown: unks.add((idx,arg))
        return ins, outs, unks

    @property
    def arity(self):
        return len(self.arguments)

    @property
    def inputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[0]))

    @property
    def outputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[1]))

    @property
    def unknowns(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[2]))


class ArgumentMode(Enum):
    Input = '+'
    Output = '-'
    Unknown = '?'


class ModeDeclaration(namedtuple('ModeDeclaration', ['predicate', 'arguments'])):
    @property
    def arity(self):
        return len(self.arguments)

    def __str__(self):
        return self.predicate + "(" + ",".join(mode.value for mode in self.arguments) + ")"

    @staticmethod
    def parse_direction_modes(str_, predicate, arity):
        arguments = []
        for argument_idx in range(int(arity)):
            result = re.search(f"[^%]direction\({predicate}, *{argument_idx}, *([^\n ]*)\).", str_)
            if result and result.group(1) == 'out':
                arguments.append(ArgumentMode.Output)
            elif result and result.group(1) == 'in':
                arguments.append(ArgumentMode.Input)
            else:
                arguments.append(ArgumentMode.Unknown)
        return __class__(predicate, tuple(arguments))

    @staticmethod
    def from_modeh(str_):
        result = re.search("[^%]modeh\(([^,]*?), *(\d+)\).", str_)
        predicate, arity = result.group(1), result.group(2)
        return __class__.parse_direction_modes(str_, predicate, arity)

    @staticmethod
    def from_modebs(str_):
        modebs = []
        for result in re.findall("[^%]modeb\(([^,\n]*?), *(\d+)\).", str_):
            predicate, arity = result
            modebs.append(__class__.parse_direction_modes(str_, predicate, arity))
        return modebs


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
        ordered_clauses.append((cl_id,
                head, selection_closure(head.predicate, head.inputs, body.copy())))
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
