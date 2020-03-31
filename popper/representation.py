import re

from enum import Enum
from collections import namedtuple

from functools import reduce


#EXAMPLE_PROGRAM = [(0, Atom('f',mode,[0,1]),{Atom('tail',mode,[0,2]),Atom('head',mode,[2,1])})],
#                   (1, Atom('f',mode,[0,1]),{Atom('reverse',mode,[0,2]),Atom('sumlist',mode,[2,1])})],

#                   ]
# where mode is an appropriate mode for each atom


class Atom(namedtuple('Atom', ['predicate', 'mode', 'arguments'])):
     @property
     def arity(self):
         return len(self.arguments)

     def __str__(self):
         return f"{self.predicate}({','.join(self.arguments)})"

     def __repr__(self): # strictly speaking wrong, but more useful for debugging
         mode_args = (f"{m.value}V{arg}" for arg, m in zip(self.arguments, self.mode.argument_modes))
         return f"{self.predicate}({','.join(mode_args)})"


class ArgumentMode(Enum):
    Input = '+'
    Output = '-'
    Unknown = '?'


class ModeDecleration(namedtuple('ModeDecleration', ['predicate', 'argument_modes'])):
    @property
    def arity(self):
        return len(self.argument_modes)

    def __str__(self):
        Out = ArgumentMode.Output
        return self.predicate + "(" + ",".join("-" if mode == Out else "+" for mode in self.argument_modes) + ")"

    @staticmethod
    def parse_direction_modes(str_, predicate, arity):
        argument_modes = []
        for argument_idx in range(int(arity)):
            result = re.search(f"direction\({predicate}, *{argument_idx}, *([^\n ]*)\).", str_)
            if result and result.group(1) == 'out':
                argument_modes.append(ArgumentMode.Output)
            elif result and result.group(1) == 'in':
                argument_modes.append(ArgumentMode.Input)
            else: # NB, this case is used as a default as well
                argument_modes.append(ArgumentMode.Unknown)
        return __class__(predicate, tuple(argument_modes))

    @staticmethod
    def from_modeh(str_):
        result = re.search("modeh\(([^,]*?), *(\d+)\).", str_)
        predicate, arity = result.group(1), result.group(2)
        return __class__.parse_direction_modes(str_, predicate, arity)

    @staticmethod
    def from_modebs(str_):
        modebs = []
        for result in re.findall("modeb\(([^,\n]*?), *(\d+)\).", str_):
            predicate, arity = result
            modebs.append(__class__.parse_direction_modes(str_, predicate, arity))
        return modebs


def literal_to_code(lit):
    predicate, _, args = lit
    args = map(lambda arg: chr(ord('A') + arg) if type(arg) == int else arg, args)
    return f"{predicate}({','.join(args)})"


def clause_to_code(clause):
    lits = list(map(literal_to_code, clause))
    return f"{lits[0]} :- {','.join(lits[1:])}"


def program_to_code(program):
    code_program = []
    for clause in program:
        code_program.append(clause_to_code(clause) + '.')
    return code_program


def is_recursive_clause(clause):
    for literal in clause[1:]:
        if literal.predicate == clause[0].predicate:
            return True
    return False


def is_recursive_program(program):
    return reduce(lambda x, y: x or is_recursive_clause(y), program, False)
