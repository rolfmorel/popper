import re

from enum import Enum

from functools import reduce


EXAMPLE_PROGRAM = [[(0,0,'f',[0,1]),(0,1,'tail',[0,2]),(0,2,'head',[2,1])],
                   [(1,0,'f',[0,1]),(1,1,'reverse',[0,2]),(1,2,'sumlist',[2,1])],
                   ]


class ArgumentMode(Enum):
    Input = '+'
    Output = '-'


class ModeDecleration(object):
    def __init__(self, predicate, argument_modes):
        self.predicate = predicate
        self.argument_modes = argument_modes

    @property
    def arity(self):
        return len(self.argument_modes)

    def __str__(self):
        Out = ArgumentMode.Output
        return self.predicate + "(" + ",".join("-" if mode == Out else "+" for mode in self.argument_modes) + ")"

    @staticmethod
    def from_str(str_):
        result = re.search("modeh\((.*),(\d+)\)", str_)
        predicate, arity = result.group(1), result.group(2)
        argument_modes = []
        for argument_idx in range(int(arity)):
            result = re.search(f"direction\({predicate},{argument_idx},(.*)\)", str_)
            if result and result.group(1) == 'out':
                argument_modes.append(ArgumentMode.Output)
            else: # NB, this case is uses as a default as well
                argument_modes.append(ArgumentMode.Input)
        return __class__(predicate, argument_modes)


def literal_to_code(lit):
    _, _, predicate, args = lit
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


def is_clause_recursive(clause):
    head_pred = clause[0][2]
    for literal in clause[1:]:
        if literal[2] == head_pred:
            return True
    return False


def is_program_recursive(program):
    return reduce(lambda x, y: x or is_clause_recursive(y), program, False)
