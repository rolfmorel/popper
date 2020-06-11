import re

from enum import Enum
from collections import namedtuple


VAR_ANY = None # identifier for when the particular variable does not matter


class Atom(namedtuple('Atom', ('predicate', 'mode', 'arguments'))):
    def __str__(self):
        args = map(lambda arg: f"V{arg}", self.arguments)
        return f"{self.predicate}({','.join(args)})"

    def to_code(self):
        return f"{self.predicate}({self.code_args})"

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
    def success(self): # interface of EvalAtom
        return None # signifying 'unknown'

    @property
    def code_args(self):
        return ','.join(map(lambda arg: chr(ord('A') + arg) if type(arg) == int else arg,
                   self.arguments))

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


class EvalAtom(namedtuple('EvaluatedAtom', ('predicate', 'grounding', 'success'))):
    def __repr__(self): # strictly speaking wrong, but more useful for debugging
        return f"{self.predicate}({self.code_args})@{'T' if self.success else 'F'}"

    @property
    def code_args(self):
        return ','.join(map(str, self.grounding))


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
            result = re.search(f"[^%]direction\({predicate}, *{argument_idx}, *([^\n ]*)\).", "\n" + str_)
            if result and result.group(1) == 'out':
                arguments.append(ArgumentMode.Output)
            elif result and result.group(1) == 'in':
                arguments.append(ArgumentMode.Input)
            else:
                arguments.append(ArgumentMode.Unknown)
        return __class__(predicate, tuple(arguments))

    @staticmethod
    def from_modeh(str_):
        result = re.search("[^%]modeh\(([^,]*?), *(\d+)\).", "\n" + str_)
        predicate, arity = result.group(1), result.group(2)
        return __class__.parse_direction_modes(str_, predicate, arity)

    @staticmethod
    def from_modebs(str_):
        modebs = []
        for result in re.findall("[^%]modeb\(([^,\n]*?), *(\d+)\).", "\n" + str_):
            predicate, arity = result
            modebs.append(__class__.parse_direction_modes(str_, predicate, arity))
        return modebs
