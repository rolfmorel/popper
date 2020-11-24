import re
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, Any, Tuple, Dict, Set

from enum import Enum
from collections import abc, defaultdict


VAR_ANY = None # identifier for when the particular variable does not matter


@dataclass(frozen=True)
class PredicateSymbol():
    name: str


class ArgumentMode(Enum):
    Input = '+'
    Output = '-'
    Unknown = '?'


@dataclass(frozen=True)
class ModeDeclaration():
    predicate: PredicateSymbol
    arguments: Tuple[ArgumentMode, ...] # : abc.Sequence[ArgumentMode, ...]

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


#class Assigned(ABC):
#    @property
#    @abstractmethod
#    def truth_value(): pass
#
#
#class Truthy(Assigned):
#    @property
#    def truth_value(): return True
#
#
#class Falsy(Assigned):
#    @property
#    def truth_value(): return False


Variable = int
#@dataclass(frozen=True)
#class Variable():
#    name: str


@dataclass(frozen=True)
class Atom():
    predicate: PredicateSymbol
    arguments: Tuple[Union[Variable,Any], ...] # : abc.Sequence[Union[Variable,Any], ...]

    def __str__(self):
        args = (f"{arg}" for arg in self.arguments)
        return f"{self.predicate}({','.join(args)})"

    @property
    def arity(self):
        return len(self.arguments)


@dataclass(frozen=True)
class ModedAtom(Atom):
    mode: ModeDeclaration


#@dataclass(frozen=True)
#class TruthyAtom(Atom,Truthy):
#    def negate(self): 
#        return DeniedAtom(self.predicate, self.arguments)
#
#
#@dataclass(frozen=True)
#class FalsyAtom(Atom,Falsy):
#    def negate(self): 
#        return AssertedAtom(self.predicate, self.arguments)


@dataclass(frozen=True, order=True)  # NB: order just to have an arbitrary canonical order
class ProgramAtom(ModedAtom):
    #arguments: Tuple[Variable,...]  # we can assume this holds

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
    def code_args(self):
        return ','.join(map(lambda arg: chr(ord('A') + arg), self.arguments))

    @property
    def inputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[0]))

    @property
    def outputs(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[1]))

    @property
    def unknowns(self):
        return set(map(lambda idx_arg: idx_arg[1], self.split_arguments()[2]))


class DefiniteClauseMixin():
    num: int 
    head: ProgramAtom
    body: abc.Collection

    def __iter__(self):
        yield self.num; yield self.head; yield self.body

    def is_recursive(self):
        return any(literal.predicate == self.head.predicate for literal in self.body)


@dataclass(frozen=True, order=True)  # NB: order just to have an arbitrary canonical order
class UnorderedClause(DefiniteClauseMixin):
    num: int  # (original) clause number
    head: ProgramAtom
    body: Set[ProgramAtom] # : abc.Collection[ProgramAtom]

    # __eq__, __le__ and __hash__ auto-generated 

    def to_code(self):
        head_, body_ = str(self.head.to_code()), (atom.to_code() for atom in self.body)
        return f"{head_} :- {{ {','.join(body_)} }}"


@dataclass(frozen=True, order=True)  # NB: order just to have an arbitrary canonical order
class OrderedClause(DefiniteClauseMixin):
    num: int  # (original) clause number
    head: ProgramAtom
    body: Tuple[ProgramAtom, ...] # : abc.Sequence[ProgramAtom, ...]

    # __eq__, __le__ and __hash__ auto-generated 

    def to_code(self):
        head_, body_ = str(self.head.to_code()), (atom.to_code() for atom in self.body)
        return f"{head_} :- {','.join(body_)}"


@dataclass(frozen=True, order=True)  # NB: order just to have an arbitrary canonical order
class DefiniteProgramMixin():
    clauses: Tuple[DefiniteClauseMixin] # : abc.Sequence[DefiniteClause,...]

    # __eq__, __le__ and __hash__ auto-generated (just for self.clauses)

    def __iter__(self):
        return iter(self.clauses)

    def __len__(self):
        return len(self.clauses)

    def is_recursive(self):
        # TODO: is incorrect on mutually recursive programs
        return any(cl.is_recursive() for cl in self.clauses)

    def to_code(self):
        return tuple(cl.to_code() for cl in self.clauses)


class Metadata():
    # the following are metadata
    before: Dict[int, abc.Set] # : abc.Mapping[int, abc.Set]
    min_clause: Dict[int, int] # : abc.Mapping[int, int]

    def __init__(self, *args,
                 before=defaultdict(set),
                 min_clause=defaultdict(lambda: 0),
                 **kwargs):
        self.before = before
        self.min_clause = min_clause
        super().__init__(*args, **kwargs)


# NB: Not dataclasses. We do this so we that __hash__, __eq__, etc. 
#     are on the actual program and not its metadata
class UnorderedProgram(Metadata, DefiniteProgramMixin):
    # clauses : Tuple[UnorderedClause] # may assume this

    # TODO: move this to UnorderedClause
    def to_ordered(self):
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
                raise ValueError(f"literals {literals} in program {self} could not be grounded")
            return [selected_lit] + \
                   selection_closure(head_pred, grounded_vars.union(selected_lit.outputs),
                                     literals.difference({selected_lit}))

        def transform_clause(clause):
            cl_id, head, body = clause
            ordered_body = tuple(selection_closure(head.predicate, head.inputs, body))
            return OrderedClause(cl_id, head, ordered_body)

        return OrderedProgram(tuple(map(transform_clause, self)),
                              before=self.before, min_clause=self.min_clause)


class OrderedProgram(Metadata, DefiniteProgramMixin):
    # clauses : Tuple[OrderedClause] # may assume this
    pass
