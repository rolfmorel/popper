import logging
from collections import OrderedDict

import clingo
from clingo import Function


class CodeFormatter(logging.Formatter):
    info_fmt = logging.Formatter('{message}', style='{')
    warning_fmt = logging.Formatter('% WARN: {message}', style='{')

    def format(record):
        if record.levelno == logging.INFO:
            return __class__.info_fmt.format(record)
        elif record.levelno == logging.WARN:
            return __class__.warning_fmt.format(record)
        else:
            return super().format(record)


class Clingo():
    def __init__(self, args=[]):
        self.code_log = logging.getLogger('solver_code')
        #TODO: enable the following logging by a cmdline option
        self.code_log.disabled = True
        #code_handler = logging.FileHandler('clingo_solver_code_debug.asp')
        #code_handler.setFormatter(CodeFormatter)
        #self.code_log.addHandler(code_handler)
        self.code_log.setLevel(logging.INFO)

        self.code_log.warn("NEW INSTANCE")

        self.clingo_ctl = clingo.Control(args)

        self.added = OrderedDict()
        self.grounded = []
        self.assigned = OrderedDict()

    def add(self, code, name=None, arguments=[]):
        if name is None:
            name = f"fragment{len(self.added)+1}"
        self.code_log.warning(f"ADDING following fragment with name '{name}'")
        self.code_log.info(code)

        self.clingo_ctl.add(name, arguments, code)
        self.added[name] = code

    def ground(self, *args, context=None, **kwargs):
        parts = [(name, []) for name in args]
        parts.extend(kwargs.items())

        self.code_log.warning(f"GROUNDING parts '{parts}'")

        if parts == []:
            grounded = set(name for name, _ in self.grounded)
            parts = list((name, []) for name in self.added not in grounded)

        self.clingo_ctl.ground(parts, context=context)
        self.grounded.extend(parts)

    def release(self, atom, *args, **kwargs):
        symbol = Function(atom.predicate, atom.arguments)
        self.assigned[atom] = False
        self.code_log.warning(f'Atom {atom} is released/made False')
        return self.clingo_ctl.release_external(symbol, *args, **kwargs)

    def assign(self, atom, truth_value, *args, **kwargs):
        symbol = Function(atom.predicate, list(atom.arguments))
        self.assigned[atom] = truth_value
        self.code_log.warning(f'Atom {atom} is assigned/made {truth_value}')
        return self.clingo_ctl.assign_external(symbol, truth_value, *args, **kwargs)

    def get_model(self):
        with self.clingo_ctl.solve(yield_=True) as handle:
            model = next(handle, None)
            if model:
                return model.symbols(atoms=True)
            return model
