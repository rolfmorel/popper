import time
import logging
import concurrent.futures.thread
from concurrent.futures import ThreadPoolExecutor, TimeoutError
from collections import OrderedDict

import clingo
from clingo import Function


class SolvingTimeout(BaseException):
    pass


class GroundingTimeout(BaseException):
    pass


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
    def __init__(self, args=[], end_time=None):
        self.end_time = end_time
        self.args = args

        self.code_log = logging.getLogger('solver_code')
        #TODO: enable the following logging by a cmdline option
        self.code_log.disabled = True
        #code_handler = logging.FileHandler('clingo_solver_code_debug.asp')
        #code_handler.setFormatter(CodeFormatter)
        #self.code_log.addHandler(code_handler)
        self.code_log.setLevel(logging.INFO)

        self.code_log.warn("NEW INSTANCE")

        self.clingo_ctl = clingo.Control(self.args)

        self.executor= ThreadPoolExecutor(max_workers=1)

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

        future = self.executor.submit(lambda : self.clingo_ctl.ground(parts, context=context))
        # The following is a hack to make sure that a cancelled thread won't be joined on by Python
        if next(iter(self.executor._threads)) in concurrent.futures.thread._threads_queues:
            del concurrent.futures.thread._threads_queues[next(iter(self.executor._threads))]
        timeout = self.end_time - time.time()
        try:
            future.result(timeout=timeout)
        except (KeyboardInterrupt, TimeoutError) as exc:
            future.cancel()
            if time.time() > self.end_time:
                raise GroundingTimeout(f"Grounding did not terminate within {timeout} seconds") from exc
            else:
                raise exc

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
        timeout = self.end_time - time.time() if self.end_time else None
        try:
            with self.clingo_ctl.solve(yield_=True, async_=True) as handle:
                handle.resume()

                did_finish = handle.wait(timeout)
                if timeout and not did_finish:
                    raise SolvingTimeout(f"Solving did not terminate within {timeout} seconds")

                m = handle.model()
                if m:
                    return m.symbols(atoms=True)
                return m
        except KeyboardInterrupt as ex:
            raise SolvingTimeout(f"Solving did not terminate within {timeout} seconds") from ex
