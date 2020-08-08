from time import time
from sys import stderr

from pyswip.prolog import PrologError

from popper.util import Result


class EvaluateMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('query')
        self.context.add_child('evaluate')
        super().__init__(*args, **kwargs)


    def query(self, example, timeout=None):
        with self.context.query:
            if timeout is None and self.eval_timeout:
                timeout = self.eval_timeout
            goal = (example if not timeout else
                    #f"catch(call_with_time_limit({timeout}, {example}),time_limit_exceeded,false)")
                    f"call_with_time_limit({timeout}, {example})")
            try:
                # TODO: query the program only on its input arguments, asserting the equality with output arguments as later atoms.
                assignments = list(self.prolog.query(goal))
                if assignments == []:
                    return Result.Failure, assignments
                else:
                    return Result.Success, assignments
            except PrologError as ex:
                self.DBG_PRINT(f"Prolog threw exception '{ex}'")
                if "time_limit_exceeded" in ex.args[0]:
                    return None, []
                if "stack" in ex.args[0]:  # NB: not so nice way to detect a stack-overflowing program
                    self.DBG_PRINT(f"STACK OVERFLOW for {example}!")
                    return None, []
                else:
                    raise ex


    def evaluate(self, program, example):
        result, assignments = self.query(example)
        with self.context.evaluate:
            if assignments != [{}] and assignments != []:
                assert False # could only possibly happen when example was non-ground, which we don't deal with now

            success_progs = failure_progs = set()
            if result:
                success_progs = { program }
            if not result:
                failure_progs = { program }
            return result, success_progs, failure_progs
