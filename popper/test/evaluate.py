from sys import stderr

from pyswip.prolog import PrologError

from ..util import Result


class EvaluateMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('evaluate')
        super().__init__(*args, **kwargs)

    def query(self, example):
        try:
            # TODO: query the program only on its input arguments, asserting the equality with output arguments as later atoms.
            if self.eval_timeout:
                goal = f"catch(call_with_time_limit({self.eval_timeout}, {example}),time_limit_exceeded,false)"
            else:
                goal = example
            return list(self.prolog.query(goal))
        except PrologError as ex:
            if "stack" in ex.args[0]:  # NB: not so nice way to detect a stack-overflowing program
                self.DBG_PRINT(f"STACK OVERFLOW for {example}!")
                return []
            else:
                raise ex

    def evaluate(self, example):
        with self.context.evaluate:
            assignments = self.query(example)

            if assignments == [{}]:
                return Result.Success
            elif assignments == []:
                return Result.Failure
            else:
                assert False # could only possibly happen when example was non-ground, which we don't deal with now
