from popper.util import Result


class EvaluateMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('evaluate')
        self.context.add_child('query')
        super().__init__(*args, **kwargs)

    def query(self, program, example):
        with self.context.query:
            if example in self.atom_strs:
                return Result.Success, set((program,))
            return Result.Failure, set((program,))

    def evaluate(self, program, example):
        with self.context.evaluate:
            if example in self.atom_strs:
                return Result.Success, set((program,))
            return Result.Failure, set((program,))
