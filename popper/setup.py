from . import generate, test, constrain

from .input import parse_examples
from .util import TimeAccumulatingContext, DummyTimeAccumulatingContext


def setup(mode_file, bk_file, examples_file,
          max_literals, eval_timeout, ground_constraints, no_pruning, debug, stats, tester):
    ContextClass = TimeAccumulatingContext if stats else DummyTimeAccumulatingContext 
    context = ContextClass()
    with context:
        pos_exs, neg_exs = parse_examples(examples_file)

        Generate = generate.Generate(mode_file, max_literals=max_literals, debug=debug, context=ContextClass())
        context.add_child('generate', Generate.context)

        Tester = None
        if tester == 'prolog': Tester = test.TestProlog
        if tester == 'datalog': Tester = test.TestDatalog

        Test = Tester(Generate.modeh, bk_file, pos_exs, neg_exs, eval_timeout, debug=debug, context=ContextClass())
        context.add_child('test', Test.context)

        Constrain = constrain.Constrain(Generate.modeh, ground=ground_constraints,
                                        no_pruning=no_pruning, debug=debug)
        # NB: Constrain does not have a (time keeping) context

        return context, (Generate, Test, Constrain)
