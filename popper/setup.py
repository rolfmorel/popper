from . import generate, test, constrain

from .input import parse_examples
from .util import TimeAccumulatingContext


def setup(mode_file, bk_file, examples_file,
          max_literals, eval_timeout, ground_constraints, no_pruning, debug):
    context = TimeAccumulatingContext()
    with context:
        pos_exs, neg_exs = parse_examples(examples_file)

        Generate = generate.Generate(mode_file, max_literals=max_literals, debug=debug)
        context.add_child('generate', Generate.context)

        Test = test.Test(Generate.modeh, bk_file, pos_exs, neg_exs, eval_timeout, debug=debug)
        context.add_child('test', Test.context)

        Constrain = constrain.Constrain(Generate.modeh, ground=ground_constraints,
                                        no_pruning=no_pruning, debug=debug)
        # NB: Constrain does not have a (time keeping) context

        return context, (Generate, Test, Constrain)
