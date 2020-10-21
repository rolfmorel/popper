from . import generate, test, constrain

from .solvers.asp import Clingo
from .input import parse_examples
from .util import TimeAccumulatingContext, DummyTimeAccumulatingContext


def setup(mode_file, bk_file, examples_file,
          max_literals, eval_timeout, ground_constraints, no_pruning, debug, stats, tester,
          clingo_args):
    ContextClass = TimeAccumulatingContext if stats else DummyTimeAccumulatingContext 
    context = ContextClass()
    with context:
        pos_exs, neg_exs = parse_examples(examples_file)

        debug = False # hack to disable debug messages from stages

        asp_solver = Clingo(clingo_args)

        Generate = generate.Generate(mode_file, max_literals=max_literals, debug=debug, context=ContextClass(),
                                     solver=asp_solver)
        context.add_child('generate', Generate.context)

        Tester = None
        if tester == 'prolog': Tester = test.TestProlog
        if tester == 'prolog.mlj':
            Tester = test.TestProlog
            Tester.analyses = 'mlj'
        if tester == 'datalog': Tester = test.TestDatalog
        if tester == 'prolog.analyse': Tester = test.AnalyseProlog

        Test = Tester(Generate.modeh, bk_file, pos_exs, neg_exs, eval_timeout, debug=debug, context=ContextClass())
        context.add_child('test', Test.context)

        Constrain = constrain.Constrain(Generate.modeh, len(pos_exs), len(neg_exs),
                                        ground=ground_constraints, no_pruning=no_pruning,
                                        solver=asp_solver, debug=debug, context=ContextClass())
        context.add_child('constrain', Constrain.context)

        return context, (Generate, Test, Constrain)
