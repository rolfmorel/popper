import sys
import time
import json
import threading
try:
    import thread
except ImportError:
    import _thread as thread

from .input import parse_args
from .setup import setup
from .loop import loop
from .representation import program_to_code


def main():
    args = parse_args()

    kwargs = { 'timeout' : args.timeout,
               'debug' : args.debug,
               'stats' : args.stats }
    if args.analyse:
        kwargs['tester'] = 'prolog.analyse'

    program, context = run(args.MODES_FILE, args.BK_FILE, args.EXAMPLES_FILE,
                           args.max_literals, args.eval_timeout, args.ground_constraints,
                           args.no_pruning, **kwargs)

    if args.stats:
        info = context.as_dict()
        print(json.dumps(info, indent=2), file=sys.stderr, flush=True)

    return program, context


def run(mode_file, bk_file, examples_file, max_literals, eval_timeout,
        ground_constraints, no_pruning, timeout, debug=False, stats=False, tester='prolog'):
    time_entered = time.time()

    if timeout:
        timer = threading.Timer(timeout, lambda: thread.interrupt_main())
        timer.start()

    try:
        context, (Generate, Test, Constrain) = \
                setup(mode_file, bk_file, examples_file, max_literals, eval_timeout,
                      ground_constraints, no_pruning, debug=debug, stats=stats, tester=tester)

        program, context = loop(context, Generate, Test, Constrain, debug=debug)
    finally:
        if timeout:
            timer.cancel()

    return program, context


def run_experiment(*args, api_version='1', **kwargs):
    if api_version == '2':
        assert args == (), args
    program, context = run(*args, **kwargs)

    if program: program = program_to_code(program)

    return program, context
