import sys
import json

from .input import parse_args
from .setup import setup
from .loop import timed_loop
from .representation import program_to_code


def main():
    args = parse_args()

    program, context = run(args.MODES_FILE, args.BK_FILE, args.EXAMPLES_FILE,
                           args.max_literals, args.eval_timeout, args.ground_constraints,
                           args.no_pruning, timeout=args.timeout, debug=args.debug, stats=args.stats)

    if args.stats:
        info = context.as_dict()
        info["programs_tested"] = context.num_programs_generated
        print(json.dumps(info, indent=2), file=sys.stderr)

    return program, context


def run(mode_file, bk_file, examples_file, max_literals, eval_timeout,
        ground_constraints, no_pruning, timeout, debug=False, stats=False):
    context, (Generate, Test, Constrain) = \
            setup(mode_file, bk_file, examples_file, max_literals, eval_timeout,
                  ground_constraints, no_pruning, debug=debug, stats=stats)

    program, context = timed_loop(context, Generate, Test, Constrain,
                                  timeout=timeout, debug=debug)

    return program, context


def run_experiment(*args, **kwargs):
    program, context = run(*args, **kwargs)

    if program: program = program_to_code(program)

    return program, context
