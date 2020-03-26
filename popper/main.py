import sys
import argparse
import json

from .input import parse_args
from .setup import setup
from .loop import timed_loop
from .test.util import program_to_prolog


def main():
    args = parse_args()

    return run(args)


def run2(mode_file, bk_file, examples_file, max_literals, ground_constraints, no_pruning, timeout, debug=False):
    main_context, examples, (clingo, prolog) = setup(mode_file, bk_file, examples_file)

    program, context = timed_loop(main_context, examples, clingo, prolog, max_literals, ground_constraints, no_pruning, timeout=timeout, debug=debug)
    if program:
        program = program_to_prolog(program)
    return program, context


def run(args):
    main_context, examples, (clingo, prolog) = setup(args.MODES_FILE, args.BK_FILE, args.EXAMPLES_FILE)

    program, context = timed_loop(main_context, examples,
                                  clingo, prolog,
                                  args.max_literals, args.ground_spec_constraints,
                                  args.no_pruning, timeout=args.timeout, debug=args.debug)

    if args.stats:
        info = context.as_dict()
        info["programs_tested"] = context.programs_tested
        print(json.dumps(info, indent=2), file=sys.stderr)

    return program, context
