import sys
import argparse
import json

from .input import parse_args
from .setup import setup
from .loop import timed_loop


def main():
    args = parse_args()

    return run(args)


def run(args):
    main_context, examples, (clingo, prolog) = setup(args.MODES_FILE, args.BK_FILE, args.EXAMPLES_FILE)

    program, context = timed_loop(main_context, examples,
                                  clingo, prolog,
                                  args.max_literals, args.ground_spec_constraints,
                                  args.no_pruning, timeout=args.timeout)

    if args.verbose:
        info = context.as_dict()
        info["programs_tested"] = context.programs_tested
        print(json.dumps(info, indent=2), file=sys.stderr)

    return program, context
