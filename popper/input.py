import re
import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Popper, a generate-test-and-constrain ILP system", formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("EXAMPLES_FILE", help="Ground examples for the testing stage")
    parser.add_argument("MODES_FILE", help="ASP mode declarations and constraints for the generate stage")
    parser.add_argument("BK_FILE", help="Prolog definitions for background knowledge predicates")

    parser.add_argument("--no-pruning", default=False, action='store_true', help="Only generate elimination constraints (i.e. no pruning of specializations/generalizations)")
    parser.add_argument("--ground-constraints", default=False, action='store_true', help="Generate ground constraints")
    parser.add_argument("--timeout", type=float, default=600, help="Timeout that needs to be enforced (in seconds)")
    parser.add_argument("--eval-timeout", type=float, default=1, help="Prolog evaluation timeout (in seconds)")
    parser.add_argument("-n","--max-literals", type=int, default=10, help="Maximum number of literals allowed in program")

    parser.add_argument("--stats", default=False, action='store_true', help="Upon return, print statistics")
    parser.add_argument("--debug", default=False, action='store_true', help="Print debugging information to stderr")

    return parser.parse_args()


def parse_examples(filename):
    code = open(filename).read()
    pos_exs = re.findall("pos\((.*)\)\.", code)
    neg_exs = re.findall("neg\((.*)\)\.", code)
    return pos_exs, neg_exs
