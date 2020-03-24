import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Popper, a generate-test-and-constrain ILP system")

    parser.add_argument("EXAMPLES_FILE", help="Ground examples for the testing stage")
    parser.add_argument("MODES_FILE", help="ASP mode declerations and constraints for the generate stage")
    parser.add_argument("BK_FILE", help="Prolog definitions for back ground knowledge predicates")

    parser.add_argument("--no-constraints", default=False, action='store_true', help="Only generate elimination constraints (i.e. no pruning of specializations/generalizations)")
    parser.add_argument("--ground-constraints", default=False, action='store_true', help="Generate only ground specialization constraints")
    parser.add_argument("--timeout", type=float, default=600, help="timeout that needs to be enforced")
    parser.add_argument("-n","--max-literals", type=int, default=5, help="maximum number of literals allowed")

    parser.add_argument("--verbose", default=False, action='store_true', help="Upon return print statistics")

    return parser.parse_args()


def retrieve_examples(filename):
    pos_exs, neg_exs = [], []
    for line in open(filename).readlines():
        if line.startswith("pos(") and line.endswith(").\n"):
            pos_exs += [(line[4:-3])]
        elif line.startswith("neg(") and line.endswith(").\n"):
            neg_exs += [line[4:-3]]
        else:
            print(f"WARNING: ignored unrecognized example: {line}", file=sys.stderr)
    return pos_exs, neg_exs
