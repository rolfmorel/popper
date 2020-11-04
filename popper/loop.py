import time

from sys import stderr
from functools import partial
from itertools import chain
from collections import defaultdict

from .representation import program_to_ordered_program, program_to_code, is_recursive_clause
from .util import Result, Outcome
from .util.debug import debug_print
from .constrain.data_types import ConstraintType


def output_program(program):
    for clause in program_to_code(program):
         print("  " + clause, file=stderr)


def test(context, Test, debug, program):
    DBG_PRINT = partial(debug_print, prefix=None, debug=debug)

    missing_answer_progs = set()
    incorrect_answer_progs = set()

    prog_missing_answers = defaultdict(int)
    prog_incorrect_answers = defaultdict(int)

    with Test.using(program):  # TODO: create a handle which abstracts the simple vs non-simple interface
        # test the positive examples and collect subprograms with missing answers 
        for pos_ex in Test.pos_examples:
            result, _, failure_progs = Test.evaluate(program, pos_ex)
            if not result:
                prog_missing_answers[program] += 1
                if result is not None: # if example evaluation did not time out
                    # any subprogram that had a failed trace will be retested, except those that timed out
                    missing_answer_progs = missing_answer_progs.union(failure_progs)

        # test the negative examples and collect subprograms with incorrect answers 
        for neg_ex in Test.neg_examples:
            result, success_progs, _ = Test.evaluate(program, neg_ex)
            if result:
                prog_incorrect_answers[program] += 1
                # any subprogram that had a successful trace will be retested
                incorrect_answer_progs = incorrect_answer_progs.union(success_progs)
            #if result is not None: # if example evaluation timed out
            #    incorrect_answer_progs = incorrect_answer_progs.union(success_progs)

        context['num_programs_tested'] += 1

    #TODO: use subsumption lattice to reduce number of tests needed
    # test the subprograms with missing answers whether they also have incorrect answers
    for subprog in filter(lambda p: p != program, missing_answer_progs | incorrect_answer_progs):
        context['num_programs_tested'] += 1
        with Test.using(subprog, basic=True): # guaranteed to *not* make use of an instrumented program
            for pos_ex in Test.pos_examples:
                result, _ = Test.query(pos_ex)
                if not result: # failed to prove example
                    prog_missing_answers[subprog] += 1
            for neg_ex in Test.neg_examples:
                result, _ = Test.query(neg_ex)
                if result: # managed to prove example
                    prog_incorrect_answers[subprog] += 1

    if debug:
        num_pos = len(Test.pos_examples)
        num_neg = len(Test.neg_examples)
        for subprog in chain((program,),
                (prog_missing_answers.keys() |
                 prog_incorrect_answers.keys()).difference(set((program,)))):
            missing_answers = prog_missing_answers[subprog]
            incorrect_answers = prog_incorrect_answers[subprog]
            if subprog != program:
                DBG_PRINT("SUBPROGRAM:")
                output_program(subprog)
            DBG_PRINT("TP: {}, TN: {}, FN: {}, FP: {}".format(
                      num_pos - missing_answers, num_neg - incorrect_answers,
                      missing_answers, incorrect_answers))

    return prog_missing_answers, prog_incorrect_answers


def constrain(context, Constrain, debug, subprog_missing_answers, subprog_incorrect_answers):
    DBG_PRINT = partial(debug_print, prefix=None, debug=debug)

    inclusion_rules = []
    constraints = []

    for subprog in subprog_missing_answers.keys() | subprog_incorrect_answers.keys():
        missing_answers = subprog_missing_answers[subprog]
        incorrect_answers = subprog_incorrect_answers[subprog]

        if missing_answers == 0:
            positive_outcome = Outcome.All
        elif missing_answers == Constrain.num_pos_examples:
            positive_outcome = Outcome.None_
        else:
            positive_outcome = Outcome.Some

        if incorrect_answers == 0:
            negative_outcome = Outcome.None_
        elif incorrect_answers == Constrain.num_neg_examples:
            negative_outcome = Outcome.All
        else:
            negative_outcome = Outcome.Some

        for type_, name, rule in Constrain.derive(subprog,
                                                  positive_outcome,
                                                  negative_outcome):
            if isinstance(type_, ConstraintType):
                constraints.append((type_, name, rule))
            else:
                inclusion_rules.append((type_, name, rule))

    if debug:
        if inclusion_rules != []:
            incl_rules = "\n".join(rule for _, _, rule in inclusion_rules)
            DBG_PRINT("inclusion rules:\n" + incl_rules)
        for type_, _, constraint in constraints:
            DBG_PRINT(f"{type_.value} constraint:\n" + constraint)

    Constrain.impose(
        ((name, rule) for _, name, rule in chain(inclusion_rules, constraints))
    )


def loop(context, Generate, Test, Constrain, debug=False):
    DBG_PRINT = partial(debug_print, prefix=None, debug=debug)

    context['num_programs_generated'] = 0
    context['num_programs_tested'] = 0
    context.enter() # start to keep time

    program = None
    try:
        for size in range(1, Generate.max_literals + 1):
            Generate.set_program_size(size)
            context['largest_size'] = size

            while True:
                with Generate.context:
                    unordered_program = Generate.get_program()
                    if unordered_program  == None:
                        DBG_PRINT(f"NO MORE PROGRAMS (with {size} literals)")
                        break  # No model could be found. Can try with more allowed literals

                    context['num_programs_generated'] += 1

                program = program_to_ordered_program(unordered_program)
                if debug:
                    DBG_PRINT(f"program {context['num_programs_generated']}:")
                    output_program(program)

                with Test.context:
                    subprog_missing_answers, subprog_incorrect_answers = \
                            test(context, Test, debug, program)

                if subprog_missing_answers[program] == 0 and subprog_incorrect_answers[program] == 0:
                    # program both complete and consistent
                    return program, context

                # TODO: keep track of already pruned programs so not to reimpose these constraints
                # TODO: further improvement: use subsumption lattice to determine effectiveness of adding a constrain

                with Constrain.context:
                    constrain(context, Constrain, debug,
                              subprog_missing_answers, subprog_incorrect_answers)
        return None, context
    except KeyboardInterrupt: # Also happens on timer interrupt
        context['interrupted'] = True
        return False, context
    except Exception as ex:
        print("PROGRAM:", file=stderr)
        output_program(program)
        raise ex
    finally:
        context.exit()
