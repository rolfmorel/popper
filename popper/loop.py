import traceback
import threading
try:
    import thread
except ImportError:
    import _thread as thread

from sys import stderr
from functools import partial
from itertools import chain
from collections import defaultdict

from .representation import program_to_ordered_program, program_to_code, is_recursive_clause
from .util import Result, Outcome
from .util.debug import debug_print


def DBG_output_program(program):
    for clause in program_to_code(program):
         print("  " + clause, file=stderr)


def timed_loop(*args, timeout=None, **kwargs):
    if timeout:
        timer = threading.Timer(timeout, lambda: thread.interrupt_main())
        timer.start()

    try:
        ret = loop(*args, **kwargs)
    finally:
        if timeout:
            timer.cancel()

    return ret

def test(context, Test, debug, program):
    DBG_PRINT = partial(debug_print, prefix='LOOP-test', debug=debug)

    subprog_missing_answers = defaultdict(int)
    subprog_incorrect_answers = defaultdict(int)

    with Test.using(program):
        # test the positive examples and collect subprograms with missing answers and how many are missing
        for pos_ex in Test.pos_examples:
            result, subprogs = Test.evaluate(program, pos_ex)
            if not result: # failed to prove example
                for subprog in filter(lambda sp: sp != program, subprogs):
                    subprog_missing_answers[subprog] += 1
                subprog_missing_answers[program] += 1

        # test the negative examples and collect subprograms with incorrect answers and how many are incorrect
        for neg_ex in Test.neg_examples:
            result, subprogs = Test.evaluate(program, neg_ex)
            if result: # managed to prove example
                for subprog in filter(lambda sp: sp != program, subprogs):
                    subprog_incorrect_answers[subprog] += 1
                subprog_incorrect_answers[program] += 1
        context.num_programs_tested += 1

    incorrect_subprogs = list(subprog_incorrect_answers.keys())
    # test the subprograms with missing answers whether they also have incorrect answers
    for subprog in filter(lambda p: p is not program, subprog_missing_answers.keys()):
        context.num_programs_tested += 1
        with Test.using(subprog, basic=True):
            for neg_ex in Test.neg_examples:
                result, _ = Test.query(neg_ex)
                if result: # managed to prove example
                    subprog_incorrect_answers[subprog] += 1
    # test the subprograms with incorrect answers whether they also have missing answers
    for subprog in filter(lambda p: p is not program, incorrect_subprogs):
        context.num_programs_tested += 1
        with Test.using(subprog, basic=True):
            for pos_ex in Test.pos_examples:
                result, _ = Test.query(pos_ex)
                if not result: # failed to prove example
                    subprog_missing_answers[subprog] += 1

    if debug:
        for subprog in subprog_missing_answers.keys() | subprog_incorrect_answers.keys():
            missing_answers = subprog_missing_answers[subprog]
            incorrect_answers = subprog_incorrect_answers[subprog]
            prefix = "PROG" if subprog == program else "(SUB)PROG"
            DBG_PRINT(f"{prefix} WITH {missing_answers} missing answers AND {incorrect_answers} incorrect answers:")
            DBG_output_program(subprog)

    # Special case for non-recursive clauses to determine whether they are useful or not
    if False and not Test.analyses and len(program) > 1:  # No point checking a single clause program again
        # BEGIN HACKS!!!
        for nr_clause in filter(lambda cl: not is_recursive_clause(cl), program):
            context.num_programs_tested += 1
            with Test.using((nr_clause,), basic=True):
                for pos_ex in Test.pos_examples:
                    result, _ = Test.evaluate(program, pos_ex)
                    if not result: # failed to prove example
                        subprog_missing_answers[(nr_clause,)] += 1
                        break
        # END OF HACKS!!!

    DBG_PRINT("DONE TESTING")
    return subprog_missing_answers, subprog_incorrect_answers


def constrain(context, Constrain, debug, subprog_missing_answers, subprog_incorrect_answers):
    DBG_PRINT = partial(debug_print, prefix='LOOP-constrain', debug=debug)

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

        constraints += Constrain.derive_constraints(subprog,
                                                    positive_outcome, negative_outcome)

    if Constrain.no_pruning:
        constraints = [Constrain.banish_constraint(program)]

    name_constraint_pairs = []
    for idx, constraint in enumerate(constraints):
        name = f"program{context.num_programs_generated}_constraint{idx}"
        name_constraint_pairs.append((name, constraint))
        DBG_PRINT("CONSTRAINT:\n  " + constraint)

    return name_constraint_pairs


def loop(context, Generate, Test, Constrain, debug=False):
    DBG_PRINT = partial(debug_print, prefix='LOOP', debug=debug)

    context.num_programs_generated = 0
    context.num_programs_tested = 0
    context.enter() # start to keep time

    program = None
    try:
        for size in range(1, Generate.max_literals + 1):
            Generate.set_program_size(size)
            context.largest_size = size

            while True:
                with Generate.context:
                    DBG_PRINT(f"START GENERATING (program {context.num_programs_generated + 1})")

                    unordered_program = Generate.get_program()
                    if unordered_program  == None:
                        DBG_PRINT(f"NO MORE PROGRAMS (with {size} literals)")
                        break  # No model could be found. Can try with more allowed literals

                    context.num_programs_generated += 1

                    DBG_PRINT(f"DONE GENERATING (program {context.num_programs_generated})")

                program = program_to_ordered_program(unordered_program)
                if debug:
                    print("PROGRAM:", file=stderr)
                    DBG_output_program(program)

                with Test.context:
                    DBG_PRINT("START TESTING")

                    subprog_missing_answers, subprog_incorrect_answers = \
                            test(context, Test, debug, program)

                    DBG_PRINT("DONE TESTING")

                if subprog_missing_answers[program] == 0 and subprog_incorrect_answers[program] == 0:
                    # program both complete and consistent
                    return program, context

                with Constrain.context:
                    DBG_PRINT("START IMPOSING CONSTRAINTS")

                    name_constraint_pairs = constrain(context, Constrain, debug,
                                                      subprog_missing_answers, subprog_incorrect_answers)

                # deindent to get out of Constrain's context, otherwise time will be counted double, by both Constrain and Generate
                Generate.impose_constraints(name_constraint_pairs)

                DBG_PRINT("DONE IMPOSING CONSTRAINTS")
        return None, context
    except KeyboardInterrupt: # Also happens on timer interrupt
        return False, context
    except Exception as ex:
        print("PROGRAM:", file=stderr)
        DBG_output_program(program)
        raise ex
    finally:
        context.exit()
