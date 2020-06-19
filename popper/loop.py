import traceback
import threading
try:
    import thread
except ImportError:
    import _thread as thread

from sys import stderr
from functools import partial

from .representation import program_to_ordered_program, program_to_code, is_recursive_clause
from .util import Result, Outcome
from .util.debug import debug_print


def DBG_output_program(program):
    print("PROGRAM:", file=stderr)
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


def loop(context, Generate, Test, Constrain, debug=False):
    context.enter()
    DBG_PRINT = partial(debug_print, prefix='LOOP', debug=debug)
    program = None
    context.num_programs_generated = 0
    try:
        for size in range(1, Generate.max_literals + 1):
            Generate.set_program_size(size)

            while True:
                DBG_PRINT(f"START GENERATING (program {context.num_programs_generated + 1})")

                program = Generate.get_program()
                if program == None:
                    DBG_PRINT(f"NO MORE PROGRAMS (with {size} literals)")
                    break  # No model could be found. Can try with more allowed literals

                context.num_programs_generated += 1

                DBG_PRINT(f"DONE GENERATING (program {context.num_programs_generated})")
                ordered_program = program_to_ordered_program(program)
                if debug: DBG_output_program(ordered_program)
                DBG_PRINT("START TESTING")

                # FIXME: nicer to do a contextmanager here for Test's evaluation
                Test.retract_program_clauses()
                Test.assert_ordered_program(ordered_program)

                subprog_missing_answers = defaultdict(int)
                subprog_incorrect_answers = defaultdict(int)

                for pos_ex in Test.pos_examples:
                    result, subprogs = Test.evaluate(ex)
                    if not result: # failed to prove example
                        for subprog in filter(lambda sp: sp != program, subprogs):
                            subprog_missing_answers[subprog] += 1
                        subprog_missing_answers[program] += 1

                for neg_ex in Test.neg_examples:
                    result, subprogs = Test.evaluate(ex)
                    if result: # managed to prove example
                        incorrect_answers += 1
                        for subprog in filter(lambda sp: sp != program, subprogs):
                            subprog_incorrect_answers[subprog] += 1
                        subprog_incorrect_answers[program] += 1

                # Special case for non-recursive clauses to determine whether they are useful or not
                if not Test.analyses and len(ordered_program) > 1:  # No point checking a single clause program again
                    # BEGIN HACKS!!!
                    for nr_clause in filter(lambda cl: not is_recursive_clause(cl),
                                            ordered_program):
                        Test.retract_program_clauses()
                        Test.assert_ordered_program((nr_clause,))

                        for pos_ex in Test.pos_examples:
                            result, subprogs = Test.evaluate(ex)
                            if not result: # failed to prove example
                                subprog_missing_answers[(nr_clause,)] += 1
                                break
                    # END OF HACKS!!!

                DBG_PRINT(f"DONE TESTING {subprog_missing_answers[program]}, {subprog_incorrect_answers[program]}")

                if subprog_missing_answers[program] == 0 and subprog_incorrect_answers[program] == 0:
                    # program both complete and consistent
                    return ordered_program, context

                DBG_PRINT("START IMPOSING CONSTRAINTS")
                constraints = []

                for subprog in subprog_missing_answers.keys().union(subprog_incorrect_answers.keys()):
                    missing_answers = subprog_missing_answers[subprog]
                    incorrect_answers = subprog_incorrect_answers[subprog]

                    if missing_answers == 0:
                        positive_outcome = Outcome.All
                    elif missing_answers == len(Test.pos_examples):
                        positive_outcome = Outcome.None_
                    else:
                        positive_outcome = Outcome.Some

                    if incorrect_answers == 0:
                        negative_outcome = Outcome.None_
                    elif incorrect_answers == len(Test.neg_examples):
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

                Generate.impose_constraints(name_constraint_pairs)

                DBG_PRINT("DONE IMPOSING CONSTRAINTS")
        return None, context
    except KeyboardInterrupt: # Also happens when timer interrupt happens
        return False, context
    except Exception as ex:
        DBG_output_program(program)
        raise ex
    finally:
        context.exit()
