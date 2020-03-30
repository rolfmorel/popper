import traceback
import threading
try:
    import thread
except ImportError:
    import _thread as thread

from sys import stderr

from .representation import program_to_code, is_recursive_clause
from .util import Result, Outcome


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
    program = None
    context.num_programs_generated = 0
    try:
        for size in range(1, Generate.max_literals + 1):
            Generate.set_program_size(size)

            while True:
                if debug: print(f"START GENERATING ({context.num_programs_generated})", file=stderr)

                program = Generate.get_program()
                if program == None:
                    break  # No model could be found. Can try with more allowed literals

                context.num_programs_generated += 1

                if debug:
                    print("DONE GENERATING", file=stderr)
                    DBG_output_program(program)
                    print("START TESTING", file=stderr)

                Test.retract_program_clauses()
                Test.assert_program(program)

                entailed_pos_exs = list(filter(lambda res: res == Result.Success,
                        map(lambda ex: Test.evaluate(ex), Test.pos_examples)))
                if len(entailed_pos_exs) == len(Test.pos_examples):
                    positive_outcome = Outcome.All
                elif entailed_pos_exs  == []:
                    positive_outcome = Outcome.None_
                else:
                    positive_outcome = Outcome.Some

                entailed_neg_exs = list(filter(lambda res: res == Result.Success,
                        map(lambda ex: Test.evaluate(ex), Test.neg_examples)))
                if entailed_neg_exs == []:
                    negative_outcome = Outcome.None_
                else:
                    negative_outcome = Outcome.Some

                # Special case for non-recursive clauses to determine whether they are useful or not
                constraints = []
                non_recursive_clauses = list(filter(lambda cl: not is_recursive_clause(cl), program))
                for nr_clause in non_recursive_clauses:
                    Test.retract_program_clauses()
                    Test.assert_program([nr_clause])

                    entailed_pos_exs = list(filter(lambda res: res == Result.Success,
                        map(lambda ex: Test.evaluate(ex), Test.pos_examples)))

                    if entailed_pos_exs == []:
                        constraints += [Constrain.elimination_constraint([nr_clause])]
                # END OF HACKS!!!

                if debug: print(f"DONE TESTING {positive_outcome.value, negative_outcome.value}", file=stderr)

                if positive_outcome == Outcome.All and negative_outcome == Outcome.None_:
                    # program both complete and consistent
                    return program, context

                if debug: print("START IMPOSING CONSTRAINTS", file=stderr)

                constraints += Constrain.derive_constraints(program,
                                                           positive_outcome, negative_outcome)
                if Constrain.no_pruning:
                    constraints = [Constrain.banish_constraint(program)]

                name_constraint_pairs = []
                for idx, constraint in enumerate(constraints):
                    name = f"program{context.num_programs_generated}_constraint{idx}"
                    name_constraint_pairs.append((name, constraint))
                    if debug:
                        print("CONSTRAINT:\n  " + constraint, file=stderr)

                Generate.impose_constraints(name_constraint_pairs)

                if debug: print("DONE IMPOSING CONSTRAINTS", file=stderr)
        return None, context
    except KeyboardInterrupt: # Also happens when timer interrupt happens
        return False, context
    except Exception as ex:
        DBG_output_program(program)
        raise ex
    finally:
        context.exit()
