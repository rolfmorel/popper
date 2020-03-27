import traceback
import threading
try:
    import thread
except ImportError:
    import _thread as thread

from sys import stderr
from functools import partial

from clingo import Function

from . import generate
from . import test
from .test import evaluate
from .test.util import program_to_prolog
from . import constrain
from .util import SUCCESS, FAILURE


def DBG_output_program(program):
    print("PROGRAM:", file=stderr)
    for clause in program_to_prolog(program):
        print("  " + clause, file=stderr)


def DBG_output_constraint(constraint_name, constraint):
    print(f"CONSTRAINT: {constraint_name}\n  {constraint}", file=stderr)


def set_program_size(clingo, size):
    clingo.release_external(Function("num_literals", [size-1]))
    # NB: might well attempt to reground the old parts of base as well
    # TODO: To be guaranteed to be efficient should carefully construct "shells" that depend on the program size
    clingo.ground([("program_size", [size])])
    clingo.assign_external(Function("num_literals", [size]), True)


def impose_constraint(clingo, clingo_context, debug, constraint_generator, program, constraint_name):
    constr = constraint_generator(program)
    if debug: DBG_output_constraint(constraint_name, constr)
    with clingo_context.adding:
        clingo.add(constraint_name, [], constr)
    return [(constraint_name, [])]


def timed_loop(*args, timeout=None, **kwargs):
    if timeout:
        timer = threading.Timer(timeout, lambda: thread.interrupt_main())
        timer.start()

    try:
        program, context = loop(*args, **kwargs)
    finally:
        if timeout:
            timer.cancel()

    return program, context


def loop(main_context, examples,
         clingo, prolog,
         max_literals, ground_constraints,
         no_pruning=False, debug=False):
    main_context.enter()
    prolog_context, clingo_context = main_context.prolog, main_context.clingo
    try:
        program = None
        pos_examples, neg_examples = examples

        elim_constr = constrain.elimination_constraint
        spec_constr = lambda prog: constrain.specialization_constraint(prog, ground_constraints)
        gene_constr = constrain.generalization_constraint
        impose_constraint_ = partial(impose_constraint, clingo, clingo_context, debug)

        main_context.programs_tested = 0
        for size in range(1, max_literals + 1):
            with clingo_context.sizing:
                set_program_size(clingo, size)

            while True:
                constraints = []

                if debug: print("START SOLVING", file=stderr)
                clingo_context.solving.enter()
                with clingo.solve(yield_=True) as handle:
                    clingo_context.solving.exit()
                    try:
                        model = next(handle)
                    except StopIteration:
                        break  # No model could be found. Can try with more allowed literals

                    # get rid of previously guessed clauses on the meta-interpreter side
                    with prolog_context.misc:
                        prolog.retractall("program(_)")

                    # extract program from answer set and add it to the meta-interpreter
                    program = generate.model_to_program(model.symbols(atoms=True))
                    prolog_program = test.program_to_metaint_repr(program)
                    with prolog_context.misc:
                        for clause in prolog_program:
                            prolog.assertz(clause)

                    if debug: DBG_output_program(program)

                    # evaluate the examples with the hypothesis loaded, using the meta-interpreter
                    if debug: print("START TESTING", file=stderr)
                    with prolog_context.example_eval:
                        failing_body_gens = []
                        failing_clause_sets = []

                        for pos_example in pos_examples:
                            result, subprogram = test.evaluate.meta_interpret(prolog, program, pos_example, debug)
                            if result == FAILURE:
                                failing_body_gens += [subprogram]

                        for neg_example in neg_examples:
                            result, subprogram = test.evaluate.meta_interpret(prolog, program, neg_example, debug)
                            if result == SUCCESS:
                                failing_clause_sets += [subprogram]

                    main_context.programs_tested += 1

                    if failing_body_gens == failing_clause_sets == []: # program both complete and consistent
                        return program, main_context

                    # derive the relevant constraints and add them (non-grounded) to the solver
                    prog_id = main_context.programs_tested
                    if no_pruning:
                        constraints += impose_constraint_(elim_constr, program,
                                f"elimination_constraint{prog_id}")
                    else:
                        for fail_idx, failing_prog in enumerate(failing_body_gens):
                            if debug: 
                                print("FAILING BODYGEN ", end='', file=stderr)
                                DBG_output_program(failing_prog)
                            constraints  += impose_constraint_(spec_constr, failing_prog,
                                    f"specialization_constraint{prog_id}_{fail_idx}")
                        for fail_idx, failing_prog in enumerate(failing_clause_sets):
                            if debug: 
                                print("FAILING CLAUSE SET ", end='', file=stderr)
                                DBG_output_program(failing_prog)
                            constraints += impose_constraint_(gene_constr, failing_prog,
                                    f"generalization_constraint{prog_id}_{fail_idx}")

                
                if debug: print("START GROUNDING", file=stderr)
                # with the solver handle released, ground the just derived constraints
                with clingo_context.grounding.constraints:
                    clingo.ground(constraints)
        return None, main_context
    except KeyboardInterrupt: # Also happens when timer interrupt happens
        return False, main_context
    except Exception as ex:
        DBG_output_program(program)
        raise ex
    finally:
        main_context.exit()
        # NB: Hack to deal with inner context entered while outer context was still on
        main_context._accumulated -= (prolog_context.accumulated + clingo_context.accumulated)
