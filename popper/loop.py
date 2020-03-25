import threading
try:
    import thread
except ImportError:
    import _thread as thread

from functools import partial

from clingo import Function

from . import generate
from . import test
from .test import evaluate
from . import constrain
from .util import SUCCESS, FAILURE


def set_program_size(clingo, size):
    clingo.release_external(Function("num_literals", [size-1]))
    # NB: might well attempt to reground the old parts of base as well
    # TODO: To be guaranteed to be efficient should carefully construct "shells" that depend on the program size
    clingo.ground([("program_size", [size])])
    clingo.assign_external(Function("num_literals", [size]), True)


def impose_constraint(clingo, clingo_context, constraint_generator, program, constraint_name):
    constr = constraint_generator(program)
    with clingo_context.adding:
        clingo.add(constraint_name, [], constr)
    return [(constraint_name, [])]


def timed_loop(*args, timeout=None, **kwargs):
    if timeout:
        timer = threading.Timer(timeout, lambda: thread.interrupt_main())
        timer.start()

    program, context = loop(*args, **kwargs)

    if timeout:
        timer.cancel()

    return program, context


def loop(main_context, examples,
         clingo, prolog,
         max_literals, ground_constraints,
         no_pruning=False):
    main_context.enter()
    prolog_context, clingo_context = main_context.prolog, main_context.clingo
    try:
        pos_examples, neg_examples = examples

        elim_constr = constrain.elimination_constraint
        spec_constr = lambda prog: constrain.specialization_constraint(prog, ground_constraints)
        gene_constr = constrain.generalization_constraint
        impose_constraint_ = partial(impose_constraint, clingo, clingo_context)

        main_context.programs_tested = 0
        for size in range(1, max_literals + 1):
            with clingo_context.sizing:
                set_program_size(clingo, size)
            
            while True:
                constraints = []

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

                    # evaluate the examples with the hypothesis loaded, using the meta-interpreter
                    with prolog_context.example_eval:
                        failing_body_gens = []
                        failing_clause_sets = []

                        for pos_example in pos_examples:
                            result, subprogram = test.evaluate.meta_interpret(prolog, program, pos_example)
                            if result == FAILURE:
                                failing_body_gens += [subprogram]

                        for neg_example in neg_examples:
                            result, subprogram = test.evaluate.meta_interpret(prolog, program, neg_example)
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
                            constraints  += impose_constraint_(spec_constr, failing_prog,
                                    f"specialization_constraint{prog_id}_{fail_idx}")
                        for fail_idx, failing_prog in enumerate(failing_clause_sets):
                            constraints += impose_constraint_(gene_constr, failing_prog,
                                    f"generalization_constraint{prog_id}_{fail_idx}")

                # with the solver handle released, ground the just derived constraints
                with clingo_context.grounding.constraints:
                    clingo.ground(constraints)
        return None, main_context
    except KeyboardInterrupt: # Also happens when timer interrupt happens
        return False, main_context
    finally:
        main_context.exit()
        # NB: Hack to deal with inner context entered while outer context was still on
        main_context._accumulated -= (prolog_context.accumulated + clingo_context.accumulated) 
