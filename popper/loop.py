import time

from sys import stderr
from functools import partial
from itertools import chain
from collections import defaultdict

from .representation import program_to_ordered_program, clause_to_code, program_to_code, is_recursive_clause
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

    confusion_matrices = defaultdict(lambda : {
        'TP': 0, # true positives
        'FN': 0, # false negatives
        'TN': 0, # true negatives
        'FP': 0, # false positives
    })

    with Test.using(program): 
        # test the positive examples and collect subprograms with missing answers 
        for pos_ex in Test.pos_examples:
            result, _, failure_progs = Test.evaluate(program, pos_ex)
            conf_matrix = confusion_matrices[program]
            if result:
                conf_matrix['TP'] += 1
            else:
                conf_matrix['FN'] += 1
                if result is not None: # if example evaluation did not time out
                    # any subprogram that had a failed trace will be retested, except those that timed out
                    missing_answer_progs = missing_answer_progs.union(failure_progs)
            if Test.minimal_testing and conf_matrix['TP'] and conf_matrix['FN']:
                break # program incomplete and can no longer be totally incomplete

        # test the negative examples and collect subprograms with incorrect answers 
        for neg_ex in Test.neg_examples:
            result, success_progs, _ = Test.evaluate(program, neg_ex)
            conf_matrix = confusion_matrices[program]
            if not result:
                conf_matrix['TN'] += 1
            else: # example was erroneously entailed
                conf_matrix['FP'] += 1
                # any subprogram that had a successful trace will be retested
                incorrect_answer_progs = incorrect_answer_progs.union(success_progs)
                if Test.minimal_testing:
                    break # testing more negative examples won't change program already being inconsistent

        context['num_programs_tested'] += 1

    #TODO: filter non-useful subprograms, e.g. singular recursive clauses

    #TODO: use subsumption lattice to reduce number of tests needed
    # test the subprograms with missing answers whether they also have incorrect answers
    for subprog in filter(lambda p: p != program, missing_answer_progs | incorrect_answer_progs):
        context['num_programs_tested'] += 1
        conf_matrix = confusion_matrices[subprog]
        with Test.using(subprog, basic=True): # guaranteed to *not* make use of an instrumented program
            for pos_ex in Test.pos_examples:
                result, _ = Test.query(pos_ex)
                if result: conf_matrix['TP'] += 1
                else: conf_matrix['FN'] += 1
                if conf_matrix['TP'] and conf_matrix['FN']:
                    break # program incomplete and can no longer be totally incomplete
            for neg_ex in Test.neg_examples:
                result, _ = Test.query(neg_ex)
                if result: 
                    conf_matrix['FP'] += 1
                    break # testing more negative examples won't change program already being inconsistent
                else: conf_matrix['TN'] += 1

    prog_outcomes = dict()
    num_pos, num_neg = len(Test.pos_examples), len(Test.neg_examples)
    for subprog in chain((program,), 
                         (prog for prog in confusion_matrices.keys() if prog != program)):
        conf_matrix = confusion_matrices[subprog]

        if conf_matrix['TP'] == num_pos:   positive_outcome = Outcome.All
        elif conf_matrix['FN'] == num_pos: positive_outcome = Outcome.None_
        else:                              positive_outcome = Outcome.Some

        if conf_matrix['FP'] == num_neg:   negative_outcome = Outcome.All
        elif conf_matrix['TN'] == num_neg: negative_outcome = Outcome.None_
        else:                              negative_outcome = Outcome.Some

        if debug:
            if subprog != program:
                DBG_PRINT("SUBPROGRAM:")
                output_program(subprog)
            approx_pos = '+' if conf_matrix['TP'] + conf_matrix['FN'] < num_pos else ''
            approx_neg = '+' if conf_matrix['TN'] + conf_matrix['FP'] < num_neg else ''
            DBG_PRINT("TP: {}{}, FN: {}{}, TN: {}{}, FP: {}{}".format(
                      conf_matrix['TP'], approx_pos, conf_matrix['FN'], approx_pos, 
                      conf_matrix['TN'], approx_neg, conf_matrix['FP'], approx_neg, 
                      ))

        prog_outcomes[program] = (positive_outcome, negative_outcome)

    return prog_outcomes


def constrain(context, Constrain, debug, prog_outcomes):
    DBG_PRINT = partial(debug_print, prefix=None, debug=debug)

    inclusion_rules = []
    constraints = []

    for prog, (positive_outcome, negative_outcome) in prog_outcomes.items():
        for type_, name, rule in Constrain.derive(prog,
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

    #TODO: check if constraints are already loaded into solver, not just inclusion_rules
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
                    prog_outcomes = test(context, Test, debug, program)

                if prog_outcomes[program] == (Outcome.All, Outcome.None_): # all positives, no negatives
                    # program both complete and consistent ...
                    with Test.using(program, basic=True):
                        program_str = '[' + ','.join(f"({clause_to_code(cl)})" for cl in program) + ']'
                        query_str = "current_predicate(popper_program_validation/1) -> "\
                                    f"popper_program_validation({program_str}) ; " \
                                    "true"
                        if next(Test.prolog.query(query_str), None) != None:
                            # ... and validated as well
                            return program, context

                # TODO: keep track of already pruned programs so not to reimpose these constraints
                # TODO: further improvement: use subsumption lattice to determine effectiveness of adding a constrain

                with Constrain.context:
                    constrain(context, Constrain, debug, prog_outcomes)
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
