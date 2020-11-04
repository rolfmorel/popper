import time

from sys import stderr
from functools import partial
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
        context['num_programs_tested'] += 1

    incorrect_subprogs = list(subprog_incorrect_answers.keys())
    # test the subprograms with missing answers whether they also have incorrect answers
    for subprog in filter(lambda p: p is not program, subprog_missing_answers.keys()):
        context['num_programs_tested'] += 1
        with Test.using(subprog, basic=True):
            for neg_ex in Test.neg_examples:
                result, _ = Test.query(neg_ex)
                if result: # managed to prove example
                    subprog_incorrect_answers[subprog] += 1
    # test the subprograms with incorrect answers whether they also have missing answers
    for subprog in filter(lambda p: p is not program, incorrect_subprogs):
        context['num_programs_tested'] += 1
        with Test.using(subprog, basic=True):
            for pos_ex in Test.pos_examples:
                result, _ = Test.query(pos_ex)
                if not result: # failed to prove example
                    subprog_missing_answers[subprog] += 1

    if debug:
        num_pos = len(Test.pos_examples)
        num_neg = len(Test.neg_examples)
        for subprog in set((program,)) | subprog_missing_answers.keys() | subprog_incorrect_answers.keys():
            missing_answers = subprog_missing_answers[subprog]
            incorrect_answers = subprog_incorrect_answers[subprog]
            if subprog != program:
                DBG_PRINT("SUBPROGRAM:")
                output_program(subprog)
            DBG_PRINT("TP: {}, TN: {}, FN: {}, FP: {}".format(
                      num_pos - missing_answers, num_neg - incorrect_answers,
                      missing_answers, incorrect_answers))

    return subprog_missing_answers, subprog_incorrect_answers


def constrain(context, Constrain, debug, subprog_missing_answers, subprog_incorrect_answers):
    DBG_PRINT = partial(debug_print, prefix=None, debug=debug)

    rules = []

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

        rules += Constrain.derive_constraints(subprog,
                                              positive_outcome,
                                              negative_outcome)

    inclusion_rules = []
    constraints = []
    for type_, rule in rules:
        if isinstance(type_, ConstraintType):
            constraints.append((type_, rule))
        else:
            inclusion_rules.append(rule)
    inclusion_rules = '\n'.join(inclusion_rules)

    if debug:
        if inclusion_rules != "":
            DBG_PRINT("inclusion rules:\n" + inclusion_rules)
        for type_, constraint in constraints:
            DBG_PRINT(f"{type_.value} constraint:\n" + constraint)

    return [(f"program{context['num_programs_generated']}",
             inclusion_rules + '\n'.join(map(lambda c: c[1], constraints)))]


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
                    # program both complete and consistent ...
                    with Test.using(program, basic=True):
                        program_str = '[' + ','.join(f"({clause_to_code(cl)})" for cl in program) + ']'
                        query_str = "current_predicate(popper_program_validation/1) -> "\
                                    f"popper_program_validation({program_str}) ; " \
                                    "true"
                        if next(Test.prolog.query(query_str), None) == {}:
                            # ... and validated as well
                            return program, context

                with Constrain.context:
                    name_constraint_pairs = constrain(context, Constrain, debug,
                                                      subprog_missing_answers, subprog_incorrect_answers)

                # deindent to get out of Constrain's context, otherwise time will be counted double, by both Constrain and Generate
                Generate.impose_constraints(name_constraint_pairs)
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
