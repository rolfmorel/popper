from ..representation import literal_to_code


def literal_to_asserting_prolog(lit):
    cl_id, lit_id, pred, args = lit
    args = ','.join(map(lambda arg: chr(ord('A') + arg) if type(arg) == int else arg, args))
    return f"({pred}({args}) *-> true ; assertz(fail_lit({cl_id},{lit_id},{pred},[{args}])),false)"


def program_to_asserting_prolog(program):
    prolog_program = []
    for clause in program:
        head_lit = literal_to_code(clause[0])
        body_lits = list(map(literal_to_asserting_prolog, clause[1:]))
        prolog_program.append(f"{head_lit} :- {','.join(body_lits)}.")
    return prolog_program
