def convert_literal(lit):
    clause_key, literal_key, predicate, args = lit
    args = map(lambda arg: f"V{arg}" if type(arg) == int else arg, args)
    return f"({clause_key}, {literal_key}, {predicate}, [{','.join(args)}])"


def program_to_metaint_repr(program):
    metaint_program = []
    for clause in program:
        lits = map(convert_literal, clause)
        metaint_program.append(f"program([{','.join(lits)}])")
    return metaint_program


def literal_to_prolog(lit):
    _, _, predicate, args = lit
    args = map(lambda arg: chr(ord('A') + arg) if type(arg) == int else arg, args)
    return f"{predicate}({','.join(args)})"


def program_to_prolog(program):
    prolog_program = []
    for clause in program:
        lits = list(map(literal_to_prolog, clause))
        prolog_program.append(f"{lits[0]} :- {','.join(lits[1:])}.")
    return prolog_program
