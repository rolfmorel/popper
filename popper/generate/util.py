from collections import defaultdict

PRINT_ATOMS = []


def model_to_program(model):
    literals = defaultdict(dict)
    variables = defaultdict(lambda: defaultdict(dict))
    for atom in model:
        if atom.name in PRINT_ATOMS:
            print(atom)
        if atom.name == "literal":
            clause = atom.arguments[0].number
            literal_index = atom.arguments[1].number
            literals[clause][literal_index] = atom.arguments[2].name
        if atom.name == "var":
            clause = atom.arguments[0].number
            literal_index = atom.arguments[1].number
            var_pos = atom.arguments[2].number
            var_id = atom.arguments[3].number
            variables[clause][literal_index][var_pos] = var_id
    program = []
    for clause_key in sorted(literals.keys()):
        clause_lits = literals[clause_key]
        clause = []
        for literal_key in sorted(clause_lits.keys()):
            literal_vars = variables[clause_key][literal_key]
            vars_ = []
            for variable_key in sorted(literal_vars.keys()):
                vars_.append(literal_vars[variable_key])
            literal = (clause_key, literal_key,
                       literals[clause_key][literal_key], vars_)
            clause.append(literal)
        program.append(clause)
    return program
