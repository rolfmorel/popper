from .entry_point import main
from .representation import program_to_code

program, context = main()
if program:
    for clause in program_to_code(program):
        print(clause, flush=True)
else:
    print(None)
