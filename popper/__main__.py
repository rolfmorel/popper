from .entry_point import main
from . import representation

program, context = main()
if program:
    for clause in representation.program_to_prolog(program):
        print(clause, flush=True)
else:
    print(None)
