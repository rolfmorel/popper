from .main import main
from .test.util import program_to_prolog

program, context = main()
if program:
    for clause in program_to_prolog(program):
        print(clause)
else:
    print(None)
