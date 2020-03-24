from popper.main import main
from popper.test.util import program_to_prolog

program, context = main()
if program:
    for clause in program_to_prolog(program):
        print(clause)
else:
    print(program)
