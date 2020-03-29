#!/usr/bin/env python

from popper import main
from popper.representation import program_to_code

program, context = main()
if program:
    for clause in program_to_code(program):
        print(clause)
else:
    print(program)
