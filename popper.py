#!/usr/bin/env python

from popper import main


program, context = main()
if program:
    for clause in program.to_code():
        print(clause)
else:
    print(program)
