#!/usr/bin/env python

import sys


import popper.constrain
import popper.generate

from popper.representation import program_to_code, program_to_ordered_program

modes_file = "examples/droplast/modes.pl" if len(sys.argv) < 2 else sys.argv[1]

Generate = popper.generate.Generate(modes_file)
Constrain = popper.constrain.Constrain(Generate.modeh)

Generate.set_program_size(5)
program = Generate.get_program()

print("Program")
print(program)
for clause in program_to_code(program_to_ordered_program(program)):
    print(clause)

def print_constraints(program, ground=False):
    Constrain.ground = ground
    print("example BANISH constraint: \n ",
          Constrain.banish_constraint(program), "\n\n")
    print("example SPECIALIZATION constraint:\n ",
          Constrain.specialization_constraint(program), "\n\n")
    print("example ELIMINATION constraint:\n ",
          Constrain.elimination_constraint(program), "\n\n")
    print("example GENERALIZATION constraint:\n ",
          Constrain.generalization_constraint(program), "\n\n")

print("GROUND CONSTRAINTS")
print_constraints(program, ground=True)

print("NON-GROUND CONSTRAINTS")
print_constraints(program, ground=False)
