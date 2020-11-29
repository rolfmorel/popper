from .entry_point import main

program, context = main()

if program:
    for clause in program.to_code():
        print(clause, flush=True)
else:
    print(None)
