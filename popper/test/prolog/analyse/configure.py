import os
from popper.representation import program_to_ordered_program, clause_to_code


def arguments_to_prolog(arguments):
    args = []
    for arg in arguments:
        if type(arg) == int:
            args.append(chr(ord('A') + arg))
        else:
            args.append(arg)
    return args


def atom_to_prolog(atom):
    pred = atom.predicate
    args = ','.join(arguments_to_prolog(atom.arguments))
    return f"{pred}({args})"


def instrumented_literal(atom, in_='CurLits', out='SeenLits'):
    pred = atom.predicate
    args = ','.join(arguments_to_prolog(atom.arguments))
    return f"{pred}({args},{in_},{out})"


def clause_to_instrumented_prolog(clause, head_preds):
    cl_id, head, body = clause

    def CurLits(idx=1): 
        yield f"CurLits{idx}"
        yield from CurLits(idx+1)

    head_code = instrumented_literal(head)
    body_codes = []

    cur_lits, lit_gen = 'CurLits', CurLits()

    for lit_id, body_lit in enumerate(body, start=1):
        if body_lit.predicate in head_preds:
            # add the calling literal's id to CurLits
            next_lits = next(lit_gen)
            body_codes.append(f"put_assoc(({cl_id},{lit_id}),{cur_lits},unit,{next_lits})")
            prev_lits = cur_lits
            cur_lits = next_lits

            # make the instrumented call
            next_lits = next(lit_gen)
            lit_code = instrumented_literal(body_lit, in_=cur_lits, out=next_lits)
            prev_lits = cur_lits
            cur_lits = next_lits
            
            body_codes.append(lit_code) # assumes that this call always resolves with another program clause
        else:
            lit_code = atom_to_prolog(body_lit)
            code = f"({lit_code}*->" \
                    "true;" \
                   f"put_assoc(({cl_id},{lit_id}),{cur_lits},unit,AllLits)," \
                    "assoc_to_keys(AllLits,FailLits)," \
                    "ffi_share(FailLits)," \
                    "fail)"
            body_codes.append(code)
    body_codes.append(f"put_assoc(({cl_id},{len(body)}),{cur_lits},unit,SeenLits)")

    return f"{head_code} :- " + ",\n".join(body_codes)


def program_to_instrumented_prolog(program):
    head_preds = [head.predicate for _, head, _ in program]

    return [clause_to_instrumented_prolog(clause, head_preds)
            for clause in program]


class ConfigureMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('configure')
        self.context.configure.add_child('assert_')
        self.context.configure.assert_.add_child('instrumented')
        self.context.configure.add_child('retract')
        super().__init__(*args, **kwargs)


    def assert_program(self, program, basic=None):
        if basic is True:
            with self.context.configure.assert_:
                for clause in program:
                    self.prolog.assertz(clause_to_code(clause))
        else:
            with self.context.configure.assert_.instrumented:
                clauses = program_to_instrumented_prolog(program)
                for clause in clauses:
                    self.prolog.assertz(clause)


    def retract(self):
        with self.context.configure.retract:
            args = ','.join(['_'] * (self.modeh.arity))
            self.prolog.retractall(f"{self.modeh.predicate}({args})")
            # 2 extra args in instrumented programs
            args = ','.join(['_'] * (self.modeh.arity + 2))  
            self.prolog.retractall(f"{self.modeh.predicate}({args})")

