import os

import clingo
from pyswip import Prolog

from .input import retrieve_examples
from .util import working_directory, TimeAccumulatingContext


def setup_timing_contexts():
    main_context = TimeAccumulatingContext()

    prolog_context = main_context.sub_accumulator('prolog')
    prolog_context.sub_accumulator('init')
    prolog_context.sub_accumulator('misc')
    prolog_context.sub_accumulator('example_eval')

    clingo_context = main_context.sub_accumulator('clingo')
    clingo_context.sub_accumulator('adding')
    clingo_context.adding.sub_accumulator('init')
    clingo_context.sub_accumulator('grounding')
    clingo_context.grounding.sub_accumulator('init')
    clingo_context.grounding.sub_accumulator('constraints')
    clingo_context.sub_accumulator('solving')
    clingo_context.sub_accumulator('sizing')

    return main_context


def setup(mode_file, bk_file, examples_file):
    file_dir = os.path.dirname(os.path.realpath(__file__))

    prolog = Prolog()
    clingo_ctl = clingo.Control()

    main_context = setup_timing_contexts()
    with main_context:
        prolog_context, clingo_context = main_context.prolog, main_context.clingo

        ##clingo_ctl.register_observer(Observer())

        with prolog_context.init:
            prolog.consult(file_dir + "/test/meta-interpreter.pl")
            prolog.consult(bk_file)

        pos_exs, neg_exs = retrieve_examples(examples_file)
        
        asp_base_prog = "" 
        with open(mode_file) as handle:
            asp_base_prog += f"\n\n\n%%%%% {mode_file} %%%%%\n" + handle.read()
        with working_directory(file_dir + "/generate"), open("alan.pl") as handle:
            asp_base_prog += f"\n\n\n%%%%% alan.pl %%%%%\n" + handle.read()

            with clingo_context.adding.init:
                clingo_ctl.add("base", [], asp_base_prog)
                clingo_ctl.add("program_size", ['n'], """
%%%%% External atom for number of literals in the program %%%%%
%body_lit_index(1..n).
#external num_literals(n).
:- num_literals(n),
   n != #count { Clause,Lit : literal(Clause,Lit,_,_) }.
""")

        with clingo_context.grounding.init:
            clingo_ctl.ground([("base", [])])

    return main_context, (pos_exs, neg_exs), (clingo_ctl, prolog)
