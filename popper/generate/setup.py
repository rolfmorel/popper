import os

from ..util import working_directory


class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def setup(self, mode_file=None):
        with self.context.setup:
            file_dir = os.path.dirname(os.path.realpath(__file__))

            #self.clingo_ctl.register_observer(Observer())

            if mode_file:
                with open(mode_file) as handle:
                    self.clingo_ctl.add("mode_file", [],
                                        f"%%%%% MODE FILE: {mode_file} %%%%%\n" +
                                        handle.read())

            with working_directory(file_dir + "/alan"), open("alan.pl") as handle:
                self.clingo_ctl.add("alan", [], handle.read())

            self.clingo_ctl.add("program_size", ['n'], """
%%%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
    size(n),
    #sum{K+1,Clause : clause_size(Clause,K)} != n.
""")

            asp_programs = [("alan", [])] + ([('mode_file',[])] if mode_file else [])
            self.clingo_ctl.ground(asp_programs)
