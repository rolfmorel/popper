import os

from ..util import working_directory


class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def setup(self, mode_file=None):
        with self.context.setup:
            file_dir = os.path.dirname(os.path.realpath(__file__))

            with working_directory(file_dir + "/alan"), \
                    open("alan.pl") as handle:
                self.solver.add(handle.read(), name='alan')

            if mode_file:
                with open(mode_file) as handle:
                    self.solver.add(handle.read(), name='modes_file')

            self.solver.add("""\
%%% External atom for number of literals in the program %%%%%
#external size(n).
:-
    size(n),
    #sum{K+1,Clause : clause_size(Clause,K)} != n.
""", name='program_size', arguments=['n'])

            fragments = ['alan'] + (['modes_file'] if mode_file else [])
            self.solver.ground(*fragments)
