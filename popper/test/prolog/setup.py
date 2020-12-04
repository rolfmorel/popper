class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def setup(self, bk_file):
        with self.context.setup:
            self.prolog.assertz(f":- dynamic {self.modeh.predicate}/{self.modeh.arity}")

            self.prolog.assertz("popper_non_functional(Atom1):- \
                                Atom1 =..[P,A,B],Atom2 =..[P,A,C], \
                                call(Atom2),C \= B")

            if bk_file:
                self.prolog.consult(bk_file)
