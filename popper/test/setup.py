class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def setup(self, bk_file):
        with self.context.setup:
            self.prolog.assertz(f":- dynamic {self.modeh.predicate}/{self.modeh.arity}")

            if bk_file:
                self.prolog.consult(bk_file)

            #self.prolog.assertz(":- dynamic failing_literal/4")
            #self.prolog.assertz(":- dynamic successful_clause/1")

