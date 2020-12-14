class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def setup(self, bk_file):
        with self.context.setup:
            if bk_file:
                self.prolog.consult(bk_file)
