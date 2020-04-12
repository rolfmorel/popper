import clingo


class SetupMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('setup')
        super().__init__(*args, **kwargs)

    def reset(self):
        with self.context.setup:
            self.clingo_ctl = clingo.Control()

    def setup(self, bk_file=None):
        with self.context.setup:
            if bk_file != None:
                self.__bk_code = open(bk_file).read()

            self.clingo_ctl.add("bk", [], self.__bk_code)

            self.clingo_ctl.ground([("bk", [])])
