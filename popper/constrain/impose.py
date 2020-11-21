class ImposeMixin(object):
    def __init__(self, *args, **kwargs):
        self.context.add_child('impose')
        self.context.impose.add_child('adding')
        self.context.impose.add_child('grounding')
        super().__init__(*args, **kwargs)


    def impose(self, named_constraints):
        with self.context.impose:
            names = []
            for name, constraint in named_constraints:
                if name not in self.solver.added:
                    with self.context.impose.adding:
                        self.solver.add(constraint, name=name)
                    names.append(name)
            with self.context.impose.grounding:
                self.solver.ground(*names)
