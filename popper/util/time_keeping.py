from functools import reduce
from collections import OrderedDict
from timeit import default_timer as timer


#def timed_method(decorated_func, context_names):
#    def wrapped(self, *args, **kwargs):
#        accumulator = None
#        for name in context_names:
#            accumulator = self.context._sub_accs[name]
#        return decorated_func(self, *args, **kwargs)
#    return wrapped

class DummyTimeAccumulatingContext(object):
    def __init__(self, parent=None):
        self.parent = parent
        self.children = {}
        self.items = {}

        self.times_entered = 0
        self._times = []

        self.currently_entered = 0
        self.entered = self.exited = 0

    @property
    def running(self):
        return self.currently_entered != 0

    def add_child(self, name, instance=None):
        child = instance if instance else type(self)(parent=self)
        child.parent = self
        self.children[name] = child
        return child

    def __setitem__(self, key, value):
        self.items[key] = value

    def __getitem__(self, key):
        return self.items[key]

    def __getattr__(self, name):
        return self.children[name]

    def __enter__(self):
        return self
    def enter(self): return self.__enter__()

    def __exit__(self, type, value, traceback): pass
    def exit(self):
        self.__exit__(None, None, None)
        return self.exited - self.entered

    @property
    def accumulated(self):
        return sum(self._times)

    def as_dict(self):
        d = OrderedDict()
        d['_total'] = self.accumulated
        if not self.children:
            d['_mean'] = self.mean
            d['_means'] = self.means 
        d['_times_entered'] = self.times_entered
        d.update(self.items)
        d.update({ k: v.as_dict() for k, v in self.children.items()})
        return d

    @property
    def mean(self):
        return sum(self._times) / len(self._times) if self._times else 0

    @property
    def means(self, n=10):
        def chunks(lst, n):
            """Yield successive n-sized chunks from lst."""
            for i in range(0, len(lst), n):
                yield lst[i:i + n]
        return list(map(lambda x: sum(x) / len(x), chunks(self._times, len(self._times) // n + 1)))


class TimeAccumulatingContext(DummyTimeAccumulatingContext):
    def __init__(self, parent=None):
        super().__init__(parent)
        self._entered_parent = False

    def __enter__(self):
        if self.parent and self.parent.currently_entered == 0:
            self.parent.enter()
            assert self.parent.currently_entered == 1
            self._entered_parent = True

        self.currently_entered += 1
        assert self.currently_entered == 1

        self.times_entered += 1
        self.entered = timer()
        return self

    def __exit__(self, type, value, traceback):
        self.currently_entered -= 1
        assert self.currently_entered == 0, str(id(self))
        self.exited = timer()

        if self._entered_parent:
            self.parent.exit()
            self._entered_parent = False

        diff = self.exited - self.entered
        self._times.append(diff)
