from functools import reduce
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

        self.times_entered = 0
        self._times = []

        self.currently_entered = 0
        self.entered = self.exited = 0
        self.paused = False

    @property
    def running(self):
        return self.currently_entered != 0

    def add_child(self, name, instance=None):
        child = instance if instance else __class__(parent=self)
        if instance: instance.parent = self
        # TODO: replace this with attribute lookup override, which would check .children
        setattr(self, name, child)
        self.children[name] = child
        return child

    def pause(self): pass
    def unpause(self): pass

    def __enter__(self): return self
    enter = __enter__

    def __exit__(self, type, value, traceback): pass
    def exit(self):
        self.__exit__(None, None, None)
        return self.exited - self.entered

    @property
    def accumulated(self):
        return sum(self._times) + sum(child.accumulated for child in self.children.values())

    def as_dict(self):
        d = { k: v.as_dict() for k, v in self.children.items() }
        d['_total'] = self.accumulated
        if not self.children:
            d['_mean'] = self.mean
            d['_means'] = self.means 
        d['_times_entered'] = self.times_entered
        return d

    @property
    def mean(self):
        # FIXME: due to pausing this is no longer accurate! Fine for leaf contexts though
        return sum(self._times) / len(self._times) if self._times else 0

    @property
    def means(self, n=20):
        # FIXME: due to pausing this is no longer accurate! Fine for leaf contexts though
        def chunks(lst, n):
            """Yield successive n-sized chunks from lst."""
            for i in range(0, len(lst), n):
                yield lst[i:i + n]
        return list(map(lambda x: sum(x) / len(x), chunks(self._times, len(self._times) // n + 1)))


class TimeAccumulatingContext(DummyTimeAccumulatingContext):
    def __init__(self, parent=None):
        super().__init__(parent)

    def pause(self):
        if self.running and not self.paused:
            self.exit()
            if self.parent: self.parent.pause()
            self.paused = True

    def unpause(self):
        any_running_children = reduce(lambda acc, child: child.running or acc,
                                      self.children.values(), False)
        if self.paused and not any_running_children:
            self.paused = False
            self.enter()
            self.times_entered -= 1

    def __enter__(self):
        self.currently_entered += 1
        assert self.currently_entered == 1
        self.times_entered += 1
        if self.parent: self.parent.pause()
        self.entered = timer()
        return self

    def __exit__(self, type, value, traceback):
        self.currently_entered -= 1
        assert self.currently_entered == 0
        self.exited = timer()
        if self.parent: self.parent.unpause()

        diff = self.exited - self.entered
        self._times.append(diff)
