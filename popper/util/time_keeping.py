from timeit import default_timer as timer


class TimeAccumulatingContext(object):
    def __init__(self):
        self._accumulated = 0
        self._sub_accs = {}
        self.mean = 0
        self.times_entered = 0
        self._times = []

    def sub_accumulator(self, name):
        sub_acc = __class__()
        setattr(self, name, sub_acc)
        self._sub_accs[name] = sub_acc
        return sub_acc

    def __enter__(self):
        self.entered = timer()
        self.times_entered += 1
        return self
    enter = __enter__

    def __exit__(self, type, value, traceback):
        self.exited = timer()
        diff = self.exited - self.entered
        n = self.times_entered
        self.mean = self.mean * (n - 1) / n + diff / n
        self._accumulated += diff
        self._times.append(diff)
    def exit(self):
        self.__exit__(None, None, None)
        return self.exited - self.entered

    @property
    def accumulated(self):
        return self._accumulated + sum(acc.accumulated for acc in self._sub_accs.values())

    def as_dict(self):
        d = { k: v.as_dict() for k, v in self._sub_accs.items() }
        d['_total'] = self.accumulated
        d['_mean'] = self.mean
        d['_means'] = self.means
        d['_times_entered'] = self.times_entered
        return d

    @property
    def means(self, n=20):
        def chunks(lst, n):
            """Yield successive n-sized chunks from lst."""
            for i in range(0, len(lst), n):
                yield lst[i:i + n]
        return list(map(lambda x: sum(x) / len(x), chunks(self._times, len(self._times) // n + 1)))

