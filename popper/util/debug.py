from sys import stderr
from time import time


def debug_print(*args, prefix="", debug=False, timestamp=True, **kwargs):
    if debug:
        time_ = f"({time():.4f}) " if timestamp else ""
        print(f"{time_}{prefix}:", *args, file=stderr, **kwargs)


class DebugMixin(object):
    def __init__(self, *args, debug=False, **kwargs):
        self.debug = debug
        print(args, kwargs)
        super().__init__(*args, **kwargs)

    def DBG_PRINT(self, *args, timestamp=True, **kwargs):
        prefix = self.__class__.__name__.upper()
        debug_print(*args, prefix=prefix, debug=self.debug, timestamp=timestamp, **kwargs)
