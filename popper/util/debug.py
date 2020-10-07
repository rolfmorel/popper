from functools import partial

from sys import stderr
from time import time


_start_time = time()

def debug_print(*args, prefix="", debug=False, **kwargs):
    if debug:
        time_ = f"({time()-_start_time:.1f})" 
        prefix = "" if prefix is None else ' ' + prefix + ':'
        print(f"{time_}{prefix}", *args, file=stderr, **kwargs)


class DebugMixin(object):
    def __init__(self, *args, debug=False, **kwargs):
        self.debug = debug
        super().__init__(*args, **kwargs)

    def DBG_PRINT(self, *args, **kwargs):
        prefix = self.__class__.__name__.upper()
        debug_print(*args, prefix=prefix, debug=self.debug, **kwargs)
