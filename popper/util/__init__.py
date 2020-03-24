from enum import Enum

from .time_keeping import TimeAccumulatingContext
from .paths import working_directory


class Result(Enum):
    SUCCESS = True
    FAILURE = False

SUCCESS = Result.SUCCESS
FAILURE = Result.FAILURE
