import math
import string
import numpy as np

# STRINGS
NUM_TRAIN = 10
NUM_TEST = 100
MAX_LEN_STRING = 10
MAX_LEN_SUBSTRING = 5
LOWER_CHARS = list(string.ascii_lowercase)
UPPER_CHARS = list(string.ascii_uppercase)
INTEGERS = list(range(1,10))
# visionS
MIN_VISION_SIZE = 3
MAX_VISION_SIZE = 6


def vision_until1():
    # UNTIL visionS 1
    # 000
    # 000
    # 000
    # =>
    # 111
    # 000
    # 000
    # vision1(A,B):-at_right(A),draw1(A,B).
    # vision1(A,B):-vision1_1(A,C),vision1(C,B).
    # vision1_1(A,B):-draw1(A,C),move_right(C,B).
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    for i in range(size):
        y[i] = 1
    return (x, y)

def vision_until2():
    # UNTIL visionS 2
    # 000
    # 000
    # 000
    # =>
    # 100
    # 100
    # 100
    # vision2(A,B):-vision2_1(A,C),vision2(C,B).
    # vision2_1(A,B):-draw1(A,C),move_down(C,B).
    # vision2(A,B):-at_bottom(A),draw1(A,B).
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    for i in range(len(x)):
        if i % size == 0:
            y[i] = 1
    return (x, y)

def vision_until3():
    # UNTIL visionS 3
    # 000
    # 000
    # 000
    # =>
    # 001
    # 000
    # 001
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    y[size-1] = 1
    y[-1] = 1
    return (x, y)

def vision_until4():
    # UNTIL visionS 4
    # 000
    # 000
    # 000
    # =>
    # 100
    # 100
    # 111
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    y[size-1] = 1
    y[-1] = 1

    for i in range(size*size):
        if i % size == 0:
            y[i] = 1
        if i > (size*size)-size:
            y[i] = 1
    return (x, y)

def vision_until5():
    # UNTIL visionS 5
    # 000
    # 000
    # 000
    # =>
    # 001
    # 000
    # 000
    # vision5(A,B):-move_right(A,C),vision5(C,B).
    # vision5(A,B):-at_right(A),draw1(A,B).
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    y[size-1] = 1
    return (x, y)


def vision_until6():
    # UNTIL visionS 6
    # 000
    # 000
    # 000
    # =>
    # 100
    # 010
    # 001
    # vision6(A,B):-at_right(A),draw1(A,B).
    # vision6(A,B):-vision6_1(A,C),vision6(C,B).
    # vision6_1(A,B):-draw1(A,C),vision6_2(C,B).
    # vision6_2(A,B):-move_right(A,C),move_down(C,B).
    # METAGOL TIMESOUT / INFINITE LOOP
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    y[0] = 1
    y[-1] = 1
    i = 0
    for _ in range(size-1):
        i += size+1
        y[i] = 1
    return (x, y)

def vision_map1():
    # MAP visionS 1
    # 000
    # 000
    # 000
    # =>
    # 111
    # 111
    # 111
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(np.random.choice([1], size*size))
    return (x, y)

def vision_map2():
    # MAP visionS 2
    # 010
    # 101
    # 010
    # =>
    # 101
    # 010
    # 101
    size = np.random.randint(MIN_VISION_SIZE, MAX_VISION_SIZE+1)
    x = list(np.random.choice([0,1], size*size))
    y = list(x)
    for i in range(len(y)):
        if y[i] == 1:
            y[i] = 0
        else:
            y[i] = 1
    return (x, y)

def gen_vision_data():
    NUM_EXAMPLES = 5
    probs = list(enumerate([vision_until1,vision_until2,vision_until3,vision_until4,vision_until5,vision_until6,vision_map1,vision_map2]))
    for i, prob in probs:
        i+=1
        with open(f'data/{i}.pl','w') as f:
            for _ in range(NUM_EXAMPLES):
                (x, y) = prob()
                size = int(math.sqrt(len(x)))
                x = f'w(1,1,{size},{size},{x})'
                y = f'w(_,_,{size},{size},{y})'
                line = f"pos(f({x},{y}))."
                f.write(line + '\n')
    # f(f'data/vision/train/{trial}.pl', NUM_TRAIN)
    # f(f'data/vision/test/{trial}.pl', NUM_TEST)

gen_vision_data()
