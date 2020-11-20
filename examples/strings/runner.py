from collections import defaultdict

TASKS = list(range(1, 10))

def load_probs():
    dic = defaultdict(list)
    with open('probs.txt', 'r') as f:
        for line in f:
            xs = line.strip().split('=>')
            # get problem id
            if len(xs) == 1 and len(xs[0]) > 0:
                # %p2 <- extracts the 2
                problem = xs[0][2:]
            # get input (x) and output (y) strings
            elif len(xs) > 1:
                x = list(xs[0].strip())
                y = list(xs[1].strip())
                dic[problem].append((x, y))
    return dic

def gen_data():
    dic = load_probs()
    # print(dic.keys())
    for task in TASKS:
        examples = [f"pos(f(s({x},{y}),s(_,[])))." for (x, y) in dic[str(task)]]
        # print(examples)
        with open(f'exs/{task}.pl','w') as f:
            f.write('\n'.join(examples))

gen_data()