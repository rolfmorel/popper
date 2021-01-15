def pybind(vars1, vars2, vars3):
    vars1 = vars1.arguments
    vars2 = vars2.arguments
    vars3 = vars3.arguments

    d = {}
    for i, v in enumerate(vars2):
        d[v] = vars1[i]

    out = []
    for v in vars3:
        if v in d:
            out.append(d[v])
        else:
            out.append(v)

    return tuple(out)
    # print(list(zip(vars1, vars2)))

    # return
#end.


pybind([(0,1), (0,2)], [(1,0), (1,1)], [(1,0), (1,2)])


# %% f(C1_A,C1_B) :- inv1(C1_A,C1_C),right(C1_C,C1_B).
# %% inv1(C2_A,C2_B) :- right(C2_A,C2_C),right(C2_C,C2_B).