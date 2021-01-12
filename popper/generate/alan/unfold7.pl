#script (python)
import clingo
def pybind(c1,c2, vars1, vars2, vars3):
    vars1 = vars1.arguments
    vars2 = vars2.arguments
    vars3 = vars3.arguments
    key = f'{c1}_{c2}_' + '_'.join(str(x) for x in vars1) + '_'
    def rename(k):
        new_k = key + str(k)
        return new_k
    vars2 = [rename(x) for x in vars2]
    vars3 = [rename(x) for x in vars3]
    d = {v:vars1[i] for i, v in enumerate(vars2)}
    out = []
    for v in vars3:
        if v in d:
            out.append(d[v])
        else:
            out.append(v)
    return tuple(out)
#end.

comp_literal(C1,C2,P,A):-
    C1 < C2,
    body_literal(C1,P,A,_),
    head_literal(C2,P,A,_).

unfolded_body(C1,P,A,C1Vars):-
    body_literal(C1,P,A,C1Vars),
    not comp_literal(C1,_,P,A).

unfolded_body(C1,Q,A,@pybind(C1,C2,C1Vars,C2HeadVars,C2BodyVars)):-
    C2 > C1,
    body_literal(C1,P,A,C1Vars),
    head_literal(C2,P,A,C2HeadVars),
    body_literal(C2,Q,A,C2BodyVars).

redundant(C):-
    comp_literal(_,C,_,_).

hlit(C,P,A,Vs):-
    head_literal(C,P,A,Vs),
    not redundant(C).

blit(C,P,A,Vs):-
    unfolded_body(C,P,A,Vs),
    not redundant(C).

head_literal(0,f,2,(a,b)).
body_literal(0,inv1,2,(a,c)).
body_literal(0,inv1,2,(c,b)).

head_literal(1,inv1,2,(a,b)).
body_literal(1,down,2,(a,c)).
body_literal(1,right,2,(c,b)).



#show hlit/4.
#show blit/4.
