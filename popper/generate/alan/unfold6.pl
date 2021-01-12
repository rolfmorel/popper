#script (python)
import clingo
def pybind(c2, q, vars1, vars2, vars3):
    vars1 = vars1.arguments
    vars2 = vars2.arguments
    vars3 = vars3.arguments
    print('---')
    print(c2)
    print(q)
    print(vars1)
    print(vars2)
    print(vars3)
    d = {v:vars1[i] for i, v in enumerate(vars2)}
    print(d)
    out = []
    for v in vars3:
        if v in d:
            out.append(d[v])
        else:
            out.append(f'c2_{v}')
    print('out',out)
    return tuple(out)
#end.

comp_literal(C1,C2,P,A):-
    C1 < C2,
    body_literal(C1,P,A,_),
    head_literal(C2,P,A,_).

unfolded_body(C1,P,A,C1Vars):-
    body_literal(C1,P,A,C1Vars),
    not comp_literal(C1,_,P,A).

unfolded_body(C1,Q,A,@pybind(C2,Q,C1Vars,C2HeadVars,C2BodyVars)):-
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

head_literal(0,f,2,(0,1)).
body_literal(0,inv1,2,(0,2)).
body_literal(0,inv1,2,(2,1)).

head_literal(1,inv1,2,(0,1)).
body_literal(1,a,2,(0,2)).
body_literal(1,b,2,(2,1)).

%% pybind(0,1,(0,2),(0,1),(0,2))
%% =>
%% (0,'1_2')

%% pybind(0,1,(0,2),(0,1),(2,1))
%% =>
%% ('1_2',2)


#show hlit/4.
#show blit/4.
