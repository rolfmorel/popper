#script (python)
import clingo
def pybind(c1,c2, vars1, vars2, vars3):
    vars1 = [x.string for x in vars1.arguments]
    vars2 = [x.string for x in vars2.arguments]
    vars3 = [x.string for x in vars3.arguments]
    key = f'{c1}_{c2}_' + '_'.join(vars1) + '_'
    def rename(k):
        new_k = key + k
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

def newclause(c1,c2):
    return f'{c1}_{c2}'
    # return int(c1.number)
#end.

comp_literal(C1,C2,P,A):-
    C1 < C2,
    body_literal(C1,P,A,_),
    head_literal(C2,P,A,_).

unfolded_body(C1,P,A,C1Vars):-
    body_literal(C1,P,A,C1Vars),
    not comp_literal(C1,_,P,A).

unfolded_body(@newclause(C1,C2),Q,A,@pybind(C1,C2,C1Vars,C2HeadVars,C2BodyVars)):-
    C2 > C1,
    body_literal(C1,P,A,C1Vars),
    head_literal(C2,P,A,C2HeadVars),
    unfolded_body(C2,Q,A,C2BodyVars).

redundant(C):-
    comp_literal(_,C,_,_).

hlit(C,P,A,Vs):-
    head_literal(C,P,A,Vs),
    not redundant(C).

blit(C,P,A,Vs):-
    unfolded_body(C,P,A,Vs),
    not redundant(C).

%% c1 = f(A) :- x1(A), inv1(A), inv2(A).
%% c2 = inv1(A) :- x1(A).
%% c3 = inv1(A) :- x2(A).
%% c4 = inv1(A) :- x3(A).
%% c5 = inv2(A) :- x1(A).
%% c6 = inv2(A) :- x4(A).

%% =>
%% c1_c2_c5 = f(A) :- x1(A), x1(A), x1(A).
%% c1_c2_c6 = f(A) :- x1(A), x1(A), x4(A).
%% c1_c3_c5 = f(A) :- x1(A), x2(A), x1(A).
%% c1_c3_c6 = f(A) :- x1(A), x2(A), x4(A).
%% c1_c4_c5 = f(A) :- x1(A), x3(A), x1(A).
%% c1_c4_c6 = f(A) :- x1(A), x3(A), x1(A).

head_literal(0,f,1,("0",)).
body_literal(0,x1,1,("0",)).
body_literal(0,inv1,1,("0",)).
body_literal(0,inv2,1,("0",)).

head_literal(1,inv1,1,("0",)).
body_literal(1,x1,1,("0",)).

head_literal(2,inv1,1,("0",)).
body_literal(2,x2,1,("0",)).

head_literal(3,inv1,1,("0",)).
body_literal(3,x3,1,("0",)).

head_literal(4,inv2,1,("0",)).
body_literal(4,x2,1,("0",)).

head_literal(5,inv2,1,("0",)).
body_literal(5,x4,1,("0",)).

%% #show hlit/4.
%% #show blit/4.
#show unfolded_body/4.