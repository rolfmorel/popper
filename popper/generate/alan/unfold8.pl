#script (python)
import clingo
def pybind(c1,c2, vars1, vars2, vars3):
    vars1 = vars1.arguments
    vars2 = vars2.arguments
    vars3 = vars3.arguments
    key = f'{c1}_{c2}_' + '_'.join(x.string for x in vars1) + '_'
    def rename(k):
        k = k.string
        new_k = key + k
        return new_k
    vars2 = [rename(x) for x in vars2]
    vars3 = [rename(x) for x in vars3]
    d = {v:vars1[i].string for i, v in enumerate(vars2)}
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
    unfolded_body(C2,Q,A,C2BodyVars).

redundant(C):-
    comp_literal(_,C,_,_).

hlit(C,P,A,Vs):-
    head_literal(C,P,A,Vs),
    not redundant(C).

blit(C,P,A,Vs):-
    unfolded_body(C,P,A,Vs),
    not redundant(C).

head_literal(0,f,2,("a","b")).
body_literal(0,inv1,2,("a","c")).
body_literal(0,right,2,("c","b")).

head_literal(1,inv1,2,("a","b")).
body_literal(1,inv2,2,("a","c")).
body_literal(1,right,2,("c","b")).

head_literal(2,inv2,2,("a","b")).
body_literal(2,right,2,("a","c")).
body_literal(2,right,2,("c","b")).

%% #show unfolded_body/4.

%% #show hlit/4.
%% #show blit/4.


lit(C,P,A,Vs):-
    hlit(C,P,A,Vs).
lit(C,P,A,Vs):-
    blit(C,P,A,Vs).

clause(C):-
    head_literal(C,_,_,_).

clause_size(C,N):-
    clause(C),
    #count{P,Vars : body_literal(C,P,_,Vars)} == N.

prog_size(N):-
    #sum{K+1,Clause : clause_size(Clause,K)} == N.

unfolded_size(N):-
    #count{C,P,Vars : lit(C,P,_,Vars)} == N.

#show prog_size/1.
#show unfolded_size/1.