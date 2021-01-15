#script (python)
import clingo
def pybind(c1,c2, vars1, vars2, vars3):
    vars1 = [str(x) for x in vars1.arguments]
    vars2 = [str(x) for x in vars2.arguments]
    vars3 = [str(x) for x in vars3.arguments]
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
#end.

disjunctive_invention:-
    invented(P,A),
    multiclause(P,A).

should_unfold:-
    has_pi,
    not recursive,
    not disjunctive_invention.

comp_literal(C1,C2,P,A):-
    %% should_unfold,
    C1 < C2,
    body_literal(C1,P,A,_),
    head_literal(C2,P,A,_).

unfolded_body(C1,P,A,C1Vars):-
    %% should_unfold,
    body_literal(C1,P,A,C1Vars),
    not comp_literal(C1,_,P,A).

unfolded_body(C1,Q,A,@pybind(C1,C2,C1Vars,C2HeadVars,C2BodyVars)):-
    %% should_unfold,
    C2 > C1,
    body_literal(C1,P,A,C1Vars),
    head_literal(C2,P,A,C2HeadVars),
    unfolded_body(C2,Q,A,C2BodyVars).

redundant(C):-
    %% should_unfold,
    comp_literal(_,C,_,_).

hlit(C,P,A,Vs):-
    %% should_unfold,
    head_literal(C,P,A,Vs),
    not redundant(C).

blit(C,P,A,Vs):-
    %% should_unfold,
    unfolded_body(C,P,A,Vs),
    not redundant(C).

lit(C,P,A,Vs):-
    hlit(C,P,A,Vs).
lit(C,P,A,Vs):-
    blit(C,P,A,Vs).

%% max_size(I*J):-
%%     max_body(I),
%%     max_clauses(J).

%% prog_size(Size):-
%%     #sum{K+1,Clause : clause_size(Clause,K)} == Size,
%%     max_size(MaxSize),
%%     Size <= MaxSize.

unfolded_size(N):-
    #count{C,P,Vars : lit(C,P,_,Vars)} == N,
    N < 30.

:-
    should_unfold,
    prog_size(N1),
    unfolded_size(N2),
    N1 >= N2.


#show prog_size/1.
%% #show unfolded_size/1.

