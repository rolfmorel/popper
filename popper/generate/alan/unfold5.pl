%% #include "unfold.pl".


%% f(A,B) :- right(A,C),inv1(C,B).
%% inv1(A,C) :- right(A,C), inv1(C,B).
%% =>
%% f(A,B) :- right(A,C),inv1(C,B).
%% inv1(X,Y) :- right(X,Z), inv1(Z,Y).
%% =>

%% f(A,B) :- right(A,C),right(X,Z),inv1(Z,Y). WRONG!

%% f(A,B) :- right(A,C),right(C,Z),inv1(Z,B). RIGHT!


%% f(A,B) :- inv1(A,C),right(C,B).
%% inv1(A,B) :- right(A,C),right(C,B).
%% =>
%% f(C1_A,C1_B) :- inv1(C1_A,C1_C),right(C1_C,C1_B).
%% inv1(C2_A,C2_B) :- right(C2_A,C2_C),right(C2_C,C2_B).

#script (python)
import clingo
def pybind(vars1, vars2, vars3):
    print(vars1)
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


    # return
#end.

%% #script (python)
%% import clingo
%% def pybind(C1Vars,C2Vars,C2BodyVars):
%%     # print(C1Vars,type(C1Vars),dir(C1Vars))
%%     print(C1Vars.arguments)
%%     [(0,0), (0,1)]
%%     return C2BodyVars
%% #end.

%% vars(Vs):-
%%     head_literal(_,_,_,Vs).
%% vars(Vs):-
%%     body_literal(_,_,_,Vs).

%% bind(A,B,C,@pybind(A,B,C)):-
%%     body_literal(_,_,_,A),
%%     head_literal(_,_,_,B),
%%     body_literal(_,_,_,C).

comp_literal(C1,C2,P,A):-
    C1 < C2,
    body_literal(C1,P,A,_),
    head_literal(C2,P,A,_).

unfolded_body(C1,P,A,C1Vars):-
    body_literal(C1,P,A,C1Vars),
    not comp_literal(C1,_,P,A).

unfolded_body(C1,Q,A,@pybind(C1Vars,C2HeadVars,C2BodyVars)):-
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

%% lit(C,P,A,Vs):-
%%     hlit(C,P,A,Vs).
%% lit(C,P,A,Vs):-
%%     blit(C,P,A,Vs).


head_literal(0,f,2,((0,0),(0,1))).
body_literal(0,inv1,2,((0,0),(0,2))).
body_literal(0,right,2,((0,2),(0,1))).

head_literal(1,inv1,2,((1,0),(1,1))).
body_literal(1,right,2,((1,0),(1,2))).
body_literal(1,right,2,((1,2),(1,1))).

%% head_literal(2,inv2,1,(0,)).
%% body_literal(2,d,1,(0,)).
%% body_literal(2,e,1,(0,)).

#show hlit/4.
#show blit/4.
%% #show bind/4.







%% bind((0_0,0_1), (1_0,1_1), (1_0,1_2), (0_0,1_2))

%% modeh(a,2).
%% modeb(@make_atom(1..3),2).

%% bind((0_0,0_1), (1_0,1_1), (1_0,1_2), (0_0,1_2), @py_bind())

%% return f'atom_{p}'
%% return p.number*2
%% return f'inv{x}'