%direction(tail,0,in)
%direction(tail,1,out)
%
%direction(length,0,in)
%direction(length,1,out)
%
%direction(append,0,in)
%direction(append,1,in)
%direction(append,2,out)
%
%% N.B.: the below is a second mode for append, not supported by this scheme
%%direction(append,0,out)
%%direction(append,1,out)
%%direction(append,2,in)
%% TODO: a possible solution is to just call this implementation append2. It does feel like you are losing something by doing this

:-
    direction(Pred,Pos,_),  % only check modes if they are provided for the predicate
    literal(Cl,Idx,Pred,Arity),
    Pos = 0..Arity-1,
    not argument_safe(Cl,Idx,Pos).

% head input arguments are safe by virtue of them being provided
argument_safe(Cl,0,Pos)
:-
    literal(Cl,0,Pred,_),
    var(Cl,0,Pos,Var),
    direction(Pred,Pos,in).

% head output arguments are safe when they occur as an output argument in the body
argument_safe(Cl,0,Pos)
:-
    literal(Cl,0,Pred,_),
    var(Cl,0,Pos,Var),
    direction(Pred,Pos,out),
    literal(Cl,Idx2,Pred2,_),
    Idx2 > 0,
    var(Cl,Idx2,Pos2,Var),
    direction(Pred2,Pos2,out).
% TODO: constraint that as soon as the left-most body literals have computed the head outputs, any further literals are useless.


% body input arguments are safe when they occur anywhere in the body to the left of this literal.
argument_safe(Cl,Idx,Pos)
:-
    literal(Cl,Idx,Pred,_),
    var(Cl,Idx,Pos,Var),
    direction(Pred,Pos,in),
    var(Cl,Idx2,Pos2,Var),
    0 < Idx2,
    Idx2 < Idx.
    
% body input arguments are safe when they occur as head input argument
argument_safe(Cl,Idx,Pos)
:-
    literal(Cl,Idx,Pred,_),
    var(Cl,Idx,Pos,Var),
    direction(Pred,Pos,in),
    Idx > 0,
    literal(Cl,0,Pred2,_),
    var(Cl,0,Pos2,Var),
    direction(Pred2,Pos2,in).

% body output arguments are always safe
argument_safe(Cl,Idx,Pos)
:-
    literal(Cl,Idx,Pred,_),
    Idx > 0,
    var(Cl,Idx,Pos,Var),
    direction(Pred,Pos,out).
