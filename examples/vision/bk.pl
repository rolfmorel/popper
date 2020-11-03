%% BACKGROUND KNOWLEDGE
at_top(w(_X,1,_Width,_Height,_Grid)).

at_bottom(w(_X,Height,_Width,Height,_Grid)).

at_left(w(1,_Y,_Width,_Height,_Grid)).

at_right(w(Width,_Y,Width,_Height,_Grid)).

pos(X,Y,Width,Pos):-
    Pos is X + (Width * (Y-1)).

draw1(w(X,Y,Width,Height,Grid1),w(X,Y,Width,Height,Grid2)):-
    pos(X,Y,Width,Pos),
    replace_at(Pos,1,Grid1,Grid2).

draw0(w(X,Y,Width,Height,Grid1),w(X,Y,Width,Height,Grid2)):-
    pos(X,Y,Width,Pos),
    replace_at(Pos,0,Grid1,Grid2).

move_right(w(X1,Y,Width,Height,Grid),w(X2,Y,Width,Height,Grid)):-
    X1 < Width,
    X2 is X1+1.

move_left(w(X1,Y,Width,Height,Grid),w(X2,Y,Width,Height,Grid)):-
    X1 > 1,
    X2 is X1-1.

move_down(w(X,Y1,Width,Height,Grid),w(X,Y2,Width,Height,Grid)):-
    Y1 < Height,
    Y2 is Y1+1.

move_up(w(X,Y1,Width,Height,Grid),w(X,Y2,Width,Height,Grid)):-
    Y1 > 1,
    Y2 is Y1-1.

replace_at(Pos, New, L1, L2):-
    findall(X, (nth1(Index,L1,Old), (Index == Pos -> X=New ; X=Old)), L2).