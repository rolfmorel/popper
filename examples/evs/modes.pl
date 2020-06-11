max_vars(5).
max_body(10).
max_clauses(2).

%% TARGET
%%evs(A) :- empty(A).
%%evs(A) :- head(A,B),even(B),tail(A,C),head(C,D),odd(D),tail(C,E),evs(E).


modeh(evs,1). 
direction(evs,0,in). 
modeb(empty,1). 
direction(empty,0,in). 
modeb(head,2). 
direction(head,0,in). 
direction(head,1,out). 
modeb(even,1). 
direction(even,0,in). 
modeb(odd,1). 
direction(odd,0,in). 
modeb(tail,2). 
direction(tail,0,in). 
direction(tail,1,out). 
modeb(evs,1). 
direction(evs,0,in). 

