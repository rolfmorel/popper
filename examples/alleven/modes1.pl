max_vars(7).
max_body(7).
max_clauses(2).

modeh(f,1).
type(f,0,list).
direction(f,0,in).
modeb(f,1).

modeb(tail,2).
type(tail,0,list).
type(tail,1,list).
direction(tail,0,in).
direction(tail,1,out).

modeb(head,2).
type(head,0,list).
type(head,1,element).
direction(head,0,in).
direction(head,1,out).

%% works without this
modeb(last,2).
type(last,0,list).
type(last,1,element).
direction(last,0,in).
direction(last,1,out).

modeb(length,2).
type(length,0,list).
type(length,1,int).
direction(length,0,in).
direction(length,1,out).

modeb(dummy1,2).
type(dummy1,0,element).
type(dummy1,1,t1).
direction(dummy1,0,in).
direction(dummy1,1,out).

modeb(dummy1m,1).
type(dummy1m,0,t1).
direction(dummy1m,0,in).

modeb(dummy2,2).
type(dummy2,0,element).
type(dummy2,1,t2).
direction(dummy2,0,in).
direction(dummy2,1,out).

modeb(dummy2m,1).
type(dummy2m,0,t2).
direction(dummy2m,0,in).

modeb(dummy3,2).
type(dummy3,0,element).
type(dummy3,1,t3).
direction(dummy3,0,in).
direction(dummy3,1,out).

modeb(dummy3m,1).
type(dummy3m,0,t3).
direction(dummy3m,0,in).

modeb(dummy4,2).
type(dummy4,0,element).
type(dummy4,1,t4).
direction(dummy4,0,in).
direction(dummy4,1,out).

modeb(sum,3).
type(sum,0,int).
type(sum,1,int).
type(sum,2,int).
direction(sum,0,in).
direction(sum,1,in).
direction(sum,2,out).

modeb(cons,3).
type(cons,0,element).
type(cons,1,list).
type(cons,2,list).
direction(cons,0,in).
direction(cons,1,in).
direction(cons,2,out).

modeb(empty,1).
type(empty,0,list).
direction(empty,0,in).

modeb(zero,1).
type(zero,0,element).
direction(zero,0,in).

modeb(even,1).
type(even,0,element).
direction(even,0,in).