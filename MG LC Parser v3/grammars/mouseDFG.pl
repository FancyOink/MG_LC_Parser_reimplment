startCategory(c).

[finds] :: ([=d,=d,v],[A,B,Out] >> (Out = find(B,A))).
[carry] :: ([=d,=d,v],[A,B,Out] >> (Out = carry(A,B))).
[eats] :: ([=d, =d, v], [A,B,Out] >> (Out = eat(B,A))).
[does] :: ([=v,+wh,c], [A,B,Out] >> (Out = A B))). % hier müsste man yall selber implementieren

[mouse] :: ([n],mouse).
[cheese] :: ([n],cheese).
[what] :: ([d,-wh],question).
[apple] :: ([d],apple).


[the] :: ([=n,d],[A,Out] >> (Out = A)).
%[the] :: ([=n,n,-g],[A,Out] >> (Out = A)).
[an] :: ([=d,d],[A,Out] >> (Out = A)).

[] ::  ([=v, c],[A,Out] >> (Out = A)).

