startCategory(c).


[lie] :: ([=n,=n,c],[A,B,Out] >> (Out = lie(B,A))).
[eats] :: ([=n,=n,n],[A,B,Out] >> (Out = eat(B,A))).
[finds] :: ([=d,=n,+g,c],[A,B,Out] >> (Out = find(B,A))).

[sleeps] :: ([=n,c],[A,Out] >> (Out = sleep(A))).

[field] :: ([=f,n],[A,Out]>> (Out = field(A))).

[in] :: ([=n,=n,n],[A,B,Out]>>(Out = loc(B,A))).

[at] :: ([=n,n],[A,Out]>>(Out = loc(A))).
[hungry] :: ([=n,n],[A,Out] >>(Out = hungry(A))).

[mouse] :: ([n],mouse).
[water] :: ([n],water).
[cheese] :: ([n],cheese).

[home] :: ([n],home).


[] :: ([=c4, =c4, f], [A,B,Out] >> (Out = xBy(B,A))).
[] :: ([=c4, =let, f], [A,B,Out] >> (Out = bFa(B,A))).
[] :: ([=n, =c1, n],[A,B,Out] >> (Out = modify(B,A))).

['B'] :: ([let],b).

[ty] :: ([=c1, =ctee, c2],[A,B,Out] >> (Out = t10plus(B,A))). % A + 10 * B
[ty] :: ([=c1, +tee, c2],[A,Out] >> (Out = t10(A))). % A * 10
[teen] :: ([=c1, +tee, c2],[A,Out] >> (Out = plus10(A))). % A + 10
[hundredand] :: ([=c2, =c1, c3],[A,B,Out] >> (Out = t100plus(B,A))). % A + 100 * B
[hundred] :: ([=cnix, =c1, c3],[A,B,Out] >> (Out = t100(B,A))). % 100 * B + epsilon
[thousand] :: ([=c3, =c3, c4],[A,B,Out] >> (Out = t1000plus(B,A))). % A + 1000 * B
[thousand] :: ([=cnix, =c3, c4],[A,B,Out] >> (Out = t1000(B,A))). % 100 * B + epsilon

[one] :: ([c1],1). % 1
[two] :: ([c1],2). % 2
[three] :: ([c1],3). % 3
[four] :: ([c1, -tee],4). % 4
[five] :: ([c1],5). % 5
[six] :: ([c1, -tee],6). % 6
[seven] :: ([c1, -tee],7). % 7
[eight] :: ([c1, -ee],8). % 8
[nine] :: ([c1, -tee],9). % 9
[ten] :: ([c2],10). % 10
[eleven] :: ([c2],11). % 11
[twelve] :: ([c2],12). % 12
[thirteen] :: ([c2],13). % 13
[fifteen] :: ([c2],15). % 15
[een] :: ([=c1, +ee, c2],[A,Out] >> (Out = plus18t0(A))). % 0 * A + 18
[twenty] :: ([=c1, c2],[A,Out] >> (Out = plus20(A))). % A + 20
[twenty] :: ([c2],20). % 20
[thirty] :: ([c2],30). % 30
[thirty] :: ([=c1, c2],[A,Out] >> (Out = plus30(A))). % A + 30
[fifty] :: ([c2],50). % 50
[fifty] :: ([=c1, c2],[A,Out] >> (Out = plus50(A))). % A + 50
[y] :: ([=c1, +ee, c2],[A,Out] >> (Out = t0plus80(A))). % 0 * A + 80
[y] :: ([=c1, =cee, c2],[A,B,Out] >> (Out = plus80t0(A,B))). % A + 0 * B + 80

[the] :: ([=n,d],[A,Out] >> (Out = A)).
[the] :: ([=n,n,-g],[A,Out] >> (Out = A)).
[] :: ([=n, n, -w],[A,Out] >> (Out = A)).
[] :: ([=c1, c4],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, c2],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c2, c3],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c2, c4],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c3, c4],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +tee, ctee],[A,Out] >> (Out = A)).	% epsilon
[] :: ([=c1, +tee, c1],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +tee, c2],[A,Out] >> (Out = A)). % epsilon
[] :: ([=c1, +ee, c1],[A,Out] >> (Out = A)). % epsilon


[] :: ([=c1, +ee, cee],[A,Out] >> (Out = A)). % epsilon
[] :: ([cnix],epsilon). % epsilon
