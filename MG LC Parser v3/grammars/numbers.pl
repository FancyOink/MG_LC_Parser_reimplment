startCategory(c4).

[ty] :: ([=c1, =ctee, c2]). % A + 10 * B
[ty] :: ([=c1, +tee, c2]). % A * 10
[teen] :: ([=c1, +tee, c2]). % A + 10
[hundredand] :: ([=c2, =c1, c3]). % A + 100 * B
[hundred] :: ([=cnix, =c1, c3]). % 100 * B + epsilon
[thousand] :: ([=c3, =c3, c4]). % A + 1000 * B
[thousand] :: ([=cnix, =c3, c4]). % 100 * B + epsilon

[one] :: ([c1]). % 1
[two] :: ([c1]). % 2
[three] :: ([c1]). % 3
[four] :: ([c1, -tee]). % 4
[five] :: ([c1]). % 5
[six] :: ([c1, -tee]). % 6
[seven] :: ([c1, -tee]). % 7
[eight] :: ([c1, -ee]). % 8
[nine] :: ([c1, -tee]). % 9
[ten] :: ([c2]). % 10
[eleven] :: ([c2]). % 11
[twelve] :: ([c2]). % 12
[thirteen] :: ([c2]). % 13
[fifteen] :: ([c2]). % 15
[een] :: ([=c1, +ee, c2]). % 0 * A + 18
[twenty] :: ([=c1, c2]). % A + 20
[twenty] :: ([c2]). % 20
[thirty] :: ([c2]). % 30
[thirty] :: ([=c1, c2]). % A + 30
[fifty] :: ([c2]). % 50
[fifty] :: ([=c1, c2]). % A + 50
[y] :: ([=c1, +ee, c2]). % 0 * A + 80
[y] :: ([=c1, =cee, c2]). % A + 0 * B + 80



[] :: ([=c1, c4]). % epsilon
[] :: ([=c1, c2]). % epsilon
[] :: ([=c2, c3]). % epsilon
[] :: ([=c2, c4]). % epsilon
[] :: ([=c3, c4]). % epsilon
[] :: ([=c1, +tee, ctee]).	% epsilon
[] :: ([=c1, +tee, c1]). % epsilon
[] :: ([=c1, +tee, c2]). % epsilon
[] :: ([=c1, +ee, c1]). % epsilon


[] :: ([=c1, +ee, cee]). % epsilon
[] :: ([cnix]). % epsilon
