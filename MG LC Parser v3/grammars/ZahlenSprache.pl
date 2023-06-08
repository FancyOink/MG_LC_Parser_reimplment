startCategory(c).

['es gibt'] :: ([=n, c]).
[liegen] :: ([=n,=n,c]).
['Feld'] :: ([=f,n]).
[im] :: ([=n,n]).

['Bananen'] :: ([n]).
['Kaese'] :: ([n]).

[] :: ([=c4, =c4, f]).
[] :: ([=n, =c4, n]).

[zig] :: ([=c1, +zi, c2, -tausU]). % 10 * A	%Hauptfunktionen
[zig] :: ([=c1, +zi, cundZIG]). % 10 * A
[und] :: ([ =cundZIG, =cun, c2, -tausU]). % A + B
[zehn] :: ([=c1, +zeh, c2, -tausU]). % A + 10
[hundert] :: ([=c2, =cun, c3, -taus]). % A + 100 * B
[hundert] :: ([=c1, +un, c3, -taus]). % 100 * A
[einhundert] :: ([=c2, c3, -taus]). % A + 100
[einhundert] :: ([c3, -taus]). % 100
[tausend] :: ([=c3, =c3,+taus, c4]). % A + 1000 * B
[tausend] :: ([=c3, +taus, c4]). % 1000 * A
[eintausend] :: ([=c3, c4]). % A + 1000
[eintausend] :: ([c4]). % 1000

[eins] :: ([c1]).	% 1		%c1
[zwei] :: ([c1, -unU]). % 2
[drei] :: ([c1, -ssi, -zeh, -unU]). % 3
[vier] :: ([c1, -zi, -zeh, -unU]). % 4
[fuenf] :: ([c1, -zi, -zeh, -unU]). % 5
[sechs] :: ([c1, -unU]). % 6
[sieben] :: ([c1, -unU]). % 7
[acht] :: ([c1, -zi, -zeh, -unU]). % 8
[neun] :: ([c1, -zi, -zeh, -unU]). % 9
[zehn] :: ([c2, -tausU]). % 10		%diverse Ausnahmen
[elf] :: ([c2, -tausU]). % 11
[zwoelf] :: ([c2, -tausU]). % 12
[sechzehn] :: ([c2, -tausU]). % 16
[siebzehn] :: ([c2, -tausU]). % 17
[zwanzig] :: ([c2, -tausU]). % 20
[einundzwanzig] :: ([c2, -tausU]). % 21
[sechzig] :: ([c2, -tausU]). % 60
[einundsechzig] :: ([c2, -tausU]). % 61
[siebzig] :: ([c2, -tausU]). % 70
[einundsiebzig] :: ([c2, -tausU]). % 71
[undzwanzig] :: ([=c1, +un, c2, -tausU]). % A + 20
[ssig] :: ([=c1, +ssi, c2, -tausU]). % 0 * A + 30
[ssig] :: ([=c1, +ssi, cundZIG]). % 0 * A + 30
[einund] :: ([=cundZIG, c2, -tausU]). % A + 1
[undsechzig] :: ([=c1, +un, c2, -tausU]). % A + 60
[undsiebzig] :: ([=c1, +un, c2, -tausU]). % A + 70


[] :: ([=c1, +unU, cun]).
[] :: ([=c1, +unU,c1,-un]).
[] :: ([=c1, +zi, c1]). % epsilon		%Schminke
[] :: ([=c1, +zi, +zeh, +unU, c1, -zi]). % epsilon
[] :: ([=c1, +ssi, c1]). % epsilon
[] :: ([=c1, +ssi, +zeh, +unU, c1, -ssi]). % epsilon
[] :: ([=c1, +zeh, c1]). % epsilon
[] :: ([=c1, +zeh, +unU, c1, -zeh]). % epsilon
[] :: ([=c1, +unU, c1]). % epsilon
[] :: ([=c2, +tausU, c2]). % epsilon
[] :: ([=c3, +taus, c3]). % epsilon
[] :: ([=c3, c4]). % epsilon		%Upgrader
[] :: ([=c2, +tausU, c3, -taus]). % epsilon
[] :: ([=c2, c3]). % epsilon
[] :: ([=c1, +unU, c2, -tausU]). % epsilon
[] :: ([=c1, c2]). % epsilon

