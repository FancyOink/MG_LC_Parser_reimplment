%   File   : arithm.pl
%   Author : Johannes Kuhn
% purpose  : simple MG-Lexicon to build sentances for arithmetic

% Bsp für  Arithmetische Ausdrücke

[] :: [=v, c].
[] :: [=v, +wh, c].
[one] :: [d].
[two] :: [d].

[plus] :: [=d,=d,v].
[minus] :: [=d,=d,v].
[times] :: [=d,=d,v].

[plus] :: [=v,=d,v].
[minus] :: [=v,=d,v].
[times] :: [=v,=d,v].

[what] :: [d, -wh].
[whom] :: [d, -wh].

[] :: [=d, c].

startCategory(c).
