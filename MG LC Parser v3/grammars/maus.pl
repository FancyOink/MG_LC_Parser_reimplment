%   File   : maus.pl
%   Author : Johannes Kuhn
% purpose  : simple MG-Lexicon to build sentences with the Mouse-Cheese-Scenario
:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
% Bsp mit Maus und Käse
startCategory(c).

%[] :: ([=d,+wh,d]).
['Maus'] :: ([d]).
['Kaese'] :: ([n]).
['Katze'] :: ([d]).
['Hund'] :: ([d,-w]).
['weis nix'] :: ([=d,+w,c]).
[frisst] :: ([=d,=d,c]).
[wer] :: ([d,-wh]).
[und] :: ([=c,=c,c]).
[wen] :: ([d,-wh]).
[fressen] :: ([=d,=d,v]).


[der] :: ([=d,d]).
[den] :: ([=d,d]).
[kennt] :: ([=d,=d,c]).
['Postbote'] :: ([d]).
['Besitzer'] :: ([d]).
['Frau'] :: ([d]).
['Mann'] :: ([d]).
['?'] :: ([=c,+f,d]).
['Satz: '] :: ([=d,c]).

[] :: ([=v,c]).
[] :: ([=v,+wh,c]).

[den] :: ([=n,d]).
[die] :: ([=d, d]).
% Bsp.:
% ['Katze',frisst,'Maus',und,'Maus',frisst,'Kaese']
% ['Maus',frisst,'Kaese']
% ['Maus',weis,wer,'Kaese',frisst]
