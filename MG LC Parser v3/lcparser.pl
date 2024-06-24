:- module(lcparser,[lcParse/2]).
% file: lcparser.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: takes a list of tokens and a lexicon and generates a fitting derivation tree

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MG mit Semantik
% merge   -> tree([([W],[Fs],L)],Subtree1,Subtree2)
% move    -> tree([([W],[Fs],L)],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs], L)
%
% MG ohne Semantik
% merge   -> tree([([W],[Fs])],Subtree1,Subtree2)
% move    -> tree([([W],[Fs])],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: implementiere Regeln
%		-shift LI
%----------------------------------------------------------
%	Formen an MG-Regeln:
%			1. B C -> A (merge) -> Konvention: B hat immer ein positives Feature als Head
%			2. B   -> A (move)
%----------------------------------------------------------
%		-lc1(R); R = MG-Rule
%		=> gegeben R = 1 und einem B: ersetze B durch C -> A
%			- merge 1
%			- merge 2
%			- merge 3
%		=> gegben R = 2 und einem B: ersetze B durch A
%			- move 1
%			- move 2
%		-lc2(R)
%		=> gegeben R = 1 und einem C: ersetze C durch B -> A
%			- merge 2
%			- merge 3	
%----------------------------------------------------------
%		-c(R) ; R = lc-Rule
%		-c1(R)
%		-c2(R)
%		-c3(R)
%----------------------------------------------------------
%  Beispiel: "drei,hundert,vier,zig"
%			 "drei"
%			 "drei,hundert"
%  Nutze \+ \+ zur Abfrage von Links ohne Variablen zu instanzieren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lcParse([Token|TokenS],Tree).