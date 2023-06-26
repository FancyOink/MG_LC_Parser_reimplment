:- module(linker,[linking/1]).
% file: linker.pl
% origin author : J. Kuhn
% origin date: June 2023
% purpose: produces LINKS(X,Y) for a given MG-lexicon

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

debugMode.	% comment this line, if debugMode should be off
debugMode:- false.

linking(NewLinks).