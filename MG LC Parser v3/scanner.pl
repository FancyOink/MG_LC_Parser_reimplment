% file: scanner.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: scans Input String and transforms  it in a list of Token

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- use_module(library(apply)).
:- ['grammars/numbersContext'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% scan(+String,-[String_List])
%
% oberste Funktion, die einen String einliest und eine Liste an STring-Tokens ausgibt
%
%NB: sollte noch alle Großbuchstaben am Anfang in Kleinbuchstaben umwandeln
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan(Input, Output) :-  split_string(Input," ","",OutString), maplist(atom_string,Output,OutString).