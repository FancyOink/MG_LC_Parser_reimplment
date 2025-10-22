% file: scanner.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: scans Input String and transforms  it in a list of Token

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

:- use_module(library(apply)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% scan(+String,-[String_List])
%
% top most function of the scanner, reads a string and gives back a list of string-token
%
% NB: should in the future change all upper case letters to lower case letters
% TODO: to scan the exponents in a lexicon and checks if the read string has a pendant in the lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scanInput("",[]).
scanInput(Input, Output) :-  string(Input),split_string(Input,",","",OutString), maplist(atom_string,Output,OutString);
						writeln("Please write a string"),false.