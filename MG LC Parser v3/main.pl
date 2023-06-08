% file: main.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: top executable for MG LC-Parser

:- ['helpers/painter'].
:- ['helpers/extermination'].
:- ['scanner'].

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verwendetes Lexikon für das Parsen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-['grammars/maus'].
%:-['grammars/numbers'].
%:-['grammars/ZahlenSprache'].
:-['grammars/EpsKetten'].

:-exterminate(Li),write("Li: "),writeln(Li).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hauptfunktion des Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mgParse(Input,Lambda,Tree):- scan(Input,TokenList).