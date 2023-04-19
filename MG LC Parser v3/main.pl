% file: main.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: top executable for MG LC-Parser

:- ['helpers/painter'].
:- ['scanner'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verwendetes Lexikon für das Parsen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-['grammars/naus'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hauptfunktion des Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mgParse(Input,Lambda,Tree):- scan(Input,TokenList).