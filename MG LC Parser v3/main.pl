% file: main.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: top executable for MG LC-Parser

:-['load'].

mainDebug.	% comment this line, if debugMode should be off
mainDebug :- false.

:- (mainDebug -> protocol('mylog.txt');true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rememberLink(+[Links])
%
%	assertz Links
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rememberLink([]).
rememberLink([Link|Links]):- assertz(Link),rememberLink(Links).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linking(-[Links(X,Y)])
%
% generates LINKS from given (transformed) MG-lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- linking(T),rememberLink(T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getLinks(-[Links(X,Y)])
%
% returns all links
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getLinks(L):- setof(link(T,X,Y),link(T,X,Y),L),
	(mainDebug->writeln(L);true).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hauptfunktion des Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(Input,Tree):- 
			scanInput(Input,TokenList),
			(mainDebug-> write("parse: Tokenliste: "),writeln(TokenList);true),
			!, % Cut to prevent unneccessary backtracking to Scanner, if unsuccsessfull parse
			lcParse(TokenList,Tree).
			%makeLambda(Tree,Lambda). % TODO: Hier später Tree -> MGTree





