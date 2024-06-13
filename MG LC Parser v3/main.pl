% file: main.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: top executable for MG LC-Parser

:-['load'].

mainDebug.	% comment this line, if debugMode should be off
mainDebug :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rememberEta(+[Lis])
%
%	assertz Eta-Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rememberEta([]).
rememberEta([Li|Lis]):- assertz(Li),rememberEta(Lis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rememberEps(+[Lis])
%
%	assertz Epsilon-Lis with the Mark = clean
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rememberEps([]).
rememberEps([epsLi(FsE,clean)|Lis]):- assertz(epsLi(FsE,clean)),rememberEps(Lis).
rememberEps([epsLi(_,dot)|Lis]):- rememberEps(Lis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linking(-[Links(X,Y)])
%
% generates LINKS from given (transformed) MG-lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-linking(Links).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hauptfunktion des Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mgParse(Input,Lambda,Tree):- 
			scanInput(Input,TokenList),
			lcParse(TokenList,Tree),
			makeLambda(Tree,Lambda). % TODO: Hier später Tree -> MGTree



