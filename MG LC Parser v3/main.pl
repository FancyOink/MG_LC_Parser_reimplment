% file: main.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: top executable for MG LC-Parser

:- ['helpers/painter'].
:- ['helpers/extermination'].
:- ['helpers/linker'].
:- ['scanner'].

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verwendetes Lexikon für das Parsen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-['grammars/maus'].
:-['grammars/numbers'].
%:-['grammars/ZahlenSprache'].
%:-['grammars/EpsKetten'].

%mainDebug.	% comment this line, if debugMode should be off
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
% exterminate(-([Eta-Li],[Epsilon-Li]))
%
% transforms the lexicon and removes epsilon-LI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-exterminate((Li,Eps)),
	(mainDebug -> write("Eta-Li: "),writeln(Li),length(Li,LLi),write("Eps-Li: "),writeln(Eps),length(Eps,LEps),write("#Eta-Li: "),write(LLi),write(" Eps-Li: "),writeln(LEps);true),
	rememberEta(Li),
	rememberEps(Eps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linking(-[Links(X,Y)])
%
% generates LINKS from given (transformed) MG-lexicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-linking(Links).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hauptfunktion des Parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mgParse(Input,Lambda,Tree):- 
			scanInput(Input,TokenList),
			lcParse(TokenList,Tree). % TODO: Hier später Tree -> MGTree



