:- module(lcparser,[lcParse/2]).
% file: lcparser.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: takes a list of tokens and a lexicon and generates a fitting derivation tree
debugMode.	% comment this line, if debugMode should be off
debugMode:- false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MG mit Semantik
% merge   -> tree([([W],[Fs],L)],Subtree1,Subtree2)
% move    -> tree([([W],[Fs],L)],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs], L)
% Derived Item -> di([W],[Fs],L)
% unspecified Item -> ti([W],[Fs],L)
%
% MG ohne Semantik
% merge   -> tree([([W],[Fs])],Subtree1,Subtree2)
% move    -> tree([([W],[Fs])],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs])
% Derived Item -> di([W],[Fs])
% unspecified Item -> ti([W],[Fs])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: implementiere Regeln
%		-shift LI
%----------------------------------------------------------
%	Formen an MG-Regeln:
%			1. B C -> A (merge) -> Konvention: B hat immer ein positives Feature als Head
%			2. B   -> A (move)
%----------------------------------------------------------
%		-lc1(R); R = MG-Rule
%		=> gegeben R = R1 und einem B: ersetze B durch C -> A
%			- merge 1
%			- merge 2
%			- merge 3
%		=> gegben R = R2 und einem B: ersetze B durch A
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
% prediction C => A, because if a B -> pre(C,A,B)
% pre(C,A,B) and C will result in tree(A',B',C')
% C and B : {li,ti}
% A : {di}
% B',C' : {tree,li}
% A' :{tree}
%----------------------------------------------------------
%  Beispiel: "drei,hundert,vier,zig"
%			 "drei"
%			 "drei,hundert"
%  Nutze \+ \+ zur Abfrage von Links ohne Variablen zu instanzieren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lcParse(+[Tokens],-[Trees]
%
% top most parse function to the outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lcParse(Tokens,Tree):- 
	parseF(Tokens,[],[],OutPut,OutWs,Tree),
	(debugMode ->write("lcParse: WS: "),write(OutWs),write(" Output: "),writeln(OutPut),
		write("lcParse: Tree: "),writeln(Tree);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parseF(+[Tokens],+[WS],+[Trees],-[Tokens],-[WS],-[Trees])
%	
% main parse function. Controlls the parse
% Finished Parse-Tree at the "Bottom"
% NB: 	- die Move-Fälle für lc1(R) mehr drüber nachdenken
%		- die Closure-Fälle genauer abgrenzen 
%		- Abfrage genauer LC- und C-Regeln in eigene Funktion?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseF([],[cRule(W,[cfin])],[li(W,[cfin])],[],[cRule(W,[cfin])],[li(W,[cfin])]).
parseF([],[di(W,[cfin])],[tree([(W,[cfin])],LTree,RTree)],[],[di(W,[cfin])],[tree([(W,[cfin])],LTree,RTree)]).
parseF(Input,[],[],OutPut,OutWs,OutTree):- 	shift(Input,RestPut,[LI]),	% First Action of the Parse
											makeWsRule(LI,WsRule),
											(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
											parseF(RestPut,[WsRule],[LI],OutPut,OutWs,OutTree).
parseF(Input,[pre(B,A,C)|Ws],InTree,OutPut,OutWs,OutTree):- checkCNRule(Input,[pre(B,A,C)|Ws],InTree,InterPut,InterWs,InterTree), % CN-Rules before LC-Rules, because they overlap them
															parseF(InterPut,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,[bRule(W,T,Fs)|Ws],InTree,OutPut,OutWs,OutTree):- lc1([cRule(W,T,Fs)|Ws],InTree,InterWs,InterTree), % LC-Rules 1
															 parseF(Input,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,[cRule(W,T,Fs)|Ws],InTree,OutPut,OutWs,OutTree):- lc2([cRule(W,T,Fs)|Ws],InTree,InterWs,InterTree), % LC-Rules 2
															 parseF(Input,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF()Input,Ws,InTree,OutPut,OutWs,OutTree):- shift(Input,RestPut,[LI]),	% Last to try shift
												makeWsRule(LI,WsRule),
												(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
												parseF(RestPut,[WsRule|Ws],[LI|InTree],OutPut,OutWs,OutTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shift(+[Token],-[Token],-LI)
% 
% shifted the first Token of a list out and returns a corresponding LI
% NB: 	- Vieleicht ich Zukunft eine Präferenz für spezielle Fs einbauen
%		- oder den Shift durch den Workspace vorsortieren
%		- shift von epsilon-LI erlauben
%		- Abfrage Links?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shift([H|HRs],HRs,LI):- setof(li([H],Fs),([H] :: Fs),LI),
	(debugMode->write("shift: LI: "),writeln(LI);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeWsRule(+[LI],-WsRule)
%
%	transform a LI into a:
%						- cRule if head feature is negative
%						- bRule if head feature is positive
% NB: Die Move-Regeln nachschärfen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeWsRule(li(W,[=f|Fs]),bRule(W,::,[=f|Fs])).
makeWsRule(di(W,[=f|Fs]),bRule(W,:,[=f|Fs])).
makeWsRule(di(W,[+f|Fs]),bRule(W,:,[+f|Fs])).
makeWsRule(di(W,[-f|Fs]),cRule(W,:,[-f|Fs])).
makeWsRule(li(W,Fs),cRule(W,::,Fs)).
makeWsRule(di(W,Fs),cRule(W,:,Fs)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LC-Rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lc1(+[WS],+[Tree],-[WS],-[Tree])
% 
%		-lc1(R); R = MG-Rule
%		=> given R= R1 and a B: replace B for C -> A
%			R1: 	- merge 1
%				- merge 2
%				- merge 3
%		=> given R = R2 and a B: replace B for A
%			R2:		- move 1
%					- move 2
%	NB: Move-Fälle genauer nachdenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc1([bRule(S,::,[=f|Fs])|Ws],InTree,OutWs,OutTree):- append(S,T,ST),OutWs = [pre(ti(T,[f]),[di(ST,Fs)]|Ws],% merge 1
													 buildTree(merge1,b,InTree,OutTree).
lc1([bRule(S,:,[=f|Fs])|Ws],InTree,OutWs,OutTree):-  append(T,S,TS),OutWs = [pre(ti(T,[f]),[di(TS,Fs)]|Ws], % merge 2
													 buildTree(merge2,b,InTree,OutTree).
lc1([bRule(S,:,[=f|FsS])|Ws],InTree,OutWs,OutTree):- OutWs = [pre(ti(T,[f|FsT]),[di(S,FsS),di(T,FsT)]|Ws],% merge 3
													 buildTree(merge3,b,InTree,OutTree).													 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lc2(+[WS],+[Tree],-[WS],-[Tree])
%
%		-lc2(R); R = MG-Rule
%		=> given R = R1 and a C: replace C for B -> A
%			R1:		- merge 2
%					- merge 3	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc2([cRule(T,_,[ f])|Ws],InTree,OutWs,OutTree):- append(T,S,TS),OutWs = [pre(di(S,[=f|FsS]),[di(TS,FsS)])|Ws], 	% merge 2
												 buildTree(merge2,c,InTree,OutTree).
lc2([cRule(T,_,[ f|FsT])|Ws],InTree,OutWs,OutTree):- OutWs = [pre(ti(S,[=f|FsS]),[di(S,FsS),di(T,FsT)]|Ws],		% merge 3
													 buildTree(merge3,c,InTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Closure-Rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCNRule(+[Strin],+[WS],+[Tree],-[String],-[WS],-[Tree])
%
%	check if a CN-Rule can be applied
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCNRule(Input,[pre(B,A,C)|Ws],InTree,InterPut,InterWs,InterTree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c(+[Ws],-[Ws])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1(+[Ws],-[Ws])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c1(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c2(+[Ws],-[Ws])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c2(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c3(+[Ws],-[Ws])
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c3(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Building the derivation Tree
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildTree(+MG-Rule,+Position,+[Tree],-[Tree])
%
% constructs a tree out of the first element of the input tree list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
