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
%----------------------------------------------------------
%	Formen an MG-Regeln:
%			1. B C -> A (merge) -> Konvention: B hat immer ein positives Feature als Head
%			2. B   -> A (move)
%----------------------------------------------------------
%		-c(R) ; R = lc-Rule
%		-c1(R)
%		-c2(R)
%		-c3(R)
%----------------------------------------------------------
% prediction C => A, because if a B -> pre(C,A)
% pre(C,A) and C will result in tree(A',B')
% C: cRule(W,{::,:,*},Fs)
% B: bRule(W,{::,:,*},Fs)
% A: bRule or cRule depending on Fs
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
%		- Ketten
%		- Abfrage genauer LC- und C-Regeln in eigene Funktion?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseF([],[cRule(W,::,[cfin])],[li(W,[cfin])],[],[cRule(W,::,[cfin])],[li(W,[cfin])]). % special end condition for Tree = 1 leaf
parseF([],[cRule(W,:,[cfin])],[tree([(W,[cfin])],LTree,RTree)],[],[cRule(W,:,[cfin])],[tree([(W,[cfin])],LTree,RTree)]). % normal end condition
parseF(Input,[],[],OutPut,OutWs,OutTree):- 	shift(Input,RestPut,[LI]),	% First Action of the Parse
											makeWsRule(LI,WsRule),
											(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
											parseF(RestPut,[WsRule],[LI],OutPut,OutWs,OutTree).
parseF(Input,Ws,InTree,OutPut,OutWs,OutTree):- checkCNRule(Input,Ws,InTree,InterPut,InterWs,InterTree), % CN-Rules before LC-Rules, because they overlap them
															parseF(InterPut,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,[bRule(W,T,Fs)|Ws],InTree,OutPut,OutWs,OutTree):-  lc1([bRule(W,T,Fs)|Ws],InTree,InterWs,InterTree), % LC-Rules 1 merge
																parseF(Input,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,[[bRule(W,T,Fs)|RsRule]|Ws],InTree,OutPut,OutWs,OutTree):-  lc1([[bRule(W,T,Fs)|RsRule]|Ws],InTree,InterWs,InterTree), % LC-Rules 1 move
																parseF(Input,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,[cRule(W,T,Fs)|Ws],InTree,OutPut,OutWs,OutTree):-  lc2([cRule(W,T,Fs)|Ws],InTree,InterWs,InterTree), % LC-Rules 2
																parseF(Input,InterWs,InterTree,OutPut,OutWs,OutTree).
parseF(Input,Ws,InTree,OutPut,OutWs,OutTree):-  shift(Input,RestPut,[LI]),	% Last to try shift
												makeWsRule(LI,WsRule),
												(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
												parseF(RestPut,[WsRule|Ws],[LI|InTree],OutPut,OutWs,OutTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shift(+[Token],-[Token],-LI)
% 
% shifted the first Token of a list out and returns a corresponding LI
% NB: 	- Vieleicht in Zukunft eine Präferenz für spezielle Fs einbauen
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
makeWsRule(li(W,[=F|Fs]),bRule(W,::,[=F|Fs])).
makeWsRule(di(W,[=F|Fs]),bRule(W,: ,[=F|Fs])).
makeWsRule(di(W,[+F|Fs]),bRule(W,: ,[+F|Fs])).
makeWsRule(di(W,[-F|Fs]),cRule(W,: ,[-F|Fs])).
makeWsRule(li(W,[ F|Fs]),CRule):- checkCat(F), CRule = cRule(W,::,[ F|Fs]).
makeWsRule(di(W,[ F|Fs]),CRule):- checkCat(F), CRule = cRule(W,: ,[ F|Fs]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCat(+Fs)
%
% checks if the Feature is a category and nothing else
% to avoid redefining category Feature with new sign
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCat(=_):-!,false.
checkCat(+_):-!,false.
checkCat(-_):-!,false.
checkCat(_).
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
%	NB: - Link abfrage einbauen
%		- Ketten bei bRules und cRules bedenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc1([bRule(S,::,[=F|FsS])|Ws],InTree,OutWs,OutTree):- 	append(S,T,ST),checkFsRule(ST,:,FsS,ARule),
														OutWs = [pre(cRule(T,_Dot,[F]),ARule)|Ws],% merge 1
														buildTree(lcMerge1,InTree,OutTree).
lc1([bRule(S,:,[=F|FsS])|Ws],InTree,OutWs,OutTree):-  	append(T,S,TS),checkFsRule(TS,:,FsS,ARule),
														OutWs = [pre(cRule(T,_Dot,[F]),ARule)|Ws],% merge 2
														buildTree(lcMerge2,InTree,OutTree).
lc1([bRule(S,:,[=F|FsS])|Ws],InTree,OutWs,OutTree):- 	checkFsRule(S,FsS,ARule),
														OutWs = [pre(cRule(T,_Dot,[F|FsT]),[ARule,cRule(T,:,FsT)])|Ws], % merge 3
														buildTree(lcMerge3,InTree,OutTree).
lc1([[bRule(S,:,[+F|FsS])|Rules]|Ws],InTree,OutWs,OutTree):- checkSMC(Rules),checkMove(bRule(S,:,[+F|FsS]),Rules,ARule), % move 1 + 2
														OutWs = [ARule|Ws],
														buildTree(lcMove,InTree,OutTree).
lc1([[bRule(S,:,[=F|FsS])|Rules]|Ws],InTree,OutWs,OutTree):- append(T,S,TS),
														checkFsRule(TS,:,FsS,ARule),
														OutWs = [pre(cRule(T,_Dot,[F|FsT]),[ARule,cRule(T,:,FsT)|Rules])|Ws], %merge 2
														buildTree(lcMerge3,InTree,OutTree).
lc1([[bRule(S,:,[=F|FsS])|Rules]|Ws],InTree,OutWs,OutTree):- checkFsRule(S,FsS,ARule),
														OutWs = [pre(cRule(T,_Dot,[F|FsT]),[ARule,cRule(T,:,FsT)|Rules])|Ws], %merge 3
														buildTree(lcMerge3,InTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lc2(+[WS],+[Tree],-[WS],-[Tree])
%
%		-lc2(R); R = MG-Rule
%		=> given R = R1 and a C: replace C for B -> A
%			R1:		- merge 2
%					- merge 3	
%		aRule is a placeholder
%		because I do not know if the bRule becomes a cRule or bRule
%	NB: - Link abfrage einbauen
%		- Ketten bei bRules und cRules bedenken
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc2([cRule(T,_,[ F])|Ws],InTree,OutWs,OutTree):- 		append(T,S,TS),
														OutWs = [pre(bRule(S,_Dot,[=F|FsS]),[aRule(TS,:,FsS)])|Ws], 	% merge 2, because I do not know if the bRule becomes a cRule or bRule, aRule is a placeholder
														buildTree(lcMerge2,InTree,OutTree).
lc2([cRule(T,_,[ F|FsT])|Ws],InTree,OutWs,OutTree):- 	OutWs = [pre(bRule(S,_Dot,[=F|FsS]),[aRule(S,:,FsS),cRule(T,:,FsT)])|Ws],	% merge 3, because I do not know if the bRule becomes a cRule or bRule, aRule is a placeholder
														buildTree(lcMerge3,InTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%														
% checkMove(+bRule,+[cRules],-[Rule])
%
% checks if any Moves can be done (legally)  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkMove(bRule(S,:,[+F|FsS]),[cRule(T,:,[-F])|Rules],ARule):-	append(T,S,TS),checkFsRule(TS,:,FsS,NewRule),
																append(NewRule,Rules,ARule).
checkMove(bRule(S,:,[+F|FsS]),[cRule(T,:,[-F|FsT])|Rules],ARule):-checkFsRule(S,:,FsS,NewRule),
																append(NewRule,[cRule(T,:,FsT)|Rules],ARule).
checkMove(BRule,[CRule|Rules],[NewB,CRule|NewRules]):-	checkMove(BRule,Rules,[NewB|NewRules]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%														
% checkSMC(Rules)
%
% checks if the SMC is violated 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkSMC([cRule(_,:,[-F|_]),cRule(_,:,[-F|_])|_]):-!,false.
checkSMC([CR1, CR2|Rules]) :- checkSMC([CR1|Rules]),checkSMC([CR2|Rules]).
checkSMC([_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFsRule(+[String],+Type,[Fs],-[Rule])
%
% checks which rule applies to the Fs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFsRule(W,T,[=F|Fs],[bRule(W,T,[=F|Fs])]).
checkFsRule(W,T,[+F|Fs],[bRule(W,T,[+F|Fs])]).
checkFsRule(W,T,[-F|Fs],[cRule(W,T,[-F|Fs])]).
checkFsRule(W,T,[ F|Fs],[ARule]):- checkCat(F),ARule = cRule(W,T,[ F|Fs]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Closure-Rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCNRule(+[Strin],+[WS],+[Tree],-[String],-[WS],-[Tree])
%
%	check if a CN-Rule can be applied, commit to the chosen checkCNRule
%	NB: nachprüfen, ob ! bei ALLEN Regeln nötig ist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCNRule(_,[],_,_,_,_):- !,false.
checkCNRule(Input,[pre(cRule(_,_,FsT),A)|Ws],InTree,InterPut,InterWs,InterTree):-!,checkWS(FsT,Ws,PreRule,NewWs).%shift?
checkCNRule(Input,[pre(bRule(_,_,FsS),A)|Ws],InTree,InterPut,InterWs,InterTree):-!,checkWS(FsS,Ws,PreRule,NewWs).%shift?
checkCNRule(Input,[pre(aRule(_,_,Fs),A)|Ws],InTree,InterPut,InterWs,InterTree):-!,checkWS(Fs,Ws,PreRule,NewWs).%shift?
checkCNRule(Input,[bRule(S,Dot,FsS)|Ws],InTree,InterPut,InterWs,InterTree):- !,checkWS(FsS,Ws,PreRule,NewWs).%lc1(Merges)
checkCNRule(Input,[[bRule(S,:,[+F|FsS])|Rules]|Ws],InTree,InterPut,InterWs,InterTree):- !.% lc1(Moves)
checkCNRule(Input,[cRule(T,Dot,FsT)|Ws],InTree,InterPut,InterWs,InterTree):-!.%lc2(Merges)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c0(+[Ws],-[Ws])
%
%	given pre(B,A) in Workspace and LC-Rule, which produces B (lc1(move1),lc1(move2),shift)
%	=> delete pre(B,A) and return A (cRule,bRule,aRule)
%	NB: nachdenken, ob aRule üverhaupt vorkommen kann hier
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c0(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1(+[Ws],-[Ws])
%
% 	given pre(C,B) in Workspace and LC-Rule, which produces pre(B,A)
%	=> delete pre(C,B) and return pre(C,A)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c1(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c2(+[Ws],-[Ws])
%
%	given pre(B,A) in Workspace and LC-Rule, which produces pre(C,B)
%	=> delte pre(B,A) and return pre(C,A)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c2(InWs,OutWs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c3(+[Ws],-[Ws])
%
%	given pre(B,A) and pre(D,C) in Workspace and LC-Rule, 
%	which produces pre(C,B)
%	=> delete pre(B,A) and pre(D,C) and return pre(D,A)
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
% NB: 	- ggf. Ketten genauer bestimmen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildTree(lcMerge1,[li(S,[=F|FsS])|Trees],OutTrees):- % For position B = LI
	OutTrees = [tree([([S|_T],FsS)|_NewChains],li(S,[=F|FsS]),_RightTreeC)|Trees].	
buildTree(lcMerge2,[tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2)|Trees],OutTrees):- % For position B = DI
	OutTrees = [tree([([_T|S],FsS)|_NewChains],tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2),_RightTreeC1)|Trees]. 
buildTree(lcMerge2,[li(T,[ F])|Trees],OutTrees):- % For position C = LI
	checkCat(F),
	OutTrees = [tree([([T|S],FsS)|_NewChains],tree([(S,[=F|FsS])|_AlphaChains],_LeftTreeB,_RightTreeB),li(T,[ F]))|Trees].	
buildTree(lcMerge2,[tree([(T,[ F])|BetaChains],LeftTreeC2,LeftTreeC2)|Trees],OutTrees):- % For position C = DI
	checkCat(F),
	OutTrees = [tree([([T|S],FsS)|_NewChains],tree([(S,[=F|FsS])|_AlphaChains],_LeftTreeB2,_RightTreeB2),tree([(T,[ F]|BetaChains)],LeftTreeC2,LeftTreeC2))|Trees]. 
buildTree(lcMerge3,[tree([(S,[=F|FsS]|AlphaChains)],LeftTreeB2,LeftTreeB2)|Trees],OutTrees):-
	OutTrees = [tree([(S,FsS),(_T,_FsT)|_NewChains],tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2),_RightTreeC1)|Trees]. % For Position B = DI
buildTree(lcMerge3,[li(S,[=F|FsS])|Trees],OutTrees):-% For Position B = LI
	OutTrees = [tree([(S,FsS),(_T,_FsT)|_NewChains],li(S,[=F|FsS]),_RightTreeC1)|Trees]. 
buildTree(lcMerge3,[li(T,[ F|FsT])|Trees],OutTrees):-  % For Position C = LI
	checkCat(F),
	OutTrees = [tree([(_S,_FsS),(T,FsT)|_NewChains],_LeftTreeB1,li(T,[ F|FsT]))|Trees]. 
buildTree(lcMerge3,[tree([(T,[ F|FsT])|BetaChains],LeftTreeC2,LeftTreeC2)|Trees],OutTrees):- % For Position C = DI
	checkCat(F),
	OutTrees = [tree([(_S,_FsS),(T,FsT)|_NewChains],_LeftTreeB1,tree([(T,[ F|FsT])|BetaChains],LeftTreeC2,LeftTreeC2))|Trees]. 
buildTree(lcMove,[tree([FChain|RsChain],LeftTreeB1,RightTreeB1)|Trees],OutTrees):-
	checkTreeMove(FChain,RsChain,NewChain),
	OutTrees = [tree(NewChain,empty,tree([FChain|RsChain],LeftTreeB1,RightTreeB1))|Trees].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkTreeMove(+ChainItem,+[ChainItems],-[ChainItems])
%
% checks in the chains for the correct Chain item to move. SMC was already checked beforehand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkTreeMove(Head,[],_):-!,write("Error: checkTreeMove: Found no Match for Move for: "), writeln(Head),false. % should never reach
checkTreeMove((S,[+F|FsS]),[(T,[-F])|RsChain],NewChain):- append(T,S,TS), % move 1
	NewChain = [(TS,FsS)|RsChain].
checkTreeMove((S,[+F|FsS]),[(T,[-F|FsT])|RsChain],NewChain):- % move 2
	NewChain = [(S,FsS),(T,FsT)|RsChain].
checkTreeMove(FChain,[KChain|RsChain],NewChain):- checkTreeMove(FChain,RsChain,[NewFChain|NewRsChain]),
	NewChain = [NewFChain,KChain|NewRsChain].