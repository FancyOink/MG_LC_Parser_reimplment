:- module(lcparser,[lcParse/2]).
% file: lcparser.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: takes a list of tokens and a lexicon and generates a fitting derivation tree
debugMode.	% comment this line, if debugMode should be off
debugMode:- false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: - buildCTree
%		- buildC0Tree
%		- Prüfen von LINK(X,Y) einfügen
%		- epsilon-LI erlauben
%		- protocol the parse-steps as an option/default
% MG with Semantics
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
%
% Item forms used in Workspace (WS): 
% cR(W,{::,:,*},Fs,(PosL,PosR),[chainL(W,Fs,PosCh)]) % Item with negative head feature
% bR(W,{::,:,*},Fs,(PosL,PosR),[chainL(W,Fs,PosCh)]) % Item with positive head feature
% aR(W,{::,:,*},Fs,(PosL,PosR),[chainL(W,Fs,PosCh)]) % Item with unknown type of head feature
% chainL(W,Fs) % Chain link of an Item
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------
%	Forms of MG-Rules:
%			1. B C -> A (merge) -> Konvention: B has always a positive Feature as Head, C has always a negative Feature as Head
%			2. B   -> A (move)
%			3. A 		(shift)
%----------------------------------------------------------
%		-c(R) ; R = lc-Rule
%		-c1(R)
%		-c2(R)
%		-c3(R)
%----------------------------------------------------------
% prediction C => A, because if a B -> pre(C,A)
% pre(C,A) and C will result in tree(A',B')
% C: cR(W,{::,:,*},Fs,(PosL,PosR),[chainL(W,Fs,PosCh)])
% B: bR(W,{::,:,*},Fs,(PosL,PosR),[chainL(W,Fs,PosCh)])
% A: bR or cR depending on Fs (head feature positve -> bR; head feature negative -> cR)
% PosL is the position left of the token
% PosR is the position right of the token
%----------------------------------------------------------
%  Example: "drei,hundert,vier,zig"
%			 "sieben,und,drei,ßig"
%			 "drei"
%			 "drei,hundert"
%  Nutze \+ \+ zur Abfrage von Links ohne Variablen zu instanzieren
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lcParse(+[Tokens],-[Trees]
%
% top most parse function to the outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lcParse(Tokens,Tree):- 
	parseF(0,Tokens,[],[],OutPut,OutWs,Tree),
	(debugMode ->write("lcParse: WS: "),write(OutWs),write(" Output: "),writeln(OutPut),
		write("lcParse: Tree: "),writeln(Tree);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parseF(+Position,+[Tokens],+[WS],+[Trees],-[Tokens],-[WS],-[Trees])
%	
% main parse function. Controlls the parse
% Finished Parse-Tree at the "Bottom"
% Position is the current left most position of the string with regards to the original input string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseF(_,[],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])],[],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])]). % special end condition for Tree = 1 leaf
parseF(_,[],[cR(W,:,[cfin],Pos,[])],[tree([(W,[cfin])],LTree,RTree)],[],[cR(W,:,[cfin],Pos,[])],[tree([(W,[cfin])],LTree,RTree)]). % normal end condition
parseF(Pos,Input,[],[],OutPut,OutWs,OutTree):- 	
	shift(Pos,Input,NewPos,RestPut,LI),	% First Action of the Parse
	makeWsRule(Pos,NewPos,LI,WsRule),
	(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
	parseF(NewPos,RestPut,[WsRule],[LI],OutPut,OutWs,OutTree).
parseF(Pos,Input,[bR(W,T,Fs,PosB,Chain)|Ws],InTree,OutPut,OutWs,OutTree):-  
	lc1([bR(W,T,Fs,PosB,Chain)|Ws],InTree,[LCRule|InterWs],[LCTree|InterTree]), % LC-Rules 1
	(checkCN(LCRule,InterWs,LCTree,InterTree,CNWs,CNTree), % check if something clicks with WS
	parseF(Pos,Input,CNWs,CNTree,OutPut,OutWs,OutTree);
	parseF(Pos,Input,[LCRule|InterWs],[LCTree|InterTree],OutPut,OutWs,OutTree)).
parseF(Pos,Input,[cR(W,T,Fs,PosC,Chain)|Ws],InTree,OutPut,OutWs,OutTree):-  
	lc2([cR(W,T,Fs,PosC,Chain)|Ws],InTree,[LCRule|InterWs],[LCTree|InterTree]), % LC-Rules 2
	(checkCN(LCRule,InterWs,LCTree,InterTree,CNWs,CNTree), % check if something clicks with WS
	parseF(Pos,Input,CNWs,CNTree,OutPut,OutWs,OutTree);
	parseF(Pos,Input,[LCRule|InterWs],[LCTree|InterTree],OutPut,OutWs,OutTree)).
parseF(Pos,Input,Ws,InTree,OutPut,OutWs,OutTree):-  
	shift(Pos,Input,NewPos,RestPut,[LI]),	% Last to try shift
	makeWsRule(LI,WsRule),
	(debugMode->write("parseF: new Ws-Rule: "),writeln(WsRule);true),
	(checkCN(RestPut,[WsRule|Ws],[LI|InTree],CNPut,CNWs,CNTree), % check if something clicks with WS
	parseF(NewPos,CNPut,CNWs,CNTree,OutPut,OutWs,OutTree);
	parseF(NewPos,RestPut,[WsRule|Ws],[LI|InTree],OutPut,OutWs,OutTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shift(+[Token],-[Token],-[LI])
% 
% shifted the first Token of a list out and returns a corresponding LI
% NB: 	- Vieleicht in Zukunft eine Präferenz für spezielle Fs einbauen
%		- oder den Shift durch den Workspace vorsortieren
%		- shift von epsilon-LI erlauben
%		- Auswahl des LI implementieren -> LINKS? Anfangsbedingung? etc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shift(Pos,[H|HRs],NewPos,HRs,LI):- LI = li([H],Fs),[H] :: Fs,
	NewPos is Pos + 1,
	(debugMode->write("shift: LI: "),writeln(LI);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeWsRule(+[LI],-WsRule)
%
%	transform a LI into a:
%						- cR if head feature is negative
%						- bR if head feature is positive
% currently all di are superfluous, since nothing currently creates di
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeWsRule(PosL,PosR,li(W,[=F|Fs]),bR(W,::,[=F|Fs],(PosL,PosR),[])).
makeWsRule(PosL,PosR,di(W,[=F|Fs]),bR(W,: ,[=F|Fs],(PosL,PosR),[])).
makeWsRule(PosL,PosR,di(W,[+F|Fs]),bR(W,: ,[+F|Fs],(PosL,PosR),[])).
makeWsRule(PosL,PosR,di(W,[-F|Fs]),cR(W,: ,[-F|Fs],(PosL,PosR),[])).
makeWsRule(PosL,PosR,li(W,[ F|Fs]),CRule):- checkCat(F), CRule = cR(W,::,[ F|Fs],(PosL,PosR),[]).
makeWsRule(PosL,PosR,di(W,[ F|Fs]),CRule):- checkCat(F), CRule = cR(W,: ,[ F|Fs],(PosL,PosR),[]).
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
%		=> given R= R1 and a B is in WS: replace B for C -> A
%			R1: - merge 1
%				- merge 2
%				- merge 3
%		=> given R = R2 and a B: replace B for A
%			R2:		- move 1
%					- move 2
%	NB: - LINK(X,Y)-Checkl einfügen
%		- nachdenken, ob SMC check nicht bei merge 3 sein sollte
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc1([bR(S,::,[=F|FsS],(LB,_),BChain)|Ws],InTree,OutWs,OutTree):- 
	append(S,T,ST),checkFsRule(ST,:,FsS,(LB,RC),BChain,ARule),
	OutWs = [pre(cR(T,_Dot,[F],(_LC,RC),_CChain),ARule)|Ws],% merge 1
	buildTree(lcMerge1,InTree,OutTree).
lc1([bR(S,:,[=F|FsS],(_,RB),BChain)|Ws],InTree,OutWs,OutTree):-  	% do/can we ever use this case?
	append(T,S,TS),checkFsRule(TS,:,FsS,(LC,RB),BChain,ARule),
	OutWs = [pre(cR(T,_Dot,[F],(LC,_RC),_CChain),ARule)|Ws],% merge 2
	buildTree(lcMerge2,InTree,OutTree).
lc1([bR(S,:,[=F|FsS],PosB,BChain)|Ws],InTree,OutWs,OutTree):- 
	append(BChain,chainL(T,[F|FsT],PosC),NewChain), checkFsRule(S,:,FsS,PosB,NewChain,ARule),
	OutWs = [pre(cR(T,_Dot,[F|FsT],PosC,_CChain),ARule)|Ws], % merge 3
	buildTree(lcMerge3,InTree,OutTree).
lc1([bR(S,:,[+F|FsS],PosB,BChain)|Ws],InTree,OutWs,OutTree):- 
	checkSMC(BChain),checkMove(bR(S,:,[+F|FsS],PosB,BChain),BChain,ARule), % move 1 + 2
	OutWs = [ARule|Ws],
	buildTree(lcMove,InTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lc2(+[WS],+[Tree],-[WS],-[Tree])
%
%		-lc2(R); R = MG-Rule
%		=> given R = R1 and a C is in WS: replace C for B -> A
%			R1:		- merge 2
%					- merge 3	
%		aR is a placeholder
%		because I do not know if the bR becomes a cR or bR
%	NB: - LINK(X,Y)-Checkl einfügen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc2([cR(T,_,[ F],(LC,_),CChain)|Ws],InTree,OutWs,OutTree):- append(T,S,TS),append(BChain,CChain,AChain),
	OutWs = [pre(bR(S,_Dot,(_LB,RB),[=F|FsS],BChain),aR(TS,:,FsS,(LC,RB),AChain))|Ws], 	% merge 2, because I do not know if the bR becomes a cR or bR, aR is a placeholder
	buildTree(lcMerge2,InTree,OutTree).
lc2([cR(T,_,[ F|FsT],PocC,CChain)|Ws],InTree,OutWs,OutTree):- 
	append(BChain,[chainL(T,FsT,PocC)|CChain],AChain), OutWs = [pre(bR(S,_Dot,PosB,[=F|FsS],BChain),aR(S,:,FsS,PosB,AChain))|Ws],	% merge 3, because I do not know if the bR becomes a cR or bR, aR is a placeholder
	buildTree(lcMerge3,InTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkMove(+bR,+[chainLinks],-Rule)
%
% checks if any Moves can be done (legally)  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkMove(_,[],_):-!,false. % nothing (legal) found
checkMove(bR(S,:,[+F|FsS],(_,RB),_),[chainL(T,[-F],(LC,_))|Rules],ARule):-	
	append(T,S,TS),checkFsRule(TS,:,FsS,(LC,RB),Rules,ARule). % move 1
checkMove(bR(S,:,[+F|FsS],PosB,_),[chainL(T,[-F|FsT],PosC)|Rules],ARule):-
	checkFsRule(S,:,FsS,PosB,[chainL(T,FsT,PosC)|Rules],ARule). % move 2
checkMove(BRule,[CRule|Rules],ARule):-	
	checkMove(BRule,Rules,NewRule),addChain(NewRule,CRule,ARule).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkSMC(Rules)
%
% checks if the SMC is violated 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkSMC([chainL(_,[-F|_],_),chainL(_,[-F|_])|_],_):-!,false.
checkSMC([CR1, CR2|Rules]) :- checkSMC([CR1|Rules]),checkSMC([CR2|Rules]).
checkSMC([_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFsRule(+[String],+Type,[Fs],+Pos,+Chain,-Rule)
%
% checks which rule applies to the Fs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFsRule(W,T,[=F|Fs],Pos,Chain,bR(W,T,[=F|Fs],Pos,Chain)).
checkFsRule(W,T,[+F|Fs],Pos,Chain,bR(W,T,[+F|Fs],Pos,Chain)).
checkFsRule(W,T,[-F|Fs],Pos,Chain,cR(W,T,[-F|Fs],Pos,Chain)).
checkFsRule(W,T,[ F|Fs],Pos,Chain,ARule):- checkCat(F),ARule = cR(W,T,[ F|Fs],Pos,Chain).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addChain(+Rule,+ChainLink,-Rule)
%
% adds a chain link to a rule at the start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addChain(bR(W,Dot,Fs,Pos,Chain),ChainL,bR(W,Dot,Fs,Pos,[ChainL|Chain])).
addChain(cR(W,Dot,Fs,Pos,Chain),ChainL,cR(W,Dot,Fs,Pos,[ChainL|Chain])).
addChain(aR(W,Dot,Fs,Pos,Chain),ChainL,aR(W,Dot,Fs,Pos,[ChainL|Chain])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Closure-Rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCN(+LCRule,+[LCRule],+LCTree,+[Tree],-[LCRule],-[Tree])
%
%	check if a CN-Rule can be applied, commit to the chosen checkCN
%	LCRule: either a bR,cR,pre(bR,bR),pre(bR,cR),pre(bR,aR),pre(cR,bR),pre(cR,cR),pre(cR,aR)
%	NB: LINK(X,Y)-Checkl einfügen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(_,[],_,_,_,_):- !,false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c0-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(bR(W,T,F,(L,R),Ch),[pre(bR(WWs,T,F,(L,R),CWs),ARule)|WSs],LCTree,[DerTree|Trees],[OutRule|WSs],[OutTree|Trees]):- 
	c0(bR(W,T,F,(L,R),Ch),pre(bR(WWs,T,F,(L,R),CWs),ARule),LCTree,DerTree,OutRule,OutTree).
checkCN(cR(W,T,F,(L,R),Ch),[pre(bR(WWs,T,F,(L,R),CWs),ARule)|WSs],LCTree,[DerTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c0(cR(W,T,F,(L,R),Ch),pre(cR(WWs,T,F,(L,R),CWs),ARule),LCTree,DerTree,OutRule,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LCTree,[Tree|Trees],OutWs,OutTree):-
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LCTree,Tree,DeepC1Rule,DeepC1Tree),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c2-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):-
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).

checkCN(LCRule,[WSItem|WSs],LCTree,[Tree|Trees],[DeepItem,WSItem|WSRest],[DeepTree,Tree|Forest]):-
	checkCN(LCRule,WSs,LCTree,[Tree|Trees],[DeepItem|WSRest],[DeepTree|Forest]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	c0(+LCRule,+Pre(B,A),+Tree,+Tree,-LCRule,-Tree)
%
%	applies c0-Rule
%	so: c0(move 1+2) and c0(shift)
%	inherent unification of Prolog should fill in the Variables,
%	otherwise I have to do the unification explicitly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c0(BRule,pre(BRule,ARule),LcTree,WsTree,ARule,OutTree):-
	buildC0Tree(LcTree,WsTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	c1(+Pre(B,A),+Pre(C,B),+Tree,+Tree,-Pre(C,A),-Tree)
%
%	applies c1-Rule
%	inherent unification of Prolog should fill in the Variables,
%	otherwise I have to do the unification explicitly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c1(pre(BRule,ARule),pre(CRule,BRule,CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree):-
	DeepC1Rule = pre(CRule,ARule),
	buildCTree(LcTree,WsTree,DeepC1Tree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	c2(+Pre(C,B),+Pre(B,A),+Tree,+Tree,-Pre(C,A),-Tree)
%
%	applies c2-Rule
%	inherent unification of Prolog should fill in the Variables,
%	otherwise I have to do the unification explicitly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c2(pre(CRule,BRule),pre(BRule,ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree):-
	DeepC2Rule = pre(CRule,ARule),
	buildCTree(WsTree,LcTree,DeepC2Tree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkC1(ARule,WSs,LCTree,Trees,DeepC3Rule,DeepC3Tree)
%
% checks if another Item in the WS fits a c1 for a potential c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkC1(_,[],_,_,_,_):- !,false.
checkC1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,OutRule,OutTree).
checkC1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,OutRule,OutTree).
checkC1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LCTree,[Tree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LCTree,Tree,OutRule,OutTree).
checkC1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,OutRule,OutTree).
checkC1(LCRule,[WSItem|WSs],LcTree,[WsTree|Trees],[DeepItem,WSItem|WSRest],[DeepTree,WsTree|Forest]):-
	checkC1(LCRule,WSs,LcTree,[WsTree|Trees],[DeepItem|WSRest],[DeepTree|Forest]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkC2(ARule,WSs,LCTree,Trees,DeepC3Rule,DeepC3Tree)
%
% checks if another Item in the WS fits a c2 for a potential c3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkC2(_,[],_,_,_,_):- !,false.
checkC2(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,OutRule,OutTree).
checkC2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,OutRule,OutTree).
checkC2(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,OutRule,OutTree).
checkC2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],[OutRule|WSs],[OutTree|Trees]):-
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,OutRule,OutTree).
checkC2(LCRule,[WSItem|WSs],LcTree,[WsTree|Trees],[DeepItem,WSItem|WSRest],[DeepTree,WsTree|Forest]):-
	checkC2(LCRule,WSs,LcTree,[WsTree|Trees],[DeepItem|WSRest],[DeepTree|Forest]).
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildCTree(+Tree,+Tree,-Tree)
%
% builds a tree out of two trees that came from a prediction each
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildCTree(LcTree,WsTree,DeepC1Tree)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildC0Tree(+Tree,+Tree,-Tree)
%
% builds a tree out of a trees that came from a prediction and a normal tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildC0Tree(LcTree,WsTree,OutTree).