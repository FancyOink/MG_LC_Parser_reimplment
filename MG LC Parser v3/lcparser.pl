:- module(lcparser,[lcParse/2]).
% file: lcparser.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: takes a list of tokens and a lexicon and generates a fitting derivation tree
debugMode.	% comment this line, if debugMode should be off
debugMode:- false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: 
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
	loop(0,Tokens,[],[],[],OutWs,Tree),
	(debugMode ->write("lcParse: Output: "),writeln(OutWs),
		write("lcParse: Tree: "),writeln(Tree);true).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loop(+Position,+[Tokens],+[WS],+[Trees],-[Tokens],-[WS],-[Trees])
%	
% main parse function. handles the shifting of new LI. Top most logic of the parse. Every backtrack should jump to here.
% Finished Parse-Tree at the "Bottom"
% Position is the current left most position of the string with regards to the original input string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(_,[],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])],[],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])]). % special end condition for Tree = 1 leaf
loop(_,[],[cR(W,:,[cfin],Pos,[])],Tree,[],[cR(W,:,[cfin],Pos,[])],Tree). % normal end condition
loop(Pos,Input,Ws,InTree,OutPut,OutWs,OutTree):-
	(debugMode->writeln(" "),write("loop: current Input: "),writeln(Input);true),
	(debugMode->write("loop: current Ws: "),writeln(Ws);true),
	(debugMode->write("loop: current Tree: "),writeln(InTree);true),
	shift(Pos,Input,NewPos,RestPut,WsRule,LI),	
	(debugMode->write("loop: new Ws-Rule: "),writeln(WsRule),
	write("loop: new Input: "),writeln(RestPut);true),
	checkCN(WsRule,Ws,LI,InTree,DWs,DTree),% check if something clicks with WS;
	(debugMode->write("loop: new Ws: "),writeln(DWs);true),
	(debugMode->write("loop: new Tree: "),writeln(DTree);true),
	parseF(RestPut,DWs,DTree,ParPut,InterWs,InterTree),
	loop(NewPos,ParPut,InterWs,InterTree,OutPut,OutWs,OutTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parseF(+[Tokens],+[WS],+[Trees],-[Tokens],-[WS],-[Trees])
%	
% lc-parse function except shift. 
% Finished Parse-Tree at the "Bottom"
% NB: nachdenken ob ich die parseF-loop wirklich haben möchte
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseF([],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])],[],[cR(W,::,[cfin],Pos,[])],[li(W,[cfin])]). % special end condition for Tree = 1 leaf
parseF([],[cR(W,:,[cfin],Pos,[])],Tree,[],[cR(W,:,[cfin],Pos,[])],Tree). % normal end condition
parseF(Input,[bR(W,T,Fs,PosB,Chain)|Ws],InTree,OutPut,OutWs,OutTree):- 
	(debugMode->write("parseF: current Ws: "),writeln([bR(W,T,Fs,PosB,Chain)|Ws]);true),
	(debugMode->write("parseF: current Tree: "),writeln(InTree);true),
	lc1([bR(W,T,Fs,PosB,Chain)|Ws],InTree,[LCRule|InterWs],[LCTree|InterTree]), % LC-Rules 1
	checkCN(LCRule,InterWs,LCTree,InterTree,DWs,DTree),% check if something clicks with WS;
	parseMoloop(Input,DWs,DTree,OutPut,OutWs,OutTree).	%check if more lc(move)-Rules may aply
parseF(Input,[cR(W,T,Fs,PosC,Chain)|Ws],InTree,OutPut,OutWs,OutTree):- 
	(debugMode->write("parseF: current Ws: "),writeln([cR(W,T,Fs,PosC,Chain)|Ws]);true),
	(debugMode->write("parseF: current Tree: "),writeln(InTree);true),
	lc2([cR(W,T,Fs,PosC,Chain)|Ws],InTree,[LCRule|InterWs],[LCTree|InterTree]), % LC-Rules 2
	checkCN(LCRule,InterWs,LCTree,InterTree,DWs,DTree),% check if something clicks with WS;
	parseMoloop(Input,DWs,DTree,OutPut,OutWs,OutTree).	%check if more lc(move)-Rules may aply

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parseMoloop(+[Tokens],+[WS],+[Trees],-[Tokens],-[WS],-[Trees])
%	
% loop for the parseF-Function to catch several moves un succession
% NB: Anpassen dass nur Moves abgearbeitet werden, und der Rest durchkommt
%	  illegale Moves führen zu einem Fail
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseMoloop(Input,[bR(W,T,Fs,PosB,Chain)|Ws],InTree,OutPut,OutWs,OutTree):- 
	parseF(Input,[bR(W,T,Fs,PosB,Chain)|Ws],InTree,OutPut,OutWs,OutTree).
parseMoloop(Input,[cR(W,T,Fs,PosC,Chain)|Ws],InTree,OutPut,OutWs,OutTree):- 
	parseF(Input,[cR(W,T,Fs,PosC,Chain)|Ws],InTree,OutPut,OutWs,OutTree).
parseMoloop(In,[bR(W,T,[=F|Fs],PosB,Chain)|Ws],Tree,In,[bR(W,T,[=F|Fs],PosB,Chain)|Ws],Tree).
parseMoloop(In,[bR(W,T,[+F|Fs],PosB,[C|Chain])|Ws],Tree,In,[bR(W,T,[+F|Fs],PosB,[C|Chain])|Ws],Tree).
parseMoloop(In,[cR(W,T,[F|Fs],PosB,Chain)|Ws],Tree,In,[cR(W,T,[F|Fs],PosB,Chain)|Ws],Tree):-
	checkCat(F).
parseMoloop(In,[pre(B,A)|Ws],Tree,In,[pre(B,A)|Ws],Tree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shift(+[Token],-[Token],-[WSRule],-[LI])
% 
% shifted the first Token of a list out and returns a corresponding LI
% NB: 	- Vieleicht in Zukunft eine Präferenz für spezielle Fs einbauen
%		- oder den Shift durch den Workspace vorsortieren
%		- shift von epsilon-LI erlauben
%		- Auswahl des LI implementieren -> LINKS? Anfangsbedingung? etc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shift(Pos,[H|HRs],NewPos,HRs,WsRule,LI):- 
	(debugMode->write("shift: current position: "),writeln(Pos);true),
	(debugMode->write("shift: shifting: "),writeln(H);true),
	(debugMode->write("shift: remaining: "),writeln(HRs);true),
	[H] :: Fs, 
	LI = li([H],Fs),
	(debugMode->write("shift: LI: "),writeln(LI);true),
	NewPos is (Pos + 1),
	(debugMode->write("shift: new position: "),writeln(NewPos);true),
	makeWsRule(Pos,NewPos,LI,WsRule).

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
% lc1(+[WS],+[Tree],-[Tokens],-[WS],-[Tree])
% 
%		-lc1(R); R = MG-Rule
%		=> given R= R1 and a B is in WS: replace B for C -> A
%			R1: - merge 1
%				- merge 2
%				- merge 3
%		=> given R = R2 and a B: replace B for A
%			R2:		- move 1
%					- move 2
%	NB:
%		- nachdenken, ob SMC check nicht bei merge 3 sein sollte
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lc1([bR(S,::,[=F|FsS],(LB,RB),BChain)|Ws],InTree,OutWs,OutTree):- 
	(debugMode->writeln("lc1: merge1");true),
	%checkLink(pre(cR(T,Dot,[F],(RB,RC),CChain),ARule)),
	append(S,T,ST),checkFsRule(ST,:,FsS,(LB,RC),BChain,ARule),
	OutWs = [pre(cR(T,_Dot,[F],(RB,RC),_CChain),ARule)|Ws],% merge 1
	(debugMode->write("lc1(me1): new WS: "),writeln(OutWs);true),
	buildTree(lcMerge1,InTree,OutTree),
	(debugMode->write("lc1(me1): new Tree: "),writeln(OutTree);true).
lc1([bR(S,:,[=F|FsS],(_,RB),BChain)|Ws],InTree,OutWs,OutTree):- 	% do/can we ever use this case?
	(debugMode->writeln("lc1: merge2");true),
	checkLink(pre(cR(T,Dot,[F],(LC,RC),CChain),ARule)),
	append(T,S,TS),checkFsRule(TS,:,FsS,(LC,RB),BChain,ARule),
	OutWs = [pre(cR(T,Dot,[F],(LC,RC),CChain),ARule)|Ws],% merge 2
	(debugMode->write("lc1(me2): new WS: "),writeln(OutWs);true),
	buildTree(lcMerge2,InTree,OutTree),
	(debugMode->write("lc1(me2): new Tree: "),writeln(OutTree);true).
lc1([bR(S,Dot,[=F|FsS],PosB,BChain)|Ws],InTree,OutWs,OutTree):- 
	(debugMode->writeln("lc1(me3): merge3");true),
	checkLink(pre(cR(T,Dot,[F|FsT],PosC,CChain),ARule)),	% check if link exist
	appendExtensibleLists(BChain,chainL(T,[F|FsT],PosC),NewChain), 
	checkFsRule(S,:,FsS,PosB,NewChain,ARule),
	OutWs = [pre(cR(T,Dot,[F|FsT],PosC,CChain),ARule)|Ws], % merge 3
	(debugMode->write("lc1(me3): new WS: "),writeln(OutWs);true),
	buildTree(lcMerge3,InTree,OutTree),
	(debugMode->write("lc1(me3): new Tree: "),writeln(OutTree);true).
lc1([bR(S,:,[+F|FsS],PosB,[BChain|RChain])|Ws],InTree,OutWs,OutTree):- 
	(debugMode->writeln("lc1: move");true),
	checkSMC([BChain|RChain]),!,
	checkMove(bR(S,:,[+F|FsS],PosB,[BChain|RChain]),[BChain|RChain],ARule), % move 1 + 2
	OutWs = [ARule|Ws],
	(debugMode->write("lc1(mo): new WS: "),writeln(OutWs);true),
	buildTree(lcMove,InTree,OutTree),
	(debugMode->write("lc1(mo): new Tree: "),writeln(OutTree);true).

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
lc2([cR(T,_,[ F],(LC,RC),CChain)|Ws],InTree,OutWs,OutTree):- 
	append(T,S,TS),appendExtensibleLists(BChain,CChain,AChain),
	(debugMode->writeln("lc2: merge2");true),
	checkLink(pre(bR(S,:,[=F|FsS],(RC,RB),BChain),aR(TS,:,FsS,(LC,RB),AChain))),
	OutWs = [pre(bR(S,:,[=F|FsS],(RC,RB),BChain),aR(TS,:,FsS,(LC,RB),AChain))|Ws], 	% merge 2, because I do not know if the bR becomes a cR or bR, aR is a placeholder
	(debugMode->write("lc2(me2): new WS: "),writeln(OutWs);true),
	buildTree(lcMerge2,InTree,OutTree),
	(debugMode->write("lc2(me2): new Tree: "),writeln(OutTree);true).
lc2([cR(T,TT,[ F|FsT],PosC,CChain)|Ws],InTree,OutWs,OutTree):- 
	(debugMode->writeln("lc2: merge3");true),
	checkLink(pre(bR(S,Dot,[=F|FsS],PosB,BChain),aR(S,:,FsS,PosB,AChain))),	% check if link exist
	checkLink(pre(bR(S,Dot,[=F|FsS],PosB,BChain),aR(T,TT,FsT,PosC,CChain))),% check with dummy aR for new chain element
	appendExtensibleLists(BChain,[chainL(T,FsT,PosC)|CChain],AChain),
	OutWs = [pre(bR(S,Dot,[=F|FsS],PosB,BChain),aR(S,:,FsS,PosB,AChain))|Ws],	% merge 3, because I do not know if the bR becomes a cR or bR, aR is a placeholder
	(debugMode->write("lc2(me3): new WS: "),writeln(OutWs);true),
	buildTree(lcMerge3,InTree,OutTree),
	(debugMode->write("lc2(me3): new Tree: "),writeln(OutTree);true).
lc2(_,_,_,_) :- !,false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkMove(+bR,+[chainLinks],-Rule)
%
% checks if any Moves can be done (legally)  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* checkMove(_,[],_):-
	(debugMode -> writeln("checkMove: nothing legal found");true),
	!,false. % nothing (legal) found */
checkMove(_,[chainL(_,[],_)|_],_):-
	(debugMode -> writeln("checkMove: no chain feature to move");true),
	!,false. % nothing (legal) found
checkMove(bR(S,:,[+F|FsS],(_,RB),_),[chainL(T,[-F],(LC,_))|Rules],ARule):-
	(debugMode -> writeln("checkMove: move1");true),
	%checkLink(pre(cR(T,:,[-F],(LC,RC),_Dummy),aR(TS,:,FsS,(LB,RB),AChain))),
	append(T,S,TS),checkFsRule(TS,:,FsS,(LC,RB),Rules,ARule). % move 1
checkMove(bR(S,:,[+F|FsS],PosB,AChain),[chainL(T,[-F|FsT],PosC)|Rules],ARule):-
	FsT \= [],
	(debugMode -> writeln("checkMove: move2");true),
	checkLink(pre(cR(T,:,[-F|FsT],PosC,_Dummy),aR(S,:,FsS,PosB,AChain))),
	checkFsRule(S,:,FsS,PosB,[chainL(T,FsT,PosC)|Rules],ARule). % move 2
checkMove(BRule,[CRule|Rules],ARule):-	
	checkMove(BRule,Rules,NewRule),appendExtensibleLists(NewRule,CRule,ARule).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkSMC(Rules)
%
% checks if the SMC is violated 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checkSMC([]):- (debugMode -> writeln("checkSMC: no chain to move");true),!,false.
checkSMC([chainL(_,[-F|_],_),chainL(_,[-F|_])|_]):-!,false.
checkSMC([CR1, CR2|Rules]) :- checkSMC([CR1|Rules]),checkSMC([CR2|Rules]).
checkSMC([_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFsRule(+[String],+Type,[Fs],+Pos,+Chain,-Rule)
%
% checks which rule applies to the Fs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFsRule(W,T,[=F|Fs],Pos,Chain,bR(W,T,[=F|Fs],Pos,Chain)):- \+var(F).
checkFsRule(W,T,[+F|Fs],Pos,Chain,bR(W,T,[+F|Fs],Pos,Chain)):- \+var(F).
checkFsRule(W,T,[-F|Fs],Pos,Chain,cR(W,T,[-F|Fs],Pos,Chain)):- \+var(F).
checkFsRule(W,T,[ F|Fs],Pos,Chain,ARule):- \+var(F),checkCat(F),ARule = cR(W,T,[ F|Fs],Pos,Chain).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addChain(+Rule,+ChainLink,-Rule)
%
% adds a chain link to a rule at the start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addChain(bR(W,Dot,Fs,Pos,Chain),ChainL,bR(W,Dot,Fs,Pos,[ChainL|Chain])).
addChain(cR(W,Dot,Fs,Pos,Chain),ChainL,cR(W,Dot,Fs,Pos,[ChainL|Chain])).
addChain(aR(W,Dot,Fs,Pos,Chain),ChainL,aR(W,Dot,Fs,Pos,[ChainL|Chain])).

% First, given a possibly partially uninstantiated list like
%      L = [A,b,[C|D]|E]
% we define splitExtensibleList so that
%      ?- splitExtensibleList(L,Prefix,Suffix)
% returns something equivalent to the bindings
%      Prefix = [A,b,[C|D]]
%      Suffix = E
% When L = [a|B]
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = [a]
%      Suffix = B
% When given a list that is not "extensible", that is, a list which does
% not have a variable tail, we return the empty tail. When L = []
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = []
%      Suffix = []
% When L = [a,B]
%      ?- splitExtensibleList(L,Prefix,Suffix)
%      Prefix = [a,B]
%      Suffix = []
% In all cases, splitExtensibleList(L,P,S) implies append(P,S,L).
splitExtensibleList(L,Prefix,Suffix) :- var(L), !, Prefix=[], Suffix=L.
splitExtensibleList([H|T],[H|Prefix],Suffix) :- !, splitExtensibleList(T,Prefix,Suffix).
splitExtensibleList([],[],[]).

% Now given 2 extensible lists
%      L0 = [A,b,[C|D]|E]
%      L1 = [f,G,e(f)|H]
% ?- appendExtensibleLists(L0,L1,L) returns something equivalent to the bindings
%      L = [A,b,[C|D],f,G,e(f)|M]
%      M = E = H
% But if either list is extensible, the result is also extensible. So:
% ?- appendExtensibleLists([],L1,L) returns something equivalent to the bindings
%      L1 = L.
% In effect, after evaluating appendExtensibleLists(L0,L1,L),
% L0 and L1 share their extensible part, and so, intuitively,
% no backtracking is needed to extend either L0 or L1.
% Extending L0 is the same as extending L1.
% Since we will only use this when appending Mover lists, we add a check to make
% sure that the instantiated movers do not violate smc ...

appendExtensibleLists(L0,L1,L) :-
    splitExtensibleList(L0,Prefix0,Suffix0),
    splitExtensibleList(L1,Prefix1,Suffix1),
    % the prefixes are NOT extensible, so no backtrack point introduced by:
    append(Prefix0,Prefix1,Prefix),
    % and none of the following should not introduce a backtrack point either:
    ( var(Suffix0), var(Suffix1) -> Suffix0=Suffix1, append(Prefix,Suffix1,L)
    ; var(Suffix0) -> append(Prefix,Suffix1,L2), append(L2,Suffix0,L)
    ; var(Suffix1) -> append(Prefix,Suffix0,L2), append(L2,Suffix1,L)
    ; Suffix0=Suffix1, append(Prefix,Suffix1,L)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Closure-Rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkCN(+LCRule,+[LCRule],+LCTree,+[Tree],-[LCRule],-[Tree])
%
%	check if a CN-Rule can be applied, commit to the chosen checkCN
%	LCRule: either a bR,cR,pre(bR,bR),pre(bR,cR),pre(bR,aR),pre(cR,bR),pre(cR,cR),pre(cR,aR)
%	NB: LINK(X,Y)-Check einfügen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(R,[],Tree,_,[R],[Tree]):- (debugMode->writeln("checkCN: no cN-Rule ");true).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c0-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(bR(W,T,F,(L,R),Ch),[pre(bR(WWs,T,F,(L,R),CWs),ARule)|WSs],LCTree,[DerTree|Trees],[OutRule|WSs],[OutTree|Trees]):- !,
	(debugMode->writeln("checkCN: c0");true),
	c0(bR(W,T,F,(L,R),Ch),pre(bR(WWs,T,F,(L,R),CWs),ARule),LCTree,DerTree,OutRule,OutTree),
	(debugMode->write("checkCN: A: "),writeln(OutRule);true).
checkCN(cR(W,T,F,(L,R),Ch),[pre(cR(WWs,T,F,(L,R),CWs),ARule)|WSs],LCTree,[DerTree|Trees],[OutRule|WSs],[OutTree|Trees]):- !,
	(debugMode->writeln("checkCN: c0");true),
	c0(cR(W,T,F,(L,R),Ch),pre(cR(WWs,T,F,(L,R),CWs),ARule),LCTree,DerTree,OutRule,OutTree),
	(debugMode->write("checkCN: A: "),writeln(OutRule);true).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c1-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c1");true),
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	\+ var(DeepC1Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC1Rule);true),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c1");true),
	c1(pre(bR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(BRule,bR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	\+ var(DeepC1Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC1Rule);true),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,aR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LCTree,[Tree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c1");true),
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LCTree,Tree,DeepC1Rule,DeepC1Tree),
	\+ var(DeepC1Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC1Rule);true),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
checkCN(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),[pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs))|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c1");true),
	c1(pre(cR(WN,T,[ F|FsN],(L,R),CNew),ARule),pre(CRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,DeepC1Rule,DeepC1Tree),
	\+ var(DeepC1Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC1Rule);true),
	(checkC2(DeepC1Rule,WSs,DeepC1Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC1Rule|WSs],
	OutTree = [DeepC1Tree|Trees]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% c2-Rules check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkCN(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c2");true),
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	\+ var(DeepC2Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC2Rule);true),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),[pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c2");true),
	c2(pre(CRule,bR(WN,T,[ F|FsN],(L,R),CNew)),pre(bR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	\+ var(DeepC2Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC2Rule);true),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,aR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c2");true),
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	\+ var(DeepC2Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC2Rule);true),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
	OutWs = [DeepC2Rule|WSs],
	OutTree = [DeepC2Tree|Trees]).
checkCN(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),[pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule)|WSs],LcTree,[WsTree|Trees],OutWs,OutTree):- !,
	(debugMode->writeln("checkCN: c2");true),
	c2(pre(CRule,cR(WN,T,[ F|FsN],(L,R),CNew)),pre(cR(WWs,T,[ F|FsWs],(L,R),CWs),ARule),LcTree,WsTree,DeepC2Rule,DeepC2Tree),
	\+ var(DeepC2Rule),
	(debugMode->write("checkCN: new Pre: "),writeln(DeepC2Rule);true),
	(checkC1(DeepC2Rule,WSs,DeepC2Tree,Trees,OutWs,OutTree),
	(debugMode->writeln("checkCN: c3");true);
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
c0(BRule,pre(BRule,aR(WWs,T,[=F|FsWs],(L,R),CWs)),LcTree,WsTree,bR(WWs,T,[=F|FsWs],(L,R),CWs),OutTree):-
	buildCTree(WsTree,LcTree,OutTree).
c0(BRule,pre(BRule,aR(WWs,T,[+F|FsWs],(L,R),CWs)),LcTree,WsTree,bR(WWs,T,[+F|FsWs],(L,R),CWs),OutTree):-
	buildCTree(WsTree,LcTree,OutTree).
c0(BRule,pre(BRule,bR(WWs,T,[=F|FsWs],(L,R),CWs)),LcTree,WsTree,bR(WWs,T,[=F|FsWs],(L,R),CWs),OutTree):-
	buildCTree(WsTree,LcTree,OutTree).
c0(BRule,pre(BRule,bR(WWs,T,[+F|FsWs],(L,R),CWs)),LcTree,WsTree,bR(WWs,T,[+F|FsWs],(L,R),CWs),OutTree):-
	buildCTree(WsTree,LcTree,OutTree).
c0(BRule,pre(BRule,aR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,cR(WWs,T,[ F|FsWs],(L,R),CWs),OutTree):-
	%checkCat(F),
	buildCTree(WsTree,LcTree,OutTree).
c0(BRule,pre(BRule,cR(WWs,T,[ F|FsWs],(L,R),CWs)),LcTree,WsTree,cR(WWs,T,[ F|FsWs],(L,R),CWs),OutTree):-
	%checkCat(F),
	buildCTree(WsTree,LcTree,OutTree).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	c1(+Pre(B,A),+Pre(C,B),+Tree,+Tree,-Pre(C,A),-Tree)
%
%	applies c1-Rule
%	inherent unification of Prolog should fill in the Variables,
%	otherwise I have to do the unification explicitly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c1(pre(BRule,ARule),pre(CRule,BRule),LcTree,WsTree,DeepC1Rule,DeepC1Tree):-
	DeepC1Rule = pre(CRule,ARule),
	checkLink(pre(CRule,ARule)),
	(debugMode->writeln("checkCN: Link found");true),
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
	checkLink(pre(CRule,ARule)),
	(debugMode->writeln("checkCN: Link found");true),
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
% checkLink(+pre(CRule,ARule))
%
% checks if a link exists for the prediction 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkLink(pre(cR(_,T,FsC,_,_),cR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
checkLink(pre(cR(_,T,FsC,_,_),aR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
checkLink(pre(cR(_,T,FsC,_,_),bR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
checkLink(pre(bR(_,T,FsC,_,_),cR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
checkLink(pre(bR(_,T,FsC,_,_),aR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
checkLink(pre(bR(_,T,FsC,_,_),bR(_,_,FsA,_,_))):- !,
	(debugMode->write("checkLink: checking: link("),write(T),write(","),write(FsC),write(","),write(FsA),writeln(")");true),
	\+ \+ link(T,FsC,FsA).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Building the derivation Tree
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildTree(+MG-Rule,+[Tree],-[Tree])
%
% constructs a tree out of the first element of the input tree list
% gap is a placeholder for a yet unknown string in the exponent and
% gapTree is a placeholder for a yet (partially) unknown subtree,
% to make finding the gap in the tree easier
% NB: 	- ggf. Ketten genauer bestimmen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildTree(lcMerge1,[li(S,[=F|FsS])|Trees],OutTrees):- % For position B = LI
	FsS \= [],
	append(S,[gap],SG),
	OutTrees = [tree([(SG,FsS)|_NewChains],li(S,[=F|FsS]),gapTree)|Trees].	
buildTree(lcMerge2,[tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2)|Trees],OutTrees):- % For position B = DI
	OutTrees = [tree([([gap|S],FsS)|_NewChains],tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2),gapTree)|Trees]. 
buildTree(lcMerge2,[li(T,[ F])|Trees],OutTrees):- % For position C = LI
	checkCat(F),
	append(T,[gap],TG),
	OutTrees = [tree([(TG,_FsS)|_NewChains],gapTree,li(T,[ F]))|Trees].	
buildTree(lcMerge2,[tree([(T,[ F])|BetaChains],LeftTreeC2,LeftTreeC2)|Trees],OutTrees):- % For position C = DI
	checkCat(F),
	append(T,[gap],TG),
	OutTrees = [tree([(TG,_FsS)|_NewChains],gapTree,tree([(T,[ F]|BetaChains)],LeftTreeC2,LeftTreeC2))|Trees]. 
buildTree(lcMerge3,[tree([(S,[=F|FsS]|AlphaChains)],LeftTreeB2,LeftTreeB2)|Trees],OutTrees):- 
	FsS \= [],
	OutTrees = [tree([(S,FsS),([gap],_FsT)|_NewChains],tree([(S,[=F|FsS])|AlphaChains],LeftTreeB2,LeftTreeB2),gapTree)|Trees]. % For Position B = DI
buildTree(lcMerge3,[li(S,[=F|FsS])|Trees],OutTrees):- % For Position B = LI with no other chain links
	OutTrees = [tree([(S,FsS),([gap],_FsT)],li(S,[=F|FsS]),gapTree)|Trees]. 
buildTree(lcMerge3,[li(S,[=F|FsS])|Trees],OutTrees):- % For Position B = LI
	OutTrees = [tree([(S,FsS),([gap],_FsT)|_NewChains],li(S,[=F|FsS]),gapTree)|Trees]. 
buildTree(lcMerge3,[li(T,[ F|FsT])|Trees],OutTrees):-  % For Position C = LI with no other chain links
	checkCat(F),FsT \= [],
	OutTrees = [tree([([gap],_FsS),(T,FsT)],gapTree,li(T,[ F|FsT]))|Trees]. 
buildTree(lcMerge3,[li(T,[ F|FsT])|Trees],OutTrees):-  % For Position C = LI
	checkCat(F),FsT \= [],
	OutTrees = [tree([([gap],_FsS),(T,FsT)|_NewChains],gapTree,li(T,[ F|FsT]))|Trees]. 
buildTree(lcMerge3,[tree([(T,[ F|FsT])|BetaChains],LeftTreeC2,LeftTreeC2)|Trees],OutTrees):- % For Position C = DI
	checkCat(F),FsT \= [],
	OutTrees = [tree([([gap],_FsS),(T,FsT)|_NewChains],gapTree,tree([(T,[ F|FsT])|BetaChains],LeftTreeC2,LeftTreeC2))|Trees]. 
buildTree(lcMove,[tree([FChain|RsChain],LeftTreeB1,RightTreeB1)|Trees],OutTrees):- 
	checkTreeMove(FChain,RsChain,NewChain),
	OutTrees = [tree(NewChain,empty,tree([FChain|RsChain],LeftTreeB1,RightTreeB1))|Trees].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildCTree(+Tree,+Tree,-Tree)
%
% builds a tree out of two trees that came from a prediction each
% the first tree gets the second tree inserted at its gapTree-position
% finding the gapTree works like depth-first search
% NB: nachdenken ob ein anderer Suchansatz besser wäre, z.B. breadth-first
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildCTree(tree(HeadB,gapTree,tree(HeadBR,LeftTreeBR,RightTreeBR)),TreeC,OutTree):-
	adjustRoot(HeadB,TreeC,NewHead),
	OutTree = tree(NewHead,TreeC,tree(HeadBR,LeftTreeBR,RightTreeBR)).
buildCTree(tree(HeadB,gapTree,li(W,F)),TreeC,OutTree):-
	adjustRoot(HeadB,TreeC,NewHead),
	OutTree = tree(NewHead,TreeC,li(W,F)).
buildCTree(tree(HeadB,tree(HeadBR,LeftTreeBR,RightTreeBR),gapTree),TreeC,OutTree):-
	adjustRoot(HeadB,TreeC,NewHead),
	OutTree = tree(NewHead,tree(HeadBR,LeftTreeBR,RightTreeBR),TreeC).
buildCTree(tree(HeadB,li(W,F),gapTree),TreeC,OutTree):-
	adjustRoot(HeadB,TreeC,NewHead),
	OutTree = tree(NewHead,li(W,F),TreeC).
buildCTree(tree(HeadB,LeftTree,RightTree),TreeC,OutTree):-
	(buildCTree(LeftTree,TreeC,DeepLeft),% see if gap is on the left subtree
	adjustRoot(HeadB,DeepLeft,NewHead),
	OutTree = tree(NewHead,DeepLeft,RightTree);
	buildCTree(RightTree,TreeC,DeepRight),% see if gap is on the right subtree
	adjustRoot(HeadB,DeepRight,NewHead),
	OutTree = tree(NewHead,LeftTree,DeepRight)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkTreeMove(+ChainItem,+[ChainItems],-[ChainItems])
%
% checks in the chains for the correct Chain item to move. SMC was already checked beforehand
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checkTreeMove(Head,[],_):-!,write("Error: checkTreeMove: Found no Match for Move for: "), writeln(Head),false. % should never reach
checkTreeMove(Head,[(_,[])|_],_):-!,write("Error: checkTreeMove: Found no Match for Move for: "), writeln(Head),false.
checkTreeMove((S,[+F|FsS]),[(T,[-F])|RsChain],NewChain):- 
	append(T,S,TS), % move 1
	NewChain = [(TS,FsS)|RsChain].
checkTreeMove((S,[+F|FsS]),[(T,[-F|FsT])|RsChain],NewChain):- 
	FsT \= [],% move 2
	NewChain = [(S,FsS),(T,FsT)|RsChain].
checkTreeMove(FChain,[KChain|RsChain],NewChain):- checkTreeMove(FChain,RsChain,[NewFChain|NewRsChain]),
	NewChain = [NewFChain,KChain|NewRsChain].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adjustRoot(+[],+tree,NewHead)
%
% incorporates the new insertion into the root of the (sub-)tree
% NB: denk über Features nochmal nach
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adjustRoot([(WB,F)|ChB],tree([(WC,[_|F]),ChC],_,_),NewHead):-
	insertExp(WB,WC,WA),
	appendExtensibleLists(ChB,ChC,ChA),
	NewHead = [(WA,F)|ChA].
adjustRoot([(WB,F)|ChB],li(WC,[_|F]),NewHead):- 
	insertExp(WB,WC,WA),
	NewHead = [(WA,F)|ChB].
adjustRoot([(WB,F)|ChB],li(WC,[_]),NewHead):- 
	insertExp(WB,WC,WA),
	NewHead = [(WA,F)|ChB].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insertExp(WB,WC,WA)
%
% inserts the second string at the position of the gap-placeholder
% in the first string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%insertExp([gap],WC,WC).
insertExp([gap|Tail],WC,WA):- append(WC,Tail,WA).
insertExp([H|Tail],WC,[H|DeepW]):- insertExp(Tail,WC,DeepW).