:- module(workspace,[workSpace/2]).
% file: workspace.pl
% origin author : J. Kuhn
% origin date: Oktober 2025
% purpose: takes a derivation tree with only LIs at the leafs and calculates the knots and root

%debugMode.	% comment this line, if debugMode should be off
debugMode:- false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge   -> tree([([W],[Fs])],Subtree1,Subtree2)
% move    -> tree([([W],[Fs])],empty   ,Subtree)
% LexItem ->   li( [W],  [Fs])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% workSpace(+Tree,-Tree)
%
% top most function of the workspace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
workSpace(InTree,OutTree):-
	generateLoop(InTree,OutTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generateLoop(+Tree,-Tree)
%
% goes through the tree and stops at the leafs,
% to than apply the MG-Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateLoop(li(W,F),li(W,F)). % special case with tree only being one knot
generateLoop(tree(_,empty,RightTree),OutTree):-
	(debugMode->writeln("generateLoop: Found Move");true),
	(debugMode->writeln("generateLoop: going deeper");true),
	generateLoop(RightTree,OutRight),
	(debugMode->write("generateLoop: Tree: "),writeln(OutRight);true),
	makeMGMove(OutRight,OutTree).
generateLoop(tree(_,li(WB,FsB),li(WC,FsC)),OutTree):-
	(debugMode->writeln("generateLoop: Found LI/LI"),write("generateLoop: LI-B: "),writeln(li(WB,FsB)),
		write("generateLoop: LI-C: "),writeln(li(WC,FsC));true),
	makeMGMerge(li(WB,FsB),li(WC,FsC),OutTree).
generateLoop(tree(_,li(WB,FsB),RightTree),OutTree):-
	(debugMode->writeln("generateLoop: Found LI/Tree");true),
	generateLoop(RightTree,OutRight),
	(debugMode->write("generateLoop: LI-B: "),writeln(li(WB,FsB));true),
	(debugMode->write("generateLoop: Tree-C: "),writeln(OutRight);true),
	makeMGMerge(li(WB,FsB),OutRight,OutTree).
generateLoop(tree(_,LeftTree,li(WC,FsC)),OutTree):-
	(debugMode->writeln("generateLoop: Found Tree/LI");true),
	generateLoop(LeftTree,OutLeft),
	(debugMode->write("generateLoop: Tree-B: "),writeln(OutLeft);true),
	(debugMode->write("generateLoop: LI-C: "),writeln(li(WC,FsC));true),
	makeMGMerge(OutLeft,li(WC,FsC),OutTree).
generateLoop(tree(_,LeftTree,RightTree),OutTree):-
	(debugMode->writeln("generateLoop: Found Tree/Tree");true),
	generateLoop(LeftTree,OutLeft),
	(debugMode->write("generateLoop: Tree-B: "),writeln(OutLeft);true),
	generateLoop(RightTree,OutRight),
	(debugMode->write("generateLoop: Tree-C: "),writeln(OutRight);true),
	makeMGMerge(OutLeft,OutRight,OutTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeMGMerge(+Tree/+LI,+Tree/+LI,-Tree)
%
% applies the MG-Merge-rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
makeMGMerge(li(WB,[=F|FsB]),li(WC,[F]),OutTree):-	%merge 1	LI/LI
	append(WB,WC,WA),
	OutTree = tree([(WA,FsB)],li(WB,[=F|FsB]),li(WC,[F])).
makeMGMerge(li(WB,[=F|FsB]),tree([(WC,[F])|ChainC],LeftTreeC,RightTreeC),OutTree):-	%merge 1	LI/Tree
	append(WB,WC,WA),
	OutTree = tree([(WA,FsB)|ChainC],li(WB,[=F|FsB]),tree([(WC,[F])|ChainC],LeftTreeC,RightTreeC)).
makeMGMerge(tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),li(WC,[F]),OutTree):-	%merge 2	Tree/LI
	append(WC,WB,WA),
	OutTree = tree([(WA,FsB)|ChainB],tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),li(WC,[F])).
makeMGMerge(tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),tree([(WC,[F])|ChainC],LeftTreeC,RightTreeC),OutTree):-	%merge 2	Tree/Tree
	append(WC,WB,WA),
	append(ChainB,ChainC,ChainA),
	OutTree = tree([(WA,FsB)|ChainA],tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),tree([(WC,[F])|ChainC],LeftTreeC,RightTreeC)).
makeMGMerge(li(WB,[=F|FsB]),li(WC,[F|FsC]),OutTree):-	%merge 3	LI/LI
	OutTree = tree([(WB,FsB),(WC,FsC)],li(WB,[=F|FsB]),li(WC,[F|FsC])).
makeMGMerge(li(WB,[=F|FsB]),tree([(WC,[F|FsC])|ChainC],LeftTreeC,RightTreeC),OutTree):-	%merge 3	LI/Tree
	OutTree = tree([(WB,FsB),(WC,FsC)|ChainC],li(WB,[=F|FsB]),tree([(WC,[F|FsC])|ChainC],LeftTreeC,RightTreeC)).
makeMGMerge(tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),li(WC,[F|FsC]),OutTree):-	%merge 3	Tree/LI
	append(ChainB,[(WC,FsC)],ChainA),
	OutTree = tree([(WB,FsB)|ChainA],tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),li(WC,[F|FsC])).
makeMGMerge(tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),tree([(WC,[F|FsC])|ChainC],LeftTreeC,RightTreeC),OutTree):-	%merge 3	Tree/Tree
	append(ChainB,[(WC,FsC)|ChainC],ChainA),
	OutTree = tree([(WB,FsB)|ChainA],tree([(WB,[=F|FsB])|ChainB],LeftTreeB,RightTreeB),tree([(WC,[F|FsC])|ChainC],LeftTreeC,RightTreeC)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeMGMove(+Tree,-Tree)
%
% applies the MG-Move-rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
makeMGMove(tree([Head|ChainB],LeftTreeB,RightTreeB),OutTree):-	%move 1
	makeHeadMove(Head,ChainB,NewHead,NewChain),
	OutTree = tree([NewHead|NewChain],empty,tree([Head|ChainB],LeftTreeB,RightTreeB)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeHeadMove(+(W,Fs),+[(W,Fs)],-(W,Fs),-[(W,Fs)]),
%
% goes through the chain and searches the maching chain link
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
makeHeadMove(_,[],_,_):- writeln("makeHeadMove: Error! Empty chain!").
makeHeadMove((WB,[+F|FsB]),[(WC,[-F])|Chain],(WA,FsB),Chain):-	%move 1
	append(WC,WB,WA).
makeHeadMove((WB,[+F|FsB]),[(WC,[-F|FsC])|Chain],(WB,FsB),[(WC,FsC)|Chain]).	%move 2
makeHeadMove(Head,[ChainHead|Chain],NewHead,[ChainHead|NewChain]):-
	makeHeadMove(Head,Chain,NewHead,NewChain).
