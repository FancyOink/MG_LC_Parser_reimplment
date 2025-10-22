:- module(filter,[getLinksFs/2]).
% file: filter.pl
% origin author : J. Kuhn
% origin date: October 2025
% purpose: filters links and feature lists and prepares them for analysis

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

%debugMode.	% comment this line, if debugMode should be off
debugMode:- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getLinksFs(+[Links],-[Feat])
%
% top function to get list of unique features from a list of links
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getLinksFs(Links,Feat):- 
	filterFs(Links,[],FeatIn),
	getUniqueFs(FeatIn,[],Feat),
	(debugMode ->write("unique Fs found: "), writeln(Feat);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filterFs(+[Links],+[Feat],-[Feat])
%
% splits and extracts feature lists from links
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filterFs([],Feat,Feat).
filterFs([link(:,X,Y)|LinkS],FeatIn,FeatOut):-
	filterFs(LinkS,[(:,X),(:,Y)|FeatIn],FeatOut).
filterFs([link(::,X,Y)|LinkS],FeatIn,FeatOut):-
	filterFs(LinkS,[(::,X),(:,Y)|FeatIn],FeatOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getUniqueFs(+[Feat],+[Feat],-[Feat])
%
% check if features lists are present in a list of 
% feature lists and adds them if they are not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getUniqueFs([],Feat,Feat):- (debugMode ->writeln("seen all Fs");true).
getUniqueFs([L|LinkS],FsIn,FsOut):-
	member(L,FsIn),
	(debugMode ->write("found known Fs: "), writeln(L);true),
	getUniqueFs(LinkS,FsIn,FsOut).
getUniqueFs([L|LinkS],FsIn,FsOut):-
	(debugMode ->write("found new Fs: "), writeln(L);true),
	getUniqueFs(LinkS,[L|FsIn],FsOut).
	