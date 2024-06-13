:- module(linker,[linking/1]).
% file: linker.pl
% origin author : J. Kuhn
% origin date: June 2023
% purpose: produces LINKS(X,Y) for a given MG-lexicon

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features

debugMode.	% comment this line, if debugMode should be off
debugMode:- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 Rules to make LINK(B,A):
% 	1. B is left corner of A 
%	2. A contains an initial licensee -f and the first feature of A is +f and LINK(B,move(A)) holds
%	3. B and A being in a transitive closure to rule (1) and (2)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Links: link(T,[Fs_1],[Fs_2])
%		 T	  -> Type of the Item
%		 Fs_1 -> Feature list of the first linking partner
%		 Fs_2 -> Feature list of the second linking partner
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linking(-[links(T,[Fs],[Fs])])
%
% top function of the Linker, calls the functions for the 3 linking rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
linking(NewLinks):-
		linkRule1(LinksR1),
		(debugMode -> write("Links after Rule 1: "),writeln(LinksR1),write("Found "), length(LinksR1,LL1),write(LL1), writeln(" unique Links");true),
		linkRule2(LinksR1,LinksR2),
		(debugMode -> write("Additional Links after Rule 2: "),writeln(LinksR2),write("Found "),length(LinksR2,LL2),write(LL2),writeln(" new unique Links");true),
		list_to_ord_set(LinksR1,LR1),list_to_ord_set(LinksR2,LR2),ord_union(LR1,LR2,NonTransLinks),
		linkRule3(NonTransLinks,NewLinks).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linkRule1(-[link(T,[Fs],[Fs])])
%
% function for first linking rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
linkRule1(LinksR1):-
		setof(('::',FsW),W^(W :: FsW),FsL1),list_to_ord_set(FsL1,L),
		(debugMode -> write("List of unique Fs from LI: "), writeln(L);true),
		tryMergLinks(L,L,LinksR1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linkRule2(+[link(T,[Fs],[Fs])],-[link(T,[Fs],[Fs])])
%
% function for second linking rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
linkRule2([],[]).
linkRule2([LinksR1|LinksR1R],LinksR2):-
		movFunc(LinksR1,LinksMove),linkRule2(LinksR1R,DeeperLinks),
		list_to_ord_set(LinksMove,L1), list_to_ord_set(DeeperLinks,L2),ord_union(L1,L2,LinksR2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% linkRule1(+[link(T,[Fs],[Fs])],-[link(T,[Fs],[Fs])])
%
% function for third linking rule
% IDEE: solange rule 2 und 1 wdh, bis keine neuen Links mehr kommen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
linkRule3(LinksR1,LinksR2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tryMergLinks(+[(T,Fs)],+[(T,Fs)],-[link(T,[Fs],[Fs])])
%
% tries the MG-Merge-Rules against a list of Features and produces LINK(B,A)
% L is the whole list of possible Features and stays constant though the linking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tryMergLinks([],_,[]).
tryMergLinks([FsB|FsR],L,LinksR1):-
		mergFunc(FsB,L,LinksMerg),tryMergLinks(FsR,L,DeeperLinks),
		list_to_ord_set(LinksMerg,L1), list_to_ord_set(DeeperLinks,L2),ord_union(L1,L2,LinksR1).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mergFunc(+(T,Fs),+[(T,Fs)],-[link(T,[Fs],[Fs])])
%
% makes Links for Merge-Rules depending on the Feature-lists provided
% NB: nachdenken, ob ich hier schon DI zulasse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mergFunc(_,[],[]).
mergFunc(FsB			,[(_ ,[])|FsR],Links)		:- mergFunc(FsB,FsR,Links).		
mergFunc(('::',[ F]	   ),[(_,[=F|_]) |FsR],Links)	:- mergFunc(('::',[F]),FsR,DeeperLinks),
		%Links = [link('::',[F],Gamma)														 |DeeperLinks].
		Links = DeeperLinks.	% um es an Stanojevic anzugleichen, kein merge1 mit X = C. Muss trotzdem abgefangen werden, um Y=[] zu vermeiden
mergFunc(('::',[ F|Delta]),[(T2,[=F|Gamma])|FsR],Links)	:- mergFunc(('::',[ F|Delta]),FsR,DeeperLinks),
		Links = [link('::',[ F|Delta],Gamma),link('::',[F|Delta ],Delta),link(T2,[=F|Gamma],Delta)|DeeperLinks].
mergFunc(('::',[=F|Gamma]),[(_,[ F	   ])|FsR],Links) 	:- mergFunc(('::',[=F|Gamma]),FsR,DeeperLinks),
		Links = [link('::',[=F|Gamma],Gamma)						 |DeeperLinks].
mergFunc(('::',[=F|Gamma]),[(T2,[ F|Delta])|FsR],Links) :- mergFunc(('::',[=F|Gamma]),FsR,DeeperLinks),
		Links = [link('::',[=F|Gamma]	,Gamma),link('::',[=F|Gamma]	,Delta),link(T2,[ F|Delta],Gamma)|DeeperLinks].
mergFunc(FsB			,[(_ ,[_ |Gamma])|FsR],Links):- mergFunc(FsB,[(':',Gamma)|FsR],Links).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% movFunc(+link(T,[Fs],[Fs],-[link(T,[Fs],[Fs])])
%
% makes Links for Move-Rules depending on the Links provided
% NB: nachdenken, ob ich hier schon DI zulasse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
movFunc(link('::',[H,-F],[+F|Gamma]),[link('::',[H,-F],Gamma),link(':',[-F],Gamma)]).
movFunc(link(':',[-F],[+F|Gamma]),[link(':',[-F],Gamma)]).
movFunc(link('::',[H,-F|Delta],[+F|Gamma]),[link('::',[H,-F|Delta],Gamma),link(':',[-F|Delta],Gamma)]).
movFunc(link(':',[-F|Delta],[+F|Gamma]),[link(':',[-F|Delta],Gamma)]).
movFunc(link('::',[H,+F|Gamma],[-F]),[link('::',[H,+F|Gamma],Gamma),link(':',[+F|Gamma],Gamma)]).
movFunc(link(':',[+F|Gamma],[-F]),[link(':',[+F|Gamma],Gamma)]).
movFunc(link('::',[H,+F|Gamma],[-F|Delta]),[link('::',[H,+F|Gamma],Gamma),link('::',[H,+F|Gamma],Delta),link(':',[+F|Gamma],Gamma)]).
movFunc(link(':',[+F|Gamma],[-F|Delta]),[link(':',[+F|Gamma],Gamma),link(':',[+F|Gamma],Delta)]).
movFunc(_,[]).