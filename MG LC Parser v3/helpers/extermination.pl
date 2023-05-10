% file: extermination.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: includes epsilon-LI into eta-LI

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features


debugMode.	% comment this line, if debugmode shouldbe off
debugMode:- false.


exterminate :- 	setof(li([W],FsW,(FsW,[])),([W]::FsW),EtaLi),findall(epsLi(FsE,clean),([]::FsE),EpsLi),
				combFeatures(EtaLi,EpsLi,NewEtaLi,MarkEpsLi),	% 1. step of extermination
				(debugMode -> writeln("Eta: " + EtaLi),writeln("Epsilon: " + EpsLi),writeln("New Eta: " + NewEtaLi),writeln("New Epsilon: " + MarkEpsLi);true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% combFeatures(+[Eta_Li],+[Epsilon_Li],-[New_Eta_Li],-[Marked_Eta_Li])
%	 1. step epsilon-extermination:
%		Combine all negative Features of an eta-Li with all positive Feature of a all possible epsilon-Li, such that all Features match in order
%			a. Create thus a new eta-Li with the exponent and the positive Features of the original eta-Li and the neagitve Features of the used epsilon-Li, If it does not already exists
%			b. Save the original Feature-list of the eta-Li and add the used epsilon-Li to the lsit of epsilon-Lis
%			c. Mark the used epsilon-Li in the list of epsilon-Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combFeatures(EtaLi,[],EtaLi,[]).
combFeatures([],EpsLi,[],EpsLi):- debugMode -> writeln("No Eta-Li found"); true. 
combFeatures([EtaLi|RestEtaLi],EpsLi,NewEtaLis,MarkEpsLis) :- checkFeatEtaEps(EtaLi,EpsLi,FitEpsLi,NoFitEpsLi), % checks for fitting epsilon-Li
															buildNewEtaLi(EtaLi,FitEpsLi,NewEta), append(FitEpsLi,NoFitEpsLi,NewEpsLi), % build all new possible eta-Li
															combFeatures(RestEtaLi,NewEpsLi,NewRestEtaLi,MarkEpsLis), % recursion
															checkIfNew(NewEta,NewRestEtaLi,NewEtaLis). % NB: ab hier weiter machen  % checks if new eta-Li is realy new, and adds it to the list 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFeatEtaEps(+Eta_Li,+[Epsilon_Li],-[Matching_Epsilon_Li],-[Nonmatching_Epsilon_Li])
% 	checks if any epsilon-Li matches its positve Features with the negative Features of the Eta-Li
% 	and seperates the matching and nonmatching into two different lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkFeatEtaEps(_,[],[],[]).
checkFeatEtaEps(li(_,FsW,_),[epsLi(FsE,Mark)|RestEta],FitEpsLi,NoFitEpsLi) :- splitFeat(FsW,_,NFsW),splitFeat(FsE,PFsE,_),	% split the feature lists into positive and negative features lists
																						( matchFeatLists(PFsE,NFsW) -> Mark = dot, FitEpsLi = [epsLi(FsE,Mark)| RestFitEps], NoFitEpsLi = RestNoFitEps % if the list from the eta-Li and from the epsilon-Li match
																						% this is also the point he epsilon-LI may get marked
																						; FitEpsLi = RestFitEps, NoFitEpsLi = [epsLi(FsE,Mark)|RestNoFitEps]), % if they do not match
																						checkFeatEtaEps(li(_,FsW,_),RestEta,RestFitEps,RestNoFitEps). % recursion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitFeat(+Fs,-PFS,-NFs)
% splits a feature list into positive and negative feature lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitFeat([],[],[]):- debugMode -> writeln("Tried to split empty list.");true.
splitFeat([-_|_],_,_):- (debugMode -> writeln("Feature List not in order! Licensee before Category found!")),false. % should never occure, but to be safe.
splitFeat([+_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!")),false. % should never occure, but to be safe.
splitFeat([=_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!")),false. % should never occure, but to be safe.
splitFeat([=F|Fs],PFs,NFs):- append(PTail,[=F],PFs),splitFeat(Fs,PTail,NFs).
splitFeat([+F|Fs],PFs,NFs):- append(PTail,[+F],PFs),splitFeat(Fs,PTail,NFs).
splitFeat([ F|Fs],[],[ F|Fs]). % the category is the first negative Feature in a list. If we found it, everything before is a positive feature and everything after a negative feature
splitFeat([ F|Fs],[],[ F|Fs]). % the category is the first negative Feature in a list. If we found it, everything before is a positive feature and everything after a negative feature

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchFeatLists(+PFs,+NFs)
% checks if a positive and a negative feature list match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatLists([],[]). % all matched
matchFeatLists(_,[]):- false. % not matching, different lengths
matchFeatLists([],_):- false. % not matching, different lengths
matchFeatLists([+A|PFs],[-A|NFs]):- matchFeatLists(PFs,NFs).
matchFeatLists([=A|PFs],[ A|NFs]):- matchFeatLists(PFs,NFs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildNewEtaLi(+EtaLi,+[EpsLi],-[NewEta])
% makes as many new eta-Lis as are epsilon-Li in [EpsLi]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildNewEtaLi(EtaLi,[],[EtaLi]).
buildNewEtaLi(li([W],FsW,(FsH,epsHist)),[epsLi(FsE,Mark)|EpsRest],NewEtaLi):- buildNewEtaLi(li([W],FsW,(FsH,epsHist)),EpsRest,DeepNewEtaLi), % recursion first to let the list of new LI grow from smalles point onward for less checking 
																splitFeat(FsW,PFsW,_),splitFeat(FsE,_,NFsE),	%split the Feature lists of the eta- and epsilon-Li
																append(PFsW,NFsW,NewFsW),	% combine the positive feature of the eta-Li and the negative feature of the epasilon-Li together
																append(epsHist,[epsLi(FsE,Mark)],newEpsHist),	% add the used epsilon-Li to the history of epsilon-Li of the eta-Li
																NewEtaLi = [li([W],NewFsW,(FsH,newEpsHist))|DeepNewEtaLi].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchFeatures(+pos_Feature,+neg_Feature)
%
% This function gives checks, if the feature match or not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatures(+A,-A).
matchFeatures(-A,+A).
matchFeatures(=A, A).
matchFeatures( A, =A).
matchFeatures(_,_,):-false.