% file: extermination.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: includes epsilon-LI into eta-LI

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features


debugMode.	% comment this line, if debugMode should be off
debugMode:- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Lexical Items(Li) are constructed as follows
%	Eta-Li: li([W],Fs,(FsO,EpsHist))
%			W -> Exponent
%			Fs -> Feature List of the current eta-Li
%			FsO -> Feature Lsit of the original Li from the lexicon
%			EpsHist -> a List of Lists containing all found epsilon-Li to construct the eta-Li from the original eta-Li and epsilon-Li
%	Epsilon-Li: epsLi(FsE,Mark)
%			FsE -> Feature List of the epsilon-Li
%			Mark -> The mark if the eta-Li was used in a step from the extermination or not. "clean" if not", "dot" if yes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exterminate :- 	setof(li([W],FsW,(FsW,[[]])),([W]::FsW),EtaLi),findall(epsLi(FsE,clean),([]::FsE),EpsLi),	% get all Li from the Lexicon and build new forms for them
				combFeatures(EtaLi,EpsLi,NewEtaLi,MarkEpsLi),	% 1. step of extermination
				(debugMode -> length(EtaLi,Leta),length(NewEtaLi,LNewEta), write("#Eta: "), write(Leta),write( " #NewEta: "), writeln( LNewEta)),
				(debugMode -> writeln("Eta: " + EtaLi),writeln("Epsilon: " + EpsLi),writeln("New Eta: " + NewEtaLi),writeln("New Epsilon: " + MarkEpsLi);true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 1. Step after here
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% combFeatures(+[Eta_Li],+[Epsilon_Li],-[New_Eta_Li],-[Marked_Eta_Li])
%	
%	top level function of the first step
%	 1. step epsilon-extermination:
%		Combine all negative Features of an eta-Li with all positive Feature of a all possible epsilon-Li, such that all Features match in order
%			a. Create thus a new eta-Li with the exponent and the positive Features of the original eta-Li and the neagitve Features of the used epsilon-Li, If it does not already exists
%			b. Save the original Feature-list of the eta-Li and add the used epsilon-Li to the lsit of epsilon-Lis
%			c. Mark the used epsilon-Li in the list of epsilon-Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combFeatures(EtaLi,[],EtaLi,[]).
combFeatures([],EpsLi,[],EpsLi):- debugMode -> writeln("No Eta-Li found"); true. 
combFeatures([EtaLi|RestEtaLi],EpsLi,NewEtaLis,MarkEpsLis) :- checkFeatEtaEps(EtaLi,EpsLi,FitEpsLi,NoFitEpsLi), % checks for fitting epsilon-Li
															(debugMode -> write("Fitting epsilon-Li: "), writeln(FitEpsLi)),
															buildNewEtaLi(EtaLi,FitEpsLi,NewEta), append(FitEpsLi,NoFitEpsLi,NewEpsLi), % build all new possible eta-Li
															(debugMode -> write("build new eta-Li: "),writeln(NewEta)),
															combFeatures(RestEtaLi,NewEpsLi,NewRestEtaLi,MarkEpsLis), % recursion (we need to go DEEPER!)
															checkIfNew(NewEta,NewRestEtaLi,NewEtaLis). % checks if new eta-Li is realy new, and adds it to the list 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFeatEtaEps(+Eta_Li,+[Epsilon_Li],-[Matching_Epsilon_Li],-[Nonmatching_Epsilon_Li])

% 	checks if any epsilon-Li matches its positve Features with the negative Features of the Eta-Li
% 	and seperates the matching and nonmatching into two different lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFeatEtaEps(_,[],[],[]).
checkFeatEtaEps(li(_,FsW,_),[epsLi(FsE,Mark)|RestEta],FitEpsLi,NoFitEpsLi) :- splitFeat(FsW,_,NFsW),splitFeat(FsE,PFsE,_),%(debug -> write("Neg-Feat W: "), writeln(NFsW), write("Pos-Feat E: "), writeln(PFsE)),	% split the feature lists into positive and negative features lists
																						( matchFeatLists(PFsE,NFsW) -> FitEpsLi = [epsLi(FsE,dot)| RestFitEps], NoFitEpsLi = RestNoFitEps % if the list from the eta-Li and from the epsilon-Li match
																						% this is also the point he epsilon-LI may get marked
																						; FitEpsLi = RestFitEps, NoFitEpsLi = [epsLi(FsE,Mark)|RestNoFitEps]), % if they do not match
																						checkFeatEtaEps(li(_,FsW,_),RestEta,RestFitEps,RestNoFitEps). % recursion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitFeat(+Fs,-PFS,-NFs)

% 	splits a feature list into positive and negative feature lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
splitFeat([],[],[]):- debugMode -> writeln("Tried to split empty list.");true.
splitFeat([-_|_],_,_):- (debugMode -> writeln("Feature List not in order! Licensee before Category found!")),false. % should never occure, but to be safe.
splitFeat([+_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!")),false. % should never occure, but to be safe.
splitFeat([=_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!")),false. % should never occure, but to be safe.
splitFeat([=F|Fs],[=F|PTail],NFs):- splitFeat(Fs,PTail,NFs).
splitFeat([+F|Fs],[+F|PTail],NFs):- splitFeat(Fs,PTail,NFs).
splitFeat([ F|Fs],[],[ F|Fs]). % the category is the first negative Feature in a list. If we found it, everything before is a positive feature and everything after a negative feature

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchFeatLists(+PFs,+NFs)

% 	checks if a positive and a negative feature list match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatLists([],[]). % all matched
matchFeatLists(_,[]):- false. % not matching, different lengths
matchFeatLists([],_):- false. % not matching, different lengths
matchFeatLists([+A|PFs],[-A|NFs]):- matchFeatLists(PFs,NFs).
matchFeatLists([=A|PFs],[ A|NFs]):- matchFeatLists(PFs,NFs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildNewEtaLi(+EtaLi,+[EpsLi],-[NewEta])

% 	makes as many new eta-Lis as are epsilon-Li in [EpsLi]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildNewEtaLi(EtaLi,[],[EtaLi]).
buildNewEtaLi(li([W],FsW,(FsH,EpsHist)),[epsLi(FsE,Mark)|EpsRest],NewEtaLi):- buildNewEtaLi(li([W],FsW,(FsH,EpsHist)),EpsRest,DeepNewEtaLi), % recursion first to let the list of new LI grow from smalles point onward for less checking 
																splitFeat(FsW,PFsW,_),splitFeat(FsE,_,NFsE),	%split the Feature lists of the eta- and epsilon-Li
																append(PFsW,NFsE,NewFsW),	% combine the positive feature of the eta-Li and the negative feature of the epasilon-Li together
																addHistory(EpsHist,[epsLi(FsE,Mark)],NewEpsHist),	% add the used epsilon-Li to the history of epsilon-Li of the eta-Li
																NewEtaLi = [li([W],NewFsW,(FsH,NewEpsHist))|DeepNewEtaLi].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkIfNew(+[Eta_Li],+[Eta_Li],-[New_Eta_Lis])

% 	checks if the first list of eta-Li is already occuring in the second part
% 	If not, they get added to the second list.
% 	If so, their epsilon-history gets added to the already existing ones (if THAT is new)
% 	regardless of occurence, eta-LI WITHOUT epsilon-history are prefered, regardless of origin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkIfNew(EtaLi,[],EtaLi). % only one LI left -> must be new
checkIfNew([],EtaLi,EtaLi). % all eta-Li checked or no eta-Li were provided
checkIfNew([HeadLi|RestLi],EtaList2,NewEtaLis):- checkHeadLi(HeadLi,EtaList2,CheckedEtaList),checkIfNew(RestLi,CheckedEtaList,NewEtaLis). % check the first eta-Li and than the rest

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkHeadLi(+Eta_Li,+[Eta_Lis],-[Checked_Eta_Lis])
%
%	checkes if a eta-Li already occurs in a list of eta-Lis
%	NB: Gleiches Ziel, unterschiedlicher Ursprung -> history muss anders aufgebaut werden.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkHeadLi(EtaLi,[],[EtaLi]).% no Eta-Li matched, so it must be new
checkHeadLi(li([W],Fs,(FsH,EpsHist1)),[li([W],Fs,(FsH,EpsHist2))|RestEta2],[li([W],Fs,(FsH,EpsHistCheck))|RestEta2]):- 	checkHistory(EpsHist1,EpsHist2,EpsHistCheck).% same exponent and same feature list -> not New, but epsilon history may be 
checkHeadLi(li([W1],Fs1,(FsH1,EpsHist1)),[li([W2],Fs2,(FsH2,EpsHist2))|RestEta2],[li([W2],Fs2,(FsH2,EpsHist2))|CheckedEta]):-	checkHeadLi(li([W1],Fs1,(FsH1,EpsHist1)),RestEta2, CheckedEta).% same exponent and different feature list -> maybe new, check rest of eta list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkHistory(+[[EpsHist1]],+[[EpsHist2]],-[[EpsHistCheck]])
%
%	compares two list of epsilon list, if members of the first one occur in the second one
%	If it does, it checks the next one from the first list
%	If it does not, but one of the eta Lists is empty, the output is the empty list
%	If it does not, it inserts the list of epsilon into the second one
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkHistory([[]],_,[[]]).
checkHistory(_,[[]],[[]]).
checkHistory([],EpsList,EpsList).
checkHistory([HeadList|RestList],Hist2,NewHist):- (member(HeadList,Hist2) -> CheckedHist = Hist2; CheckedHist = [HeadList|Hist2]),checkHistory(RestList,CheckedHist,NewHist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addHistory(+[[Eps_Hist]],+[eps_Li],-[[NewEpsHist]])
%
%	adds a new eps-Li to all history Lists 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addHistory([[]],[EpsLi],[[EpsLi]]).
addHistory([[EpsList]],[EpsLi],[[NewEpsList]]):- append(EpsList,[EpsLi],NewEpsList).
addHistory([HeadList|RestList],[EpsLi],[NewHeadList|NewRestList]):- append(HeadList,[EpsLi],NewHeadList),addHistory(RestList,[EpsLi],NewRestList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2. Step after here
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchFeatures(+pos_Feature,+neg_Feature)
%
% 	This function gives checks, if the feature match or not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatures(+A,-A).
matchFeatures(-A,+A).
matchFeatures(=A, A).
matchFeatures( A, =A).
matchFeatures(_,_):-false.