:- module(extermination,[exterminate/1]).
% file: extermination.pl
% origin author : J. Kuhn
% origin date: April 2023
% purpose: includes epsilon-LI into eta-LI

:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features


%debugMode.	% comment this line, if debugMode should be off
debugMode:- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Lexical Items(Li) are constructed as follows
%	Eta-Li: li([W],Fs,(FsO,EpsHist))
%			W -> Exponent
%			Fs -> Feature List of the current eta-LI
%			FsO -> Feature List of the original LI from the lexicon
%			EpsHist -> a List of Lists containing all feature lists of found epsilon-LI to construct the eta-Li from the original eta-LI and epsilon-LI
%	Epsilon-Li: epsLi(FsE,Mark)
%			FsE -> Feature List of the epsilon-LI
%			Mark -> The mark if the eta-LI was used in a step from the extermination or not. "clean" if not", "dot" if yes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exterminate
%	TODO: History auf Liste aus Listen ausbauen und EpsLI in Hist ein Herkunfts-LI hinzufügen
%	Top level function of the module and initial round through the extermination algorithm
%	equals step 1,4,5 and 6 as step 2 and 3 need esteblished chains, which are not exisitng at the beginning
% Später einen Rückgabewert hinzufügen, der die Liste an Li ist, die in dieser Funktion erstellt wurden.
% Die Liste soll dann in der Main-Fkt. mit assert "festgesetzt" werden. -> Sonst unständliche Form der Einträge, wegen Modul.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exterminate(NewEtaLi) :- 	
			setof(li([W],FsW,(FsW,[[]])),([W]::FsW),EtaLi),findall(epsLi(FsE,clean),([]::FsE),EpsLi),	% get all Li from the Lexicon and build new forms for them
			combFeatures(EtaLi,EpsLi,CFeatEtaLi,MarkEpsLi),	% 1. step of extermination (Initial round)
			(debugMode -> write("Old Eta: "),writeln(EtaLi),write("Epsilon: "),writeln(EpsLi),write("New Eta: "),writeln(CFeatEtaLi),write("New Epsilon: "),writeln(MarkEpsLi);true),
			createChains(EtaLi,[],MarkEpsLi,[],[],NewChains,NewMarkEps,NewHist), % 4. step of extermination (Initial round)
			(debugMode -> write("Chains: "), writeln(NewChains);true),
			length(EtaLi,Leta),length(CFeatEtaLi,LNewEta),length(NewChains,LChains),
			(debugMode ->write("#Eta: "), write(Leta),write(" #NewEta: "), writeln( LNewEta);true),
			(debugMode ->write("#Chains: "),writeln(LChains);true),
			( ((Leta < LNewEta);(LChains > 0)) -> loopExtermination(CFeatEtaLi,NewChains,NewMarkEps,NewHist,NewEtaLiDeep,ReEpsLi),NewEtaLi = (NewEtaLiDeep,ReEpsLi)% 5. Step of extermination (Initial round) + Loop
			; (debugMode -> writeln("No loop!");true)
			, NewEtaLi = (EtaLi,EpsLi)). % 6.Step (Initial round)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loopExtermination(+[EtaLis],+[Chains],+[EpsLis],+[ChainHist],-[EtaLis],-[EpsLi])
%	TODO: Herkunft erzeugung Duplikate nachvollziehen und beheben
%	loop function of the extermination algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loopExtermination(EtaLi,[],EpsLi,Hist,NewEtaLi,NewEpsLi):-
			(debugMode -> writeln("now looping without Chains");true),
			combFeatures(EtaLi,EpsLi,CFeatEtaLi,MarkEpsLi), %	1. step
			(debugMode->write("Eta-LIs after Eps-LI: "),writeln(CFeatEtaLi);true),
			createChains(CFeatEtaLi,[],MarkEpsLi,Hist,[],NewChains,NewMarkEps,NewHist), %4.Step
			(debugMode -> write("Old Eta: "),writeln(EtaLi),write("Epsilon: "),writeln(EpsLi),write("New Eta: "),writeln(CFeatEtaLi),write("New Epsilon: "),writeln(NewMarkEps);true),
			(debugMode -> write("Chains: "), writeln(NewChains);true),
			length(EtaLi,Leta),length(NewChains,LChains),length(CFeatEtaLi,LNewEta),
			(debugMode ->write("#Eta: "), write(Leta),write(" #NewEta: "), writeln(LNewEta);true),
			(debugMode ->write("#Chains: "),writeln(LChains);true),
			( ((Leta < LNewEta); nonEmptyList(NewChains)) -> loopExtermination(CFeatEtaLi,NewChains,NewMarkEps,NewHist,NewEtaLi,NewEpsLi)
			; (debugMode -> writeln("Stop the loop!");true) % 5.step
			, cleanEpsList(EpsLi,CleanEpsLis)
			, (debugMode->write("Final Eta: "),writeln(EtaLi),write("Epsilon: "),writeln(CleanEpsLis);true)
			, NewEtaLi = EtaLi,NewEpsLi = CleanEpsLis). % 6.step -> Fehlt noch die Löschfunktion für die Epsilon-Li
loopExtermination(EtaLi,Chains,EpsLi,Hist,NewEtaLi,NewEpsLi):-
			(debugMode -> writeln("now looping with Chains");true),
			combFeatures(EtaLi,EpsLi,CFeatEtaLi,MarkEpsLi), %	1. step
			(debugMode->write("Eta-LIs after Eps-LI: "),writeln(CFeatEtaLi);true),
			createChains(CFeatEtaLi,Chains,MarkEpsLi,Hist,ChainsEtaLi,DeeperChains,DeeperEps,DeeperHist), % 2-3.step
			(debugMode ->write("Eta-LIs after Chains: "),writeln(ChainsEtaLi);true),
			createChains(ChainsEtaLi,[],DeeperEps,DeeperHist,[],IterChains,NewMarkEps,NewHist), %4.Step
			append(DeeperChains,IterChains,NewChains),
			(debugMode -> write("Old Eta: "),writeln(EtaLi),write("Epsilon: "),writeln(EpsLi),write("New Eta: "),writeln(ChainsEtaLi),write("New Epsilon: "),writeln(NewMarkEps);true),
			(debugMode -> write("Chains: "), writeln(NewChains);true),
			length(EtaLi,Leta),length(ChainsEtaLi,LNewEta),length(NewChains,LChains),
			(debugMode ->write("#Eta: "), write(Leta),write(" #NewEta: "), writeln(LNewEta);true),
			(debugMode ->write("#Chains: "),writeln(LChains);true),
			( ((Leta < LNewEta); nonEmptyList(NewChains)) -> loopExtermination(ChainsEtaLi,NewChains,NewMarkEps,NewHist,NewEtaLi,NewEpsLi)
			; (debugMode -> writeln("Stop the loop!");true) % 5.step
			, cleanEpsList(EpsLi,CleanEpsLis)
			, (debugMode->write("Final Eta: "),writeln(EtaLi),write("Epsilon: "),writeln(CleanEpsLis);true)
			, NewEtaLi = EtaLi,NewEpsLi = CleanEpsLis). % 6.step -> Fehlt noch die Löschfunktion für die Epsilon-Li

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
%			b. Save the original Feature-list of the eta-Li and add the used epsilon-Li to the list of epsilon-Lis
%			c. Mark the used epsilon-Li in the list of epsilon-Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
combFeatures(EtaLi,[],EtaLi,[]).
combFeatures([],EpsLi,[],EpsLi):- (debugMode -> writeln("No Eta-Li found"); true). 
combFeatures([EtaLi|RestEtaLi],EpsLi,NewEtaLis,MarkEpsLis) :- 
			checkFeatEtaEps(EtaLi,EpsLi,FitEpsLi,NoFitEpsLi), % checks for fitting epsilon-Li
			(debugMode -> write("Fitting epsilon-Li: "), write(FitEpsLi), write(" for eta-Li: "), writeln(EtaLi);true),
			% HIER BUG -> Bitte lösen
			buildNewEtaLi(EtaLi,FitEpsLi,NewEta), append(FitEpsLi,NoFitEpsLi,NewEpsLi), % build all new possible eta-Li
			(debugMode -> write("build new eta-Li: "),writeln(NewEta);true),
			combFeatures(RestEtaLi,NewEpsLi,NewRestEtaLi,MarkEpsLis), % recursion (we need to go DEEPER!)
			checkIfNew(NewEta,NewRestEtaLi,NewEtaLis),
			(debugMode -> write("New Eta-Li to use: "), writeln(NewEtaLis);true). % checks if new eta-Li is realy new, and adds it to the list 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkFeatEtaEps(+Eta_Li,+[Epsilon_Li],-[Matching_Epsilon_Li],-[Nonmatching_Epsilon_Li])
%
% 	checks if any epsilon-Li matches its positve Features with the negative Features of the Eta-Li
% 	and seperates the matching and nonmatching into two different lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkFeatEtaEps(_,[],[],[]).
checkFeatEtaEps(li(_,FsW,_),[epsLi(FsE,Mark)|RestEta],FitEpsLi,NoFitEpsLi) :- 
			splitFeat(FsW,_,NFsW),splitFeat(FsE,PFsE,_),%(debug -> write("Neg-Feat W: "), writeln(NFsW), write("Pos-Feat E: "), writeln(PFsE)),	% split the feature lists into positive and negative features lists
			( matchFeatLists(PFsE,NFsW) -> FitEpsLi = [epsLi(FsE,dot)| RestFitEps], NoFitEpsLi = RestNoFitEps % if the list from the eta-Li and from the epsilon-Li match
			% this is also the point he epsilon-LI may get marked
			; FitEpsLi = RestFitEps, NoFitEpsLi = [epsLi(FsE,Mark)|RestNoFitEps]), % if they do not match
			checkFeatEtaEps(li(_,FsW,_),RestEta,RestFitEps,RestNoFitEps). % recursion

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitFeat(+Fs,-PFS,-NFs)
%
% 	splits a feature list into positive and negative feature lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
splitFeat([],[],[]):- (debugMode -> writeln("Tried to split empty list.");true).
splitFeat([-_|_],_,_):- (debugMode -> writeln("Feature List not in order! Licensee before Category found!");true),
			false. % should never occure, but to be safe.
splitFeat([+_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!");true),
			false. % should never occure, but to be safe.
splitFeat([=_],_,_):- (debugMode -> writeln("Feature List not in order! Single positive Feature found!");true),
			false. % should never occure, but to be safe.
splitFeat([=F|Fs],[=F|PTail],NFs):- splitFeat(Fs,PTail,NFs).
splitFeat([+F|Fs],[+F|PTail],NFs):- splitFeat(Fs,PTail,NFs).
splitFeat([ F|Fs],[],[ F|Fs]). % the category is the first negative Feature in a list. If we found it, everything before is a positive feature and everything after a negative feature

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchFeatLists(+PFs,+NFs)
%
% 	checks if a positive and a negative feature list match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchFeatLists([],[]). % all matched
matchFeatLists(_,[]):- false. % not matching, different lengths
matchFeatLists([],_):- false. % not matching, different lengths
matchFeatLists([+A|PFs],[-A|NFs]):- matchFeatLists(PFs,NFs).
matchFeatLists([=A|PFs],[ A|NFs]):- matchFeatLists(PFs,NFs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildNewEtaLi(+EtaLi,+[EpsLi],-[NewEta])
%	TODO: Einfügen von Duplikaten nachvollziehen und Beheben
% 	makes as many new eta-Lis as are epsilon-Li in [EpsLi]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildNewEtaLi(EtaLi,[],[EtaLi]).
buildNewEtaLi(li([W],FsW,(FsH,EpsHist)),[epsLi(FsE,Mark)|EpsRest],NewEtaLi):- 
			buildNewEtaLi(li([W],FsW,(FsH,EpsHist)),EpsRest,DeepNewEtaLi), % recursion first to let the list of new LI grow from smalles point onward for less checking 
			(debugMode -> write("Start Building with: "),writeln(epsLi(FsE,Mark));true),
			splitFeat(FsW,PFsW,_),splitFeat(FsE,_,NFsE),	%split the Feature lists of the eta- and epsilon-Li
			(debugMode -> writeln("I split the Features");true),
			append(PFsW,NFsE,NewFsW),	% combine the positive feature of the eta-Li and the negative feature of the epasilon-Li together
			(debugMode -> writeln("Before History");true),
			addHistory(EpsHist,[epsLi(FsE,Mark)],NewEpsHist),	% add the used epsilon-Li to the history of epsilon-Li of the eta-Li
			(debugMode -> writeln("After History");true),
			NewEtaLi = [li([W],NewFsW,(FsH,NewEpsHist))|DeepNewEtaLi].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkIfNew(+[Eta_Li],+[Eta_Li],-[New_Eta_Lis])
%
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
% addHistory(+[Eps_Hist],+[eps_Li],-[NewEpsHist])
%	TODO: History für Liste an Listen ausweiten
%	adds a new eps-Li to all history Lists 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addHistory([[]],[EpsLi],[EpsLi]).
addHistory([EpsList],[EpsLi],[EpsList|[EpsLi]]). % :- append(EpsList,[EpsLi],NewEpsList).
addHistory(List1,List2,NewList):-append(List1,List2,NewList).% bei Ausweitung auf Liste an Listen auskommentieren
%addHistory([HeadList|RestList],[EpsLi],[NewHeadList|NewRestList]):- append(HeadList,[EpsLi],NewHeadList),addHistory(RestList,[EpsLi],NewRestList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2-4. Step after here
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createChains(+[EtaLi],+[OldChains],+[MarkEpsLi],+[Tried_epsilon_Chains],-[NewEtaLi],-[NewChains],-[NewMarkEps],-[NewHist])
%
%	top level function of the second and fourth step
%
%	 2. Try to combine Epsilon-Lis with old chains, such that at least one feature from the eta chain item is matched in order and all positive features of the new epsilon-Li
%			a. If all features of the eta chain item are matched, create a new eta-Li like in step 1.
%			b. check if the newly created eta-Li is truly new.
%				b1. If no, delete it and save the epslion history with the old one
%				b2. If yes, save it
%			c. Mark the used epsilon-List
%			d. If not all features of the eta chain item are matched, check wether the combination of epsilon-Li has already been tried.
%				d1. If yes, delete the chain
%				d2. If no, create a new chain like in step 4.
%	 3. Delete all chains that were not created in this round. Check if the combination of epsilon-Lis of the deleted chains are already recorded
%			a. if no, add them to the recording.
%			b. if yes, disregard them.
%	 4. step epsilon-extermination:
%		Combine in order the first m negative Features of an eta-Li with all positive Features of all posible epsilon-Lis, such that only negative features of the eta-Li remain.
%			a. Create a Chain with the head being the negative Features of the epsilon-Li and the next item being the eta-Li with its remaining negative features
%			b. check if the thus created chained has already been unsuccesfully tried.
%				b1. If yes, delete the new Chain
%				b2. If no, keep it 
%			c. Save the original Feature-list of the eta-Li and add the used epsilon-Li to the list of epsilon-Lis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createChains([],[],EpsLi,ChainHist,[],[],EpsLi,ChainHist).
createChains([EtaHead|EtaRest],[],EpsLis,ChainHist,NewEtaLis,OutChains,NewEpsLis,NewHist):- 
			(debugMode -> writeln("Check for new Chains");true),
			checkEtaEpsChain(EtaHead,EpsLis,FitEpsLis,NoFitEpsLis),
			(debugMode -> write("Semi matching Epsilon-Li: "), writeln(FitEpsLis);true),	% 4.Step -> Case without chains present 
			buildNewChain(EtaHead,FitEpsLis,EtaHeadChains), append(FitEpsLis,NoFitEpsLis,MarkedEpsLis),
			createChains(EtaRest,[],MarkedEpsLis,ChainHist,NewEtaLis,DeeperChains,NewEpsLis,DeeperChainHist), % recursion
			(debugMode ->write("Possible new Chain: "),writeln(EtaHeadChains);true),
			(debugMode ->write("Our History: "),writeln(DeeperChainHist) ;true),
			compHistory(EtaHeadChains,DeeperChainHist,NewChains,NewHist), append(NewChains,DeeperChains,OutChains).
createChains(EtaLis,Chains,EpsLis,ChainHist,NewEtaLis,NewChains,MarkEpsLis,NewChainHist):- 
			(debugMode -> write("Check old Chains: "),writeln(Chains);true),
			checkOldChainsEtaLi(Chains,EpsLis,NewPosEtaLis,MarkEpsLis,RemChains),  % 2.Step -> Case with chains present,
			(debugMode ->write("Possible new Eta-Li from old ones: "),writeln(NewPosEtaLis);true),
			checkIfNew(NewPosEtaLis,EtaLis,NewEtaLis),
			(debugMode ->write("Chains remaining to check for further Chains: "), writeln(RemChains);true),
			checkOldChainsChain(RemChains,MarkEpsLis,ChainHist,NewChains,NewChainHist),% 2.Step + 3.Step
			(debugMode->write("New History: "),writeln(NewChainHist);true).
			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkEtaEpsChain(+Eta_Li,+[Epsilon_Lis],-[Fitting_Eps_Lis],-[Non_Fitting_EpsLis])
%
%	seperates the epsilon-Lis into those that may result in a chain and those that do not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkEtaEpsChain(_,[],[],[]).
checkEtaEpsChain(li(_,FsW,_),[epsLi(FsE,Mark)|EpsRest],FitEpsLis,NoFitEpsLis):- 
			splitFeat(FsW,_,NFsW),splitFeat(FsE,PFsE,_),
			checkEtaEpsChain(li(_,FsW,_),EpsRest,RestFitEps,RestNoFitEps),
			( ((PFsE \= []),matchPartFeatures(PFsE,NFsW,nonempty)) -> FitEpsLis = [epsLi(FsE,Mark)|RestFitEps],NoFitEpsLis = RestNoFitEps
			; FitEpsLis = RestFitEps, NoFitEpsLis = [epsLi(FsE,Mark)|RestNoFitEps]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildNewChain(+Eta_Li,+[Epsilon_Lis],-[Eta_Chains])
%
%	builds a chain from the eta-Li and all possible epsilon-Li
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildNewChain(_,[],[]).
buildNewChain(li([W],FsW,(FsH,EpsHist)),[epsLi(FsE,_)|EpsRest],EtaChains):- 
			splitFeat(FsW,_,NFsW),splitFeat(FsE,PFsE,NFsE), getPartFeatures(PFsE,NFsW,RFsW),
			EtaChains = [chainItem([NFsE],RFsW,([W],FsH,EpsHist),[epsChainLi([W],FsH,FsE)])|RestChain],
			buildNewChain(li([W],FsW,(FsH,EpsHist)),EpsRest,RestChain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchPartFeatures(+[pos_Feature],+[neg_Feature],-[Remain_Neg_Feat],-Empty_Status)
%
% 	This function checks, if two feature list match only in part 
% 	Funltioniert zwar, macht aber uU zu viele Sachen auch einmal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchPartFeatures([],[],empty):- false.
matchPartFeatures([],[_|_],nonempty).
matchPartFeatures(_,[],empty):- false.
matchPartFeatures([+A|RPos],[-A|RNeg],nonempty):- matchPartFeatures(RPos,RNeg,nonempty).%,((Stat1 == bottom) -> Stat = nonempty).%
matchPartFeatures([=A|RPos],[ A|RNeg],nonempty):- matchPartFeatures(RPos,RNeg,nonempty).%,((Stat1 == bottom) -> Stat = nonempty).%
matchPartFeatures(_,_,empty):- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getPartFeatures(+[Pos_Feat],+[Neg_Feat],-[Neg_Feat])
%
%	extracts remaining features between semi matching feature lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getPartFeatures([],[],_):- false.
getPartFeatures([],[NFsW|RNFsW],[NFsW|RNFsW]).
getPartFeatures([+A|RPos],[-A|RNeg],RNFsW):- getPartFeatures(RPos,RNeg,RNFsW).
getPartFeatures([=A|RPos],[ A|RNeg],RNFsW):- getPartFeatures(RPos,RNeg,RNFsW).
getPartFeatures(_,_,[]):- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compHistory(+[Eta_Chains],+[Eta_Chain_Hist],-[New_Chains],-[New_Chain_Hist])
%	ToDO: auf Listen an Listen ausweiten
%	Compares eta-chain-items to known epsilon-combinations
%	If it is new, add the chain and add to the history]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compHistory([],ChainHist,[],ChainHist).
compHistory([chainItem([NFsE],RFsW,([W],FsH,EpsHist),ChainItemHist)|RestChain],ChainHist,NewChain,NewChainHist):- 
			member(ChainItemHist,ChainHist), % did that epsilon combination already occur?
			(debugMode -> writeln("Found match. Add Nothing."),write("Item History: "),writeln(ChainItemHist),write("Chain History: "),writeln(ChainHist);true),
			compHistory(RestChain,ChainHist,NewChain,NewChainHist); 								% yes -> discard chain and check rest
			compHistory(RestChain,[ChainItemHist|ChainHist],NewRestChain,NewChainHist),
			NewChain = [chainItem([NFsE],RFsW,([W],FsH,EpsHist),ChainItemHist)|NewRestChain], 		% no -> add chain and check rest
			(debugMode -> write("Found no match. New Chain: "),writeln(NewChain);true).
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkOldChainsEtaLi(+[Chains],+[EpsLis],-[EtaLis],-[EpsLis],-[RemChains])
% 
%	checks if chains can result in eta-Li with one more epsilon-Li added
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkOldChainsEtaLi([],Eps,[],Eps,[]).
checkOldChainsEtaLi([HeadChain|RestChain],EpsLis,NewEtaLis,MarkEpsLis,RemChains):-
			(debugMode->write("Try this Chain for Eta-LI: "),writeln(HeadChain);true),
			forgeNewEta(HeadChain,EpsLis,NewHeadEtaLis,NewMarks,HeadRemChain),
			checkOldChainsEtaLi(RestChain,NewMarks,DeeperEtaLis,MarkEpsLis,DeeperRemChains),
			append(NewHeadEtaLis,DeeperEtaLis,NewEtaLis),
			append(HeadRemChain,DeeperRemChains,RemChains).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkOldChainsChain(+[Chains],+[EpsLis],+[ChainsHist],-[Chains],-[ChainHist]).
%
%	checks if chains can have an additional epsilon-Li added.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkOldChainsChain([],_,Hist,[],Hist).
checkOldChainsChain([HeadChain|RestChain],EpsLis,ChainsHist,NewChains,NewChainHist):-
			(debugMode->writeln("Check old Chains for new ones.");true),
			forgeNewChain(HeadChain,EpsLis,NewPotChains),
			(debugMode->write("Try this new chain: "), writeln(NewPotChains);true),
			compHistory(NewPotChains,ChainsHist,NewHeadChains,DeepChainHist),
			checkOldChainsChain(RestChain,EpsLis,DeepChainHist,DeeperChains,NewChainHist),
			append(NewHeadChains,DeeperChains,NewChains).
										
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% forgeNewChain(+ChainItem,+[EpsLi],-[ChainItems])
%
%	makes Chains with one old Chain and as many epsilon-Li as possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			
forgeNewChain(_,[],[]).							
forgeNewChain(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),[epsLi(FsE,_)|RestEta],NewChain):-
			splitFeat(FsE,PFsE,NFsE),matchChainFeat(NFsC,FsW,PFsE,FsC,RsW),
			forgeNewChain(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),RestEta,DeeperChains),
			( (nonEmptyList(RsW),length(FsW,LOld),length(RsW,LNew),(LOld > LNew)) -> append(ChainItemHist,[epsChainLi([W],FsH,FsE)],NewChainItemHist)
			, RFsW = RsW, append(FsC,NFsE, NewFsC)
			, NewChain = [chainItem([NewFsC], RFsW, ([W],FsH,EpsHist),NewChainItemHist)|DeeperChains]
			; NewChain = DeeperChains).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% forgeNewEta(+ChainItem,+[EpsLi],-[EtaLi],-[EpsLi],-[ChainItem])
%	TODO: Hier eps-LI aus Chain mit dot versehen
%	makes Eta-Li with one old Chain and as many epsilon-Li as possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
forgeNewEta(_,[],[],[],[]).
forgeNewEta(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),[epsLi(FsE,Mark)|RestEta],NewEtaLis,MarkEps,RemChain):-
			splitFeat(FsE,PFsE,_),matchEtaChain(NFsC,FsW,PFsE,RF),
			forgeNewEta(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),RestEta,DeeperEta,DeeperMarks,DeeperChain),
			(debugMode->write("Test for this Eps-LI: "),writeln(FsE);true),
			( emptyList(RF) 
			-> (debugMode->writeln("Matched all remaining Features.");true) 
			, buildNewEtaLiChain(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),epsLi(FsE,Mark),NewLi,NewHist)
			, (debugMode->write("Build Eta-LI: "),writeln(NewLi);true)
			, NewEtaLis = [NewLi|DeeperEta]
			, append(NewHist,DeeperMarks,UncleanEpsList)
			, cleanEpsList(UncleanEpsList,MarkEps)
			, (debugMode->write("Uncleaned Eps List: "),writeln(UncleanEpsList),write("Cleaned Eps List: "),writeln(MarkEps);true)
			, RemChain = DeeperChain
			; (debugMode->write("Could not fully match for this Eps-LI. Remaining Features: "),writeln(RF);true)
			, NewEtaLis = DeeperEta
			, MarkEps = [epsLi(FsE,Mark)|DeeperMarks]
			, (member(chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist),DeeperChain) 
			  ->RemChain = DeeperChain
			  ; RemChain =[chainItem(NFsC,FsW,([W],FsH,EpsHist),ChainItemHist)|DeeperChain])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchChainFeat(+[[NFs]],+[NFsW],+[PFsE],-[[NewFsC]],-[NFsW])
%
%	matches a positive Feature list to several negative Feature lists until the positive list is empty
%	fail state:  RFsW = []
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchChainFeat(NFs,NFsW,[],NFs,NFsW).
matchChainFeat([[]|RestNFs],NFsW,PFs,NewFsC,RFsW):- matchChainFeat(RestNFs,NFsW,PFs,NewFsC,RFsW).
matchChainFeat([[F|Fs]|RestNFs],NFsW,[=F|PFs],NewFsC,RFsW):- matchChainFeat([Fs|RestNFs],NFsW,PFs,NewFsC,RFsW).
matchChainFeat([[-F|Fs]|RestNFs],NFsW,[+F|PFs],NewFsC,RFsW):- matchChainFeat([Fs|RestNFs],NFsW,PFs,NewFsC,RFsW).
matchChainFeat(NFs,[-F|RFsW],[+F|PFs],NewFsC,FsW):- matchChainFeat(NFs,RFsW,PFs,NewFsC,FsW).
matchChainFeat(_,_,_,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% matchEtaChain(+[[NFsC]],+[FsW],+[PFsE],-[RFs])
%
% same as matchChainFeat, except that fail state: NFsW != [] at the end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matchEtaChain([],[],[],[]):- debugMode->writeln("Matched all");true.
matchEtaChain([[]|RestNFs],NFsW,PFs,RF):- (debugMode->writeln("Finished a NFsC");true),matchEtaChain(RestNFs,NFsW,PFs,RF).
matchEtaChain([[F|Fs]|RestNFs],NFsW,[=F|PFs],RF):- (debugMode->write("Matched Category: "),writeln(F);true),matchEtaChain([Fs|RestNFs],NFsW,PFs,RF).
matchEtaChain([[-F|Fs]|RestNFs],NFsW,[+F|PFs],RF):- (debugMode->write("Matched Licensee NFsC: "),writeln(F);true),matchEtaChain([Fs|RestNFs],NFsW,PFs,RF).
matchEtaChain(NFs,[-F|RFsW],[+F|PFs],RF):- (debugMode->write("Matched Licensee FsW: "),writeln(F);true),matchEtaChain(NFs,RFsW,PFs,RF).
matchEtaChain(_,_,_,[[]]):- (debugMode->writeln("Didn't match all");true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% buildNewEtaLiChain(+ChainItem,+EpsLi,-EtaLi)
%
%	builds a new Eta-LI out of a chain item and an epsilon-LI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
buildNewEtaLiChain(chainItem(_,_,([W],FsH,[[]]),ChainItemHist),epsLi(FsE,_),NewLi,NewHist):-
			splitFeat(FsE,_,NFsE),splitFeat(FsH,PFsW,_),append(PFsW,NFsE,NewFs),
			reforgeChainHist(ChainItemHist,epsLi(FsE,dot),NewHist),
			NewLi = li([W],NewFs,(FsH,NewHist)).
buildNewEtaLiChain(chainItem(_,_,([W],FsH,EpsHist),ChainItemHist),epsLi(FsE,_),NewLi,NewHist):- 
			splitFeat(FsE,_,NFsE),splitFeat(FsH,PFsW,_),append(PFsW,NFsE,NewFs),
			reforgeChainHist(ChainItemHist,epsLi(FsE,dot),NewHist),
			append(EpsHist,NewHist,LiHist),
			NewLi = li([W],NewFs,(FsH,LiHist)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reforgeChainHist(+[ChainItemHist],+EpsLi,-EpsHist)
%	TODO: Für Liste an Listen ausweiten
%	turns Chain history and epsilon-Li into an Epsilon history
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reforgeChainHist([],EpsLi,[EpsLi]).
reforgeChainHist([epsChainLi([_],_,FsE)|RestChain],EpsLi,NewHist):- 
			reforgeChainHist(RestChain,EpsLi,DeeperHist),
			NewHist = [epsLi(FsE,dot)|DeeperHist].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nonEmptyList(-[List])
%
%	checks if a list is not empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nonEmptyList([]):-!,false.
nonEmptyList(_):-true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% emptyList(-[List])
%
%	checks if a list is empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emptyList([]):-true.
emptyList(_):-!, false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cleanEpsList(+[Eps_List],-[Eps_List])
%
%	Removes double entries in an epsilon List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cleanEpsList([],[]).
cleanEpsList([Ep|Eps],[OutEps|DeeperEpsList]):- 
			checkEpsList(Ep,Eps,OutEps,RestEps),
			cleanEpsList(RestEps,DeeperEpsList).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
% checkEpsList(+Eps_Li,+[Eps_List],-Eps_Li,-[Eps_List]),			
%
%	compares one Epsilon Li with a List of Epsilon Li 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkEpsList(epsLi(FsE,Mark),[],epsLi(FsE,Mark),[]).
checkEpsList(epsLi(FsE,dot),[epsLi(FsE,_)|RestEps],epsLi(FsE,dot),RestEps).
checkEpsList(epsLi(FsE,_),[epsLi(FsE,dot)|RestEps],epsLi(FsE,dot),RestEps).
checkEpsList(epsLi(FsE1,Mark1),[epsLi(FsE2,Mark2)|RestEps],OutEpsLi,[epsLi(FsE2,Mark2)|DeeperEpsList]):-
			checkEpsList(epsLi(FsE1,Mark1),RestEps,OutEpsLi,DeeperEpsList).