:- module(tree_painter, [tree_painter/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tree_painter(+Tree)
%
% top most function of the painter module
% takes a derivation tree and writes a latex-file that visuailizes the tree 
% using the qtree-package
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tree_painter(Tree) :-
  open('ltree.tex', write, Stream),
  writeBegin(Stream,Stream2),
  writeTree(Stream2,Tree,Stream3),
  writeEnd(Stream3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writeBegin(+Stream,-Stream)
%
% writes the head of the latex-file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
writeBegin(Stream, Stream) :-
  format(Stream, "\\documentclass[4pt, amsfonts]{book}~n", []),
  %format(Stream, "\\documentclass{standalone}~n", []),
  format(Stream, "\\usepackage{amssymb, amsmath}~n", []),
  format(Stream,"\\usepackage[paperwidth=200cm,paperheight=200cm,landscape,top=2cm,bottom=2cm,left=2cm,right=2cm]{geometry}~n",[]),
  format(Stream, "\\usepackage{stmaryrd}~n", []),
  format(Stream, "\\usepackage[T1]{fontenc}~n", []),
  format(Stream, "\\usepackage{graphicx}~n", []),
  format(Stream, "\\usepackage[utf8]{inputenc}~n",[]),
  format(Stream, "\\usepackage{qtree}~n", []),
  format(Stream, "\\begin{document}~n", []),
  format(Stream, "\\begin{figure}[ht]~n", []),
  format(Stream, "\\Tree ",[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writeEnd(+Stream)
%
% writes the end of the latex-file and closes the stream
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
writeEnd(Stream) :-
  format(Stream, "~n\\end{figure}~n\\end{document}~n", []),
  close(Stream).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writeTree(+Stream,+Tree,-Stream)
%
% writes the tree structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
writeTree(Stream,[],Stream).  
writeTree(Stream, tree(Items,empty, Subr), Stream2) :- % tree with 1 subtree found
  prettyOutPutKnot(Stream,Items),
  %format(Stream, "[.{\\detokenize{~w}} ",[Items]),
  writeTree(Stream, Subr,Stream2),
  format(Stream2," ]",[]).
writeTree(Stream, tree(Items,Subl, Subr), Stream3) :- % tree with 2 subtrees found
  prettyOutPutKnot(Stream,Items),
  %format(Stream, "[.{\\detokenize{~w}} ", [Items]),
  writeTree(Stream,Subl,Stream2),
  writeTree(Stream2,Subr,Stream3),
  format(Stream3," ]",[]).
writeTree(Stream,li([],Fs,L) , Stream) :- % Epsilon-leaf found with semantics
  format(Stream, "[.{[$\\epsilon$],~w,[",[Fs]),
  prettyOutPutLambda(Stream,L),
  format(Stream,"]} ]",[]).
writeTree(Stream, li(W,Fs,L) , Stream) :- % leaf found with semantics
  format(Stream,  "[.{~w,~w,[", [W,Fs]),
  prettyOutPutLambda(Stream,L),
  format(Stream,"]} ]",[]).
writeTree(Stream, li(W,Fs) , Stream) :- % leaf found without semantics
  format(Stream,  "[.{$~w$$~w$} ]", [W,Fs]).
writeTree(Stream,li([],Fs) , Stream) :- % Epsilon-leaf found without semantics
  format(Stream, "[.{$\\epsilon$}~w ]",[Fs]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyOutPutKnot(+Stream,+([Word,Features,Lambda]))
%
% constructs the knots of the trees
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prettyOutPutKnot(Stream,[([],Features,Lambda)|Chains]):-
  format(Stream,"[.{[([$\\epsilon$],~w,[", [Features]),
  prettyOutPutLambda(Stream,Lambda),
  format(Stream, "])",[]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutKnot(Stream,[(Word,Features,Lambda)|Chains]):-
  format(Stream,"[.{[(~w,~w,[", [Word,Features]),
  prettyOutPutLambda(Stream,Lambda),
  format(Stream, "])",[]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutKnot(Stream,[([],Features)|Chains]):-
  format(Stream,"[.{([$\\epsilon$],~w)", [Features]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutKnot(Stream,[(Word,Features)|Chains]):-
  format(Stream,"[.{(~w,~w)", [Word,Features]),
  prettyOutPutChains(Stream,Chains).  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyOutPutChains(+Stream,+([Word,Features,Lambda]))
%
% constructs the chains of the knots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
prettyOutPutChains(Stream,[]):-
  format(Stream,"} ",[]).
prettyOutPutChains(Stream,[([],Features,Lambda)|Chains]):-
  format(Stream,",([$\\epsilon$]~w,[", [Features]),
  prettyOutPutLambda(Stream,Lambda),
  format(Stream, "])",[]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutChains(Stream,[(Word,Features,Lambda)|Chains]):-
  format(Stream,",(~w,~w,[", [Word,Features]),
  prettyOutPutLambda(Stream,Lambda),
  format(Stream, "])",[]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutChains(Stream,[([],Features)|Chains]):-
  format(Stream,",([$\\epsilon$]~w)", [Features]),
  prettyOutPutChains(Stream,Chains).
prettyOutPutChains(Stream,[(Word,Features)|Chains]):-
  format(Stream,",(~w,~w)", [Word,Features]),
  prettyOutPutChains(Stream,Chains).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyOutPutLambda(+Stream,+Lambda)
%
% constructs the lambda expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
prettyOutPutLambda(Stream,(X>>(Out is Term))):-
  length(X,LambdaLength), prettyLambdaList(LambdaLength,PList), append(PList,[StreamOut],NewLambdaList), Ex =..[call,(X>>(Out = Term))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda),
  format(Stream,"~w (~w)",[StringLambda,StreamOut]).
prettyOutPutLambda(Stream,(X>>(Out = Term))):-
  length(X,LambdaLength), prettyLambdaList(LambdaLength,PList), append(PList,[StreamOut],NewLambdaList), Ex =..[call,(X>>(Out = Term))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda),StreamOut =.. [Name|Rest],prettyEpsilon([Name|Rest],OutRest), OutString =.. OutRest,
  format(Stream,"~w (~w)",[StringLambda,OutString]).
prettyOutPutLambda(Stream,(_>>makeLambda(_,_,_))):-
  format(Stream,"$\\lambda$X1.$\\lambda$X2. (X2(X1))",[]).
prettyOutPutLambda(Stream,(X>>Term)):-
  length(X,LambdaLength), prettyLambdaList(LambdaLength,PList), append(PList,[StreamOut],NewLambdaList), Term =..[TermName|Var], last(Var,Out), removeLast(Var,NewVar), NewTerm =..[TermName|NewVar], Ex =..[call,(X>>(Out = NewTerm))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda),StreamOut =.. [Name|Rest],prettyEpsilon([Name|Rest],OutRest), OutString =.. OutRest,
  format(Stream,"~w (~w)",[StringLambda,OutString]).
prettyOutPutLambda(Stream,cl(X>>(Out is Term),List)):-
  length(X,LambdaLength),length(List,LengthList), prettyLambdaList(LambdaLength,LengthList,PList), append(List,PList,NewList),  append(NewList,[StreamOut],NewLambdaList),Ex =..[call,(X>>(Out = Term))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda),
  format(Stream,"~w (~w)",[StringLambda,StreamOut]).
prettyOutPutLambda(Stream,cl(X>>(Out = Term),List)):-
  length(X,LambdaLength),length(List,LengthList), prettyLambdaList(LambdaLength,LengthList,PList), append(List,PList,NewList),  append(NewList,[StreamOut],NewLambdaList),Ex =..[call,(X>>(Out = Term))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda),StreamOut =.. [Name|Rest],prettyEpsilon([Name|Rest],OutRest), OutString =.. OutRest,
  format(Stream,"~w (~w)",[StringLambda,OutString]).
prettyOutPutLambda(Stream,cl((_>>makeLambda(_,_,_)),[List])):-
  format(Stream,"$\\lambda$X1.(X1 (~w))",[List]).
prettyOutPutLambda(Stream,cl((X>>Term),List)):-
  length(X,LambdaLength),length(List,LengthList), prettyLambdaList(LambdaLength,LengthList,PList), append(List,PList,NewList),Term =..[TermName|Var], last(Var,Out), removeLast(Var,NewVar), NewTerm =..[TermName|NewVar], append(NewList,[StreamOut],NewLambdaList),Ex =..[call,(X>>(Out = NewTerm))|NewLambdaList], call(Ex),
  maplist([In,LOut]>>atom_concat('$\\lambda$',In,LOut), PList, ConcatLambda),maplist([In,COut]>>atom_concat(In,'.',COut), ConcatLambda, ConcatDot),atomics_to_string(ConcatDot,StringLambda), StreamOut =.. [Name|Rest], prettyEpsilon([Name|Rest],OutRest), OutString =.. OutRest,
  format(Stream,"~w (~w)",[StringLambda,OutString]).
prettyOutPutLambda(Stream,epsilon):-
    format(Stream,"$\\epsilon$",[]).
prettyOutPutLambda(Stream,X):-
  prettyExpression(X,OutString),
  format(Stream,"~w",[OutString]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyLambdaList(+Length,+[Variable])
% prettyLambdaList(+Length,+Length,+[Variable])
%
% makes a list of variables for abstraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prettyLambdaList(Length,PList) :-
  L is (Length - 1),findall(Num, between(1,L,Num),LOut),maplist([In,Out]>>atom_concat('X',In,Out),LOut,PList).
prettyLambdaList(Length,LengthList,PList):-
  L is (Length - (LengthList + 1)), findall(Num, between(1,L,Num),LOut),maplist([In,Out]>>atom_concat('X',In,Out),LOut,PList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyEpsilon(+[Epsilons],-String)
%
% makes an epsilon for the latex-file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prettyEpsilon([],[]).
prettyEpsilon([epsilon|Rest],OutString):-
  Head ='$\\epsilon$',prettyEpsilon(Rest,OutRest), OutString = [Head|OutRest].
prettyEpsilon([epsilon(X)|Rest],OutString):-
  X =.. InputList, prettyEpsilon(InputList,OutUnder), NewX =.. OutUnder, Head =..['$\\epsilon$'|[NewX]],prettyEpsilon(Rest,OutRest), OutString = [Head|OutRest].
prettyEpsilon([Head|Rest],OutString):-
  Head =..[Name|X], prettyEpsilon(X,OutUnder), NewHead =.. [Name|OutUnder], prettyEpsilon(Rest,OutRest), OutString = [NewHead|OutRest].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prettyExpression(+[Epsilons],-String)
%
% makes the expression for the latex-file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prettyExpression([],[]).
prettyExpression(X,OutString):-
  compound(X), X =.. List, prettyEpsilon(List,OutName), OutString =.. OutName; OutString = X.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% removeLast(+List,-list)
%
% removes the last element of a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeLast([_],[]).
removeLast([List|Rest],Out):- removeLast(Rest,OutLevelDown),Out = [List|OutLevelDown].
