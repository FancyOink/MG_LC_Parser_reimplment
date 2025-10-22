% file: load.pl
% origin author : J. Kuhn
% origin date: May 2024
% purpose: load file for the lc parser

:- set_prolog_flag(encoding,utf8).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%:- op(500, fx, #). % for category features, if the need arises to redefine the sign of Category features
:- ['helpers/painter'].
:- ['helpers/extermination'].
:- ['helpers/linker'].
:- ['helpers/filter'].
:- ['helpers/workspace'].
%:- ['helpers/lambda'].
:- ['scanner'].
:- ['lcparser'].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Used Lexikon for the Parsen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-['grammars/grammar_test/maus'].
%:-['grammars/grammar_test/sanity'].
%:-['grammars/grammar_test/German_trans_pruned'].
%:-['grammars/grammar_test/German_trans'].
:-['grammars/grammar_test/German1M_trans'].
%:-['grammars/grammar_test/German1M_trans_pruned'].
%:-['grammars/grammar_test/German1M'].
%:-['grammars/grammar_test/German'].
%:-['grammars/grammar_test/French1k_trans'].
%:-['grammars/grammar_test/French'].
%:-['grammars/grammar_test/French_test'].
%:-['grammars/grammar_test/English1k_trans'].
%:-['grammars/grammar_test/English'].
%:-['grammars/grammar_test/English_trans'].
%:-['grammars/grammar_test/English_trans_pruned'].
%:-['grammars/grammar_test/ZahlenSprache'].
%:-['grammars/grammar_test/EpsKetten'].
%:-['grammars/grammar_test/Georgian'].
%:-['grammars/grammar_test/Georgian_trans'].
%:-['grammars/grammar_test/Georgian_trans_pruned'].
%:-['grammars/grammar_test/Italian_trans'].
%:-['grammars/grammar_test/g1'].
%:-['grammars/grammar_test/Norwegian-Bokmal_trans'].