% file: load.pl
% origin author : J. Kuhn
% origin date: May 2024
% purpose: load file for the lc parser

:- set_prolog_flag(encoding,utf8).
:- use_module(library(ordsets)).
:- ['helpers/painter'].
:- ['helpers/extermination'].
:- ['helpers/linker'].
:- ['scanner'].


:- op(500, xfy, ::). % infix predicate for lexical items
:- op(500, fx, =). % for selection features
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Verwendetes Lexikon für das Parsen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-['grammars/maus'].
:-['grammars/German_trans_pruned'].
%:-['grammars/ZahlenSprache'].
%:-['grammars/EpsKetten'].
