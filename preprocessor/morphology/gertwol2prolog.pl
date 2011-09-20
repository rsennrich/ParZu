:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- use_module(library(lists)).


:- use_module('map_gertwol2stts').
:- dynamic option/1.


option(output(pl)).
option(morphset(stts)).
%option(morphset(tiger)).
:- dynamic arg_specify/3.
arg_specify('-var',flag,"Encode STTS * categories as anonymous variables"):-
	asserta(option(var)).
arg_specify('-debug',flag,"Emit debuging information on transformation process"):-
	asserta(map_gertwol2stts:option(debug)).
arg_specify('-txt',flag,"Emit text format: WORDFORM TAB STTSTAG TAB LEMMA NL"):-
	retract(option(output(_))),
	asserta(option(output(txt))).
arg_specify('-lex',flag,"Emit text format: WORDFORM TAB LEMMA {TAB STTSTAG}+ NL"):-
	retract(option(output(_))),
	asserta(option(output(lex))).
arg_specify('-mtxt',flag,"Emit text format: WORDFORM TAB STTSTAG TAB LEMMA TAB MORPH  NL"):-
	retract(option(output(_))),
	asserta(option(output(mtxt))).
arg_specify('-ltag',flag,"Append lexical information to POS tag NN<ADJ (only for textual output)"):-
	asserta(option(output(ltag))).
arg_specify('-fix_lemma',flag,"Modify lemmas of functional words"):-
	asserta(map_gertwol2stts:option(fix_lemma)).

arg_specify('-nopragma',atom(Feat),"<fea> #Do not emit analysis with pragma feature <fea>"):-
  asserta(map_gertwol2stts:option(nopragma(Feat))),
  asserta(map_gertwol2sttstiger:option(nopragma(Feat))).
arg_specify('-withpragma',atom(Feat),"<fea> #Do emit analysis with pragma feature <fea>"):-
  retract(map_gertwol2stts:option(nopragma(Feat)));
  retract(map_gertwol2sttstiger:option(nopragma(Feat)));
  true.
arg_specify('-keeplemma',atom(Lemma),"<lemma> # Do not suppress lemmatization with lemma <lemma>"):-
  asserta(map_gertwol2stts:option(keep_lemma(Lemma))),
  asserta(map_gertwol2sttstiger:option(keep_lemma(Lemma))).
	
arg_specify('-tiger',flag,"Use morphological tags of TIGER corpus."):-
	retract(option(morphset(_))),
	asserta(option(morphset(tiger))).
arg_specify('-Version',flag,"Print version"):-
	write(user_error,'$Revision: 1.21 $ $Date: 2008/02/21 21:53:44 $'),
	nl(user_error),
	halt(0).
arg_specify('',atom(File),"usage: gertwol2prolog <options> [file]
# file is produced by gertwol2prolog.perl
# Output consists of gertwol Prolog facts
# gertwol(Wordform(Atom),Lemma(Atom),STTSTag(Atom),STTSMorphology(List),Score(Int))"):-
	open(File,read,S),
	go(S).
runtime_entry(start) :-
	catch((argv_parse,go),E,(writeq(user_error,E),nl(user_error),halt(3))).
go:-
	go(user_input).
go(IS):-
	option(morphset(tiger)),
	!,
	(   option(var)
	->  
	assert_varstar_gertwol2sttstiger
	;   
	assert_gertwol2sttstiger
	),
	map_stream_raw_gertwol2sttstiger_gertwol(IS),
	option(output(Form)),
	output(Form),
	halt(0).
go(IS):-
	option(morphset(stts)),
	!,
	(   option(var)
	->  
	assert_varstar_gertwol2stts
	;   
	assert_gertwol2stts
	),
	map_stream_raw_gertwol2stts_gertwol(IS),
	option(output(Form)),
	output(Form),
	halt(0).


output(pl):- 
	write_plgertwol5.

output(txt):- !,
	output_txt.
output(mtxt):- !,
	output_mtxt.

output(lex):-
	!,
	(option(output(ltag))
	->
	output_ltaglex
	;
	output_lex).
output_lex:-
	setof(STTS,Lex^Morph^Score^gertwol(WD,Lemma,STTS,Morph,Lex,Score),Tags),
	format('~a\t~a\t~@~N',[WD,Lemma,print_atomic_list(Tags,'\t')]),
	fail.
output_lex.

output_mtxt:-
	option(output(ltag)),
	setof(Morph,Score^gertwol(WD,Lemma,STTS,Morph,Lex,Score),Morphs),
	nonvar(STTS),
	(var(Lex) -> Lex = ''; true),
	member(Morph,Morphs),
	format('~a\t~a\t~a~a\t~@~N',[WD,Lemma,STTS,Lex,print_atomic_list(Morph,'.')]),
	fail.
output_mtxt:-
	\+ option(output(ltag)),
	setof(Morph,Lex^Score^gertwol(WD,Lemma,STTS,Morph,Lex,Score),Morphs),
	nonvar(STTS),
	member(Morph,Morphs),
	format('~a\t~a\t~a\t~@~N',[WD,Lemma,STTS,print_atomic_list(Morph,'.')]),
	fail.
output_mtxt.
output_ltaglex:-
	setof(STTS,(Morph^Score^(gertwol(WD,Lemma,STTS0,Morph,Lex,Score),concat_ltag(Lex,STTS0,STTS))),Tags),
	format('~a\t~a\t~@~N',[WD,Lemma,print_atomic_list(Tags,'\t')]),
	fail.
output_ltaglex.

output_txt:-
	bagof(_,Lex^Morph^Score^gertwol(WD,Lemma,STTS,Morph,Lex,Score),_),
	(var(STTS)
	->
		format('~a\t\t~a~N',[WD,Lemma])
	;
		format('~a\t~a\t~a~N',[WD,STTS,Lemma])
	),
	fail.
output_txt.

write_plgertwol5 :-
%	listing(gertwol),
	map_gertwol2stts:gertwol(W,L,T,M,Le,_),
	format('gertwol(~q,~q,~q,~q,~q).~N',[W,L,T,M,Le]),
	fail.
write_plgertwol5.

concat_ltag('',STTS,STTS):-!.
concat_ltag(LEX,STTS,STTSLEX):-
	atom_concat(STTS,LEX,STTSLEX).
