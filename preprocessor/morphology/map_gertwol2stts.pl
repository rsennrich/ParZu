%%% 
/*

TODO: - lexikalische Kategorie: VINF erkennen
      - Lexikalische Kategorie fuer andere Wortarten asl NN, NE, ADJA, ADJD

  
  assert_gertwol2stts

  Dynamically assert mapping clauses between GERTWOL categories and
STTS categories. STTS categories of the form * contain a literal '*'
atom. Every call to assert_gertwol2stts deletes any previous clauses
of the dynamic predicate gertwol2stts/4.


  assert_varstar_gertwol2stts

   Dynamically asssert mapping clauses between GERTWOL categories and
STTS categories. STTS categories of the form * are represented by an
anonymous variable.



gertwol2stts(STTSTag,GERTWOLTags,STTSMorphTags,Lemma)

Dynamically asserted clauses realizing the mapping between GERTWOL and
STTS.


assert_raw_gertwol2stts_gertwol(GertwolClause)

Maps GertwoClause which is a gertwol/4 fact into the STTS version of
it as dynamically asserted stts_gertwol/6 facts.

gertwol(Token,Lemma, TagInfo, STTSMorphInfo, Score)


option()

	   option(fix_lemma)
*/
:- module(map_gertwol2stts,[
							assert_gertwol2stts/0,
							assert_varstar_gertwol2stts/0,
							gertwol2stts/4,
							assert_raw_gertwol2stts_gertwol/1,
							gertwol/6,
							map_stream_raw_gertwol2stts_gertwol/1
						   ]).
:- use_module(library(lists)).
% :- use_module(feedback).

:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.

:- dynamic option/1.
 %option(debug).
%option(fix_lemma).

% nopragma(+GERTWOLFEATURE)
% All GERTWOL analysis which include GERTWOLFEATURE are discarded
option(nopragma('SELTEN')).

:- dynamic gertwol2stts/4.

:- dynamic gertwol/6.

revision('$Id: map_gertwol2stts.pl,v 1.16 2008/02/06 00:50:56 siclemat Exp $').

%:- dynamic gertwol2stts/4.
% gertwol2stts(STTSTag, GERTWOLPattern, STTSMorph, Lemma)
  % STTSTag: Das STTS Wortartenkategoriekuerzel
  % GERTWOLPattern: Die GERTWOLkategorienkuerzel als Liste
  % STTSMorph: Die STTS Morphologiekuerzel als Liste
  % Lemma: GERTWOL-Lemma

assert_gertwol2stts :-
	(current_predicate(map_gertwol2stts:gertwol2stts/4,_) -> abolish(gertwol2stts/4); true),
	assert_nn_patterns,
	assert_ne_patterns,
	assert_adja_patterns,
	assert_adjd_patterns,
	assert_card_patterns,
	assert_vafin_patterns,
	assert_vaimp_patterns,
	assert_vvfin_patterns, 
	assert_vvimp_patterns,
	assert_vmfin_patterns,
	assert_vvinf_patterns,
	assert_vainf_patterns,
	assert_vminf_patterns,
	assert_vvizu_patterns,
	assert_vvpp_patterns,
	assert_vmpp_patterns,
	assert_vapp_patterns,
	assert_art_patterns,
	assert_appr_patterns,
	assert_appo_patterns,
	assert_apzr_patterns,
	assert_apprart_patterns,
	assert_adv_patterns,
	assert_pper_patterns,
	assert_prf_patterns,
	assert_pposat_patterns,
	assert_pposs_patterns,
	assert_pdat_patterns,
	assert_pds_patterns,
	assert_pidat_patterns,
	assert_pis_patterns,
	assert_piat_patterns,
	assert_prels_patterns,
	assert_pwat_patterns,
	assert_pws_patterns,
	assert_pav_patterns,
	assert_koui_patterns,
	assert_kous_patterns,
	assert_kon_patterns,
	assert_kokom_patterns,
	assert_ptkzu_patterns,
	assert_ptkvz_patterns,
	assert_itj_patterns,
	assert_trunc_patterns,
	true.


assert_varstar_gertwol2stts :-
	assert_gertwol2stts,
	assert_varstar_gertwol2stts_aux.

assert_varstar_gertwol2stts :-
assert_varstar_gertwol2stts_aux.

assert_varstar_gertwol2stts_aux:-
	retract((gertwol2stts(A,B,C,D):- E)),
	star_to_var(C,CV),
	assertz((gertwol2stts(A,B,CV,D):- E)),
	fail.
assert_varstar_gertwol2stts_aux.

% Moegliche Genus-Patterns
nn_genus(Gertwol,STTS) :- genus(Gertwol,STTS).
nn_kasus(Gertwol, STTS) :- kasus(Gertwol, STTS), Gertwol \== nil.
nn_numerus(Gertwol, STTS) :- numerus(Gertwol, STTS), Gertwol \== 'SG/PL', Gertwol \== nil.
nn_flexion(Gertwol, STTS) :- flexion(Gertwol, STTS).

% Normale Substantive
assert_nn_patterns :-
	nn_genus(GGEN, SGEN),
	nn_kasus(GKAS, SKAS),
	nn_numerus(GNUM, SNUM),
%	nn_flexion(_GFLE, _SFLE),
	abkuerzung(GABK,SABK),
	List = [GABK,'S',GGEN,GNUM,GKAS],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('NN',List1,[SABK,SGEN,SKAS,SNUM,'*'], _)),
	fail.

% Flektierte Substantivierte Adjektive
assert_nn_patterns :-
	nn_genus(GGEN, SGEN),
	nn_kasus(GKAS, SKAS), GKAS \== nil,
	nn_numerus(GNUM, SNUM),
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	\+ (GNUM == 'SG', GGEN == nil),
	grad(GGRA, _),
	List = ['S(A)',GGRA,GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('NN',List1, ['<ADJ',SGEN,SKAS,SNUM,'*'],_)),
	fail.

% Unflektierte substantivierte Adjektive
assert_nn_patterns :-
	assert_gertwol2stts(gertwol2stts('NN',['S(A)','POS'],[ '<ADJ','*','*','*','*'],_)),
	fail.

% flektierte substantivierte Partizip Praesens und Perfekt
assert_nn_patterns :-
	nn_genus(GGEN, SGEN),
	nn_kasus(GKAS, SKAS),
	nn_numerus(GNUM, SNUM),
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	\+ (GNUM == 'SG', GGEN == nil),
	List = ['S(PART)','POS',GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('NN',List1,['<VPART',SGEN,SKAS,SNUM,'*'],_)),
	fail.
% unflektierte substantivierte Partizip Praesens und Perfekt

% Abkuerzungen ohne Flektion
assert_nn_patterns :-
	assert_gertwol2stts(gertwol2stts('NN',['ABK','S'],['<ABK','*','*','*','*'],_)),
	fail.
assert_nn_patterns.

% NE
ne_genus(Gertwol, STTS) :- genus(Gertwol, STTS).
ne_kasus(Gertwol, STTS) :- kasus(Gertwol, STTS).
ne_numerus(Gertwol, STTS) :- numerus(Gertwol, STTS), Gertwol \== 'SG/PL'.

assert_ne_patterns :-
	ne_genus(GGEN, SGEN),
	ne_kasus(GKAS, SKAS),
	ne_numerus(GNUM, SNUM),
	ne_cat(GKAT, _),
	abkuerzung(GABK,SABK),
	List = [GABK,'S','EIGEN',GKAT,GGEN,GNUM,GKAS],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('NE',List1,[SABK,SGEN,SKAS,SNUM],_)),
	fail.
assert_ne_patterns :-
	assert_gertwol2stts(gertwol2stts('NE',['EIGEN'],['','*','*','*'],_)),
	fail.

assert_ne_patterns.


% ADJA
% normale Adjektive
assert_adja_patterns :-
	grad(GGRA, SGRA), 
	GGRA \== 'nil', % Nicht steigerungsfaehige Adjektive sind in GERTWOL NUMs oder mit INV gekennzeichnet
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), GNUM \== 'SG/PL', GNUM \== nil,
	flexion(GFLE, SFLE), GFLE \== nil,
	List = ['A',GGRA,GNUM,GKAS,GGEN,GFLE],
	delete(List, nil, List1),
	length(List1, Len1),
	Len1 > 1,
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	assert_gertwol2stts(gertwol2stts('ADJA',List1,['',SGRA,SGEN,SKAS,SNUM,SFLE],_)),
	fail.
% Adjektive wie andere, vordere, obere, die keine Gradierungsinfo tragen

assert_adja_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), GNUM \== 'SG/PL', GNUM \== nil,
	flexion(GFLE, SFLE), GFLE \== nil,
	List = ['A',GNUM,GKAS,GGEN,GFLE],
	delete(List, nil, List1),
	length(List1, Len1),
	Len1 > 1,
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	assert_gertwol2stts(gertwol2stts('ADJA',List1,['','Pos',SGEN,SKAS,SNUM,SFLE],_)),
	fail.
% Adjektive aus flektiertem Partizip Perfekt und Praesens
assert_adja_patterns :-
	grad(GGRA, SGRA), 
	GGRA \== 'nil', % Nicht steigerungsfaehige Adjektive sind in GERTWOL NUMs oder mit INV gekennzeichnet
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), GNUM \== 'SG/PL', GNUM \== nil,
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	flexion(GFLE, SFLE), GFLE \== nil,
	List = ['A(PART)',GGRA,GNUM,GKAS,GGEN,GFLE],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('ADJA',List1,['<VPART',SGRA,SGEN,SKAS,SNUM,SFLE],_)),
	fail.



% unflektierte Adjektive aus Partizip Perfekt und Praesens
/*assert_adja_patterns :-
	grad(GGRA, SGRA), 
	GGRA \== 'nil',
	assert_gertwol2stts(gertwol2stts('ADJA',['A(PART)',GGRA],['<VPART',SGRA,'*','*','*','*'],_)),
	fail.
  */
% unflektier und ungradierbare Adjektive
assert_adja_patterns :-
	assert_gertwol2stts(gertwol2stts('ADJA',['A','INV'],['','*','*','*','*','*'],_)),
	fail.

% Ordinalzahlen
assert_adja_patterns :-
	assert_gertwol2stts(gertwol2stts('ADJA',['NUM','ORD'],['<ORD','*'],_)),
	fail.
	
assert_adja_patterns.

% ADJD
assert_adjd_patterns :-
	grad(GGRA, SGRA), 
	GGRA \== nil,	  % nicht-komparierbare Ordinalia, Bruchzahlen und
					  % invariante Adjektive werden speziell
					  % ausgezeichnet
	assert_gertwol2stts(gertwol2stts('ADJD',['A',GGRA], ['',SGRA],_)),
	assert_gertwol2stts(gertwol2stts('ADJD',['A(PART)',GGRA],['<VPART',SGRA],_)),
	fail.
assert_adjd_patterns :-
	assert_gertwol2stts(gertwol2stts('ADJD',['A','INV'], ['','*'],_)),
	fail.
% Superlativ 2 
assert_adjd_patterns :-
	GCAT = 'A',
	assert_gertwol2stts(gertwol2stts('ADJD',[GCAT,'SUP2','NOM'],['','Sup'],_)),
	fail.
assert_adjd_patterns :-
	GCAT = 'A(PART)',
	assert_gertwol2stts(gertwol2stts('ADJD',[GCAT,'SUP2','NOM'],['<VPART','Sup'],_)),
	fail.
	
assert_adjd_patterns :-
	assert_gertwol2stts(gertwol2stts('ADJD',['NUM','BRUCH'],['<FRACT','*'],_)),
	fail.
assert_adjd_patterns :-
	assert_gertwol2stts(gertwol2stts('ADJD',['NUM','ORD'],['<ORD','*'],_)),
	fail.

%%% GERTWOL BUG
%%% Gertwol kennt zwar positiv und Komparativ von deverbalen Adjektiven als ADJD, aber nicht vom Superlativ
%%% Als Workaround wird hier "un|be|wacht"  A(PART) SUP PL DAT STARK genommen

assert_adjd_patterns :-
	grad('SUP', SGRA), 
	genus(nil, _SGEN),
	kasus('DAT', _SKAS),
	numerus('PL', _SNUM),
	flexion('STARK', _SFLE),
	List = ['A(PART)','SUP','PL','DAT','STARK'],
	assert_gertwol2stts(gertwol2stts('ADJD',List,['<VPART',SGRA],_)),
	fail.	
assert_adjd_patterns.

% CARD
assert_card_patterns :-
	numeral(GNUM, _SNUM),
	num_notation(GNNO, _SNNO),
	\+ (GNNO == 'RÖM' , GNUM == 'BRUCH'),
	List = ['NUM',GNUM,GNNO],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('CARD',List1,[''],_)),
	fail.

assert_card_patterns.

% VAFIN
assert_vafin_patterns :-
	v_person(GPERNUM, SPER),
	v_numerus(GPERNUM, SNUM),
	tempus(GTEM, STEM),
	modus(GMOD, SMOD),
	List = ['V',GMOD,GTEM,GPERNUM],
	delete(List, nil, List1),
	assert_gertwol2stts((gertwol2stts('VAFIN',List1,['',SPER,SNUM,STEM,SMOD],L):- va_lemma(L))),
	fail.
	
assert_vafin_patterns.

% VAIMP
assert_vaimp_patterns :-
	v_numerus(GPERNUM, SNUM),
	assert_gertwol2stts((gertwol2stts('VAIMP',['V','IMP',_GTEM,GPERNUM], ['',SNUM],L) :- va_lemma(L))),
	fail.
	
assert_vaimp_patterns.

% VVFIN
assert_vvfin_patterns :-
	v_person(GPERNUM, SPER),
	v_numerus(GPERNUM, SNUM),
	tempus(GTEM, STEM),
	modus(GMOD, SMOD),
	trennbarkeit(GTRE,_),
	List = ['V',GTRE,GMOD,GTEM,GPERNUM],
	delete(List, nil, List1),
	assert_gertwol2stts((gertwol2stts('VVFIN',List1,['',SPER,SNUM,STEM,SMOD],L):- vv_lemma(L))),
	fail.
	
assert_vvfin_patterns.


% VVIMP
assert_vvimp_patterns :-
	v_numerus(GPERNUM, SNUM),
	assert_gertwol2stts((gertwol2stts('VVIMP',['V','IMP',_GTEM,GPERNUM], ['',SNUM],L) :- vv_lemma(L))),
	fail.
assert_vvimp_patterns.

% VMFIN
assert_vmfin_patterns :-
	v_person(GPERNUM, SPER),
	v_numerus(GPERNUM, SNUM),
	tempus(GTEM, STEM),
	modus(GMOD, SMOD),
	List = ['V',GMOD,GTEM,GPERNUM],
	delete(List, nil, List1),
	assert_gertwol2stts((gertwol2stts('VMFIN',List1,['',SPER,SNUM,STEM,SMOD],L):- vm_lemma(L))),
	fail.
	
assert_vmfin_patterns.

%VVINF
assert_vvinf_patterns :-
	trennbarkeit(GTRE,_),
	List = ['V',GTRE,'INF'],
	delete(List, nil, List1),
	assert_gertwol2stts((gertwol2stts('VVINF',List1,[''],L) :- vv_lemma(L))),
	fail.
	
assert_vvinf_patterns.

%VAINF
assert_vainf_patterns :-
	assert_gertwol2stts((gertwol2stts('VAINF',['V','INF'],[''],L) :- va_lemma(L))),
	fail.
	
assert_vainf_patterns.

%VMINF
assert_vminf_patterns :-
	assert_gertwol2stts((gertwol2stts('VMINF',['V','INF'],[''],L) :- vm_lemma(L))),
	fail.
	
assert_vminf_patterns.


%VVIZU
assert_vvizu_patterns :-
	assert_gertwol2stts(gertwol2stts('VVIZU',['V','TRENNBAR','INF','zu'],[''],_)).
						 
%VVPP
assert_vvpp_patterns :-
	trennbarkeit(GTRE, _STRE),
	List = ['V',GTRE,'PART','PERF'],
	delete(List, nil, List1),
	assert_gertwol2stts((gertwol2stts('VVPP',List1,[''],L):- vv_lemma(L))),
	fail.

assert_vvpp_patterns.

%VMPP
assert_vmpp_patterns :-
	assert_gertwol2stts((gertwol2stts('VMPP',['V','PART','PERF'],[''],L):- vm_lemma(L))).

%VAPP
assert_vapp_patterns :-
	assert_gertwol2stts((gertwol2stts('VAPP',['V','PART','PERF'],[''],L):- va_lemma(L))).

%ART
assert_art_patterns :-
	definitheit(GDEF, SDEF),
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), GNUM \== 'SG/PL', GNUM \== nil,
	List = ['ART',GDEF,GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	assert_gertwol2stts(gertwol2stts('ART',List1,['',SDEF,SGEN,SKAS,SNUM],_)),
	fail.
assert_art_patterns.

% APPR
assert_appr_patterns :-
	p_kasus(GKAS, SKAS),
	assert_gertwol2stts(gertwol2stts('APPR',['pre','PRÄP',GKAS],['',SKAS],_)),
	fail.
% das franz. á hat keinen Kasus in GERTWOL
assert_appr_patterns :-
	assert_gertwol2stts(gertwol2stts('APPR',['pre','PRÄP'],['','Nom'],_)),
	fail.
assert_appr_patterns.

%APPO
assert_appo_patterns :-
	p_kasus(GKAS, SKAS),
	assert_gertwol2stts(gertwol2stts('APPO',['post','PRÄP',GKAS],['',SKAS],_)),
	fail.
	
assert_appo_patterns.

%APZR
assert_apzr_patterns :-
	assert_gertwol2stts(gertwol2stts('APZR',['post','PRÄP',_GKAS],[''],_)),
	fail.
	
assert_apzr_patterns.

% APPRART
assert_apprart_patterns :-
	genus(GGEN, SGEN), GGEN \== nil,
	kasus(GKAS, SKAS),
	(GKAS == 'DAT' ; GKAS == 'AKK'),
	assert_gertwol2stts(gertwol2stts('APPRART',['PRÄP','ART','DEF','SG',GKAS, GGEN],['',SGEN,SKAS],_)),
	fail.

assert_apprart_patterns.

% ADV
assert_adv_patterns :-
	grad(GGRA, SGRA), % bald, gern, oft, wohl und sehr sind komparierbar
	List = ['ADV',GGRA],
	delete(List, nil, Liste1),
	assert_gertwol2stts(gertwol2stts('ADV',Liste1,['',SGRA],_)),
	fail.
assert_adv_patterns.

% PPER
assert_pper_patterns :-
	v_person(GPERNUM, SPER),
	v_numerus(GPERNUM, SNUM),
	genus(GGEN, SGEN),
	( 	% Genus ist nur fuer 3. Person singular definiert
	  SPER == 3 , SNUM == 'Sg', GGEN \== nil 
	;
	  SPER \== 3, GGEN == nil
	;
		SPER == 3, SNUM == 'Pl', GGEN == nil
	),						
	kasus(GKAS, SKAS), GKAS \== nil,
	List = ['PRON','PERS',GPERNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PPER',List1,['',SPER,SNUM,SGEN,SKAS],_)),
	fail.
assert_pper_patterns :-
	kasus(GKAS, SKAS),
	GKAS \== nil,
	hoeflich_numerus(_GNUM, SNUM),
	assert_gertwol2stts(gertwol2stts('PPER',['PRON','PERS','SG/PL',GKAS],['',3,SNUM,'*',SKAS],_)),
	fail.
assert_pper_patterns.

% PRF
assert_prf_patterns :-
	v_person(GPERNUM, SPER), SPER < 3,
	v_numerus(GPERNUM, SNUM),
	kasus(GKAS, SKAS),
	( GKAS == 'DAT' ; GKAS == 'AKK'),
	List = ['refl','PRON','PERS',GPERNUM,GKAS],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PRF',List1,['',SPER,SNUM,SKAS],_)),
	fail.

% Reziproke Pronomen werden in stts wie Reflexive behandelt

assert_prf_patterns :-
	% einander
	assert_gertwol2stts(gertwol2stts('PRF',[rez,'PRON','PERS'],['','*','*','*'],_)),
	% sich
	assert_gertwol2stts(gertwol2stts('PRF',[rez,'PRON','PERS','PL3'],['',3,'Sg','Dat'],_)),
	assert_gertwol2stts(gertwol2stts('PRF',[rez,'PRON','PERS','PL3'],['',3,'Sg','Akk'],_)),
	fail.

assert_prf_patterns.

% PPOSAT
assert_pposat_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), (GNUM = 'SG';GNUM = 'PL'),
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	List = ['poss','DET','PERS',GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PPOSAT',List1,['',SGEN,SKAS,SNUM],_)),
	fail.
assert_pposat_patterns.

% PPOSS
assert_pposs_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM), (GNUM = 'SG';GNUM = 'PL'),
	\+ (GNUM == 'PL', GGEN \== 'nil'),
	List = ['poss','PRON','PERS',GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PPOSS',List1,['',SGEN,SKAS,SNUM],_)),
	fail.
assert_pposs_patterns.

%PDAT
% Ausdruecke wie "derlei" haben keinen Numerus und Genus in GERTWOL
assert_pdat_patterns :-
	kasus(GKAS,SKAS), GKAS \== nil,
	assert_gertwol2stts(gertwol2stts('PDAT',['DET','DEM',GKAS],['','*',SKAS,'*'],_)),
	fail.

assert_pdat_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM),
	( GNUM == 'SG' ; GNUM == 'PL' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	List = ['DET','DEM',GNUM,GKAS,GGEN],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('PDAT',List1,['',SGEN,SKAS,SNUM],_)),
	fail.

assert_pdat_patterns.

% PDS
% Ausdruecke wie "derlei" haben keinen Numerus und Genus in GERTWOL
assert_pds_patterns :-
	kasus(GKAS,SKAS), GKAS \== nil,
	assert_gertwol2stts(gertwol2stts('PDS',['PRON','DEM',GKAS],['','*',SKAS,'*'],_)),
	fail.

assert_pds_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM),
	( GNUM == 'SG' ; GNUM == 'PL' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	List = ['PRON','DEM',GNUM,GKAS, GGEN],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('PDS',List1,['',SGEN,SKAS,SNUM],_)),
	fail.

assert_pds_patterns.

% PIDAT
% Gertwol liefert fuer "solch" spezielle Lesart
assert_pidat_patterns :-
	assert_gertwol2stts(gertwol2stts('PIDAT',['A'],['','*','*','*'],solch)),fail.

assert_pidat_patterns :-
	assert_gertwol2stts(gertwol2stts('PIDAT',['A'],['','*','*','*'],solche)).

% PIS
assert_pis_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS),
	numerus(GNUM, SNUM),
	( GNUM == 'SG' ; GNUM == 'PL' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	negation(GNEG,_SNEG),
	List = ['PRON','INDEF',GNEG,GNUM,GKAS,GGEN],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('PIS',List1,['',SGEN,SKAS,SNUM],_)),
	fail.

assert_pis_patterns :-
  	assert_gertwol2stts(gertwol2stts('PIS',['PRON','INDEF','SG3','NOM'],['','*','Nom','Sg'],man)),
	fail.
assert_pis_patterns.

% PIAT
assert_piat_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS),
	numerus(GNUM, SNUM),
	( GNUM == 'SG' ; GNUM == 'PL' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	negation(GNEG,_SNEG),
	List = ['DET','INDEF',GNEG,GNUM,GKAS,GGEN],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('PIAT',List1,['',SGEN,SKAS,SNUM],_)),
	fail.

assert_piat_patterns.

% PRELAT
%assert_prelat_pattern :-

% PRELS
assert_prels_patterns :-
	genus(GGEN, SGEN),
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM),
	( GNUM == 'SG' ; GNUM == 'PL' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	List = ['PRON','RELAT',GNUM,GKAS,GGEN],
	delete(List,nil,List1),
	assert_gertwol2stts(gertwol2stts('PRELS',List1,['',SGEN,SKAS,SNUM],_)),
	fail.

assert_prels_patterns.

% PWAT
assert_pwat_patterns :-
	genus(GGEN,SGEN), 
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM),
	( GNUM == 'PL' ; GNUM == 'SG' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	\+ (GNUM == 'SG', GGEN == nil),
	List = ['DET','INTERROG',GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PWAT',List1,['',SGEN,SKAS,SNUM],_)),
	fail.
% wessen
assert_pwat_patterns :-
	assert_gertwol2stts(gertwol2stts('PWAT',['DET','INTERROG','GEN'],['','*','*','*'],wessen)),
	fail.

assert_pwat_patterns.

% PWS
assert_pws_patterns :-
	genus(GGEN,SGEN), 
	kasus(GKAS, SKAS), GKAS \== nil,
	numerus(GNUM, SNUM),
	( GNUM == 'PL' ; GNUM == 'SG' ),
	\+ (GNUM == 'PL', GGEN \== nil),
	\+ (GNUM == 'SG', GGEN == nil),
	List = ['PRON','INTERROG',GNUM,GKAS,GGEN],
	delete(List, nil, List1),
	assert_gertwol2stts(gertwol2stts('PWS',List1,['',SGEN,SKAS,SNUM],_)),
	fail.
assert_pws_patterns :-
	kasus(GKAS, SKAS), GKAS \== nil, GKAS \== 'GEN',
	assert_gertwol2stts(gertwol2stts('PWS',['PRON','INTERROG',GKAS],['','*',SKAS,'Sg'],_)),
	fail.
% wessen
assert_pws_patterns :-
	assert_gertwol2stts(gertwol2stts('PWS',['PRON','INTERROG','GEN'],['','*','Gen','Sg'],wessen)),
	fail.

assert_pws_patterns.

% PWAV
% sind als Adverbien in GERTWOL kodiert


%PAV
% Stimmt z.T. nicht!
assert_pav_patterns :-
	assert_gertwol2stts(gertwol2stts('PAV',['PRONADV'],[''],_)).

% KOUI
assert_koui_patterns :-
	assert_gertwol2stts(gertwol2stts('KOUI',['iKONJ'],[''],_)).
% KOUS
assert_kous_patterns :-
	assert_gertwol2stts(gertwol2stts('KOUS',['uKONJ'],[''],_)).
% KON
assert_kon_patterns :-
	assert_gertwol2stts(gertwol2stts('KON',['nKONJ'],[''],_)).
% KOKOM
assert_kokom_patterns :-
	assert_gertwol2stts(gertwol2stts('KOKOM',['sKONJ'],[''],_)).
%PTKZU
assert_ptkzu_patterns :-
	assert_gertwol2stts(gertwol2stts('PTKZU',['iKONJ'],[''],zu)).

% PTKVZ
assert_ptkvz_patterns :-
	assert_gertwol2stts(gertwol2stts('PTKVZ',['PRÄF'],[''],_)).

% ITJ
assert_itj_patterns :-
	assert_gertwol2stts(gertwol2stts('ITJ',['INTERJ'],[''],_)).
	
%TRUNC
assert_trunc_patterns :-
	assert_gertwol2stts(gertwol2stts('TRUNC',['ERSTGLIED'], [''],_)).



test :-
	retractall(gertwol2stts),
	assert_gertwol2stts,
	apply_patterns,
	fail.
test.


export_patterns(File) :-
	tell(File),
	listing(gertwol2stts),
	told.

export_patterns :-
	assert_gertwol2stts,
	export_file(File),
	export_patterns(File).

export_file('/home/ludwig7/siclemat/MacClematide/Public/projekte/gertwol2prolog/patterns.pl').

export_var_patterns(File) :-
	assert_gertwol2stts,
	tell(File),
	clause(gertwol2stts(Tag,GertTags,STTS,Lemma),Rumpf),
	star_to_var(STTS,STTSVar),
	(
	  Rumpf == true
	->
	  writeq(gertwol2stts(Tag,GertTags,STTSVar, Lemma)),
	  write('.'),nl
	;
	  writeq((gertwol2stts(Tag,GertTags,STTSVar, Lemma):- Rumpf)),
	  write('.'),nl
	  ),
	fail.
export_var_patterns(_):-
	told.

star_to_var([],[]).
star_to_var(['*'|R],[_|R2]) :-
  !,
  star_to_var(R, R2).
star_to_var([E|R],[E|R2]) :-
  star_to_var(R, R2).

	
% 
apply_patterns :-
	user:raw_gertwol(_Token, Lemma, AnalyseList),
	member([MorphInfo,PragmaInfo,CapInfo], AnalyseList),
	if(	gertwol2stts(MorphInfo,TagInfo, STTSMorphInfo)
	,	pp_gertwol2stts(Lemma,MorphInfo,TagInfo, STTSMorphInfo)
	,
	(	write('### no matching gertwol2stts for '),
		write([MorphInfo,PragmaInfo,CapInfo]),nl
	)
	),
	fail.
apply_patterns.

pp_gertwol2stts(Lemma, MorphInfo, TagInfo, STTSMorphInfo):-
	concat_atoms(MorphInfo, ' ', MorphInfoAtom),
	concat_atoms(STTSMorphInfo, '.',STTSMorphsAtom),
	format("~a/~a:~a\t~a~N",[Lemma,TagInfo,STTSMorphsAtom,MorphInfoAtom]).
	
concat_atoms(AtomList, Divider, Result) :-
	name(Divider, DividerCodes),
	concat_atoms_aux(AtomList, DividerCodes, ConcatCodeList),
	atom_chars(Result, ConcatCodeList).

concat_atoms_aux([Atom], _, Codes):-
	!,
	name(Atom, Codes).
concat_atoms_aux([Atom|Rest], Divider, Result) :-
	name(Atom, Codes),
	concat_atoms_aux(Rest, Divider, RestCodes),
	append(Divider, RestCodes, DivRestCodes),
	append(Codes, DivRestCodes, Result).
	

stts_atom_of_structure(stts(Atom,'',''), Atom) :- !.
stts_atom_of_structure(stts(A1,A2,''), Atom) :-
	!,
	concat_atoms([A1,A2],'',Atom).
stts_atom_of_structure(stts(A1,A2,A3), Atom) :-
	!,
	concat_atoms([A1,A2,A3], '',Atom).



assert_gertwol2stts((H:-R)):-
!,
(
 clause(H,R)
->
 true
;
 assertz((H:-R))
 ).
assert_gertwol2stts(F):-
	!,
	(
	  clause(F,true)
	->
	  true
	;
	  assertz(F)
	).
definitheit('DEF', 'Def').
definitheit('INDEF', 'Indef').

genus('FEM','Fem').
genus('MASK','Masc').
genus('NEUTR','Neut').
genus(nil, '*').


kasus('NOM','Nom').
kasus('GEN','Gen').
kasus('DAT','Dat').
kasus('AKK','Akk').
kasus(nil, '*').

p_kasus('Akk','Akk').
p_kasus('Dat', 'Dat').
p_kasus('Gen', 'Gen').

numerus('SG','Sg').
numerus('PL','Pl').
numerus('SG/PL','Sg').
numerus('SG/PL','Pl').
numerus(nil, '*').

negation('NEG',_).
negation(nil, _).

numeral('KARD',_).
numeral('BRUCH',_).

num_notation(nil,_).
num_notation('RÖM',_).

hoeflich_numerus('SG/PL','Sg').
hoeflich_numerus('SG/PL','Pl').

flexion('STARK','St').
flexion('SCHWACH','Sw').
flexion(nil,'*').

v_person('SG1', 1).
v_person('PL1', 1).
v_person('SG2', 2).
v_person('PL2', 2).
v_person('SG3', 3).
v_person('PL3', 3).

p_person(GPER, SPER) :-
	v_person(GPER, SPER).
p_person('SG/PL', 3).

v_numerus('SG1', 'Sg').
v_numerus('SG2', 'Sg').
v_numerus('SG3', 'Sg').
v_numerus('PL1', 'Pl').
v_numerus('PL2', 'Pl').
v_numerus('PL3', 'Pl').

grad('POS','Pos').
grad('KOMP','Comp').
grad('SUP','Sup').
grad(nil, '*').

tempus('PRÄS', 'Pres').
tempus('PRÄT', 'Past').

modus('IND', 'Ind').
modus('KONJ', 'Konj').

abkuerzung('ABK','<ABK').
abkuerzung(nil,'').

trennbarkeit('TRENNBAR',_).
trennbarkeit(nil,_).

ne_cat('Vorname',_).
ne_cat('Famname',_). 
ne_cat(nil, _).


zu('zu',_).
zu(nil,_).


% Modalverb-Gertwol-Lemmata
vm_lemma('woll~en').
vm_lemma('soll~en').
vm_lemma('müss~en').
vm_lemma('mög~en').
vm_lemma('könn~en').
vm_lemma('dürf~en').

% Hilfsverb-Gertwol-Lemmata
va_lemma('werd~en').
va_lemma('hab~en').
va_lemma('sein').

% Vollverb-Gertwol-Lemmata
vv_lemma(L) :-
	\+ (
		 vm_lemma(L)
	   ;
		 va_lemma(L)
	   ).
assert_raw_gertwol2stts_gertwol(raw_gertwol(T,_L,'<unknown>')):- !,
	assertz(gertwol(T,'<unknown>',_,_,_,_)).
assert_raw_gertwol2stts_gertwol(raw_gertwol(Token,Lemma0,AnalyseList)) :-
	member([MI,PI,_CI],AnalyseList),
	findall(_,(option(nopragma(GertwolTag)),memberchk(GertwolTag,PI)),[]),
	score(PI, Score),
	gertwol2stts(TagInfo, MI, [STTSLexInfo|STTSMorphInfo], Lemma0),
	(
	  option(fix_lemma)
	->
	  fix_lemma_by_tag(TagInfo,Lemma0, Lemma)
	;
	  Lemma0 = Lemma
	  ),
	( % Falls schon eine schlechtere Lemmatisierung existiert
		catch(
			  check_lemma(Token, Lemma, TagInfo, Score),
			  gertwol_exc(BadLemma, Score2),
			  (
				(% Falls existierende Lemma schlecht ist
				  BadLemma \== Lemma
				->				% Dann loesche sie in Wissensbasis
					(option(debug)
					->
						format(user_error,"#Retracting gertwol(~a,~a,~p,~d)~N",
							   [Token,BadLemma,TagInfo,Score2])
					;true
					),
					retractall(gertwol(Token,BadLemma,TagInfo,_,STTSLexInfo,Score2))
								% Sonst mache gar nix!
				)
			  )
			 )
	->
		Clause=gertwol(Token, Lemma, TagInfo, STTSMorphInfo,STTSLexInfo, Score),
		(Clause -> true; assertz(Clause)),
		(option(debug) -> format(user_error,"#Asserted: ~p~N",[Clause]); fail)
	).
%build_gertwol(Tag,Token,Lemma,STTSMorph,Score

check_lemma(Token, Lemma, Tag, Score) :-
	(option(keep_lemma(Lemma)) -> fail;true),
	gertwol(Token, Lemma2, Tag, _,_, Score2),
	(integer(Score2) -> true; write(user_error,'## map_gertwol2stts Score2 wrong type'),fail),
	(integer(Score) -> true; write(user_error,'## map_gertwol2stts Score wrong type'),fail),
	(option(debug)-> format(user_error,"#Checking new lemma ~a/~d against ~a/~d~N",[Lemma2,Score2,Lemma,Score]);true),
	Lemma \== Lemma2,
	(
	  Score2 > Score
	->
	  (
		!,
		throw(gertwol_exc(Lemma2,Score2))
	  )
	;
	  (
		Score2 < Score
	  ->
		!,
		raise_exception(gertwol_exc(Lemma,Score))
	  ;
		(
		  option(debug)
		->
		  format(user_error,"# Warning: Different Lemma ~a and ~a for token ~a have same score.~N",[Lemma,Lemma2,Token]),
		  fail
		  )
	  
	  
	  )
	  
	).
check_lemma(_,_,_,_).


fix_lemma_by_tag('ART',_,'$ART$'):- !.
fix_lemma_by_tag('PPER',_,'$PPER$'):- !.
fix_lemma_by_tag('PDS',_,'$PDS$'):- !.
fix_lemma_by_tag('PRELS',_,'$PRELS$'):- !.
fix_lemma_by_tag('APPRART','an-der','an'):- !.
fix_lemma_by_tag('APPRART','an-das','an'):- !.
fix_lemma_by_tag('APPRART','auf-das','auf'):- !.
fix_lemma_by_tag('APPRART','bei-der','bei'):- !.
fix_lemma_by_tag('APPRART','bei-das','bei'):- !.
fix_lemma_by_tag('APPRART','durch-das','durch'):- !.
fix_lemma_by_tag('APPRART','für-das','für'):- !.
fix_lemma_by_tag('APPRART','hinter-der','hinter'):- !.
fix_lemma_by_tag('APPRART','hinter-das','hinter'):- !.
fix_lemma_by_tag('APPRART','in-das','in'):- !.
fix_lemma_by_tag('APPRART','in-der','in'):- !.
fix_lemma_by_tag('APPRART','um-das','um'):- !.
fix_lemma_by_tag('APPRART','unter-das','unter'):- !.
fix_lemma_by_tag('APPRART','unter-der','unter'):- !.
fix_lemma_by_tag('APPRART','von-das','von'):- !.
fix_lemma_by_tag('APPRART','von-der','von'):- !.
fix_lemma_by_tag('APPRART','vor-das','vor'):- !.
fix_lemma_by_tag('APPRART','vor-der','vor'):- !.
fix_lemma_by_tag('APPRART','zu-das','zu'):- !.
fix_lemma_by_tag('APPRART','zu-der','zu'):- !.
fix_lemma_by_tag('APPRART','über-das','über'):- !.
fix_lemma_by_tag('APPRART','über-der','über'):- !.

fix_lemma_by_tag(_,X,X).



score([],0).
score([Number|_],Score):-
	(
	  integer(Number)
	->
	  Score = Number
	;
	  Score = 0
	).

map_stream_raw_gertwol2stts_gertwol(S):-
% 	init_feedback(map_stream_raw_gertwol2ssts_gertwol),
	repeat,
	(
	  at_end_of_stream(S)
	->
	!
	;
	  read(S,T),
% 	  feedback(map_stream_raw_gertwol2ssts_gertwol,10),
	  assert_raw_gertwol2stts_gertwol(T),
	  fail
	).

