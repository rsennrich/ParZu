% Copyright © 2009-2011 University of Zürich
% Author: Rico Sennrich <sennrich@cl.uzh.ch>

% This module assigns a local (pseudo-)probability to each edge in the tree
% We obtain the probability of the parse by multiplying all edge probabilities.
% This works nicely for the disambiguation between different possible labels (i.e. subj vs. obja vs. objd) between two nodes.
% But we also need to consider competing structures:
% a noun could not only be subj/obja/objd of a verb, but also genitive modifier or apposition of another noun.
% This is where things get ugly: Since locally, we do not know if any "better" structures are grammatically possible,
% we use constant pseudo-probabilities (or constant factors and upper/lower limits for probabilities) all over the code, mostly to prefer one structure over the other.
% We also heuristically modifiy the probabilities to ensure that, in a phrase such as "A sees B", 
% where A and B could both be either subj or obja, we prefer the parse in which the subject comes first.
% If you start tinkering with the numbers (a more theoretically founded system would be welcome), always check with a development set that you don't break anything.


:- style_check(-discontiguous).

:- ensure_loaded('../core/helper_predicates.pl').
:- ensure_loaded('../core/morph_predicates.pl').

%word classes other than nouns are penalised so that they are only chosen as head of an NP if there is no better alternative (removed from grammar for now, since too many FPs).
stats2(det,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).
stats2(det,'CARD',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.3,_D,_HC,_DC).
stats2(attr,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.3,_D,_HC,_DC).
stats2(attr,'CARD',_FH,SH,_MORPHH,_Dtag,_FD,SD,_MORPHD,0.1,_D,_HC,_DC) :- lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    RealDist is HPos-DPos,
    RealDist > 0.

%noun phrases that do not meet congruency constraints may still be allowed by parser
stats2(bad_det,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).
stats2(bad_attr,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).
stats2(bad_pn,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).

%word classes other than articles or attributive pronouns should only be analysed as determiners if there is no other option.
stats2(det,_Htag,_FH,_SH,_MORPHH,'PRELS',_FD,_SD,_MORPHD,0.45,_D,_HC,_DC).
stats2(det,_Htag,_FH,_SH,_MORPHH,'PWS',_FD,_SD,_MORPHD,0.3,_D,_HC,_DC).

stats2(pn,'PTKA',_FH,_SH,_MORPHH,_DTag,_FD,_SD,_MORPHD,0.5,_D,_HC,_DC). %is only in grammar in case of tagging errors; should not be preferred
stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADV',_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADJD',_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'PP',_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADJA',_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'CARD',_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).

% hack for "er sieht drei bis fünf Leute" (with bis/APPR)
stats2(pn,'APPR',bis,_SH,_MORPHH,'CARD',_FD,_SD,_MORPHD,1,_D,_HC,_DC).
stats2(pp,'CARD',_FH,_SH,_MORPHH,'PP_bis',_FD,_SD,_MORPHD,2,_D,_HC,_DC).

stats2(unknown,_Htag,_FH,_SH,_MORPHH,_,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).


%might be useful in case of tagging errors
stats2(part,_,_FH,_SH,_MORPHH,'ART',_FD,_SD,_MORPHD,0.9,_D,_HC,_DC).


%expl. should only be used together with objc/subjc.
stats2(explsubj,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,_D,_HC-OG,_DC) :- ((member('->subjc->',OG);member('->obji->',OG);member('<-subjc<-',OG);member('<-obji<-',OG))-> P is 1.06; P is 0.2).

stats2(explobja,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,_D,_HC-OG,_DC) :- ((member('->objc->',OG);member('<-objc<-',OG))-> P is 1.06; ((member('->obji->',OG);member('<-obji<-',OG))->P is 0.8;P is 0.2)).

stats2(vok,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).

stats2(comma,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1,_D,_HC,_DC).

%use the conjunction to distinguish between subordinated clauses and clausal objects
stats2(konjneb,_Htag,_FH,_SH,_MORPHH,_Dtag,FD,_SD,_MORPHD,P,_D,_HC-OG,_DC) :-
	member('<-konjneb<-',OG)->P is 1.06;(
	downcase_atom(FD,FL), 
	(konjstats(FL,neb,NEB);NEB is 0),
	(konjstats(FL,objc,OBJC);OBJC is 0),
	(NEB + OBJC =:= 0-> Ratio is 1;Ratio is (NEB+1)/(NEB+OBJC+1)),
	(Ratio > 0.7->(P is 1.06);(
	Ratio< 0.3->P is 0.05;P is Ratio))).


stats2(konjobjc,_Htag,_FH,_SH,_MORPHH,_Dtag,FD,_SD,_MORPHD,P,_D,_HC-OG,_DC) :-
	member('<-konjneb<-',OG)->P is 0;(
	downcase_atom(FD,FL), 
	(konjstats(FL,neb,NEB);NEB is 0),
	(konjstats(FL,objc,OBJC);OBJC is 0),
	(NEB + OBJC =:= 0-> Ratio is 0;Ratio is (OBJC+1)/(NEB+OBJC+1)),
	(Ratio > 0.7->P is 1.06;(
	Ratio< 0.3->P is 0;P is Ratio))).


%the following structure always have precedence:
stats2(pn,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,_D,_HC,_DC).
stats2(attr,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,_D,_HC,_DC).
stats2(det,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,_D,_HC,_DC).
stats2(aux,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,_D,_HC,_DC).
stats2(avz,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,_D,_HC,_DC).

%adverbs

%exception: nicht (nur) X, sondern (auch) Y: prefer attaching 'nicht' to head of coordination
stats2(adv_kon,_Htag,_FH,SH,_MORPHH,_Dtag,_FD,SD,_MORPHD,P,D,_HC,_DC) :-
    lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    RealDist is HPos-DPos,
    RealDist > 0,
    distModifier(D,adv,DistMod),
    P is DistMod.


stats2(adv,Htag,_FH,SH,_MORPHH,Dtag,FD,SD,_MORPHD,P,D,_HC,_DC) :-
    lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    RealDist is HPos-DPos,
    getadvprob(Htag,Dtag,FD,RealDist,POSMod),
    distModifier(D,adv,DistMod),
    P is DistMod * POSMod.


%getadvprob(+Htag,+Dtag,+DWord,+RealDist,-POSMod)

%sie sind alle tot.
getadvprob(_,'PIS',all,RealDist,P) :- P is 0.2-RealDist*0.003, !.
getadvprob(_,'PIS',alle,RealDist,P) :- P is 0.2-RealDist*0.003, !.
getadvprob(_,'PIS',alles,RealDist,P) :- P is 0.2-RealDist*0.003, !.
getadvprob(_,'PWAV',_,_,0.8) :- !.

%ein bisschen traurig
getadvprob(_,'PIS',DWord,RealDist,P) :- member(DWord,['bißchen',bisschen,wenig]), P is 0.2+RealDist*0.003, !.

%these occur in the training corpus, but very rarely
% getadvprob(_,'PIS',DWord,RealDist,P) :- member(DWord,[andere,beide,einer,etwas,jeder,meisten,nichts,solche,soviel,sowas,viele]), P is 0.1-RealDist*0.005, !.

getadvprob(_,'PIS',_,_,0) :- !.

%lexical disambiguation based on number of times the adverb occurs with different POS.
getadvprob(Htag,_Dtag,DWord,RealDist,POSMod2) :- (Htag = 'KOMPX' -> Htag2 = 'kokom' ; Htag = 'PP' -> Htag2 = 'appr' ; downcase_atom(Htag,Htag2)),
		    (verbtag(Htag2)->Htag3='v';Htag3=Htag2),
		    downcase_atom(DWord,DWordL),
		    (RealDist > 0->advbigramleft(DWordL,Htag3,Total,ADV); advbigramright(Htag3,DWordL,Total,ADV)),
		      (Total > 10; ADV > 0),
		      POSMod is ADV / Total,
             (verbtag(Htag2)->POSMod2 is max(0.5+POSMod/100,POSMod);POSMod2 is max(0.06,POSMod)), !.

%backoff probability for verbs
getadvprob(Htag,_,_,_,0.55) :- (nonfinite(Htag);Htag = 'VVFIN';Htag = 'VAFIN';Htag = 'VMFIN';Htag = 'VVIZU'), !.

%catchall
getadvprob(_Htag,_Dtag,_DWord,_RealDist,0.15) :- !.


%zeit should be lower than app.
stats2(zeit,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC,_DC).
stats2(zeit,_Htag,_FH,_SH,_MORPHH,'CARD',_FD,_SD,_MORPHD,0.8,_D,_HC,_DC).
stats2(zeit,_Htag,_FH,_SH,_MORPHH,'NN',_FD,_SD,_MORPHD,0.2,_D,_HC,_DC).


%sentence coordinations: should be lower than 's' relation (if the latter occurs in training corpus)
stats2(konc,_Htag,_FH,_SH,_MORPHH,_,_FD,_SD,_MORPHD,P,D,_HC,_DC) :- P is  0.65 - D*0.001.

%coordinations: encourage short distances. Should have precedence over APP
stats2(kon,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :-
	posModifier(Htag,Dtag,kon,PosMod),
	P is  PosMod*(1.06-D*0.05).



%grad (measurement)
stats2(grad,Htag,FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,_D,_HC,_DC) :-
    (Htag='PP';Htag='PIAT';adverbial_pronoun(Htag)), !,
    (adverbial_pronoun(Htag)->derive_prep_from_pav(FH,Prep);Prep=FH),
    splitappr(Prep,Prep2,_),
    (grad_head(Prep2)->P is 0.6; P is 0.1).

stats2(grad,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.3,_D,_HC,_DC).

%closing bracket - better than having two opening ones / prefer short distances
stats2(bracket,_Htag,_FH,SH,_MORPHH,_Dtag,_FD,SD,_MORPHD,P,_D,_HC-OG,_DC) :-
    lexic(SD,_,RIGHTPOS),
    lexic(SH,_,MIDPOS),
    append(_,[BRACKET, '<-bracket<-'|_],OG),
    BRACKET =.. [LEX|_],
    lexic(LEX,_,LEFTPOS),
    LEFTPOS < MIDPOS,
    MIDPOS < RIGHTPOS,
    Dist is RIGHTPOS - LEFTPOS,
    P is 1.06 - Dist*0.01.

stats2(bracket,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.5,_D,_HC,_DC).

stats2(badbracket,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.55,_D,_HC,_DC).

%loose apposition
stats2(app_loose,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,D,_HC-_OG,_DC) :-
    distModifier(D,app_loose,DISTMOD),
    P is DISTMOD.

%close appositions: encourage short distances.
stats2(app_close,Htag,FH,_SH,_MORPHH,Dtag,FD,_SD,MORPHD,P,D,_HC-OG,_DC) :-
    lexModifier(Htag,Dtag,FH,FD,app_close,LEXMOD),
    posModifier(Htag,Dtag,app_close,POSMOD),
    distModifier(D,app_close,DISTMOD),
    depModifier(OG,app_close,DEPMOD),
    ((case_gen(MORPHD,Dtag), \+ case_nom(MORPHD,Dtag))->MORPHMOD is 0.5; MORPHMOD is 1), %make sure that gmod beats app if possible
    P is LEXMOD*DISTMOD*POSMOD*DEPMOD*MORPHMOD.


stats2(objp,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,D,HC-OG,DC) :-
        statsppobjp(objp,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,D,HC-OG,DC).


% PPs pre-modifying NPs are pretty rare, so these rules should only be used if there's no other choice. Still produces some false positives...
stats2(pp,Htag,_FH,SH,_MORPHH,_Dtag,_FD,SD,_MORPHD,0.05,_D,_,_DC) :-
    (Htag = 'NN'; Htag = 'NE'; Htag = 'PDS'; Htag = 'PPER'; Htag = 'PIS'; Htag = 'FM'),
    lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    HPos > DPos.

stats2(pp,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,D,HC-OG,DC) :-
        statsppobjp(pp,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,D,HC-OG,DC).

statsppobjp(Rel,_Htag,_FH,_SH,_MORPHH,Dtag,FD,_SD,MORPHD,P,D,HC-_OG,_DC) :-
        getheadandnormalise(HC,Head,HeadTagTemp),
        (verbtag(HeadTagTemp)->HeadTag=v;HeadTag=HeadTagTemp),
        distModifier(D,HeadTag,Dtag,Rel,DISTMOD),
        downcase_atom(FD,FDNorm), %(statistics files are in lower case letters).
        (splitappr(FDNorm,Prep,_),!;Prep = FDNorm),
        (var(MORPHD) -> List = [];setof(Case,member([Case],MORPHD),List)),
        ((List = [CaseTemp], \+ var(CaseTemp))->(case_tueba(CaseTemp,Case)); Case = _),
        get_pp_statistics(Head,HeadTag,Case,Prep,NumPP1,NumObjP1,NumHead),
        (((adverbial_pronoun(Dtag);Dtag='PWAV'),derive_prep_from_pav(Prep,Prep2))->get_pp_statistics(Head,HeadTag,Case,Prep2,NumPP2,NumObjP2,_);(NumPP2=0,NumObjP2=0)),
        NumPP is NumPP1 + NumPP2,
        NumObjP is NumObjP1 + NumObjP2,
        NumPPOBJP is NumPP + NumObjP,
        ppPosMod(Prep,Case,HeadTag,POSMOD),
        ppobjp(NumPP,NumObjP,Rel,PPOBJP),
        pplexmod(NumPPOBJP,NumHead,LEXMOD),
        noun_factor(HeadTag,NF),
        P is PPOBJP*min(1,DISTMOD*0.25+POSMOD*0.3+LEXMOD*NF*0.6).


%ppobjp/4 decides whether construction is 'pp' or 'objp'.
ppobjp(NumPP,NumObjP,pp,0.15) :- NumPP+NumObjP > 0, NumPP =< NumObjP, !.
ppobjp(NumPP,NumObjP,pp,1) :- NumPP+NumObjP > 0, NumPP > NumObjP, !.
ppobjp(_,_,pp,1) :- !.

ppobjp(NumPP,NumObjP,objp,0.95) :- NumPP+NumObjP > 0, NumPP =< NumObjP, !.
ppobjp(NumPP,NumObjP,objp,0) :- NumPP+NumObjP > 0, NumPP > NumObjP, !.
ppobjp(_,_,objp,0) :- !.


%bilexical disambigation of pp attachment. Formula doesn't make sense stochastically, but we're only interested in which attachment has highest value.
pplexmod(NumPPOBJP,NumHead,P) :- NumPPOBJP > 0, NumHead>0, PTemp is 10*NumPPOBJP / NumHead, PTemp2 is max(0.1,PTemp), P is min(PTemp2,1), !.
pplexmod(NumPPOBJP,NumHead,P) :- NumPPOBJP = 0, P is max(0,0.25-NumHead/1000), !.
pplexmod(_,_,0.1) :- !.

%get_pp_statistics(+Head,+HeadTag,+DepMorph,+Prep,-NumPP,-NumObjP,-NumHead)
get_pp_statistics(Head,HeadTag,Case,Prep,NumPP,NumObjP,NumHead) :-
	((HeadTag='card';HeadTag='ne')->Head2='*any*';Head2=Head), %assume that names/numbers are interchangeable
	findall(NumHead,occurs(Head2,HeadTag,NumHead),ListHead),
	sumlist(ListHead,NumHead),
	findall(NumObjP,hasobjp(Head2,HeadTag,Prep,Case,NumObjP),ListObjP),
	sumlist(ListObjP,NumObjP),
	findall(NumPP,haspp(Head2,HeadTag,Prep,Case,NumPP),ListPP),
	sumlist(ListPP,NumPP), !.


%statistics are slightly biased against noun heads; compensate this.
noun_factor(nn,1.2) :- !.
noun_factor(_,0.8) :- !.


%non-lexical disambiguation of pp attachment: only consider preposition and PoS of head.
%ppPosMod(+Prep,?Case,+HeadTag,-P)
ppPosMod(Prep,Case,Tag,P) :- findall(MyFreq,(haspp('*any*','*any*',Prep,Case,MyFreq), Case \= '*any*'),List1),
				 findall(MyFreq,(hasobjp('*any*','*any*',Prep,Case,MyFreq), Case \= '*any*'),List2),
				 sumlist(List1,Freq1),
				 sumlist(List2,Freq2),
				 Freq is Freq1 + Freq2,
				 findall(MyFreq,haspp('*any*',Tag,Prep,Case,MyFreq),ListTag1),
				 findall(MyFreq,hasobjp('*any*',Tag,Prep,Case,MyFreq),ListTag2),
				 sumlist(ListTag1,FreqTag1),
				 sumlist(ListTag2,FreqTag2),
				 FreqTag is FreqTag1 + FreqTag2,
				 (Freq < 5->P is 0.5;P is FreqTag/Freq).


%subjects.
stats2(subj,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,_D,HC-_OG,_DC) :-
	lexic(SD,_,DPos),
	(Dtag='PWS'->DistMod is 1;DistMod is 1+((50-DPos)*0.0001)),
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,subj,PLabel),
	P is PLabel*DistMod.


%accusative objects.
stats2(obja,Htag,_FH,_SH,_MORPHH,Dtag,FD,_SD,MORPHD,P,_D,HC-_OG,_DC) :-
	getheadandnormalise(HC,Head,_),
	lexModifier(Htag,Dtag,Head,FD,obja,PTemp),
	(PTemp=1->npidsamb(Head,MORPHD,Dtag,obja,PTemp2);PTemp2=PTemp),
	((Htag = 'ADJA';Htag='ADJD')->PosMod is 0.5;PosMod is 1),
	P is PTemp2*PosMod.


%accusative objects.
stats2(obja2,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,MORPHD,P,_D,HC-_OG,_DC) :-
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,obja2,PTemp),
	((Htag = 'ADJA';Htag='ADJD')->PosMod is 0.5;PosMod is 1),
	P is PTemp*PosMod.


%dative objects: exception: "Ihnen gemein"
stats2(objd,_Htag,gemein,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.2,_D,_HC-_OG,_DC).

%dative objects
stats2(objd,Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,_D,HC-_OG,_DC) :-
% 	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
% 	RealDist is HPos-DPos,
	DistMod is 1+((50-DPos)*0.00005),
	getheadandnormalise(HC,Head,_),
    ((verb(Head,Occurs,_,_,Objd,_,_,_,_,_,_,_,_,_,_,_),Objd > 0)->Max is max(0.8,2*Objd/Occurs);Max is 0.06), %we set max values because objd often competes with gmod (das Ende der Vertreibung kommt)
	npidsamb(Head,MORPHD,Dtag,objd,PLabel),
	((Htag = 'ADJA';Htag='ADJD')->PosMod is 0.5;PosMod is 1),
	P is min(Max,PLabel)*DistMod*PosMod.


%genitive objects
stats2(objg,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,MORPHD,P,D,HC-_OG,_DC) :-
	getheadandnormalise(HC,Head,_),
	distModifier(D,objg,DISTMOD),
	((verb(Head,Occurs,_,_,_,_,Objg,_,_,_,_,_,_,_,_,_),Objg > 0)->Max is max(0.4,2*Objg/Occurs);Max is 0.06), %only overrule gmod if objg is relatively common (more than 50% of cases)
	npidsamb(Head,MORPHD,Dtag,objg,PTemp),
	P is min(Max,PTemp)*DISTMOD.

%predicate nouns
stats2(pred,Htag,_FH,SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,_D,HC-_OG,_DC) :-
	\+ predcand_adverb(Dtag),
	getheadandnormalise(HC,Head,_),
	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
	DistMod is 1+((50-DPos)*0.00005),
	((HPos > DPos, member('mainclause',HC))->DistMod2 = 0.2;DistMod2=1), % prefer subject-verb-predicate over predicate-verb-subject
	npidsamb(Head,MORPHD,Dtag,pred,PLabel),
	((Htag = 'ADJA';Htag='ADJD')->PosMod is 0.5;PosMod is 1),
	P is PLabel*DistMod*PosMod*DistMod2.

%disambiguate between subj/obja/objd/objg/pred. Use lexical information of verb and morphology of dependent.
npidsamb(Head,Morph,Tag,Cand,Prob) :- verb(Head,Occurs,Subj,Obja,Objd,Obja2,Objg,_,_,_,Pred,_,_,_,_,_),
				      Occurs > 3,
				      (Obja>Subj->Subj2 is Obja+1;Subj2 is Subj), %fix some statistical outliers: subj should be more probable than obja.
				      (Objd>Subj2->Subj3 is Objd+1;Subj3 is Subj2), %fix some statistical outliers: subj should be more probable than obja.
				      (Pred>Subj3->Pred2 is Subj3-1;Pred2 is Pred),
				      ((Cand=subj,CandNum=Subj3);(Cand=obja,CandNum=Obja);(Cand=objd,CandNum=Objd);(Cand=objg,CandNum=Objg);(Cand=pred,CandNum=Pred2);(Cand=obja2,CandNum=Obja2)),
				      testmorphology(Morph,Tag,Subj3,Obja,Objd,Objg,SubjNew,ObjaNew,ObjdNew,ObjgNew),
				      Total is SubjNew + ObjaNew + ObjdNew + ObjgNew,
				      (Total > 0 -> Prob is CandNum / Total; fail), !.

%no lexical information found: use constant values.
npidsamb(_,_,_,subj,0.7) :- !.
npidsamb(_,_,_,obja,0.4) :- !.
npidsamb(_,_,_,obja2,0.01) :- !.
npidsamb(_,_,_,objd,0.1) :- !.
npidsamb(_,_,_,objg,0.01) :- !.
npidsamb(_,_,_,pred,0.01) :- !.


%predicate nouns (ADJD) - not competing with subj/obj, but with ADV, which has its own disambiguation method (because they often have multiple potential heads).
stats2(pred,_Htag,_FH,_SH,_MORPHH,Dtag,FD,_SD,_MORPHD,P,_D,HC-_OG,_DC) :-
        getheadandnormalise(HC,Head,_),
        downcase_atom(FD,Dep),
        downcase_atom(Dtag,DtagLower),
        pred_disambiguate(Head,Dep,DtagLower,Score),
        (Score > 0.3->P is 1;P is 0).

pred_disambiguate(Head, Dep, Dtag, Score) :- (adverbial(Head, Dep, Dtag, BilexAdv)->true;BilexAdv=0),
        (predicative(Head, Dep, Dtag, BilexPred)->true;BilexPred=0),
        (adverbial('*any*', Dep, Dtag, VAdv)->true;VAdv=0),
        (predicative('*any*', Dep, Dtag, VPred)->true;VPred=0),
        (adverbial(Head, '*any*', Dtag, ADJDAdv)->true;ADJDAdv=0),
        (predicative(Head, '*any*', Dtag, ADJDPred)->true;ADJDPred=0),
        BilexStats is BilexPred/(BilexPred+BilexAdv+2),
        VStats is VPred/(VPred+VAdv+2),
        ADJDStats is ADJDPred/(ADJDPred+ADJDAdv+2),
        Score is 0.35*BilexStats+0.25*VStats+0.4*ADJDStats.


%genitive modifiers

%pre-modifying gmod: if not NE, must be morphologically unambiguous
stats2(gmod,_Htag,_FH,SH,_WFormH,_MORPHH,Dtag,_FD,SD,_WFormD,MORPHD,P,_D,_HC-_OG,_DC) :-
        \+ morphology(off), %if parsing with morphology turned off, this rule is counterproductive
        lexic(SH,_,HPos),
        lexic(SD,_,DPos),
        RealDist is HPos-DPos,
        RealDist > 0,
        Dtag \= 'NE',
        distModifier(RealDist,gmod,DISTMOD), %prefer close attachment and modifier after head noun ("gestern hat der nachbar des mannes peter getroffen")
        ((case_nom(MORPHD,Dtag);case_acc(MORPHD,Dtag);case_dat(MORPHD,Dtag))->P is 0;P is 1*DISTMOD).

%post-modifying gmod (or pre-modifying NE)
stats2(gmod,_Htag,_FH,SH,_WFormH,_MORPHH,Dtag,_FD,SD,WFormD,MORPHD,P,_D,_HC-_OG,_DC-DepRels) :-
        \+ morphology(off), %if parsing with morphology turned off, this rule is counterproductive
        lexic(SH,_,HPos),
        lexic(SD,_,DPos),
        RealDist is HPos-DPos,
        get_relevant_name_for_gmod_disamb(Dtag,DepRels,WFormD,WFormDisamb),
        distModifier(RealDist,gmod,DISTMOD), %prefer close attachment and modifier after head noun ("gestern hat der nachbar des mannes peter getroffen")
        findall(_,case_acc(MORPHD,Dtag),LA),length(LA,LAL), %testing for accusative since there is no article that can be both accusative and genitive. dat/nom: 'eine mitarbeiterin der awo'
        ((      LAL =:= 0, %word not fully ambiguous
                P is 1*DISTMOD
        )
        ; %word case is fully ambiguous
        (   LAL > 0,
            ((Dtag = 'NE',atom_concat(_,s,WFormDisamb))->(gmod_disamb(WFormDisamb,P_GMOD), P is P_GMOD*DISTMOD); %ambiguous name ending with -s could be genitive
            ((Dtag = 'NE',atom_concat(_,'\'',WFormDisamb))->P is DISTMOD); %name ending in -' is genitive
            P is 0.01*DISTMOD) % else, assume that genitive is unlikely
        )).

stats2(gmod,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC-_OG,_DC) :- morphology(off).

gmod_disamb(DWord, P) :- downcase_atom(DWord,DWordL),
    gmod_ne(DWordL, GMOD, Total),
    P is GMOD/Total.

gmod_disamb(_DWord, 0.3).

% "Bill Clintons Aussenminister": use "Clintons" for lexical disambiguation of gmod attachment, even though "Bill" is head of "Clintons".
get_relevant_name_for_gmod_disamb('NE',DepRels,_,Word) :- length(DepRels,Len),
        Pos is Len-1,
        nth1(Pos, DepRels,'->app_close->'),
        last(DepRels,Last),
        Last =.. [App|_],
        lexic(App,_,AppPos),
        checkPos(AppPos,_,_,_,[Word|_]), !.

get_relevant_name_for_gmod_disamb(_,_,Word,Word) :- !.

%subordinated clauses:
stats2(neb,_Htag,_FH,_SH,_MORPHH,'NEBCONJLESS',_FD,_SD,_MORPHD,P,_D,_HC,_DC) :- P is 0.75. %should be higher than probability for 'kon'

stats2(neb,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :- P is 0.3 * (1-D*0.01).
stats2(rel,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :- P is 0.4 * (1-D*0.01).
stats2(subjc,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,_D,HC-_OG,_DC) :-
        getheadandnormalise(HC,Head,_),
        ((      verb(Head,Occurs,_,_,_,_,_,_,_,SubjC,_,_,_,_,_,_),
                Occurs > 3,
                (   Threshold is (SubjC / Occurs),
                    Threshold > 0.05,
                    P is 0.4)
                    ;
                    P is 0.2)
        ;
        (       ((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
                P is 0.2)).

stats2(obji,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,D,HC-_OG,_DC) :-
        getheadandnormalise(HC,Head,_),
        distModifier(D,Htag,Dtag,obji,DISTMOD),
        ((      verb(Head,Occurs,_,_,_,_,_,_,ObjC,_,_,ObjI,_,_,_,_),
                Occurs > 3,
                (   Threshold is ((ObjI+ObjC) / Occurs),
                    Threshold > 0.05,
                    P is 0.3*DISTMOD)
                    ;
                    P is 0.2*DISTMOD)
        ;
        (       ((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
                P is 0.3*DISTMOD)).

%ObjC: probability higher than other subordinated structures unless valency information shows that verb doesn't have clausal object.
stats2(objc,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,D,HC-_OG,_DC) :-
	getheadandnormalise(HC,Head,HTagLC),
	posModifier(Htag,Dtag,objc,POSMOD),
	distModifier(D,objc,DISTMOD),
	(verbtag(HTagLC)->
		((	verb(Head,Occurs,_,_,_,_,_,_,ObjC,_,_,_,_,_,_,_),
			Occurs > 3,
				(Threshold is (ObjC / Occurs),
				Threshold > 0.05,
				P is POSMOD*DISTMOD*0.55)
				;
				P is POSMOD*DISTMOD*0.2)
		;
		(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
			P is POSMOD*DISTMOD*0.2))
	;P is POSMOD*DISTMOD*0.1).





%quotes - (in)direct speech
stats2(s,Htag,_FH,SH,_MORPHH,Dtag,_FD,SD,_MORPHD,P,_D,HC-_OG,_DC) :-
	getheadandnormalise(HC,Head,_),
	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
	RealDist is HPos-DPos,
	distModifier(RealDist,Htag,Dtag,s,DISTMOD),
	((	verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,Quote,_),
		Occurs > 3,
		Ratio is Quote / Occurs,
		(   (Ratio > 0.05,
		    P is 1*DISTMOD)
		    ;
		    P is 0.06*DISTMOD
		)
	)
	; %backoff: if there is no lexical information, use a fixed probability.
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
		P is 0.06*DISTMOD
	)).



%parenthetical quotes - (in)direct speech
stats2(par,Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,_MORPHD,P,D,_HC-_OG,_DC) :-
	(Dtag='PAR';Dtag='QUOTE'),
	lexic(SD,_,DPos),
	chart(DPos,DPos,DPos,[_,_,HC,_],_,_,_,_,_,_),
	getheadandnormalise(HC,Head,HTagLC),
    distModifier(D,Htag,Dtag,par,DISTMOD),
	verbtag(HTagLC),
	((	verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,Quote,_),
		Occurs > 3,
		Ratio is Quote / Occurs,
		(   (Ratio > 0.05,
		    P is 0.7*DISTMOD)
		    ;
		    P is 0.01*DISTMOD
		)
	)
	; %backoff: if there is no lexical information, use a fixed probability.
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
		P is 0.01*DISTMOD
	)).


%should have precedence over app
stats2(par,_Htag,_FH,_SH,_MORPHH,'APP',_FD,_SD,_MORPHD,0.9,_D,_HC-_OG,_DC).

stats2(par,_Htag,_FH,_SH,_MORPHH,'PARSO',_FD,_SD,_MORPHD,0.1,_D,_HC-_OG,_DC).

stats2(par,_Htag,_FH,_SH,_MORPHH,'PPNEB',_FD,_SD,_MORPHD,0.9,_D,_HC-_OG,_DC).
stats2(par,_Htag,_FH,_SH,_MORPHH,'VVPP',_FD,_SD,_MORPHD,0.9,_D,_HC-_OG,_DC).

stats2(par,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,_D,_HC-_OG,_DC).

%comparatives
%prefer attachment to verb, unless NN/ADJ is very close. *tiny* improvement over baseline...
stats2(kom,Htag,_FH,SH,_MORPHH,Dtag,_FD,SD,_MORPHD,P,D,_HC,_DC) :-
    lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    RealDist is HPos-DPos,
    ((RealDist > 0, member(Htag,['NN','NE','FM','ADJD','ADJA', 'PP']))-> DistMod = 0.1-0.001*RealDist;distModifier(D,Htag,Dtag,kom,DistMod)),
    posModifier(Htag,Dtag,kom,PosMod),
    P is DistMod*PosMod.


%attachment of comparatives to comparative conjunction. we will penalize some tag combinations; others get catchall weight.
stats2(cj,'KOKOM',FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :-
    distModifier(D,cj,DistMod),
    cj_penalty('KOKOM',FH,Dtag,Penalty),
    P is DistMod*Penalty.


%conjunctions. we will penalize some tag combinations; others get catchall weight.
stats2(cj,Htag,FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :-
    distModifier(D,cj,DistMod),
    (Htag='KON'->kon_mapping(Dtag,Kontag);Kontag=Dtag),
    cj_penalty(Htag, FH,Kontag,Penalty),
    P is DistMod*Penalty.


%some conjunctions typically conjoin verbs; disallow some other tags.
%often, sentence-level conjunctions are something like "X hat Rechte, aber wir erwarten...", so this prevents false positives.
cj_penalty(_,aber,'KON_PPER',0) :- !.
cj_penalty(_,aber,'KON_PRONOUN',0) :- !.

cj_penalty(_,doch,'KON_PRONOUN',0) :- !.
cj_penalty(_,doch,'KON_PPER',0) :- !.
cj_penalty(_,doch,'KON_NOUN',0) :- !.
cj_penalty(_,doch,'KON_ADV',0) :- !.

cj_penalty(_,denn,'KON_PRONOUN',0) :- !.
cj_penalty(_,denn,'KON_PPER',0) :- !.
cj_penalty(_,denn,'KON_NOUN',0) :- !.
cj_penalty(_,denn,'KON_ADV',0) :- !.

cj_penalty('KOKOM',_,'ADV',0.5) :- !.

cj_penalty(_,_,_,1).

%catchall for now...
stats2(Rel,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,D,_HC,_DC) :-
	distModifier(D,Rel,DistMod),
	P is DistMod.

stats2(Rel,Htag,FH,SH,_WordFormH,MORPHH,Dtag,FD,SD,_WordFormD,MORPHD,P,D,HC,DC) :- stats2(Rel,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,D,HC,DC).


% if HC is ['müssen_VMFIN', 'prüfen_VVINF'], Head should be 'prüfen', head tag should be 'vvinf' (statistic files are in lower case letters).
getheadandnormalise(HC,HeadOut,HeadTagOut) :- 	last(HC,Main),
    downcase_atom(Main,MainL),
    sub_atom(MainL,Before,_,After,'_'),
    sub_atom(MainL,_,After,0,HeadTag),
    \+ sub_atom(HeadTag,_,_,_,'_'),
    sub_atom(MainL,0,Before,_,Head),
    (  %if the last element in the chunk is a ptkvz, the main verb is the second last element.
    (HeadTag = 'ptkvz', attachptkvz(Head,HC,HeadOut,HeadTagOut));
    (HeadTag = 'adja', adj_getverbstem(Head,HeadOut), HeadTagOut = HeadTag);
    (HeadOut = Head, HeadTagOut = HeadTag)
    ), !.


%attachptkvz(+Particle,+HeadChunk,?HeadOut,?HeadTagOut).
attachptkvz(PTKVZ,HC,HeadOut,HeadTagOut) :- append(NewHC,[_],HC),
					   getheadandnormalise(NewHC,HeadTemp,HeadTagOut),
                       atom_concat(PTKVZ,HeadTemp,HeadOut), !.

% if adjective is derived from verb, make sure we use the verb lemma for subcategorization statistics. (currently only works for present participle ending in '-end', by simply cutting off the '-d')
adj_getverbstem(In, Out) :-
    sub_atom(In, Stem, 1, 0, Last),
    (Last = 'd' -> sub_atom(In, 0, Stem, 1, Out); Out = In).

%allows probability modifications according to dependents of the head.
depModifier(OG,app_close,0) :- (member('->app_close->',OG);member('->app_loose->',OG)), !.
% depModifier(OG,pp,0.8) :- nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV']), !.
depModifier(_,_,1) :- !.

%allows to modify likelihood for certain head/dependent combinations.

posModifier(_HTag,'PIS',app_close,1) :- !.
posModifier('PPER',_,app_close,0.05) :- !.
posModifier('PDS',_,app_close,0) :- !.
posModifier('NN','NN',app_close,0.4) :- !.
posModifier('NE','NN',app_close,0.2) :- !.
posModifier(_Htag,'NN',app_close,0.75) :- !.
posModifier('NE','NE',app_close,1.9) :- !.
posModifier(_Htag,'NE',app_close,0.9) :- !.
posModifier(_Htag,_Dtag,app_close,1) :- !.


posModifier(Htag,_Dtag,kom,1) :- (Htag = 'VVFIN'; Htag='VAFIN';Htag='VMFIN'), !.
posModifier('ADV',_Dtag,kom,0.3) :- !.
posModifier(_Htag,_Dtag,kom,0.95) :- !.

posModifier(_Htag,'OBJC',objc,1) :- !.
posModifier(_Htag,'QC',objc,1) :- !.

posModifier('ADJA','KON_ADV',kon,0.2) :- !.
posModifier(_Htag,'APP',kon,0.5) :- !.
posModifier(_Htag,'APPX',kon,0.5) :- !.

posModifier(_Htag,_Dtag,_Class,1) :- !. %catchall


%allows to modify likelihood for certain distances.

distModifier(D,app_close,DISTMOD) :- 	(D < 6, !,
				DISTMOD is 1-((D-1)/5)
				)
				;
				DISTMOD is 0.2-D*0.001, !.

distModifier(D,app_loose,DISTMOD) :- DISTMOD is (0.8 - D*0.01).

distModifier(D,gmod,DISTMOD) :- (D > 0, !,
				DISTMOD is 0.8-D*0.01)
				;
				DISTMOD is 1+D*0.01, !.


distModifier(D,cj,DISTMOD) :- (D < 12->DISTMOD is max(0.1,exp((1-D)/5));
                              DISTMOD is 0.1-D*0.001), !.


%catchall
distModifier(D,_Class,DISTMOD) :- DISTMOD is 1-D*0.01.


%distmodifier depending on tags

distModifier(D,_Htag,_Dtag,s,DISTMOD) :- D > 0->DISTMOD is 1-D*0.01;DISTMOD is 0.95-D*0.01.

distModifier(D,Htag,_Dtag,pp,DISTMOD) :- (nountag(Htag);nametag(Htag)),DISTMOD is 1-D*0.2, !.

distModifier(D,Htag,_Dtag,kom,DISTMOD) :- (Htag = 'VVFIN'; Htag='VAFIN';Htag='VMFIN'), DISTMOD is 1-D*0.01.
distModifier(D,_Htag,_Dtag,kom,DISTMOD) :- DISTMOD is 1-D*0.04.

%catchall
distModifier(D,_Htag,_Dtag,Class,DISTMOD) :- distModifier(D,Class,DISTMOD).


%allows for some lexical pseudo-probabilities (which aren't automatically extracted from treebank)

%only small list of indefinite pronouns can have close apposition
lexModifier('PIS',_Dtag,alle,_FD,app_close,1).
lexModifier('PIS',_Dtag,beide,_FD,app_close,1).

lexModifier('PIS','NN',etwas,_FD,app_close,0.3).
lexModifier('PIS','PIS',etwas,_FD,app_close,1).
lexModifier('PIS','NN',jemand,_FD,app_close,0.3).
lexModifier('PIS','PIS',jemand,_FD,app_close,1).
lexModifier('PIS','NN',nichts,_FD,app_close,0.3).
lexModifier('PIS','PIS',nichts,_FD,app_close,1).
lexModifier('PIS','NN',niemand,_FD,app_close,0.3).
lexModifier('PIS','PIS',niemand,_FD,app_close,1).

lexModifier('PIS',_Dtag,_FH,_FD,app_close,0).

%exception: "er lässt sich XY": "sich" is very unlikely to be obja
% er lässt uns das Essen bezahlen -> 'uns' is obja of lassen and subj of bezahlen (secondary edge)
% er lässt sich das Essen bezahlen -> 'sich' is objd of bezahlen, and it is unknown who pays
lexModifier(_Htag,'PRF','lassen',_FD,obja,0.1).

%exception: "xxx gibt es": "es" is very unlikely to be obja
lexModifier(_Htag,_Dtag,'geben',es,obja,0.1).
lexModifier(_Htag,_Dtag,'gibt',es,obja,0.1).

lexModifier(_Htag,_Dtag,_FH,_FD,_Rel,1).


%make sure that probability mass is distributed among analyses that are morphologically possible.
testmorphology(List,Tag,Subj,Obja,Objd,Objg,SubjOut,ObjaOut,ObjdOut,ObjgOut) :- 
            ((case_nom(List,Tag), Tag \= 'PRF')->SubjOut is Subj;SubjOut is 0),
            (case_acc(List,Tag)->ObjaOut is Obja;ObjaOut is 0),
            (case_dat(List,Tag)->ObjdOut is Objd;ObjdOut is 0),
            (case_gen(List,Tag)->ObjgOut is Objg;ObjgOut is 0), !.