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


:- dynamic noun_factor_chart/2.
:- style_check(-discontiguous).

%word classes other than nouns are penalised so that they are only chosen as head of an NP if there is no better alternative (removed from grammar for now, since too many FPs).
stats2(det,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,0.1,_D,_HC).
stats2(det,'CARD',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.3,0.3,_D,_HC).
stats2(attr,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.3,0.3,_D,_HC).
stats2(attr,'CARD',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,0.1,_D,_HC).


%deverbal adjectives can have same arguments as verbs
stats2(obja,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0,0,_D,_HC).
stats2(obja,'ADJD',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0,0,_D,_HC).
stats2(objd,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0,0,_D,_HC).
stats2(objd,'ADJD',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0,0,_D,_HC).
stats2(zeit,'ADJA',_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,0.1,_D,_HC).

%word classes other than articles or attributive pronouns should only be analysed as determiners if there is no other option.
stats2(det,_Htag,_FH,_SH,_MORPHH,'PRELS',_FD,_SD,_MORPHD,0.45,0.45,_D,_HC).
stats2(det,_Htag,_FH,_SH,_MORPHH,'PWS',_FD,_SD,_MORPHD,0.3,0.3,_D,_HC).

stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADV',_FD,_SD,_MORPHD,1,1,_D,_HC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADJD',_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'PP',_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'ADJA',_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).
stats2(pn,_Htag,_FH,_SH,_MORPHH,'CARD',_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).



%might be useful in case of tagging errors
stats2(part,_,_FH,_SH,_MORPHH,'ART',_FD,_SD,_MORPHD,0.9,0.9,_D,_HC).


%expl. should only be used together with objc/subjc.
stats2(explsubj,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,_D,_HC-OG) :- ((member('->subjc->',OG);member('->obji->',OG);member('<-subjc<-',OG);member('<-obji<-',OG))-> (P is 1.04, NP is 1.04); (P is 0.2,NP is 0.2)).

stats2(explobja,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,_D,_HC-OG) :- ((member('->objc->',OG);member('<-objc<-',OG))-> (P is 1.04, NP is 1.04); ((member('->obji->',OG);member('<-obji<-',OG))->(P is 0.8,NP is 0.8);(P is 0.2,NP is 0.2))).


%use the conjunction to distinguish between subordinated clauses and clausal objects
stats2(konjneb,_Htag,_FH,_SH,_MORPHH,_Dtag,FD,_SD,_MORPHD,P,NP,_D,_HC-OG) :- 
	member('<-konjneb<-',OG)->P is 1,NP is 1;(
	downcase_atom(FD,FL), 
	(konjstats(FL,neb,NEB);NEB is 0),
	(konjstats(FL,objc,OBJC);OBJC is 0),
	(NEB + OBJC =:= 0-> Ratio is 1;Ratio is (NEB+1)/(NEB+OBJC+1)),
	(Ratio > 0.7->(P is 1, NP is 1);(
	Ratio< 0.3->(P is 0, NP is 0);(P is Ratio, NP is Ratio)))).


stats2(konjobjc,_Htag,_FH,_SH,_MORPHH,_Dtag,FD,_SD,_MORPHD,P,NP,_D,_HC-OG) :- 
	member('<-konjneb<-',OG)->P is 0,NP is 0;(
	downcase_atom(FD,FL), 
	(konjstats(FL,neb,NEB);NEB is 0),
	(konjstats(FL,objc,OBJC);OBJC is 0),
	(NEB + OBJC =:= 0-> Ratio is 0;Ratio is (OBJC+1)/(NEB+OBJC+1)),
	(Ratio > 0.7->(P is 1, NP is 1);(
	Ratio< 0.3->(P is 0, NP is 0);(P is Ratio, NP is Ratio)))).


%the following structure always have precedence:
stats2(pn,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,1.06,_D,_HC).
stats2(attr,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,1.06,_D,_HC).
stats2(det,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.06,1.06,_D,_HC).

/*
%all non-verbal adverbs that are allowed in the grammar should have precedence (only reliable tag pairs have been included in grammar).
stats2(adv,Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,1.1,1.1,_D,_HC) :- \+ nonfinite(Htag), \+ Htag = 'VVFIN', \+ Htag = 'VAFIN', \+ Htag = 'VMFIN', \+ Htag = 'VVIZU'.
*/

%als auch: exception to rule below
stats2(adv,_Htag,als,_SH,_MORPHH,_Dtag,auch,_SD,_MORPHD,1,1,_D,_HC).

%non-verbal adverbs get probability from bigram statistics. attention: bigram statistics are distance 1 and may thus not be useful for long-distance relationships
stats2(adv,Htag,_FH,SH,_MORPHH,Dtag,FD,SD,_MORPHD,P,NP,D,_HC) :- 
    lexic(SH,_,HPos),
    lexic(SD,_,DPos),
    RealDist is HPos-DPos,
    getadvprob(Htag,Dtag,FD,RealDist,POSMod),
    distModifier(D,adv,DistMod),
    P is DistMod * POSMod,
    NP is DistMod * POSMod.


%getadvprob(+Htag,+Dtag,+DWord,+RealDist,-POSMod)

%sie sind alle tot.
getadvprob(_,'PIS',all,RealDist,P) :- P is 0.2-RealDist*0.003, !.
getadvprob(_,'PIS',alle,RealDist,P) :- P is 0.2-RealDist*0.003, !.
getadvprob(_,'PIS',alles,RealDist,P) :- P is 0.2-RealDist*0.003, !.

%ein bisschen traurig
getadvprob(_,'PIS',DWord,RealDist,P) :- member(DWord,['bißchen',bisschen,wenig]), P is 0.2+RealDist*0.003, !.

%these occur in the training corpus, but very rarely
% getadvprob(_,'PIS',DWord,RealDist,P) :- member(DWord,[andere,beide,einer,etwas,jeder,meisten,nichts,solche,soviel,sowas,viele]), P is 0.1-RealDist*0.005, !.

getadvprob(_,'PIS',_,_,0) :- !.

getadvprob(_,'ADVKOUS',_,_,0.1) :- !.

%using constant value for verbs (should be default behaviour)
getadvprob(Htag,_,_,_,0.55) :- (nonfinite(Htag);Htag = 'VVFIN';Htag = 'VAFIN';Htag = 'VMFIN';Htag = 'VVIZU'), !.

%use lexical disambiguation for some special constructions (e.g. adverbs modifying adverbs: noch heute)
getadvprob(Htag,_Dtag,DWord,RealDist,POSMod) :- (Htag = 'KOMPX' -> HTL = 'kokom' ; downcase_atom(Htag,HTL)), 
		    downcase_atom(DWord,DWordL), 
		    (RealDist > 0->advbigramleft(DWordL,HTL,Total,ADV); advbigramright(DWordL,HTL,Total,ADV)),
		      (Total > 10; ADV > 0),
		      POSMod is ADV / Total, !.


%catchall
getadvprob(_Htag,_Dtag,_DWord,_RealDist,0.15) :- !.


%zeit should be lower than app.
stats2(zeit,_Htag,_FH,_SH,_MORPHH,'CARD',_FD,_SD,_MORPHD,0.8,0.8,_D,_HC).
stats2(zeit,_Htag,_FH,_SH,_MORPHH,'NN',_FD,_SD,_MORPHD,0.2,0.2,_D,_HC).


%sentence coordinations: should be lower than 's' relation (if the latter occurs in training corpus)
stats2(kon,_Htag,_FH,_SH,_MORPHH,'KONC',_FD,_SD,_MORPHD,P,NP,_D,_HC) :-
	P is  0.65,
	NP is  0.65.

%coordinations: encourage short distances. Should have precedence over APP
stats2(kon,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC) :-
	P is  1.06-D*0.05,
	NP is  1.06-D*0.05.



%gradation: Should have precedence over long APP chains, but not over subj
stats2(grad,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).


%closing bracket - better than having two opening ones / prefer short distances
stats2(bracket,'BRACKET',_FH,SH,_MORPHH,_Dtag,_FD,SD,_MORPHD,P,NP,_D,_HC-OG) :-
    lexic(SD,_,RIGHTPOS),
    lexic(SH,_,MIDPOS),
    append(_,[BRACKET, '<-bracket<-'|_],OG),
    BRACKET =.. [LEX|_],
    lexic(LEX,_,LEFTPOS),
    LEFTPOS < MIDPOS,
    MIDPOS < RIGHTPOS,
    Dist is RIGHTPOS - LEFTPOS,
    P is 1.06 - Dist*0.01,
    NP is 1.06 - Dist*0.01.

stats2(bracket,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.5,0.5,_D,_HC).

stats2(badbracket,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.65,0.65,_D,_HC).

%appositions: encourage short distances.
stats2(app,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC-OG):-
% commented out: effect is minimal.
%     downcase_atom(FH,FHNorm), %(statistics files are in lower case letters).
%     downcase_atom(FD,FDNorm), %(statistics files are in lower case letters).
%     (1 is D,
%         ((appbigram(FH,FD,Total,AsAPP),
%         Total > 0,
%         LexMod is AsAPP / Total)
%         ;
%         ((appunigram(FH,As1,As1APP,_,_);(As1 is 0, As1APP is 0)),
%         (appunigram(FD,_,_,As2,As2APP); (As2 is 0, As2APP is 0)),
%         Total is As1 + As2,
%         Total > 0,
%         (As1 > 0 -> Val1 is As1APP / As1; Val1 is 0.9),
%         (As2 > 0 -> Val2 is As2APP / As2; Val2 is 0.9),
%         LexMod is (Val1 + Val2) / 2))
%     ;LexMod is 0.9),
    LexMod is 1,
	(Dtag = 'APP' -> POSMOD is 1;posModifier(Htag,Dtag,app,POSMOD)),
	(Dtag = 'APP' -> DISTMOD is 0.8 - D*0.01;distModifier(D,app,DISTMOD)),
	depModifier(OG,app,DEPMOD),
	P is DISTMOD*POSMOD*DEPMOD*LexMod,
	NP is DISTMOD*POSMOD*DEPMOD*LexMod.


stats2(objp,_Htag,_FH,_SH,_MORPHH,Dtag,FD,_SD,MORPHD,P,P,D,HC-_OG) :-
	getheadandnormalise(HC,Head,HeadTagTemp),
	combineverbtags(HeadTagTemp,HeadTag),
	distModifier(D,HeadTag,Dtag,pp,DISTMOD),
	downcase_atom(FD,FDNorm), %(statistics files are in lower case letters).
	(splitappr(FDNorm,Prep,_);Prep = FDNorm),
	(var(MORPHD) -> List = [];findall(Case,member([Case],MORPHD),List)),
	length(List,Len),
	(Len = 1->(List = [CaseTemp],case_tueba(CaseTemp,Case)); Case = _),
	get_pp_statistics(Head,HeadTag,Case,Prep,NumPP,NumObjP,NumHead),
	NumPPOBJP is NumPP + NumObjP,
	ppPosMod(Prep,Case,HeadTag,POSMOD),
	ppobjp(NumPP,NumObjP,objp,PPOBJP), 
	pplexmod(NumPPOBJP,NumHead,LEXMOD),
%         depModifier(OG,pp,DEPMOD),
	P is PPOBJP*(DISTMOD*0.15+POSMOD*0.35+LEXMOD*0.5). %probability space is split up between 3 disambiguation methods, weights set by hand

stats2(pp,_Htag,_FH,_SH,_MORPHH,Dtag,FD,_SD,MORPHD,P,P,D,HC-_OG) :- 
	getheadandnormalise(HC,Head,HeadTagTemp),
	combineverbtags(HeadTagTemp,HeadTag),
	distModifier(D,HeadTag,Dtag,pp,DISTMOD),
	downcase_atom(FD,FDNorm), %(statistics files are in lower case letters).
	(splitappr(FDNorm,Prep,_);Prep = FDNorm),
	(var(MORPHD) -> List = [];findall(Case,member([Case],MORPHD),List)),
	length(List,Len),
	(Len = 1->(List = [CaseTemp],case_tueba(CaseTemp,Case)); Case = _),
	get_pp_statistics(Head,HeadTag,Case,Prep,NumPP,NumObjP,NumHead),
	NumPPOBJP is NumPP + NumObjP,
	ppPosMod(Prep,Case,HeadTag,POSMOD),
	ppobjp(NumPP,NumObjP,pp,PPOBJP),
	pplexmod(NumPPOBJP,NumHead,LEXMOD),
%         depModifier(OG,pp,DEPMOD), %disfavours PP attached to verb after pred.
	P is PPOBJP*(DISTMOD*0.15+POSMOD*0.35+LEXMOD*0.5). %probability space is split up between 3 disambiguation methods, weights set by hand


%ppobjp/4 decides whether construction is 'pp' or 'objp'.
ppobjp(NumPP,NumObjP,pp,0.15) :- NumPP > 0, (NumPP < NumObjP;NumPP = NumObjP), !.
ppobjp(NumPP,NumObjP,pp,1) :- NumPP > 0, NumPP > NumObjP, !.
ppobjp(_,_,pp,0.85) :- !.

ppobjp(NumPP,NumObjP,objp,0.95) :- NumObjP > 0, (NumPP < NumObjP;NumPP = NumObjP), !.
ppobjp(NumPP,NumObjP,objp,0) :- NumObjP > 0, NumPP > NumObjP, !.
ppobjp(_,_,objp,0) :- !.


%bilexical disambigation of pp attachment. Formula doesn't make sense stochastically, but we're only interested in which attachment has highest value.
pplexmod(NumPPOBJP,NumHead,P) :- NumPPOBJP > 0, PTemp is 10*NumPPOBJP / NumHead, PTemp2 is max(0.1,PTemp), P is min(PTemp2,1), !.
pplexmod(NumPPOBJP,NumHead,P) :- NumPPOBJP = 0, P is max(0,0.25-NumHead/1000), !.

%get_pp_statistics(+Head,+HeadTag,+DepMorph,+Prep,-NumPP,-NumObjP,-NumHead)
get_pp_statistics(Head,HeadTag,Case,Prep,NumPP,NumObjP,NumHead) :- 	
	((HeadTag='card';HeadTag='ne')->Head2='*any*';Head2=Head),
	findall(NumHead,occurs(Head2,HeadTag,NumHead),ListHead),
	sumlist(ListHead,NumHead),
	findall(NumObjP,hasobjp(Head2,HeadTag,Prep,Case,NumObjP),ListObjP),
	sumlist(ListObjP,NumObjP),
	findall(NumPP,haspp(Head2,HeadTag,Prep,Case,NumPP),ListPP),
	sumlist(ListPP,NumPP), !.


%'in-das' -> 'in' + 'das'
splitappr(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'-'),
	sub_atom(WordI,0,Before,_,Word), 
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa), !.

splitappr(WordI,WordI,_) :- !.



%non-lexical disambiguation of pp attachment: only consider preposition and PoS of head.
%ppPosMod(+Prep,?Case,+HeadTag,-P)
ppPosMod(Prep,Case,HeadTag,P) :- verbtag(HeadTag),
				 findall(Freq,haspp('*any*','v',Prep,Case,Freq),ListV1),
				 sumlist(ListV1,FreqV1),
				 findall(Freq,hasobjp('*any*','v',Prep,Case,Freq),ListV2),
				 sumlist(ListV2,FreqV2),
				 FreqV is FreqV1 + FreqV2,
				 findall(FreqX,(haspp('*any*','nn',Prep,Case,FreqX)),ListX1),
				 sumlist(ListX1,FreqX1),
				 findall(FreqX,(hasobjp('*any*','nn',Prep,Case,FreqX)),ListX2),
				 sumlist(ListX2,FreqX2),
			    Freq is FreqV+FreqX1+FreqX2, 
			    (Freq < 5->PTemp is 0.6;PTemp is FreqV/Freq),
			    PTemp2 is max(0,PTemp), P is min(PTemp2,1), !.

ppPosMod(Prep,Case,Tag,P) :- findall(Freq,haspp('*any*','v',Prep,Case,Freq),ListV1),
				 findall(Freq,hasobjp('*any*','v',Prep,Case,Freq),ListV2),
				 sumlist(ListV1,FreqV1),
				 sumlist(ListV2,FreqV2),
				 FreqV is FreqV1 + FreqV2,
				 findall(Freq,haspp('*any*',Tag,Prep,Case,Freq),ListN1),
				 sumlist(ListN1,FreqN1),
				 findall(Freq,hasobjp('*any*',Tag,Prep,Case,Freq),ListN2),
				 sumlist(ListN2,FreqN2),
				 FreqN is FreqN1 + FreqN2,
				 noun_factor(Tag,NF),
			    Freq is FreqV+FreqN*NF,
			    (Freq < 5->PTemp is 0.4;PTemp is FreqN*NF/Freq),
			    PTemp2 is max(0,PTemp), P is min(PTemp2,1), !.

ppPosMod(_,_,_,0) :- !.


%noun factor (as described in Martin Volk 2002: Combining unsupervised and supervised methods for PP attachment disambiguation)
%generalised to work with all tags (except for verbs, whose probabilities are unmodified).
%noun_factor(+Tag,-NF)
noun_factor(Tag,NF) :- noun_factor_chart(Tag,NF), !.

noun_factor(Tag,NF) :- findall(Freq,occurs('*any*',Tag,Freq),ListA),
		    sumlist(ListA,FreqA),
		    findall(Freq,(occurs('*any*',v,Freq)),ListB),
		    sumlist(ListB,FreqB),
		    findall(Freq,haspp('*any*',Tag,_,_,Freq),List1),
			sumlist(List1,Freq1),
		    findall(Freq,hasobjp('*any*',Tag,_,_,Freq),List2),
			sumlist(List2,Freq2),
		    FreqY is Freq1 + Freq2,
		    findall(Freq,(haspp('*any*',v,_,_,Freq)),ListX1),
			sumlist(ListX1,FreqX1),
		    findall(Freq,(hasobjp('*any*',v,_,_,Freq)),ListX2),
			sumlist(ListX2,FreqX2),
			FreqX is FreqX1 + FreqX2,
            (FreqY =:= 0->NF is 1;NF is FreqX/FreqB*FreqA/FreqY), 
		    assert(noun_factor_chart(Tag,NF)), !.


%subjects.
stats2(subj,_Htag,_FH,SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,NP,_D,HC-_OG) :-
	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
 	LeftPos is HPos-1, 
	(checkPos(LeftPos,_,'KON',_,_)->PunishKon is 0.2;PunishKon is 1), %punishes subjects occurring just after coordinating conjunction: er kommt und sieht laura.
% 	RealDist is HPos-DPos,
	DistMod is 1+((50-DPos)*0.0001),
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,subj,PLabel),
	P is PLabel*DistMod*PunishKon, NP is PLabel*DistMod*PunishKon.


%accusative objects.
stats2(obja,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,MORPHD,P,P,_D,HC-_OG) :-
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,obja,P).


%accusative objects.
stats2(obja2,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,MORPHD,P,P,_D,HC-_OG) :-
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,obja2,P).


%dative objects
stats2(objd,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,NP,_D,HC-_OG) :-
% 	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
% 	RealDist is HPos-DPos,
	DistMod is 1+((50-DPos)*0.00005),
	getheadandnormalise(HC,Head,_),
	npidsamb(Head,MORPHD,Dtag,objd,PLabel),
	P is PLabel*DistMod, NP is PLabel*DistMod.


%genitive objects
stats2(objg,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,MORPHD,P,P,_D,HC-_OG) :-
	getheadandnormalise(HC,Head,_),
	((verb(Head,_,_,_,_,_,Objg,_,_,_,_,_,_,_,_,_),Objg > 0)->Max is 0.8;Max is 0.06), %should always be lower than prob for gmod; if no statistical evidence found, only use if all else is impossible
	npidsamb(Head,MORPHD,Dtag,objg,PTemp),
	P is min(Max,PTemp).

%predicate nouns
stats2(pred,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,NP,_D,HC-_OG) :-
	(morph_noun(Dtag);morph_pronoun(Dtag)),
	getheadandnormalise(HC,Head,_),
	lexic(SD,_,DPos),
	DistMod is 1+((50-DPos)*0.00005),
	npidsamb(Head,MORPHD,Dtag,pred,PLabel),
	P is PLabel*DistMod, NP is PLabel*DistMod.

%disambiguate between subj/obja/objd/objg/pred. Use lexical information of verb and morphology of dependent.
npidsamb(Head,Morph,Tag,Cand,Prob) :- verb(Head,Occurs,Subj,Obja,Objd,Obja2,Objg,_,_,_,Pred,_,_,_,_,_),
				      Occurs > 3,
				      (Obja>Subj->Subj2 is Obja+1;Subj2 is Subj), %fix some statistical outliers: subj should be more probable than obja.
				      (Pred>Subj2->Pred2 is Subj2-1;Pred2 is Pred),
				      ((Cand=subj,CandNum=Subj2);(Cand=obja,CandNum=Obja);(Cand=objd,CandNum=Objd);(Cand=objg,CandNum=Objg);(Cand=pred,CandNum=Pred2);(Cand=obja2,CandNum=Obja2)),
				      testmorphology(Morph,Tag,Subj2,Obja,Objd,Objg,SubjNew,ObjaNew,ObjdNew,ObjgNew),
				      Prob is CandNum / (SubjNew + ObjaNew + ObjdNew + ObjgNew + 0.1), !.

%no lexical information found: use constant values.
npidsamb(_,_,_,subj,0.7) :- !.
npidsamb(_,_,_,obja,0.4) :- !.
npidsamb(_,_,_,obja2,0.01) :- !.
npidsamb(_,_,_,objd,0.1) :- !.
npidsamb(_,_,_,objg,0.01) :- !.
npidsamb(_,_,_,pred,0.01) :- !.


%predicate nouns (ADJD) - not competing with subj/obj, but with ADV, which has default probability of 1.
stats2(pred,_Htag,_FH,_SH,_MORPHH,'ADJD',_FD,_SD,_MORPHD,P,NP,_D,HC-_OG) :-
% 	((downcase_atom(FD,FLower),npstats(FLower,'adjd',_,AsPred,AsAdv), %lexical stats
% 	AsPred+AsAdv > 5,
% 	LexProb = (AsPred+1)/(AsPred+AsAdv+1));LexProb is 0.5),
	getheadandnormalise(HC,Head,_),
	((		verb(Head,Occurs,_,_,_,_,_,_,_,_,Pred,_,_,_,_,_),
		Occurs > 1,
		Freq is Pred / Occurs,
		(Freq >= 0.3 -> (P is 1.06, NP is 1.06);
		(Freq < 0.3 -> (P is 0.01, NP is 0.01)))
	)
	; %backoff: if there is no lexical information, use a fixed probability.
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 2)),
		P is 0.01, NP is 0.01
	)).

%genitive modifiers
stats2(gmod,_Htag,_FH,SH,_MORPHH,Dtag,_FD,SD,MORPHD,P,NP,_D,_HC-_OG) :-
	\+ morphology(off), %if parsing with morphology turned off, this rule is counterproductive
	lexic(SH,_,HPos),
	lexic(SD,_,DPos),
	RealDist is HPos-DPos,
	distModifier(RealDist,gmod,DISTMOD), %prefer close attachment and modifier after head noun ("gestern hat der nachbar des mannes peter getroffen")
	findall(_,case_acc(MORPHD,Dtag),LA),length(LA,LAL), %testing for accusative since there is no article that can be both accusative and genitive. dat/nom: 'eine mitarbeiterin der awo'
	((	LAL =:= 0, %word not fully ambiguous
		P is 1*DISTMOD, NP is 1*DISTMOD
	)
	; %if word case is fully ambiguous, let's assume genitive is unlikely.
	(	LAL > 0,
		P is 0.01*DISTMOD, NP is 0.01*DISTMOD
	)).

stats2(gmod,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.1,0.1,_D,_HC-_OG) :-
	morphology(off).

%subordinated clauses:
stats2(neb,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC) :-
	P is 0.3 - D*0.01, 
	NP is 0.3 - D*0.01.
stats2(rel,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC) :- 
	P is 0.7 - D*0.01, 
	NP is 0.7 - D*0.01.
stats2(subjc,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,0.2,0.2,_D,_HC).

%ObjC: probability higher than other subordinated structures unless valency information shows that verb doesn't have clausal object.
stats2(objc,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,NP,D,HC-_OG) :-
	getheadandnormalise(HC,Head,_),
	posModifier(Htag,Dtag,objc,POSMOD),
	distModifier(D,objc,DISTMOD),
	((	verb(Head,Occurs,_,_,_,_,_,_,ObjC,_,_,_,_,_,_,_),
		Occurs > 3,
			(Threshold is (ObjC / Occurs),
			Threshold > 0.05,
			P is POSMOD*DISTMOD*0.55, NP is POSMOD*DISTMOD*0.55)
			;
			(P is POSMOD*DISTMOD*0.2, NP is POSMOD*DISTMOD*0.2))
	;
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
		P is POSMOD*DISTMOD*0.2, NP is POSMOD*DISTMOD*0.2)).





%quotes - (in)direct speech
stats2(s,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,_D,HC-_OG) :-
	getheadandnormalise(HC,Head,_),
	((	verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,Quote,_),
		Occurs > 3,
		Ratio is Quote / Occurs,
		(   (Ratio > 0.05,
		    P is 1, NP is 1)
		    ;
		    (P is 0.01, NP is 0.01)
		)
	)
	; %backoff: if there is no lexical information, use a fixed probability.
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
		P is 0.01, NP is 0.01
	)).



%parenthetical quotes - (in)direct speech
stats2(par,_Htag,_FH,_SH,_MORPHH,Dtag,_FD,SD,_MORPHD,P,NP,_D,_HC-_OG) :-
	(Dtag='PAR';Dtag='QUOTE'),
	lexic(SD,_,DPos),
	chart(DPos,DPos,DPos,[[_,_,HC,_,_]],_,_,_,_,_),
	getheadandnormalise(HC,Head,_),
	((	verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,Quote,_),
		Occurs > 3,
		Ratio is Quote / Occurs,
		(   (Ratio > 0.05,
		    P is 0.6, NP is 0.6)
		    ;
		    (P is 0.01, NP is 0.01)
		)
	)
	; %backoff: if there is no lexical information, use a fixed probability.
	(	((\+ verb(Head,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)); (verb(Head,Occurs,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Occurs < 4)),
		P is 0.01, NP is 0.01
	)).


stats2(par,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,P,_D,_HC-_OG) :-
    P is 0.1.

%comparatives
%prefer attachment to verb, unless NN/ADJ is very close. *tiny* improvement over baseline...
stats2(kom,Htag,_FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,P,D,_HC) :- 
    distModifier(D,Htag,Dtag,kom,DistMod),
    posModifier(Htag,Dtag,kom,PosMod),
    P is DistMod*PosMod.



%conjunctions. we will penalize some tag combinations; others get catchall weight.
stats2(cj,'KON',FH,_SH,_MORPHH,Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC) :- 
    distModifier(D,cj,DistMod),
    kon_mapping(Dtag,Kontag),
    cj_penalty(FH,Kontag,Penalty),
    P is DistMod*Penalty,
    NP is DistMod*Penalty.


%some conjunctions typically conjoin verbs; disallow some other tags.
%often, sentence-level conjunctions are something like "X hat Rechte, aber wir erwarten...", so this prevents false positives.
cj_penalty(aber,'KON_PPER',0) :- !.
cj_penalty(aber,'KON_PRONOUN',0) :- !.

cj_penalty(doch,'KON_PRONOUN',0) :- !.
cj_penalty(doch,'KON_PPER',0) :- !.
cj_penalty(doch,'KON_NOUN',0) :- !.
cj_penalty(doch,'KON_ADV',0) :- !.

cj_penalty(denn,'KON_PRONOUN',0) :- !.
cj_penalty(denn,'KON_PPER',0) :- !.
cj_penalty(denn,'KON_NOUN',0) :- !.
cj_penalty(denn,'KON_ADV',0) :- !.

%catchall for now...
stats2(Rel,_Htag,_FH,_SH,_MORPHH,_Dtag,_FD,_SD,_MORPHD,P,NP,D,_HC) :- 
	distModifier(D,Rel,DistMod),
	P is DistMod,
	NP is DistMod.



% if HC is ['müssen_VMFIN', 'prüfen_VVINF'], Head should be 'prüfen', head tag should be 'vvinf' (statistic files are in lower case letters).
getheadandnormalise(HC,HeadOut,HeadTagOut) :- 	last(HC,Main),
    downcase_atom(Main,MainL),
    sub_atom(MainL,Before,_,After,'_'),
    sub_atom(MainL,_,After,0,HeadTag),
    \+ sub_atom(HeadTag,_,_,_,'_'),
    sub_atom(MainL,0,Before,_,Head),
	(  %if the last element in the chunk is a ptkvz, the main verb is the second last element.
	(HeadTag = 'ptkvz', attachptkvz(Head,HC,HeadOut,HeadTagOut))	; 	(HeadOut = Head, HeadTagOut = HeadTag)
	), !.


combineverbtags('vvinf','v') :- !.
combineverbtags('vvfin','v') :- !.
combineverbtags('vvpp','v') :- !.
combineverbtags('vvizu','v') :- !.
combineverbtags('vvimp','v') :- !.
combineverbtags(X,X) :- !.

%attachptkvz(+Particle,+HeadChunk,?HeadOut,?HeadTagOut).
attachptkvz(PTKVZ,HC,HeadOut,HeadTagOut) :- append(NewHC,[_],HC),
					   getheadandnormalise(NewHC,HeadTemp,HeadTagOut),
                       atom_concat(PTKVZ,HeadTemp,HeadOut), !.

%allows probability modifications according to dependents of the head.
depModifier(OG,app,0) :- member('->app->',OG), !.
% depModifier(OG,pp,0.8) :- nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV']), !.
depModifier(_,_,1) :- !.

%allows to modify likelihood for certain head/dependent combinations.
posModifier('PPER',_,app,0.01) :- !.
posModifier('PIS',_,app,0.3) :- !.
posModifier('NN','NN',app,0.4) :- !.
posModifier('NE','NN',app,0.6) :- !.
posModifier(_Htag,'NN',app,0.75) :- !.
posModifier('NE','NE',app,1.9) :- !.
posModifier(_Htag,'NE',app,1.2) :- !.

posModifier(Htag,_Dtag,kom,1) :- (Htag = 'VVFIN'; Htag='VAFIN';Htag='VMFIN'), !.
posModifier('ADV',_Dtag,kom,0.3) :- !.
posModifier(_Htag,_Dtag,kom,0.95) :- !.

posModifier(_Htag,'OBJC',objc,1) :- !.
posModifier(_Htag,'RC',objc,0.7) :- !.
posModifier(_Htag,'QC',objc,1) :- !.


posModifier(_Htag,_Dtag,_Class,1) :- !. %catchall


%allows to modify likelihood for certain distances.

distModifier(D,app,DISTMOD) :- 	(D < 6, !,
				DISTMOD is 1-((D-1)/5)
				)
				;
				DISTMOD is 0.2-D*0.001, !.

distModifier(D,gmod,DISTMOD) :- (D > 0, !,
				DISTMOD is 0.8-D*0.01)
				;
				DISTMOD is 1+D*0.01, !.
%catchall
distModifier(D,_Class,DISTMOD) :- DISTMOD is 1-D*0.01.


%distmodifier depending on tags
distModifier(D,Htag,_Dtag,pp,DISTMOD) :- (nountag(Htag);nametag(Htag)), DISTMOD is 1-D*0.2.

distModifier(D,Htag,_Dtag,kom,DISTMOD) :- (Htag = 'VVFIN'; Htag='VAFIN';Htag='VMFIN'), DISTMOD is 1-D*0.01.
distModifier(D,_Htag,_Dtag,kom,DISTMOD) :- DISTMOD is 1-D*0.04.

%catchall
distModifier(D,_Htag,_Dtag,Class,DISTMOD) :- distModifier(D,Class,DISTMOD).


%findall might seem unnecessary, but keeps possible variables uninstantiated.
testmorphology(List,Tag,Subj,Obja,Objd,Objg,SubjOut,ObjaOut,ObjdOut,ObjgOut) :- 
			(((findall(_,case_nom(List,Tag),LN), 
			LN \= [],SubjOut is Subj)	; SubjOut is 0),
			((findall(_,case_acc(List,Tag),LA), 
			LA \= [], ObjaOut is Obja)	; ObjaOut is 0),
			((findall(_,case_dat(List,Tag),LD), 
			LD \= [], ObjdOut is Objd)	; ObjdOut is 0),
			((findall(_,case_gen(List,Tag),LG), 
			LG \= [], ObjgOut is Objg)	; ObjgOut is 0
			)), !.



verbtag('v').
verbtag('vvinf').
verbtag('vainf').
verbtag('vminf').
verbtag('vvinf').
verbtag('vafin').
verbtag('vmfin').
verbtag('vvpp').
verbtag('vapp').
verbtag('vmpp').
verbtag('vaimp').
verbtag('vvimp').
verbtag('vvizu').
nountag('nn').
nametag('ne').
nametag('fm').