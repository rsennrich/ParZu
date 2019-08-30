% Grammar Rules: head(+Head,+Dependent,?Direction,+Type,_)

%%head(HeadTag,DepTag,Dir,Type,Transtag,[HeadChunk,DepChunk,HeadWord,DepWord,HeadRels,DepRels,HeadID,DepID],HeadPos-DepPos,HeadMorph,DepMorph,TransMorph)

:- style_check(-discontiguous).
:- ensure_loaded('helper_predicates.pl').
:- ensure_loaded('morph_predicates.pl').

%======================================================================================
%determiners

head('NN',DET,l,det,'NN',[_,_,_,_,OF,_,_,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'NN',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('NE',DET,l,det,'NE',[_,_,_,_,OF,_,_,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'NE',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('FM',DET,l,det,'FM',[_,_,_,_,OF,_,_,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'FM',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

%ein paar Leute: Take morphology from noun; no agreement necessary
head('NIDEF',DET,l,det,'NN',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- detcan(DET,G), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).


%allow (with low probability) non-congruent noun phrases
head('NN',DET,l,bad_det,'NN',[_,_,_,_,OF,_,_,_],_-G,_,MTemp,MNew) :- detcan(DET,G), relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), convertMorphList(DET,MTemp,'NN',MNew).
head('NE',DET,l,bad_det,'NE',[_,_,_,_,OF,_,_,_],_-G,_,MTemp,MNew) :- detcan(DET,G), relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), convertMorphList(DET,MTemp,'NE',MNew).
head('FM',DET,l,bad_det,'FM',[_,_,_,_,OF,_,_,_],_-G,_,MTemp,MNew) :- detcan(DET,G), relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), convertMorphList(DET,MTemp,'FM',MNew).

%solch eine Friedenstruppe: double determiner possible with PIDAT.
head('NN','PIDAT',l,det,'NN',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).

head('NE','PIDAT',l,det,'NE',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).

head('FM','PIDAT',l,det,'FM',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).


%all das; von all denjenigen usw.
head('PDS','PIDAT',l,det,'PDS', [_,_,_,all,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.

%alles andere
head('PIS','PIDAT',l,det,'PIS',_,H-D,MH,_,MH) :- 1 is H-D.

%"das Ihre"
% a relation between 'PPOSS' as head and a determiner (detcan/2 checks if DET is valid POS tag)
% direction is 'l', meaning that dependent comes before the head
% if successfull, the relation will receive label 'det',
% we give the resulting construction a new internal POS-like tag ('NN') - this will be used when the head is combined with other words in later rules
% We check that the position of the head (F) has no nouns or similar following it via endOfNP/1 (otherwise, this indicates a tagging error)
% We check that the morphological information of the head (MF) and the dependent (MG) are compatible via check_agreement/5. The list of compatible morphological analyses is stored as MNew, and saved as the morphological information of the full structure
% We check the construction of the head (OF). We dont allow this relation if the head already has a determiner to the left. 'bad_det' is a special type of relationship (defined above), which does not undergo morphological agreement check, but is allowed with low probability.
% if all checks succeed, this relation is considered during parsing, and will be assigned a score that is defined in statmodule_ger.pl.
% if one of the check fails, the relation will not be considered.
head('PPOSS',DET,l,det,'NN',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MNew) :- detcan(DET,G), endOfNP(F), check_agreement(MF,'NN',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).


head('NN','PIAT',l,det,'NN',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).

head('NE','PIAT',l,det,'NE',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).

head('FM','PIAT',l,det,'FM',[_,_,_,_,OF,_,_,_],_-G,MH,_,MH) :- \+ member('<-gmod<-',OF), OldPos is G - 1, checkPos(OldPos,_,LTag,_,_), \+ attributive_pronoun(LTag), \+ member(LTag,['ART','ADJA']).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA',DET,l,det,'NN',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MNew) :- detcan(DET,G), endOfNP(F), check_agreement(MF,'ADJA',MG,DET,MTemp), convertMorphList('ADJA',MTemp,'NN',MNew) ,\+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).

head('CARD',DET,l,det,'NN',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MNew) :- detcan(DET,G), endOfNP(F), check_agreement(MF,'NN',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).


%new transtag (PRELS) to identify subordinated clauses - interfering with other rules? perhaps new rules for appositions etc. needed.
head('NN','PRELAT',l,det,'PRELS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'NN',MG,'PRELAT',MNew).

head('NE','PRELAT',l,det,'PRELS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'NE',MG,'PRELAT',MNew).

head('FM','PRELAT',l,det,'PRELS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'FM',MG,'PRELAT',MNew).

%ungrammatical in theory, but tagging errors possible
head('NN','PRELS',l,det,'PRELS',[_,_,_,_,OF,OG,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-det<-',OG), \+ member('<-bad_det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'NN',MG,'PRELS',MNew).

head('NE','PRELS',l,det,'PRELS',[_,_,_,_,OF,OG,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-det<-',OG), \+ member('<-bad_det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'NE',MG,'PRELS',MNew).

head('FM','PRELS',l,det,'PRELS',[_,_,_,_,OF,OG,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-det<-',OG), \+ member('<-bad_det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'FM',MG,'PRELS',MNew).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA','PRELAT',l,det,'PRELS',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F),\+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).

head('CARD','PRELAT',l,det,'PRELS',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F),\+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).



head('NN','PWAT',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- check_agreement(MF,'NN',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('NE','PWAT',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- check_agreement(MF,'NE',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('FM','PWAT',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- check_agreement(MF,'FM',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

%ungrammatical in theory, but tagging errors possible
head('NN','PWS',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), check_agreement(MF,'NN',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('NE','PWS',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), check_agreement(MF,'NE',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).

head('FM','PWS',l,det,'PWS',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- correct_mistagging(yes), check_agreement(MF,'FM',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA','PWAT',l,det,'PWS',[_,_,_,_,OF,_,_,_],F-_,MF,MG,MNew) :- endOfNP(F), check_agreement(MF,'ADJA',MG,'PWAT',MTemp), convertMorphList('ADJA',MTemp,'NN',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).

head('CARD','PWAT',l,det,'PWS',[_,_,_,_,OF,_,_,_],F-_,MF,MG,MNew) :- endOfNP(F), check_agreement(MF,'NN',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF).


%======================================================================================
%attributes

head('NN','ADJA',l,attr,'NN',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), check_agreement(MF,'NN',MG,'ADJA',MNew).

head('NE','ADJA',l,attr,'NE',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), check_agreement(MF,'NE',MG,'ADJA',MNew).

head('FM','ADJA',l,attr,'FM',[_,_,_,_,OF,_,_,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), check_agreement(MF,'FM',MG,'ADJA',MNew).

%allow (with low probability) non-congruent noun phrases
head('NN','ADJA',l,bad_attr,'NN',[_,_,_,_,OF,_,_,_],_,MF,_,MF) :- relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).
head('NE','ADJA',l,bad_attr,'NE',[_,_,_,_,OF,_,_,_],_,MF,_,MF) :- relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).
head('FM','ADJA',l,bad_attr,'FM',[_,_,_,_,OF,_,_,_],_,MF,_,MF) :- relax_agreement(yes), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).


%this rule only applies if there is another article/attribute/pronoun left of the pronoun. "Ein paar Leute". Special transtag if article morphology is to be ignored in 'det' rules.
head('NN','PIDAT',l,attr,TransTag,[_,_,_,GWord,OF,_,_,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), LPos is G - 1, checkPos(LPos,LWord,LTag,_,_), (attributive_pronoun(LTag); member(LTag,['ART','ADJA'])), check_agreement(MF,'NN',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),(LWord=ein;LWord=eine))->TransTag='NIDEF';TransTag='NN').

head('NE','PIDAT',l,attr,TransTag,[_,_,_,GWord,OF,_,_,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), LPos is G - 1, checkPos(LPos,LWord,LTag,_,_), (attributive_pronoun(LTag); member(LTag,['ART','ADJA'])), check_agreement(MF,'NE',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),(LWord=ein;LWord=eine))->TransTag='NIDEF';TransTag='NE').

head('FM','PIDAT',l,attr,TransTag,[_,_,_,GWord,OF,_,_,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), LPos is G - 1, checkPos(LPos,LWord,LTag,_,_), (attributive_pronoun(LTag); member(LTag,['ART','ADJA'])), check_agreement(MF,'FM',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),(LWord=ein;LWord=eine))-TransTag='NIDEF';TransTag='FM').

head('PIS','PIDAT',l,attr,TransTag,[_,_,_,GWord,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G, LPos is G - 1, checkPos(LPos,LWord,LTag,_,_), (attributive_pronoun(LTag); member(LTag,['ART','ADJA'])), ((pidat_anymorph(GWord),(LWord=ein;LWord=eine))->TransTag='NIDEF';TransTag='PIS').

%ein paar Leute - no morphology check
pidat_anymorph('paar').
pidat_anymorph('bisschen').
pidat_anymorph('bißchen').
pidat_anymorph('wenig').


head('NN','CARD',l,attr,'NN',[_,_,HeadWord,DepWord,OF,_,_,_],_,MH,_,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), unify_card(HeadWord,DepWord,MH,MNew).

head('NE','CARD',l,attr,'NE',[_,_,HeadWord,DepWord,OF,_,_,_],_,MH,_,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), unify_card(HeadWord,DepWord,MH,MNew).

head('FM','CARD',l,attr,'FM',[_,_,HeadWord,DepWord,OF,_,_,_],_,MH,_,MNew) :- \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF), unify_card(HeadWord,DepWord,MH,MNew).


%Der Ex- Aussenminister. Might be treated as two words (for instance on line breaks)
head('NN','TRUNC',l,attr,'NN',[_,_,_,_,OF,_,_,_],D,MH,_,MH) :- 1 is D, \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).

head('NE','TRUNC',l,attr,'NE',[_,_,_,_,OF,_,_,_],D,MH,_,MH) :- 1 is D, \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).

head('FM','TRUNC',l,attr,'FM',[_,_,_,_,OF,_,_,_],D,MH,_,MH) :- 1 is D, \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).


%CARD or ADJA as head of NP if noun is missing.
head('ADJA','CARD',l,attr,'NN',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).

head('CARD','CARD',l,attr,'NN',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).

head('ADJA','ADJA',l,attr,'NN',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).

head('CARD','ADJA',l,attr,'NN',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- endOfNP(F), \+ member('<-det<-',OF), \+ member('<-bad_det<-',OF), \+ member('<-gmod<-',OF), \+ member('<-adv<-',OF).


%exception: '2 mal', '5 mal' etc.
head('ADV','CARD',l,attr,'ADV',[_,_,'mal',_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-attr<-',OF), \+ member('<-bad_attr<-',OF).
head('ADV','CARD',l,attr,'ADV',[_,_,'Mal',_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-attr<-',OF), \+ member('<-bad_attr<-',OF).



%'drei andere', 'viele mehr' etc.
head('PIS','CARD',l,attr,'PIS', _,F-G,MH,_,MH) :- 1 is F-G.
head('PIS','ADJD',l,attr,'PIS', _,F-G,MH,_,MH) :- 1 is F-G.
head('PIS','ADJA',l,attr,'PIS', _,F-G,MH,_,MH) :- 1 is F-G.

%phone numbers etc. have postmodifying 
head('CARD','CARD',r,attr,'CARD',[_,_,_,_,_,_,_,_],H-_,MH,_,MH) :- LeftPos is H-1, checkPos(LeftPos,Word,_,_,_), number_head(Word).

%======================================================================================
%prep(osition)

%use prepcompl/1 to list all valid dependents of prepositions.
head('APPR',PN,r,pn,'PP',[_,_,HWord,DWord,HRels,DRels,_,_],_-F,MG,MF,MNew) :- prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew), \+ member('->pn->',HRels), \+ member('->bad_pn->',HRels), enforce_vor_allem_segmentation(HWord,DWord,DRels).

head('APPR','ADJD',r,pn,'PP',[_,_,HWord,DWord,HRels,DRels,_,_],_-F,MH,_,MH) :- endOfNP(F), \+ member('<-obja<-',DRels), \+ member('->pn->',HRels), \+ member('->bad_pn->',HRels), enforce_vor_allem_segmentation(HWord,DWord,DRels).

%allow (with low probability) non-congruent prepositional phrases
head('APPR',PN,r,bad_pn,'PP',[_,_,HWord,DWord,HRels,DRels,_,_],_-F,MG,_,MG) :- prepcompl(PN,F), relax_agreement(yes), \+ member('->pn->',HRels), \+ member('->bad_pn->',HRels), enforce_vor_allem_segmentation(HWord,DWord,DRels).

%bis auf weiteres - may be mistagged.
head(_,'PP',r,pn,'PP',[_,_,bis,_,_,_,_,_],G-F,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.
head(_,'PP',r,pn,'PP',[_,_,'Bis',_,_,_,_,_],G-F,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.


%drei/CARD bis/APPR fünf/CARD
head('APPR','CARD',r,pn,'PP_bis',[_,_,_,_,_,_,_,_],G-F,MH,_,MH) :- 1 is F-G.
head('CARD','PP_bis',r,pp,'CARD',[_,_,_,bis,_,_,_,_],G-F,MH,_,MH) :- 1 is F-G.


%zu might be mistagged as PTKA/PTKZU/PTKVZ
head(_,PN,r,pn,'PP',[_,_,zu,_,OG,_,_,_],_-F,MG,MF,MNew) :- correct_mistagging(yes), prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).
head(_,PN,r,pn,'PP',[_,_,'Zu',_,OG,_,_,_],_-F,MG,MF,MNew) :- correct_mistagging(yes), prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).


%"mit mehr als x" - no distance restriction. (inconsistency in gold standard: pn or kom?)
head('APPR','KOMPX',r,kom,'PP',_,_,MH,_,MH).



%relative clause
head('APPR','PRELAT',r,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MG,MF,MNew) :- correct_mistagging(yes), unify_case(MG,'APPR',MF,'PRELS',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

head('APPR','PRELS',r,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MG,MF,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

head('APPR','PWS',r,pn,'PPQ',[_,_,_,_,OG,_,_,_],_,MG,MF,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

head('APPR','PWAV',r,pn,'PPQ',[_,_,_,_,OG,_,_,_],_,MG,MF,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).


%use prepcompl/1 to list all valid dependents of prepositions.
head('APPRART',PN,r,pn,'PP',[_,_,_,_,OG,_,_,_],_-F,MG,MF,MNew) :- prepcompl(PN,F), check_agreement(MG,'APPRART',MF,PN,MTemp), convertMorphList('APPRART',MTemp,'APPR',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).
head('APPRART',PN,r,bad_pn,'PP',[_,_,_,_,OG,_,_,_],_-F,MG,_,MNew) :- prepcompl(PN,F), relax_agreement(yes), convertMorphList('APPRART',MG,'APPR',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

%relative clause
head('APPRART','PRELAT',r,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MG,_,MNew) :- correct_mistagging(yes), convertMorphList('APPRART',MG,'APPR',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

head('APPRART','PRELS',r,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MG,_,MNew) :- convertMorphList('APPRART',MG,'APPR',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).

head('APPRART','PWS',r,pn,'PPQ',[_,_,_,_,OG,_,_,_],_,MG,_,MNew) :- convertMorphList('APPRART',MG,'APPR',MNew), \+ member('->pn->',OG), \+ member('->bad_pn->',OG).


% lexical exception: "vor allem" is so fixed that in "Er sieht vor allem Peter", Peter can never be part of PP.
% other words seem to be fine: "Eine Situation, die für beide, Peter und Mark, blöd ist."
enforce_vor_allem_segmentation(vor, DepWord, DepRels) :- member(DepWord, ['all','alle','allem']), !,
            \+ member('->app_close->',DepRels),
            \+ member('->app_loose->',DepRels),
            \+ member('->gmod->',DepRels).

enforce_vor_allem_segmentation(_,_,_).

%======================================================================================
%postposition


%use prepcompl/1 to list all valid dependents of prepositions.
head('APPO',PN,l,pn,'PP',[_,_,_,_,OG,_,_,_],_-G,MH,_,MH) :- prepcompl(PN,G), \+ member('<-pn<-',OG), \+ member('->bad_pn->',OG).

%relative clause
head('APPO','PRELS',l,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('<-pn<-',OG), \+ member('->bad_pn->',OG).

head('APPO','PRELAT',l,pn,'PPREL',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), \+ member('<-pn<-',OG), \+ member('->bad_pn->',OG).

head('APPO','PWS',l,pn,'PPQ',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('<-pn<-',OG), \+ member('->bad_pn->',OG).

head('APPO','PWAV',l,pn,'PPQ',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('<-pn<-',OG), \+ member('->bad_pn->',OG).

%======================================================================================
%Subject, only one is allowed    

%subject before finite verb
head('V*FIN',SUBJ,l,subj,'V*FIN',[FC,_,_,DWord,UG,OG,_,_],_-G,MF,MG,MNew) :- subjcandidate(SUBJ,G), ((among_dependents(OG,'->kon->',-1);collective(DWord))->(case_nom(MG,SUBJ),MNew=MF);check_agreement(MF,'VVFIN',MG,SUBJ,MNew)), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

%allow subject before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
head('VVPP', SUBJ,l,subj,'NEB',[FC,_,_,_,UG,_,_,_],F-G,MF,MG,MF) :- subjcandidate(SUBJ,G), \+ SUBJ = 'CARD', verbchunklength(FC,1), RightPos is F + 1, \+ checkPos(RightPos,_,'KON',_,_), case_nom(MG,SUBJ), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

%relative pronoun (new transtag 'RC')
head('V*FIN','PRELS',l,subj,'RC',[FC,_,_,_,UG,OG,_,_],_,MF,MG,MNew) :- (among_dependents(OG,'->kon->',-1)->(case_nom(MG,'PRELS'),MNew=MF); check_agreement(MF,'VVFIN',MG,'PRELS',MNew)), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,subj,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_nom(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%interrogative pronoun (new transtag 'QC')
head('V*FIN','PWS',l,subj,'QC',[FC,_,_,_,UG,OG,_,_],_,MF,MG,MNew) :- (among_dependents(OG,'->kon->',-1)->(case_nom(MG,'PWS'),MNew=MF); check_agreement(MF,'VVFIN',MG,'PWS',MNew)), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%"Was" can be (mis)tagged PWS or PRELS
head('V*FIN','PWS',l,subj,'RC',[FC,_,_,was,UG,_,_,_],_,MF,MG,MNew) :- check_agreement(MF,'VVFIN',MG,'PWS',MNew), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('V*FIN','PRELS',l,subj,'QC',[FC,_,_,was,UG,_,_,_],_,MF,MG,MNew) :- check_agreement(MF,'VVFIN',MG,'PRELS',MNew), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%interrogative pronoun (new transtag 'QC'); special case with "sein": don't require number agreement (Wer sind die Beatles?)
head('VAFIN','PWS',l,subj,'QC',[FC,_,sein,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PWS',l,subj,'QC',[FC,_,sind,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PWS',l,subj,'QC',[FC,_,waren,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%demonstrative pronoun; special case with "sein": don't require number agreement (Das sind die grössten Türme der Welt)
head('VAFIN','PDS',l,subj,'VAFIN',[FC,_,sein,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), gender_neut(MG,'PDS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PDS',l,subj,'VAFIN',[FC,_,sind,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), gender_neut(MG,'PDS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PDS',l,subj,'VAFIN',[FC,_,waren,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), gender_neut(MG,'PDS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

head('VAFIN','PDS',r,subj,'VAFIN',[_,_,sein,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PDS',r,subj,'VAFIN',[_,_,sind,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PDS',r,subj,'VAFIN',[_,_,waren,_,UG,_,_,_],_,MF,MG,MF) :- case_nom(MG,'PDS'), restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

%personal pronoun: special case with "es sind/waren": (es sind keine leeren Worte)
head('VAFIN','PPER',l,subj,'VAFIN',[FC,_,sein,es,UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PPER',l,subj,'VAFIN',[FC,_,sind,es,UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PPER',l,subj,'VAFIN',[FC,_,waren,es,UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

head('VAFIN','PPER',r,subj,'VAFIN',[_,_,sein,es,UG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PPER',r,subj,'VAFIN',[_,_,sind,es,UG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).
head('VAFIN','PPER',r,subj,'VAFIN',[_,_,waren,es,UG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%only necessary in case of tagging errors
head('VVPP','PWS',l,subj,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%subject after finite verb
head('V*FIN',SUBJ,r,subj,'V*FIN',[_,_,_,DWord,OG,OF,_,_],_-F,MG,MF,MNew)  :- subjcandidate(SUBJ,F), ((among_dependents(OF,'->kon->',-1);collective(DWord))->(case_nom(MF,SUBJ),MNew=MG); check_agreement(MG,'VVFIN',MF,SUBJ,MNew)), restrict_coord_and_nachfeld(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG), \+ member('->obji->',OG), \+ member('<-explsubj<-',OG), \+ member('->explsubj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG).


%======================================================================================
%Accusative object, only one allowed (for verbs using two accusative objects, use OBJA2)

%object before finite verb.
head('V*FIN',OBJ,l,obja,'V*FIN',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).


%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,obja,'VVIZU',[FC,_,_,_,UG,_,_,_],__-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%allow OBJAs before nonfinite verb in coordination chain
head('V*INF', OBJ,l,obja,'V*INF',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('V*PP', OBJ,l,obja,'V*PP',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%relative pronoun (new transtag 'RC')
head('V*FIN','PRELS',l,obja,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,obja,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%interrogative pronoun (new transtag 'QC')
head('V*FIN','PWS',l,obja,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,obja,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%was tun?
head('V*INF', 'PWS',l,obja,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- in_coordination(FC,UG), case_acc(MG,'PWS'), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).


%"Was" can be relative (Der Film hat ein Happy-End, was ich sehr schön finde) or interrogative (Ich frage mich, was er da tut)
head('V*FIN','PWS',l,obja,'RC',[FC,_,_,was,UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('V*FIN','PRELS',l,obja,'QC',[FC,_,_,was,UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).


%object after finite verb.
head('V*FIN',OBJ,r,obja,'V*FIN',[GC,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord_and_nachfeld(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obji<-',OG), \+ member('->obji->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('V*IMP',OBJ,r,obja,'V*IMP',[GC,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord_and_nachfeld(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obji<-',OG), \+ member('->obji->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

%rollkragenpullover tragende brillenträger
head('ADJA', OBJ,l,obja,'ADJA',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MF) :- 1 is F-G, derived_from_ppres(MF,'ADJA'), objcandidate(OBJ,F), case_acc(MG,OBJ), \+ member('<-obja<-',OF), \+ member('->obja->',OF).

head('ADJD', OBJ,l,obja,'ADJD',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MF) :- 1 is F-G, derived_from_ppres(MF,'ADJD'), objcandidate(OBJ,F), case_acc(MG,OBJ), \+ member('<-obja<-',OF), \+ member('->obja->',OF).


%======================================================================================
%2nd accusative object. Mirror rules for the first, but without uniqueness requirement

%object before finite verb.
head('V*FIN',OBJ,l,obja2,'V*FIN',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,obja2,'VVIZU',[FC,_,_,_,UG,_,_,_],__-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%allow OBJAs before nonfinite verb in coordination chain
head('V*INF', OBJ,l,obja2,'V*INF',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('V*PP', OBJ,l,obja2,'V*PP',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%relative pronoun (new transtag 'RC')
head('V*FIN','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%interrogative pronoun (new transtag 'QC')
head('V*FIN','PWS',l,obja2,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,obja2,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object after finite verb.
head('V*FIN',OBJ,r,obja2,'V*FIN',[GC,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord_and_nachfeld(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('V*IMP',OBJ,r,obja2,'V*IMP',[GC,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord_and_nachfeld(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).


%======================================================================================
%Dative object, only one allowed    

%object before finite verb. 
head('V*FIN',OBJ,l,objd,'V*FIN',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,objd,'VVIZU',[_,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%allow OBJDs before nonfinite verb in coordination chain
head('V*INF', OBJ,l,objd,'V*INF',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('V*PP', OBJ,l,objd,'V*PP',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

%relative pronoun (new transtag 'RC')
head('V*FIN','PRELS',l,objd,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,objd,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objd<-',UG), \+ member('->objd->',UG).



%interrogative pronoun (new transtag 'QC')
head('V*FIN','PWS',l,objd,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,objd,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%die der partei nahestehenden wähler
head('ADJA', OBJ,l,objd,'ADJA',[_,_,Lemma,_,OF,_,_,_],F-G,MF,MG,MF) :-  1 is F-G, (derived_from_ppres(MF,'ADJA');derived_from_ppast(MF,'ADJA');Lemma=gemein), objcandidate(OBJ,F), case_dat(MG,OBJ), \+ member('<-objd<-',OF), \+ member('->objd->',OF).

head('ADJD', OBJ,l,objd,'ADJD',[_,_,Lemma,_,OF,_,_,_],F-G,MF,MG,MF) :-  1 is F-G, (derived_from_ppres(MF,'ADJD');derived_from_ppast(MF,'ADJD');Lemma=gemein), objcandidate(OBJ,F), case_dat(MG,OBJ), \+ member('<-objd<-',OF), \+ member('->objd->',OF).



%object after finite verb.
head('V*FIN',OBJ,r,objd,'V*FIN',[_,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord_and_nachfeld(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).

head('V*IMP',OBJ,r,objd,'V*IMP',[_,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord_and_nachfeld(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).


%======================================================================================
%predicate noun (pred), only one allowed.


%predicate noun before finite verb.
head('V*FIN',Dtag,l,pred,'V*FIN',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF)  :- predcand(Dtag,G), case_nom(MG,Dtag), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-subjc<-',UG), \+ member('<-adv<-',UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('->objp->', UG), \+ member('<-objp<-', UG).

head('V*FIN',Dtag,l,pred,'V*FIN',[FC,_,_,_,UG,_,_,_],_,MH,_,MH)  :- predcand_adverb(Dtag), Dtag \= 'PWAV', \+ member('<-pred<-',UG), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-subjc<-',UG), \+ member('<-adv<-',UG), \+ member('->pred->',UG), \+ member('<-objd<-',UG).



%predicate noun before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',Dtag,l,pred,'VVIZU',[_,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- predcand(Dtag,G), case_nom(MG,Dtag), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG), \+ member('->objp->', UG), \+ member('<-objp<-', UG).

head('VVIZU',Dtag,l,pred,'VVIZU',[_,_,_,_,UG,_,_,_],_,MH,_,MH) :- predcand_adverb(Dtag), Dtag \= 'PWAV', \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG).


%allow PRED before nonfinite verb if it is coordinated
head('V*INF', OBJ,l,pred,'V*INF',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- predcand(OBJ,G), in_coordination(FC,UG), case_nom(MG,OBJ), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG), \+ member('->objp->', UG), \+ member('<-objp<-', UG).
head('V*PP', OBJ,l,pred,'V*PP',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- predcand(OBJ,G), in_coordination(FC,UG), case_nom(MG,OBJ), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG), \+ member('->objp->', UG), \+ member('<-objp<-', UG).

head('V*INF', OBJ,l,pred,'V*INF',[FC,_,_,_,UG,_,_,_],_,MF,_,MF) :- predcand_adverb(OBJ), OBJ \= 'PWAV', in_coordination(FC,UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG).
head('V*PP', OBJ,l,pred,'V*PP',[FC,_,_,_,UG,_,_,_],_,MF,_,MF) :- predcand_adverb(OBJ), OBJ \= 'PWAV', in_coordination(FC,UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG).



%das Columbia genannte Raumschiff
head('ADJA', OBJ,l,pred,'ADJA',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MF) :-  1 is F-G, derived_from_ppast(MF,'ADJA'), predcand(OBJ,G), case_nom(MG,OBJ), \+ member('<-pred<-',OF).

head('ADJD', OBJ,l,pred,'ADJD',[_,_,_,_,OF,_,_,_],F-G,MF,MG,MF) :-  1 is F-G, derived_from_ppast(MF,'ADJD'), predcand(OBJ,G), case_nom(MG,OBJ), \+ member('<-pred<-',OF).


%interrogative clause
head('V*FIN','PWAV',l,pred,'QC',[FC,_,_,DWord,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-pred<-',OF), \+ member('->pred->',OF), \+ derive_prep_from_pav(DWord,_).
head('VVPP','PWAV',l,pred,'QC',[FC,_,_,DWord,HeadRels,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), verbchunklength(FC,1), \+ member('<-pred<-',HeadRels), \+ member('->pred->',HeadRels),  \+ derive_prep_from_pav(DWord,_).


%predicate noun after finite verb.
head('V*FIN',Dtag,r,pred,'V*FIN',[_,_,_,_,OG,_,_,_],_-G,MG,MF,MG)  :- predcand(Dtag,G), case_nom(MF,Dtag), restrict_coord_and_nachfeld(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG), \+ member('<-objp<-', OG).

head('V*IMP',Dtag,r,pred,'V*IMP',[_,_,_,_,OG,_,_,_],_-G,MG,MF,MG)  :- predcand(Dtag,G), case_nom(MF,Dtag), restrict_coord_and_nachfeld(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG), \+ member('<-objp<-', OG).



head('V*FIN',Dtag,r,pred,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH)  :- predcand_adverb(Dtag), restrict_coord_and_nachfeld(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).

head('V*IMP',Dtag,r,pred,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH)  :- predcand_adverb(Dtag), restrict_coord_and_nachfeld(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).


predcand('NN', _).
predcand('NE', _).
predcand('FM', _).
predcand('PIS', _).
predcand('PPER', _).
predcand('PWS', _).
predcand('ADJA',Pos) :- \+ var(Pos), endOfNP(Pos).

predcand_adverb('ADJD').
predcand_adverb('ADV').
predcand_adverb('PWAV').


%======================================================================================
%Genitive object, only one allowed    

%object before finite verb. 
head('V*FIN',OBJ,l,objg,'V*FIN',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,objg,'VVIZU',[_,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%allow OBJDs before nonfinite verb in coordination chain
head('V*INF', OBJ,l,objg,'V*INF',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('V*PP', OBJ,l,objg,'V*PP',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VVIZU', OBJ,l,objg,'VVIZU',[FC,_,_,_,UG,_,_,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), in_coordination(FC,UG), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%relative pronoun (new transtag 'RC')
head('V*FIN','PRELS',l,objg,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,objg,'RC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%interrogative pronoun (new transtag 'QC')
head('V*FIN','PWS',l,objg,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,objg,'QC',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- correct_mistagging(yes), case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%object after finite verb.
head('V*FIN',OBJ,r,objg,'V*FIN',[_,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord_and_nachfeld(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

head('V*IMP',OBJ,r,objg,'V*IMP',[_,_,_,_,OG,_,_,_],_-F,MG,MF,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord_and_nachfeld(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

%======================================================================================
%g(enitive) mod(ifier). only one on each side of the head allowed.

%Genitive modifier after head noun.

head('NN', GEN,r, gmod, 'NN',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; GEN = 'NE'; GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).

head('NE', GEN,r, gmod, 'NE',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'NE')); GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('<-adv<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).

head('FM', GEN,r, gmod, 'FM',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'FM')); GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).

head('PDS', GEN,r, gmod, 'PDS',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'PDS')); GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).

head('PIS', GEN,r, gmod, 'PIS',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'PIS')); GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).

head('CARD', GEN,r, gmod, 'CARD',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- (F-G > 1; GEN = 'NE'; GEN = 'PDS'; GEN = 'PIS'), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app_loose->',OG), \+ member('->app_close->',OG).


%no occurrences of these in exploration corpus. still leave in? 
%Yes for 'PWS': "Welcher der beiden ist der Mörder?" No for the others.
head('PWS', GEN,r, gmod, 'PWS',[_,_,_,_,OG,OF,_,_],G-F,MG,MF,MG) :- F-G > 1, validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('<-pp<-',OF), \+ among_dependents(OF, '->app_loose->',-1), \+ member('->pp->',OG), \+ member('->kon->',OG).

%head('PPER', GEN, r, gmod, 'PPER',[_,_,_,_,OG,_,_,_],_,MG,MF,MG) :- validgmod(GEN), case_gen(MF), \+ member('->gmod->',OG).

%head('PRELS', GEN, r, gmod, 'PRELS',[_,_,_,_,OG,_,_,_],_,MG,MF,MG) :- validgmod(GEN), case_gen(MF), \+ member('->gmod->',OG).


%Anfang des Jahres etc.: can be adverbial expression -> special metatag
head('NN','NN',r,gmod,'NZEIT',[_,_,HeadWord,DepWord,HeadRels,DepRels,_,_],_,HeadMorph,DepMorph,HeadMorph) :- zeit_like_anfang(HeadWord), zeitcand(DepWord), case_gen(DepMorph,'NN'), \+ member('->gmod->',HeadRels), \+ member('<-gmod<-',DepRels), \+ member('<-pp<-',DepRels), \+ member('->pp->',HeadRels), \+ member('->kon->',HeadRels), \+ member('->app_loose->',HeadRels), \+ member('->app_close->',HeadRels).


%Genitive modfier before head noun. Pronoun heads seem to be ungrammatical: "Einer der Fischer" vs. *"Bremens einer".

head('NN', 'NE',l, gmod, 'NN',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG).

head('NE', 'NE',l, gmod, 'NE',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG).

head('FM', 'NE',l, gmod, 'FM',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG).


head('NN', 'NN',l, gmod, 'NN',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).

head('NE', 'NN',l, gmod, 'NE',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).

head('FM', 'NN',l, gmod, 'FM',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).


head('NN', 'FM',l, gmod, 'NN',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).

head('NE', 'FM',l, gmod, 'NE',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).

head('FM', 'FM',l, gmod, 'FM',[_,_,_,_,UG,OG,_,_],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG), \+ member('<-bad_det<-',UG), \+ member('<-bad_det<-',OG), \+ member('->kon->',OG).


%======================================================================================
%EXPLetive 'es'


%EXPL after finite verb - takes position of OBJA or SUBJ
head('V*FIN','PPER',r,explsubj,'V*FIN',[_,_,_,'es',OG,_,_,_],_,MH,_,MH)  :- restrict_coord_and_nachfeld(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG).

head('V*FIN','PPER',r,explobja,'V*FIN',[GC,_,_,'es',OG,_,_,_],_,MH,_,MH)  :- restrict_coord_and_nachfeld(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('passive',GC), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG).


%EXPL before finite verb - takes position of SUBJ
head('V*FIN','PPER',l,explsubj,'V*FIN',[FC,_,_,'es',UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), (member('->subj->',UG);member('->subjc->',UG);member('->obji->',UG)).
head('V*FIN','PPER',l,explsubj,'V*FIN',[FC,_,_,'Es',UG,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,UG), (member('->subj->',UG);member('->subjc->',UG);member('->obji->',UG)).


%EXPL before finite verb - takes position of OBJA (only possible in Verbletztstellung)
head('V*FIN','PPER',l,explobja,'V*FIN',[FC,_,_,'es',UG,_,_,_],_,MH,_,MH) :- \+ member('mainclause',FC), (member('->objc->',UG);member('->obji->',UG)), \+ member('passive',FC), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-obja<-',UG), \+ member('->obja->',UG).


%======================================================================================
%pp

%allows prepositional phrases to be enclosed by commas
head('PP','$,',l,comma,'PP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).

head('PP','$,',r,comma,'PP',[_,_,_,_,HRels,_,_,_],_,HM,_,HM) :- (member('<-comma<-', HRels);member('->kon->', HRels)), \+ member('->comma->', HRels).


%prepositional phrases postmodifying a np or pronoun
head('NN', 'PP',r,pp,'NN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('NE', 'PP',r,pp,'NE',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('FM', 'PP',r,pp,'FM',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('PIS', 'PP',r,pp,'PIS',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('ADJD', 'PP',r,pp,'ADJD',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

%rare heads; little impact on performance
head('PPER', 'PP',r,pp,'PPER',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

head('PDS', 'PP',r,pp,'PDS',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

head('CARD', 'PP',r,pp,'CARD',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

head('PTKANT', 'PP',r,pp,'PTKANT',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

head('PWS', 'PP',r,pp,'PWS',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('->pp->',OG).

%included in case of tagging errors
head('ART', 'PP',r,pp,'PIS',[_,_,_,_,OG,_,_,_],G-F,MH,_,MH) :- correct_mistagging(yes), 1 is F-G, \+ member('->pp->',OG).


%needs disambiguation, otherwise too many FPs
head('NN', 'PAV',r,pp,'NN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('NE', 'PAV',r,pp,'NE',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('FM', 'PAV',r,pp,'FM',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('PIS', 'PAV',r,pp,'PIS',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('CARD', 'PAV',r,pp,'CARD',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).

head('ADJD', 'PAV',r,pp,'ADJD',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('->pp->',OG).


%prepositional phrase after verb
head('V*FIN', 'PP',r,pp,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->objp->',OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).

head('V*IMP', 'PP',r,pp,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->objp->',OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).



head('V*FIN', 'PAV',r,pp,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->objp->',OG).

head('V*IMP', 'PAV',r,pp,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->objp->',OG).



%other direction, prepositional phrase before verb
head('V*FIN', 'PP',l,pp,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).

head('VVIZU', 'PP',l,pp,'VVIZU',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).


%relative pronoun (new transtag 'RC')
head('V*FIN', 'PPREL',l,pp,'RC',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).


%interrogative pronoun (new transtag 'QC')
head('V*FIN', 'PPQ',l,pp,'QC',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).


head('V*FIN', 'PAV',l,pp,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).

head('V*FIN', 'PWAV',l,pp,'V*FIN',[FC,_,_,DWord,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), derive_prep_from_pav(DWord,_).





%allow PPs before nonfinite verb in coordination chain
%example/motivation: das kind, 1999 in cottbus geboren, konnte schon klavier spielen.
head(NONFIN, 'PP',l,pp,NONFIN,[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- nonfinite(NONFIN), in_coordination(FC,UG).
head(NONFIN, 'PAV',l,pp,NONFIN,[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- nonfinite(NONFIN), in_coordination(FC,UG).

%allow PPs after nonfinite verb in coordination chain
%example/motivation: das kind, geboren in cottbus, konnte schon klavier spielen
head(NONFIN, 'PP',r,pp,NONFIN,[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- nonfinite(NONFIN), in_coordination(FC,UG), \+ member('->objp->',UG).
head(NONFIN, 'PAV',r,pp,NONFIN,[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- nonfinite(NONFIN), in_coordination(FC,UG), \+ member('->objp->',UG).


%PP premodifying participial adjective (der auf dem boden liegende mann)
head('ADJA', 'PP',l,pp,'ADJA',_,_,MH,_,MH).
head('ADJD', 'PP',l,pp,'ADJD',_,_,MH,_,MH).

%daran angedockt
head('ADJA', 'PAV',l,pp,'ADJA',_,F-G,MH,_,MH) :- 1 is F-G.
head('ADJD', 'PAV',l,pp,'ADJD',_,F-G,MH,_,MH) :- 1 is F-G.

%darunter viele Kinder
head('NN', 'PAV',l,pp,'NN',_,_,MH,_,MH).
head('NE', 'PAV',l,pp,'NE',_,_,MH,_,MH).
head('FM', 'PAV',l,pp,'FM',_,_,MH,_,MH).
head('PIS', 'PAV',l,pp,'PIS',_,_,MH,_,MH).
head('PDS', 'PAV',l,pp,'PDS',_,_,MH,_,MH).
head('PPER', 'PAV',l,pp,'PPER',_,_,MH,_,MH).

%pre-modifying PP
head('NN', 'PP',l,pp,'NN',_,_,MH,_,MH).
head('NE', 'PP',l,pp,'NE',_,_,MH,_,MH).
head('FM', 'PP',l,pp,'FM',_,_,MH,_,MH).
head('PIS', 'PP',l,pp,'PIS',_,_,MH,_,MH).
head('PDS', 'PP',l,pp,'PDS',_,_,MH,_,MH).
head('PPER', 'PP',l,pp,'PPER',_,_,MH,_,MH).


%pp premodifying comparative clause:
%es erweist sich als Schnäppchen und im Idealfall als Kauf des Lebens
head('KOMPX', 'PP',l,pp,'KOMPX',_,_,MH,_,MH).
head('KOMPX', 'PAV',l,pp,'KOMPX',_,F-G,MH,_,MH) :- 1 is F-G.


%complex prepositions:
%bis auf weiteres
head('APPR', 'PP',r,pp,'PP',_,G-F,MH,_,MH) :-  1 is F-G.

%bis dahin
head('APPR', 'PAV',r,pp,'PP',_,G-F,MH,_,MH) :- 1 is F-G.


%======================================================================================
%objp (prepositions as complements)


%prepositional object after verb
head('V*FIN', 'PP',r,objp,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG), \+ noun_pred(OG).

head('V*IMP', 'PP',r,objp,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG), \+ noun_pred(OG).


head('V*FIN', 'PAV',r,objp,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG), \+ noun_pred(OG).

head('V*IMP', 'PAV',r,objp,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG), \+ noun_pred(OG).



%other direction, prepositional object before verb
head('V*FIN', 'PP',l,objp,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF).

head('VVIZU', 'PP',l,objp,'VVIZU',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF).


%relative pronoun (new transtag 'RC')
head('V*FIN', 'PPREL',l,objp,'RC',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF).


%interrogative pronoun (new transtag 'QC')
head('V*FIN', 'PPQ',l,objp,'QC',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF).


%OBJP modifying participial adjective (der auf dem boden liegende mann)
head('ADJA', 'PP',l,objp,'ADJA',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF),\+ endOfNP(F).

head('ADJD', 'PP',l,objp,'ADJD',[_,_,_,_,OF,_,_,_],F-_,MH,_,MH) :- \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF),\+ endOfNP(F).


head('V*FIN', 'PAV',l,objp,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF).

head('V*FIN', 'PWAV',l,objp,'V*FIN',[FC,_,_,DWord,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF), \+ noun_pred(OF), derive_prep_from_pav(DWord,_).


%allow PPs before nonfinite verb in coordination chain
%example/motivation: das kind, 1999 in cottbus geboren, konnte schon klavier spielen.
head('V*INF', 'PP',l,objp,'V*INF',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('V*PP', 'PP',l,objp,'V*PP',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVIZU', 'PP',l,objp,'VVIZU',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('V*INF', 'PAV',l,objp,'V*INF',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('V*PP', 'PAV',l,objp,'V*PP',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVIZU', 'PAV',l,objp,'VVIZU',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


noun_pred(Rels) :- nth1(Pos, Rels, '->pred->'),
    RightPos is Pos + 1,
    nth1(RightPos,Rels,Pred),
    predcand(Tag,_),
    among_dependents(Pred, Tag, 1), !.

noun_pred(Rels) :- nth1(Pos, Rels, '<-pred<-'),
    LeftPos is Pos - 1,
    nth1(LeftPos,Rels,Pred),
    predcand(Tag,_),
    among_dependents(Pred, Tag, 1), !.

%======================================================================================
%appositions


%noun with comma on its left could be apposition.
head('NN','$,',l,comma,'APPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).

head('NE','$,',l,comma,'APPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).

head('FM','$,',l,comma,'APPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).

head('PIS','$,',l,comma,'APPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF), (member('->gmod->', OF); member('->pp->', OF)).

head('CARD','$,',l,comma,'APPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).


head('NN','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID);(bracketToRight(HID,Bracket),rightbracket(Bracket,_))), \+ member('<-comma<-', HRels).

head('NE','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID);(bracketToRight(HID,Bracket),rightbracket(Bracket,_))), \+ member('<-comma<-', HRels).

head('FM','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID);(bracketToRight(HID,Bracket),rightbracket(Bracket,_))), \+ member('<-comma<-', HRels).

head('PIS','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,MH,_,MH) :- (commaToRight(HID);stopToRight(HID);(bracketToRight(HID,Bracket),rightbracket(Bracket,_))), \+ member('<-comma<-', HRels), (member('->gmod->', HRels); member('->pp->', HRels)).

head('CARD','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID);(bracketToRight(HID,Bracket),rightbracket(Bracket,_))), \+ member('<-comma<-', HRels).


%comma at end of apposition allowed/included (but only if there is one on its left).

head('APPX','$,',r,comma,'APP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- member('<-comma<-', OG), \+ member('->comma->', OG).


%appositions that are enclosed by comma are bound to noun on their left.
head('NN','APP',r,app_loose,'NN',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).

head('NE','APP',r,app_loose,'NE',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).

head('FM','APP',r,app_loose,'FM',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'NN',MNew), \+ member('->bracket->', HRels),  \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).

head('PIS','APP',r,app_loose,'PIS',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).

head('PPER','APP',r,app_loose,'PPER',[_,_,HeadWord,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'NN',MNew), HeadWord \= es, \+ member('->bracket->', HRels), \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).


%"am Samstag, dem 5. Oktober" (comma to right is optional)
head('NN','APPX',r,app_loose,'NN',[_,_,_,DepWord,HRels,DRels,_,_],_,MG,MF,MNew) :- month(DepWord), unify_case(MG,'NN',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->app_loose->', DRels), \+ among_dependents(HRels, '->app_loose->', -1).


%apposition enclosed in bracket
head(HTag,'NE',r,app_loose,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,MF,MNew) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), member('<-bracket<-', OF), \+ member('->kon->', HeadRels), member('->bracket->', OF), \+ among_dependents(HeadRels, '->app_loose->', -1), \+ member('->app_loose->', OF), unify_case(MF,'NE',MG,HTag,MNew).

head(HTag,'NN',r,app_loose,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,MF,MNew) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), member('<-bracket<-', OF), \+ member('->kon->', HeadRels), member('->bracket->', OF), \+ among_dependents(HeadRels, '->app_loose->', -1), \+ member('->app_loose->', OF), unify_case(MF,'NN',MG,HTag,MNew).

head(HTag,'FM',r,app_loose,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,MF,MNew) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), member('<-bracket<-', OF), \+ member('->kon->', HeadRels), member('->bracket->', OF), \+ among_dependents(HeadRels, '->app_loose->', -1), \+ member('->app_loose->', OF), unify_case(MF,'FM',MG,HTag,MNew).


%Anfang Oktober etc.: can be adverbial expression -> special metatag
head('NN','NN',r,app_close,'NZEIT',[_,_,HeadWord,DepWord,HeadRels,DepRels,_,_],_,HeadMorph,DepMorph,TransMorph) :- zeit_like_anfang(HeadWord), zeitcand(DepWord), \+ member('<-det<-', DepRels), \+ member('<-bad_det<-', DepRels), \+ member('<-attr<-', DepRels), \+ member('<-bad_attr<-', DepRels), \+ member('<-det<-', HeadRels), \+ member('<-bad_det<-', HeadRels), \+ member('<-attr<-', HeadRels), \+ member('<-bad_attr<-', HeadRels), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), unify_case(HeadMorph,'NN',DepMorph,'NN',TransMorph).

head('NN','CARD',r,app_close,'NZEIT',[_,_,HeadWord,_,HeadRels,DepRels,_,_],_,HeadMorph,DepMorph,TransMorph) :- zeit_like_anfang(HeadWord), \+ member('<-det<-', DepRels), \+ member('<-bad_det<-', DepRels), \+ member('<-attr<-', DepRels), \+ member('<-bad_attr<-', DepRels), \+ member('<-det<-', HeadRels), \+ member('<-bad_det<-', HeadRels), \+ member('<-attr<-', HeadRels), \+ member('<-bad_attr<-', HeadRels), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), unify_case(HeadMorph,'NN',DepMorph,'NN',TransMorph).


%close apposition.

%name as apposition of noun: morphological mismatch is ok in some, but not all cases
%Der Ministerpräsident Italiens -> no app (must be gmod)
%Des Ministerpräsidenten Berlusconi -> (app is ok)
head(HTag,'NE',r,app_close,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,_,MG) :- HTag \= 'NE', apphead(HTag), (HTag = 'PPER'->HeadWord \= es;true), \+ member('->bracket->', HeadRels), \+ member('<-det<-', OF), \+ member('<-bad_det<-', OF), \+ member('<-attr<-', OF), \+ member('<-bad_attr<-', OF), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), \+ member('->gmod->', HeadRels).

%John Lennons -> take genitive information from last element
%if first element is genitive, don't allow relation (except if morphology is ambiguous)
%Der Ministerpräsident Italiens Silvio Berlusconi -> "Silvio" is not apposition of "Italiens", but of "Ministerpräsident"
head('NE','NE',r,app_close,'NE',[_,_,_,_,HeadRels,OF,_,_],_,MG,MF,MF) :- \+ member('<-det<-', OF), \+ member('->bracket->', HeadRels), \+ member('<-bad_det<-', OF), (case_gen(MG,'NE')->case_nom(MG,'NE');true), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), \+ member('->gmod->', HeadRels).

%der bürgermeister meier vs. der internet browser: if last element is nn (but not if ne), use it for np agreement.
%if dependent has attribute ("Mindestgebot 50 Mark"), use head for np agreement
head(HTag,'NN',r,app_close,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,MF,MNew) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), \+ member('->bracket->', HeadRels), \+ member('<-det<-', OF), \+ member('<-bad_det<-', OF), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), \+ member('->gmod->', HeadRels), ((member('<-attr<-', OF);member('<-bad_attr<-', OF))->unify_case(MG,HTag,MF,'NN',MNew);unify_case(MF,'NN',MG,HTag,MNew)).

head(HTag,'FM',r,app_close,HTag,[_,_,HeadWord,_,HeadRels,OF,_,_],_,MG,MF,MNew) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), \+ member('->bracket->', HeadRels), \+ member('<-det<-', OF), \+ member('<-bad_det<-', OF), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), \+ member('->gmod->', HeadRels), ((member('<-attr<-', OF);member('<-bad_attr<-', OF))->unify_case(MG,HTag,MF,'FM',MNew);unify_case(MF,'FM',MG,HTag,MNew)).

head(HTag,'CARD',r,app_close,HTag,[_,_,HeadWord,_,HeadRels,DepRels,_,_],_,MH,_,MH) :- apphead(HTag), (HTag = 'PPER'-> HeadWord \= es;true), \+ member('->bracket->', HeadRels), \+ member('->app_loose->', HeadRels), \+ member('->kon->', HeadRels), \+ member('->gmod->', HeadRels), \+ member('<-det<-', DepRels), \+ member('<-bad_det<-', DepRels), \+ member('<-attr<-', DepRels), \+ member('<-bad_attr<-', DepRels).


%nichts anderes
head('PIS','PIS',r,app_close,'PIS',[_,_,_,andere,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PIS','PIS',r,app_close,'PIS',[_,_,_,anderes,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PIS','PIS',r,app_close,'PIS',[_,_,_,ander,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.

%was/wer alles
head('PWS','PIS',r,app_close,'PWS',[_,_,_,andere,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PWS','PIS',r,app_close,'PWS',[_,_,_,anderes,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PWS','PIS',r,app_close,'PWS',[_,_,_,ander,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.

%wir alle/beide
head(HTag,'PIS',r,app_close,HTag,[_,_,_,alle,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H, apphead(HTag).
head(HTag,'PIS',r,app_close,HTag,[_,_,_,alles,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H, apphead(HTag).
head(HTag,'PIS',r,app_close,HTag,[_,_,_,all,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H, apphead(HTag).
head(HTag,'PIS',r,app_close,HTag,[_,_,_,beide,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H, apphead(HTag).

%das alles
head('PDS','PIS',r,app_close,'PDS',[_,_,_,alle,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PDS','PIS',r,app_close,'PDS',[_,_,_,alles,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PDS','PIS',r,app_close,'PDS',[_,_,_,all,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.

%was/wer alles
head('PWS','PIS',r,app_close,'PWS',[_,_,_,alle,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PWS','PIS',r,app_close,'PWS',[_,_,_,alles,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.
head('PWS','PIS',r,app_close,'PWS',[_,_,_,all,_,_,_,_],H-D,MH,_,MH) :- 1 is D-H.

apphead('PPER').
apphead('PIS').
apphead('PDS').
apphead('FM').
apphead('NN').
apphead('NE').


%======================================================================================
%relative clauses


%allows relative clauses to be enclosed by commas
head('RC','$,',r,comma,'RC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head('RC','$,',l,comma,'RC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).


%allows question clauses to be enclosed by commas
head('QC','$,',l,comma,'QC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head('QC','$,',r,comma,'QC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).


%attaches relative clause to last noun, pronoun or verb. better attachment strategies only available through morphological information and in postprocessing ("er hat den Mann aus Zürich gekannt, der gestorben ist")

head('NN','RC',r,rel,'NN',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('NE','RC',r,rel,'NE',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('FM','RC',r,rel,'FM',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('PDS','RC',r,rel,'PDS',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('PIS','RC',r,rel,'PIS',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('PPER','RC',r,rel,'PPER',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('PPOSS','RC',r,rel,'PPOSS',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('V*FIN','RC',r,rel,'VVFIN',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('V*IMP','RC',r,rel,'V*IMP',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('VVIZU','RC',r,rel,'VVIZU',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('RC','RC',r,rel,'RC',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('NEB','RC',r,rel,'NEB',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels).

head('ADV','RC',r,rel,'ADV',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels), among_dependents(DepRels,'PWAV',1).

head('PAV','RC',r,rel,'PAV',[_,_,_,_,OG,DepRels,_,_],_,MH,_,MH) :- restrict_coord(OG), member('<-comma<-', DepRels), among_dependents(DepRels,'PWAV',1).

%======================================================================================
%subordinating conjunctions

%conjunctions: wenn, obwohl etc.
head('V*FIN','KOUS',l,konjneb,'NEB',_,_,MH,_,MH).

head('ADJD','KOUS',l,konjneb,'NEB',_,_,MH,_,MH).

%wenn gewünscht; wie versprochen
head('VVPP','KOUS',l,konjneb,'PPNEB',[FC,_,_,_,_,_,_,_],_,MH,_,MH) :- verbchunklength(FC,1).

%consider possibility of tagging error
head('VVPP','KOKOM',l,konjneb,'PPNEB',[FC,_,_,_,_,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), verbchunklength(FC,1).

%um/statt/ohne + inf
head('VVIZU','KOUI',l,konjneb,'NEB',_,_,MH,_,MH).


%ohne daß: whole construction is finite.
head('KOUS',_,l,konjneb,'KOUS',[_,_,'daß','Ohne',_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.
head('KOUS',_,l,konjneb,'KOUS',[_,_,'daß',ohne,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.
head('KOUS',_,l,konjneb,'KOUS',[_,_,dass,'Ohne',_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.
head('KOUS',_,l,konjneb,'KOUS',[_,_,dass,ohne,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.

%so daß
head('KOUS',_,l,konjneb,'KOUS',[_,_,'daß',so,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.
head('KOUS',_,l,konjneb,'KOUS',[_,_,dass,so,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.

% als ob: same idea as "ohne dass"
head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,ob,als,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.
head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,ob,'Als',_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.

head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,wenn,als,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.
head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,wenn,'Als',_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.

head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,wenn,wie,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.
head('KOUS','KOUS',l,konjneb,'KOUS',[_,_,wenn,'Wie',_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.

%als habe er geschlafen
head('V*FIN',_,l,konjneb,'NEB',[_,_,_,als,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.

%======================================================================================
%subordinated clauses

%allow commas to enclose a subordinated clause
head('NEB','$,',l,comma,'NEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head('NEB','$,',r,comma,'NEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).


%subordinated clause before main clause
head('V*FIN','NEB',l,neb,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- (restrict_vorfeld(FC,OF); member('<-adv<-',OF)).

head('QC','NEB',l,neb,'QC',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- (restrict_vorfeld(FC,OF); member('<-adv<-',OF)).


%subordinated clause after main clause
head('V*FIN','NEB',r,neb,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('V*IMP','NEB',r,neb,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('VVIZU','NEB',r,neb,'VVIZU',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).


%participle construction
head('VVPP','$,',l,comma,'VVPP',[HC,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels), verbchunklength(HC,1).

head('VVPP','$,',r,comma,'VVPP',[HC,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels), verbchunklength(HC,1).

% before main clause
%einige Bedingungen vorausgesetzt, kannst du gerne kommen.
head('V*FIN','VVPP',l,neb,'V*FIN',[FC,DC,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), verbchunklength(DC,1).

%after main clause
%du kannst gerne kommen, einige Bedingungen vorausgesetzt.
head('V*FIN','VVPP',r,neb,'V*FIN',[_,DC,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), verbchunklength(DC,1), \+ member('->aux->', OG).

head('V*IMP','VVPP',r,neb,'V*IMP',[_,DC,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), verbchunklength(DC,1), \+ member('->aux->', OG).

head('VVIZU','VVPP',r,neb,'VVIZU',[_,DC,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), verbchunklength(DC,1), \+ member('->aux->', OG).


%conjunction-less subordinated clause (marked by verb-first word order)
head('V*FIN','$,',r,comma,'NEBCONJLESS',[HC,_,_,_,HeadRels,_,HID,_],_,HM,_,HM) :- stopToLeft(HID), member('mainclause',HC), restrict_vorfeld(HC,HeadRels), \+ member('->comma->', HeadRels).
head('V*FIN','NEBCONJLESS',l,neb,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- (restrict_vorfeld(FC,OF); member('<-adv<-',OF)).

% other direction (conjunction-less clause after main clause) causes too many false positives; left out for now
% head('V*FIN','$,',l,comma,'NEBCONJLESS',[_,_,_,_,_,H-D,_,_],_,HM,_,HM) :- 1 is H-D.
% head('V*FIN','NEBCONJLESS',r,neb,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

%======================================================================================
%clausal object
%dass, ob, indirect questions, quotes.
%TODO: quotes. subjc

%clausal objects. mostly dass
head('V*FIN','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('<-konjneb<-',DepRels).

head('ADJD','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('<-konjneb<-',DepRels).


%consider possibility of tagging error
head('VVPP','KOUS',l,konjobjc,'OBJC',[FC,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- correct_mistagging(yes), verbchunklength(FC,1), \+ member('<-konjneb<-',DepRels).


%clausal object before main clause. Takes the place of the accusative object (except for reflexive ones. Exception could be implemented, but causes new problems)

head('V*FIN','OBJC/SUBJC',l,objc,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('passive',FC), \+ member('<-obja<-',OF), \+ member('->obja->',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF).


%clausal object after main clause
head('V*FIN','OBJC/SUBJC',r,objc,'V*FIN',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :-  \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('V*IMP','OBJC/SUBJC',r,objc,'V*IMP',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :-  \+ member('passive',GC), restrict_coord(OG),  (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).


%clausal object doesn't have to depend on main clause:
head('RC','OBJC/SUBJC',r,objc,'RC',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('NEB','OBJC/SUBJC',r,objc,'NEB',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('VVIZU','OBJC/SUBJC',r,objc,'VVIZU',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).


%er hat nichts dazu gesagt, was passiert ist" ('dazu' or other prepositional adverbs allow obja/objc to co-exist; subclause may alternatively be analysed as depending on adverb, or being adverbial)
head('V*FIN','OBJC/SUBJC',r,objc,'V*FIN',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :-  \+ member('passive',GC), restrict_coord(OG),\+ member('<-objc<-',OG), \+ member('->objc->',OG), adverbial_pronoun(Tag), among_dependents(OG, Tag, 1).

head('V*IMP','OBJC/SUBJC',r,objc,'V*IMP',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :-  \+ member('passive',GC), restrict_coord(OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), adverbial_pronoun(Tag), among_dependents(OG, Tag, 1).

head('RC','OBJC/SUBJC',r,objc,'RC',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), adverbial_pronoun(Tag), among_dependents(OG, Tag, 1).

head('NEB','OBJC/SUBJC',r,objc,'NEB',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), adverbial_pronoun(Tag), among_dependents(OG, Tag, 1).

head('VVIZU','OBJC/SUBJC',r,objc,'VVIZU',[GC,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('passive',GC), restrict_coord(OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), adverbial_pronoun(Tag), among_dependents(OG, Tag, 1).


%die Möglichkeit, dass...
head('NN','OBJC/SUBJC',r,objc,'NN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('<-objc<-',OG), \+ member('->objc->',OG).


%darauf, dass...
head('PAV','OBJC/SUBJC',r,objc,'PAV',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- \+ member('<-objc<-',OG), \+ member('->objc->',OG).

%allow commas to enclose a clausal object
head('OBJC','$,',r,comma,'OBJC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head('OBJC','$,',l,comma,'OBJC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).


%=====================================================================================
%clausal subjects

%clausal subject before main clause. Takes the place of the subject

head('V*FIN','OBJC/SUBJC',l,subjc,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ member('<-subj<-',OF), \+ member('->subj->',OF), \+ member('<-subjc<-',OF), \+ member('->subjc->',OF).

%infinitive clausal subject before main clause 
head('V*FIN','VVIZU_WITH_COMMA',l,subjc,'V*FIN',[FC,GC,_,_,OF,_,_,_],_,MH,_,MH) :- FC \= GC, restrict_vorfeld(FC,OF), \+ member('<-subj<-',OF), \+ member('->subj->',OF), \+ member('<-subjc<-',OF), \+ member('->subjc->',OF).

%clausal subject after main clause
head('V*FIN','OBJC/SUBJC',r,subjc,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).

%infinitive clausal subject after main clause 
head('V*FIN','VVIZU_WITH_COMMA',r,subjc,'V*FIN',[GC,FC,_,_,HeadRels,_,_,_],_,MH,_,MH) :- FC \= GC, restrict_coord(HeadRels), \+ member('<-subj<-',HeadRels), \+ member('->subj->',HeadRels), \+ member('<-subjc<-',HeadRels), \+ member('->subjc->',HeadRels).
head('V*IMP','VVIZU_WITH_COMMA',r,subjc,'V*IMP',[GC,FC,_,_,HeadRels,_,_,_],_,MH,_,MH) :- FC \= GC, restrict_coord(HeadRels), \+ member('<-subj<-',HeadRels), \+ member('->subj->',HeadRels), \+ member('<-subjc<-',HeadRels), \+ member('->subjc->',HeadRels).

%======================================================================================
%infinitive object

%infinitive objects are bound by comma and need the infinitive particle 'zu'.
head('VVIZU','$,',l,comma,'VVIZU_WITH_COMMA',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels), \+ member('->comma->', HeadRels).

%allow (single) comma to right, but only if there's one to the left
head('VVIZU_WITH_COMMA','$,',r,comma,'VVIZU_WITH_COMMA',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head('VVIZU','$,',r,comma,'VVIZU_WITH_COMMA',[_,_,_,_,HeadRels,_,HID,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels), getRange(HID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$.';Tag='$(';sentdelim(Tag);Tag='KON').

%infinitive objects depend on finite verb on their left.
head('V*FIN','VVIZU',r,obji,'V*FIN',[GC,FC,FF,_,HeadRels,DepRels,_,_],_,MH,_,MH) :- FC \= GC, restrict_coord(HeadRels), (FF = sein-> \+ member('<-obja<-',DepRels);true).
head('V*IMP','VVIZU',r,obji,'V*IMP',[GC,FC,_,_,HeadRels,_,_,_],_,MH,_,MH) :- FC \= GC, restrict_coord(HeadRels).

head('V*FIN','VVIZU_WITH_COMMA',r,obji,'V*FIN',[_,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- restrict_coord(HeadRels).
head('V*IMP','VVIZU_WITH_COMMA',r,obji,'V*IMP',[_,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- restrict_coord(HeadRels).
head('VVIZU','VVIZU_WITH_COMMA',r,obji,'VVIZU',[_,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- restrict_coord(HeadRels).

head('V*INF','VVIZU_WITH_COMMA',r,obji,'V*INF',[HC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- in_coordination(HC, HeadRels), restrict_coord(HeadRels).
head('V*PP','VVIZU_WITH_COMMA',r,obji,'V*PP',[HC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- in_coordination(HC, HeadRels), restrict_coord(HeadRels).

%VVIZU to the left of finite verb (topicalized or in subordinated clause)
head('V*FIN','VVIZU',l,obji,'V*FIN',[FC,GC,FF,_,OF,OG,_,_],H-D,MH,_,MH) :- FC \= GC, 1 is H-D, restrict_vorfeld(FC,OF), (FF = sein-> \+ member('<-obja<-',OG);true).
head('V*FIN','VVIZU_WITH_COMMA',l,obji,'V*FIN',[FC,GC,_,_,OF,_,_,_],_,MH,_,MH) :- FC \= GC, restrict_vorfeld(FC,OF).



%Noun can have infinitive object, but should be separated by comma -> competition with other functions of NNs
head('NN','VVIZU_WITH_COMMA',r,obji,'NN',_,_,MH,_,MH).

%froh, etwas tun zu können
head('ADV','VVIZU_WITH_COMMA',r,obji,'ADV',_,_,MH,_,MH).
head('ADJD','VVIZU_WITH_COMMA',r,obji,'ADJD',_,_,MH,_,MH).

%ein nicht enden wollender Krieg
head('ADJA', 'VVINF',l,obji,'ADJA',[_,_,'wollend',_,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.
head('ADJA', 'VVINF',l,obji,'ADJA',[_,_,'wollen',_,_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.

%experimentieren lassen, kommen sehen usw.
%TüBa sometimes gives the tag "aux". We follow Foth in using obji
%currently, no distinction between infinitive and past participle (although obji is misleading for the latter):
%"er bleibt stehen", "er bleibt geschlossen"
head('VVFIN','V*INF/PP',l,obji,'VVFIN',[_,DC,HW,_,_,_,_,_],_,MH,_,MH) :- modallike(HW), verbchunklength(DC,1).
head('VVINF','V*INF/PP',l,obji,'VVINF',[_,DC,HW,_,_,_,_,_],_,MH,_,MH) :- modallike(HW), verbchunklength(DC,1).
head('VVPP','V*INF/PP',l,obji,'VVPP',[_,DC,HW,_,_,_,_,_],_,MH,_,MH) :- modallike(HW), verbchunklength(DC,1).
head('VVIZU','V*INF/PP',l,obji,'VVIZU',[_,DC,HW,_,_,_,_,_],_,MH,_,MH) :- modallike(HW), verbchunklength(DC,1).

head('VVFIN','V*INF/PP',r,obji,'VVFIN',[_,DC,HW,_,_,_,_,_],_,MH,_,MH) :- modallike(HW), verbchunklength(DC,1).


%======================================================================================
%adverbs. needs to be more permissive later on.

%allows adverbs to be enclosed by commas (if they are head of comparative: "er hat, früher als erwartet, den Vertrag unterzeichnet"
head('ADV','$,',l,comma,'ADV',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- member('->kom->',HRels), (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).
head('ADV','$,',r,comma,'ADV',[_,_,_,_,HRels,_,_,_],_,HM,_,HM) :- member('->kom->',HRels), (member('<-comma<-', HRels);member('->kon->', HRels)), \+ member('->comma->', HRels).

head('ADJD','$,',l,comma,'ADJD',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- member('->kom->',HRels), (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).
head('ADJD','$,',r,comma,'ADJD',[_,_,_,_,HRels,_,_,_],_,HM,_,HM) :- member('->kom->',HRels), (member('<-comma<-', HRels);member('->kon->', HRels)), \+ member('->comma->', HRels).

head('PTKNEG','$,',l,comma,'PTKNEG',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- member('->kom->',HRels), (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).
head('PTKNEG','$,',r,comma,'PTKNEG',[_,_,_,_,HRels,_,_,_],_,HM,_,HM) :- member('->kom->',HRels), (member('<-comma<-', HRels);member('->kon->', HRels)), \+ member('->comma->', HRels).


%adverb before finite verb
head('V*FIN','ADV',l,adv,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- (restrict_vorfeld(FC,OF); member('<-neb<-',OF)).

head('V*IMP','ADV',l,adv,'V*IMP',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- (restrict_vorfeld(FC,OF); member('<-neb<-',OF)).

head('VVIZU','ADV',l,adv,'VVIZU',_,_,MH,_,MH).


head('V*FIN','ADJD',l,adv,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).

head('VVIZU','ADJD',l,adv,'VVIZU',_,_,MH,_,MH).


head('V*FIN','PTKNEG',l,adv,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).

head('VVIZU','PTKNEG',l,adv,'VVIZU',_,_,MH,_,MH).


%answer particle. Included because of tagging errors:
%example: das ist ja toll
head('V*FIN','PTKANT',l,adv,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), restrict_vorfeld(FC,OF).

head('VVIZU','PTKANT',l,adv,'VVIZU',_,_,MH,_,MH) :- correct_mistagging(yes).



%weil ich ein wenig schüchtern bin
head('V*FIN','PIS',l,adv,'V*FIN',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF).

head('VVIZU','PIS',l,adv,'VVIZU',_,_,MH,_,MH).



%interrogative adverb (new transtag 'QC')
head('V*FIN','PWAV',l,adv,'QC',[FC,_,_,DWord,OF,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,OF), \+ derive_prep_from_pav(DWord,_).


%only necessary in case of tagging errors
head('VVPP','PWAV',l,adv,'QC',[FC,_,_,DWord,_,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), verbchunklength(FC,1), \+ derive_prep_from_pav(DWord,_).


%"wo" can be relative (der Ort, wo es am schönsten ist), or interrogative (ich frage mich, wo du bist).
head('V*FIN','PWAV',l,adv,'RC',[_,_,_,wo,_,_,_,_],_,MH,_,MH).
head('V*FIN','PRELS',l,adv,'QC',[_,_,_,wo,_,_,_,_],_,MH,_,MH).


%allow adverbs before nonfinite verb in coordination chain
%example/motivation: das kind, 1999 geboren, konnte schon klavier spielen.
head('V*INF','ADV',l,adv,'V*INF',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).
head('V*PP','ADV',l,adv,'V*PP',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).

head('V*INF','ADJD',l,adv,'V*INF',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).
head('V*PP','ADJD',l,adv,'V*PP',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).

head('V*INF','PTKNEG',l,adv,'V*INF',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).
head('V*PP','PTKNEG',l,adv,'V*PP',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- in_coordination(FC,UG).

head('V*INF','PTKANT',l,adv,'V*INF',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), in_coordination(FC,UG).
head('V*PP','PTKANT',l,adv,'V*PP',[FC,_,_,_,UG,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), in_coordination(FC,UG).


%adverb after finite verb
head('V*FIN','ADV',r,adv,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).

head('V*IMP','ADV',r,adv,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).



head('V*FIN','ADJD',r,adv,'V*FIN',[_,_,HWord,_,OG,_,_,DepID],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG), (HWord=sein-> \+ (commaToRight(DepID);stopToRight(DepID));true).

head('V*IMP','ADJD',r,adv,'V*IMP',[_,_,HWord,_,OG,_,_,DepID],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG), (HWord=sein-> \+ (commaToRight(DepID);stopToRight(DepID));true).



head('V*FIN','PTKNEG',r,adv,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).

head('V*IMP','PTKNEG',r,adv,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).


%sie sind alle tot.

head('V*FIN','PIS',r,adv,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).

head('V*IMP','PIS',r,adv,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG), \+ member('->pred->', OG).

head('NN','PIS',r,adv,'NN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('PDS','PIS',r,adv,'PDS',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('PPER','PIS',r,adv,'PPER',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('PRELS','PIS',r,adv,'PRELS',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('PWS','PIS',r,adv,'PWS',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).




%answer particle. Included because of tagging errors:
%example: das ist ja toll
head('V*FIN','PTKANT',r,adv,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), restrict_coord(OG), \+ member('->pred->', OG).

head('V*IMP','PTKANT',r,adv,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- correct_mistagging(yes), restrict_coord(OG), \+ member('->pred->', OG).


%'nonverbal' adverbials: inclusion of tag pairs based on bigram statistics (only tag pairs with high probability of 'adv' included). other possibility: include more/all pairs here and give them lower probability value.


%adverbial particles: "am besten"..
head('ADJD', 'PTKA',l, adv, 'ADJD',_,F-G,MH,_,MH) :- 1 is F-G.

head('ADJA', 'PTKA',l, adv, 'ADJA',_,F-G,MH,_,MH) :- 1 is F-G.

head('ADV', 'PTKA',l, adv, 'ADV',_,F-G,MH,_,MH) :- 1 is F-G.

head('PIAT', 'PTKA',l, adv, 'PIAT',_,F-G,MH,_,MH) :- 1 is F-G.

head('PIDAT', 'PTKA',l, adv, 'PIDAT',_,F-G,MH,_,MH) :- 1 is F-G.

head('PIS', 'PTKA',l, adv, 'PIS',_,F-G,MH,_,MH) :- 1 is F-G.


%'zu' might be mistagged as preposition
head('ADJD', 'APPR',l, adv, 'ADJD',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJA', 'APPR',l, adv, 'ADJA',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADV', 'APPR',l, adv, 'ADV',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIAT', 'APPR',l, adv, 'PIAT',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIDAT', 'APPR',l, adv, 'PIDAT',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIS', 'APPR',l, adv, 'PIS',[_,_,_,zu,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJD', 'APPR',l, adv, 'ADJD',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJA', 'APPR',l, adv, 'ADJA',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADV', 'APPR',l, adv, 'ADV',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIAT', 'APPR',l, adv, 'PIAT',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIDAT', 'APPR',l, adv, 'PIDAT',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('PIS', 'APPR',l, adv, 'PIS',[_,_,_,'Zu',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.


%dealing with mistaggings of 'am' as in 'am besten' (at the moment, pn/pp usually has better prob than adv/adv).
head('ADJD','APPR',l, adv, 'ADJD',[_,_,_,am,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJD','APPR',l, adv, 'ADJD',[_,_,_,'an-der',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJA','APPR',l, adv, 'ADJD',[_,_,_,am,_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

head('ADJA','APPR',l, adv, 'ADJD',[_,_,_,'an-der',_,_,_,_],F-G,MH,_,MH) :- correct_mistagging(yes), 1 is F-G.

%nicht (nur) X, sondern (auch) Y.
head(Tag, 'PTKNEG',l, adv_kon, Tag,[_,_,_,nicht,HeadRels,_,_,_],_,MH,_,MH) :- member('->kon->', HeadRels), among_dependents(HeadRels,'sondern_KON',2).
head(Tag, 'ADV',l, adv_kon, Tag,[_,_,_,nur,HeadRels,_,_,_],_,MH,_,MH) :- member('->kon->', HeadRels), among_dependents(HeadRels,'sondern_KON',2).

%"mehr als x"
head('KOKOM','ADV',l,adv,'KOKOM', _,F-G,MH,_,MH) :- 1 is F-G.


%sehr gut
head('ADJD', 'ADV',l, adv, 'ADJD',_,_,MH,_,MH).

head('ADJA', 'ADV',l, adv, 'ADJA',_,_,MH,_,MH).

head('KOUS', 'ADV',l, adv, 'KOUS',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('->comma->',DepRels).

head('NN', 'ADV',l, adv, 'NN',_,_,MH,_,MH).

head('NE', 'ADV',l, adv, 'NE',_,_,MH,_,MH).

head('APPR', 'ADV',l, adv, 'APPR',_,_,MH,_,MH).

head('APPRART', 'ADV',l, adv, 'APPRART',_,_,MH,_,MH).

head('ADV', 'ADV',l, adv, 'ADV',_,_,MH,_,MH).

head('PTKNEG', 'ADV',l, adv, 'PTKNEG',_,_,MH,_,MH).

head('CARD', 'ADV',l, adv, 'CARD',_,_,MH,_,MH).

head('PDS', 'ADV',l, adv, 'PDS',_,_,MH,_,MH).

head('PIAT', 'ADV',l, adv, 'PIAT',_,_,MH,_,MH).

head('PIDAT', 'ADV',l, adv, 'PIDAT',_,_,MH,_,MH).

head('PIS', 'ADV',l, adv, 'PIS',_,_,MH,_,MH).

head('PPER', 'ADV',l, adv, 'PPER',_,_,MH,_,MH).

head('PAV', 'ADV',l, adv, 'PAV',_,_,MH,_,MH).

% head(Tag, 'ADV', l, adv, Tag,_,_,MH,_,MH).
% head(Tag, 'ADV', r, adv, Tag,_,_,MH,_,MH).



head('ADJA', 'PTKNEG',l, adv, 'ADJA',_,_,MH,_,MH).

head('ADJD', 'PTKNEG',l, adv, 'ADJD',_,_,MH,_,MH).

head('ADV', 'PTKNEG',l, adv, 'ADV',_,_,MH,_,MH).

head('KOUS', 'PTKNEG',l, adv, 'KOUS',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('->comma->',DepRels).

head('CARD', 'PTKNEG',l, adv, 'CARD',_,_,MH,_,MH).

head('NN', 'PTKNEG',l, adv, 'NN',_,_,MH,_,MH).

head('NE', 'PTKNEG',l, adv, 'NE',_,_,MH,_,MH).

head('APPR', 'PTKNEG',l, adv, 'APPR',_,_,MH,_,MH).

head('APPRART', 'PTKNEG',l, adv, 'APPRART',_,_,MH,_,MH).

head('KOKOM','PTKNEG',l,adv,'KOKOM', _,_,MH,_,MH).

head('PDS', 'PTKNEG',l, adv, 'PDS',_,_,MH,_,MH).

head('PIAT', 'PTKNEG',l, adv, 'PIAT',_,_,MH,_,MH).

head('PIDAT', 'PTKNEG',l, adv, 'PIDAT',_,_,MH,_,MH).

head('PIS','PTKNEG',l,adv,'PIS', _,_,MH,_,MH).

head('PPER', 'PTKNEG',l, adv, 'PPER',_,_,MH,_,MH).


%little hack: use PWAV as transtag to make sure whole thing gets recognised as question
head('ADJD', 'PWAV',l, adv, 'PWAV',_,F-G,MH,_,MH) :- 1 is F-G.

head('PIDAT', 'PWAV',l, adv, 'PWAV',_,F-G,MH,_,MH) :- 1 is F-G.


%betriebswirtschaftlich günstig
head('ADJA', 'ADJD',l, adv, 'ADJA',_,_,MH,_,MH).

head('ADJD', 'ADJD',l, adv, 'ADJD',_,F-G,MH,_,MH) :- 1 is F-G.

%most of these are very rare, but there are possibly tagging errors (and not allowing these blocks analysis of non-local dependencies)
head('KOUS', 'ADJD',l, adv, 'KOUS',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('->comma->',DepRels).

head('NN', 'ADJD',l, adv, 'NN',_,_,MH,_,MH).

head('NE', 'ADJD',l, adv, 'NE',_,_,MH,_,MH).

head('APPR', 'ADJD',l, adv, 'APPR',_,_,MH,_,MH).

head('APPRART', 'ADJD',l, adv, 'APPRART',_,_,MH,_,MH).

head('ADV', 'ADJD',l, adv, 'ADV',_,_,MH,_,MH).

head('PTKNEG', 'ADJD',l, adv, 'PTKNEG',_,_,MH,_,MH).

head('CARD', 'ADJD',l, adv, 'CARD',_,_,MH,_,MH).

head('PDS', 'ADJD',l, adv, 'PDS',_,_,MH,_,MH).

head('PIAT', 'ADJD',l, adv, 'PIAT',_,_,MH,_,MH).

head('PIDAT', 'ADJD',l, adv, 'PIDAT',_,_,MH,_,MH).

head('PIS', 'ADJD',l, adv, 'PIS',_,_,MH,_,MH).

head('PPER', 'ADJD',l, adv, 'PPER',_,_,MH,_,MH).

head('PAV', 'ADJD',l, adv, 'PAV',_,_,MH,_,MH).

%seit wann
%little hack: use PWAV as transtag to make sure whole thing gets recognised as question
head('APPR','PWAV',r,adv,'PWAV',_,G-F,MH,_,MH) :- 1 is F-G.


%'seit heute'.
head('APPR','ADV',r,adv,'PP',_,G-F,MH,_,MH) :- 1 is F-G, endOfNP(F).
head('APPRART','ADV',r,adv,'PP',_,G-F,MH,_,MH) :- 1 is F-G, endOfNP(F).


%postmodifying adverbs:
head('NN','ADV',r,adv,'NN',_,_,MH,_,MH).
head('NE','ADV',r,adv,'NE',_,_,MH,_,MH).
head('PP','ADV',r,adv,'PP',_,_,MH,_,MH).
head('PPER','ADV',r,adv,'PPER',_,_,MH,_,MH).
head('PIS','ADV',r,adv,'PIS',_,_,MH,_,MH).
head('PDS','ADV',r,adv,'PDS',_,_,MH,_,MH).
head('PTKNEG', 'ADV',r, adv, 'PTKNEG',_,_,MH,_,MH).
head('PWAV', 'ADV',r, adv, 'PWAV',_,_,MH,_,MH).
head('PWS', 'ADV',r, adv, 'PWS',_,_,MH,_,MH).
head('PAV', 'ADV',r, adv, 'PAV',_,_,MH,_,MH).
head('ADJD','ADV',r,adv,'ADJD',_,_,MH,_,MH).

head('NN','PTKNEG',r,adv,'NN',_,_,MH,_,MH).
head('NE','PTKNEG',r,adv,'NE',_,_,MH,_,MH).
head('PP','PTKNEG',r,adv,'PP',_,_,MH,_,MH).
head('PPER','PTKNEG',r,adv,'PPER',_,_,MH,_,MH).
head('PIS','PTKNEG',r,adv,'PIS',_,_,MH,_,MH).
head('PDS','PTKNEG',r,adv,'PDS',_,_,MH,_,MH).
head('PWAV', 'PTKNEG',r, adv, 'PWAV',_,_,MH,_,MH).
head('PWS', 'PTKNEG',r, adv, 'PWS',_,_,MH,_,MH).
head('ADJD','PTKNEG',r,adv,'ADJD',_,_,MH,_,MH).

%sich selbst
head('PRF', 'ADV',r, adv, 'PRF',[_,_,_,selbst,_,_,_,_],G-F,MH,_,MH) :- 1 is F-G.
head('PRF', 'ADV',r, adv, 'PRF',[_,_,_,allein,_,_,_,_],G-F,MH,_,MH) :- 1 is F-G.


% head(Tag, 'ADV',l, adv, Transtag,[_,_,_,dort,_,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,Transtag).

%======================================================================================
%auxiliary verbs



%rules only work if verb chunking done in preprocessing.

head('VAFIN','V*INF/PP',r,aux,'VAFIN',[Chunk,Chunk,_,_,_,_,_,_],_,MH,_,MH).

head('VMFIN','V*INF/PP',r,aux,'VMFIN',[Chunk,Chunk,_,_,_,_,_,_],_,MH,_,MH).

head('VAFIN','VVIZU',r,aux,'VAFIN',[Chunk,Chunk,_,_,_,OF,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).

head('VMFIN','VVIZU',r,aux,'VMFIN',[Chunk,Chunk,_,_,_,OF,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).



head('VAFIN','V*INF/PP',l,aux,'VAFIN',[Chunk,Chunk,_,_,_,_,_,_],_,MH,_,MH).

head('VMFIN','V*INF/PP',l,aux,'VMFIN',[Chunk,Chunk,_,_,_,_,_,_],_,MH,_,MH).

head('VAFIN','VVIZU',l,aux,'VAFIN',[Chunk,Chunk,_,_,_,OG,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OG).

head('VMFIN','VVIZU',l,aux,'VMFIN',[Chunk,Chunk,_,_,_,OG,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OG).


% FChunk = GChunk (preprocessing) may fail in cases such as "er muss erreichbar sein und *telefonieren können*"
head('V*INF','V*INF/PP',l,aux,'V*INF',[FChunk,GChunk,_,_,_,_,_,_],_,MH,_,MH) :- verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('V*PP','V*INF/PP',l,aux,'V*PP',[FChunk,GChunk,_,_,_,_,_,_],_,MH,_,MH) :- verbchunklength(FChunk,1), verbchunklength(GChunk,1).


%"um geprüft zu werden / um haben zu wollen".
head('VVIZU','V*INF/PP',l,aux,'VVIZU',[Chunk,Chunk,_,_,_,_,_,_],_,MH,_,MH).



%======================================================================================
%verb particles

head('V*FIN','PTKVZ',l,avz,'V*FIN', _,F-G,MH,_,MH) :- 1 is F-G.
head('V*IMP','PTKVZ',l,avz,'V*IMP', _,F-G,MH,_,MH) :- 1 is F-G.

head('V*FIN','PTKVZ',r,avz,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG).
head('V*IMP','PTKVZ',r,avz,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord_and_nachfeld(OG).


%new transtag to simplify other attachment rules. 
head('V*INF','PTKZU',l,part,'VVIZU',_,F-G,MH,_,MH) :- 1 is F-G.


%die zu erwartenden Störmanöver
head('ADJA','PTKZU',l,part,'ADJA',_,F-G,MH,_,MH) :- 1 is F-G.

%might be useful in case of tagging errors
%head('ADJA','APPR',l,part,'ADJA',[_,_,_,'zu',_,_,_,_],F-G,MH,_,MH) :- 1 is F-G.



%======================================================================================
%circumposition particle

%(doesn't exist in exploration corpus).

head('PP','APZR',r,part,'PP',_,_,MH,_,MH).

head('PPQ','APZR',r,part,'PPQ',_,_,MH,_,MH).

head('PPREL','APZR',r,part,'PPREL',_,_,MH,_,MH).

%======================================================================================
%ART + PIS ("ein wenig"; "ein anderer") marked as particle

head('PIS','ART',l,part,'PIS',[_,_,HeadWord,_,_,_,_,_],F-G,MH,_,MH) :- 2 >= F-G, \+ member(HeadWord,['ander','andere','anderen','anderem']).

% "ein anderer" (with agreement constraints)
head('PIS','ART',l,part,'PIS',[_,_,HeadWord,_,_,_,_,_],F-G,MF,MG,MNew) :- 2 >= F-G, member(HeadWord,['ander','andere','anderen','anderem']), check_agreement(MF,'PIS',MG,'ART',MNew).

%======================================================================================
%comparatives



head('KOKOM','NN',r,cj,'KOMPX', _,_,MH,_,MH).

head('KOKOM','NE',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','FM',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','ADJA',r,cj,'KOMPX',[_,_,_,_,_,_,HID,_],_-D,MH,_,MH) :- endOfNP(D); (getRange(HID,From-_), LeftPos is From-1, checkPos(LeftPos,_,Tag,_,_), detcan(Tag,LeftPos)).

head('KOKOM','ADJD',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','PP',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','PIS',r,cj,'KOMPX',_,_,MH,_,MH).


head('KOKOM','PPER',r,cj,'KOMPX',_,_,MH,_,MH).
 
head('KOKOM','PDS',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','ADV',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','PP',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','CARD',r,cj,'KOMPX',_,_,MH,_,MH).

head('KOKOM','VVPP',r,cj,'KOMPX',_,_,MH,_,MH).



%härter als stahl; kraftvoll wie immer
head('ADJA','KOMPX',r,kom,'ADJA',[_,_,_,Komp,_,DepRels,_,_],_,MH,_,MH) :- (Komp=als -> degree_comp(MH,'ADJA');true), \+ member('<-adv<-',DepRels).

head('ADJD','KOMPX',r,kom,'ADJD',[_,_,_,Komp,_,DepRels,_,_],_,MH,_,MH) :- (Komp=als -> degree_comp(MH,'ADJD');true), \+ member('<-adv<-',DepRels).

head('ADV','KOMPX',r,kom,'ADV',[_,_,_,_,_,DepRels,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('<-adv<-',DepRels).


%ein Mann wie stahl
head('NN','KOMPX',r,kom,'NN',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('<-adv<-',DepRels).

head('NE','KOMPX',r,kom,'NE',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('<-adv<-',DepRels).

head('FM','KOMPX',r,kom,'FM',[_,_,_,_,_,DepRels,_,_],_,MH,_,MH) :- \+ member('<-adv<-',DepRels).


%so etwas wie Würde
head('PIS','KOMPX',r,kom,'PIS',[_,_,_,_,_,DepRels,_,_],G-F,MH,_,MH) :- 1 is F-G, \+ member('<-adv<-',DepRels).

%Er als Tierfreund
head('PPER','KOMPX',r,kom,'PPER',[_,_,_,_,_,DepRels,_,_],G-F,MH,_,MH) :- case_nom(MH,'PPER'), 1 is F-G, \+ member('<-adv<-',DepRels).



%sich als Unschuldig ausgeben
head('V*FIN','KOMPX',r,kom,'V*FIN',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).

head('V*IMP','KOMPX',r,kom,'V*IMP',[_,_,_,_,OG,_,_,_],_,MH,_,MH) :- restrict_coord(OG).


%pre-modifying comparatives

%wie immer kraftvoll
head('ADJA','KOMPX',l,kom,'ADJA',[_,_,_,Komp,_,_,_,_],_,MH,_,MH) :- (Komp=als -> (derived_from_ppres(MH,'ADJA');derived_from_ppast(MH,'ADJA'));true).

head('ADJD','KOMPX',l,kom,'ADJD',[_,_,_,Komp,_,_,_,_],_,MH,_,MH) :- (Komp=als -> (derived_from_ppres(MH,'ADJD');derived_from_ppast(MH,'ADJD'));true).

head('PP','KOMPX',l,kom,'PP',_,_,MH,_,MH).


%als nächstes mein Lieblingslied
head('NN','KOMPX',l,kom,'NN',_,_,MH,_,MH).

head('NE','KOMPX',l,kom,'NE',_,_,MH,_,MH).

head('FM','KOMPX',l,kom,'FM',_,_,MH,_,MH).



head('V*FIN','KOMPX',l,kom,'V*FIN',[FC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- restrict_vorfeld(FC,HeadRels).

head('V*INF','KOMPX',l,kom,'V*INF',[FC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- in_coordination(FC,HeadRels), restrict_vorfeld(FC,HeadRels).
head('V*PP','KOMPX',l,kom,'V*PP',[FC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- in_coordination(FC,HeadRels), restrict_vorfeld(FC,HeadRels).
head('VVIZU','KOMPX',l,kom,'VVIZU',[FC,_,_,_,HeadRels,_,_,_],_,MH,_,MH) :- in_coordination(FC,HeadRels), restrict_vorfeld(FC,HeadRels).

%======================================================================================
%conjunction


%(peter) 'und' ->cj-> 'mark'. special: new morphological information is that of dependent, not that of head
head('KON',Tag,r,cj,Transtag,[_,_,HeadWord,_,HeadRels,DepRels,_,_],H-D,_,MD,MD) :- kon_mapping(Tag,Transtag), Transtag \= 'KON_FINVERB', Transtag \= 'KON_ADV', \+ member(HeadWord,['Sowohl',sowohl,weder,'Weder',entweder,'Entweder',als]), restrict_comma_for_kon(Transtag, HeadWord, HeadRels),  (member('->gmod->', DepRels)->1 < D-H;true).

%seltsam, aber nie albern. "albern" should be coordinated adverb, not "nie".
head('KON',Tag,r,cj,'KON_ADV',[_,_,HeadWord,_,_,_,_,_],_-D,_,MD,MD) :- kon_mapping(Tag,'KON_ADV'), RightPos is D+1, checkPos(RightPos,_,Tag2,_,_), (member(Tag,['ADV','ADJD','PTKNEG'])-> \+ member(Tag2,['ADV','ADJD','PTKNEG']);true), \+ member(HeadWord,['Sowohl',sowohl,weder,'Weder',entweder,'Entweder',als]).

% in "Er kommt und sieht Laura", disallow Laura as subject, but not in "Er kommt und dann sieht Laura ihn"
head('KON',Tag,r,cj,'KON_FINVERB',[_,_,HeadWord,_,HeadRels,DepRels,HID,_],H-D,_,MD,MD) :- 1 is D-H, kon_mapping(Tag,'KON_FINVERB'), ((member('<-comma<-', HeadRels);stopToLeft(HID))->true;(\+ member('->subj->', DepRels), \+ member('->subjc->', DepRels))), \+ member(HeadWord,['Sowohl',sowohl,weder,'Weder',entweder,'Entweder',als]).

head('KON',Tag,r,cj,'KON_FINVERB',[_,_,HeadWord,_,_,DepRels,_,_],H-D,_,MD,MD) :- D-H > 1, kon_mapping(Tag,'KON_FINVERB'), \+ member('<-kon<-', DepRels), \+ member('<-s<-', DepRels), \+ member(HeadWord,['Sowohl',sowohl,weder,'Weder',entweder,'Entweder',als]).

%als Babysitter oder als Anwalt arbeiten
head('KON','KOMPX',r,cj,'KON_KOMPX',[_,_,HeadWord,_,_,_,_,_],_,_,MD,MD) :- \+ member(HeadWord,['Sowohl',sowohl,weder,'Weder',entweder,'Entweder',als]).


%allows comma before conjunction.
head('KON', '$,',l, comma, 'KON',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).


%X, Y etc.
head(Tag,'ADV',r,kon,Tag,[_,_,_,'etc.',_,_,_,_],_,_,MF,MF).
head(Tag,'ADV',r,kon,Tag,[_,_,_,'usw.',_,_,_,_],_,_,MF,MF).

%'und so weiter'
head(_,_,l,adv,'CJ',[_,_,weiter,so,_,_,_,_],F-_,MH,_,MH) :- LeftPos is F-1, checkPos(LeftPos,und,_,_,_).
head(_,'CJ',r,cj,'KON_ANY',[_,_,und,weiter,_,_,_,_],_,MH,_,MH).

%'sondern auch'
head(_,_,r,cj,'KON',[_,_,sondern,auch,_,_,_,_],_,MH,_,MH).


%"sowohl als auch" ; "entweder oder"; "weder noch"
head(Tag, 'KON',l, kon, Tag,[_,_,_,sowohl,OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).
head(Tag, 'KON',l, kon, Tag,[_,_,_,'Sowohl',OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).
head(Tag, 'KON',l, kon, Tag,[_,_,_,entweder,OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).
head(Tag, 'KON',l, kon, Tag,[_,_,_,'Entweder',OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).
head(Tag, 'KON',l, kon, Tag,[_,_,_,weder,OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).
head(Tag, 'KON',l, kon, Tag,[_,_,_,'Weder',OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF).

head('KOKOM',Tag,r,cj,Tag,[_,_,als,auch,_,DepRels,_,_],G-F,MH,_,MH) :- 1 is F-G, member('->kon->', DepRels).
head('KON',Tag,r,cj,Tag,[_,_,als,auch,_,DepRels,_,_],G-F,MH,_,MH) :- 1 is F-G, member('->kon->', DepRels).
head('ADV',Tag,r,kon,Transtag,[_,_,auch,_,_,_,_,_],H-_,_,MD,MD) :- LeftPos is H-1, checkPos(LeftPos,als,_,_,_), kon_mapping(Tag,Transtag).



%comma can join two elements if there is a conjunction after them: "ich kam, sah und siegte"
head(Tag,'$,',l,comma,'KON_NOUN',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,'KON_NOUN'), coordinated_element(OF), \+ member('<-comma<-', OF).

head('CARD','$,',l,comma,'KON_CARD',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- coordinated_element(OF), \+ member('<-comma<-', OF).

head('PP','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- coordinated_element(OF), \+ member('<-comma<-', OF).

head('PPREL','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('PPQ','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('ADV','$,',l,comma,'KON_ADV',[_,_,HWord,_,OF,_,_,_],_,MH,_,MH) :- coordinated_element(OF), HWord \= 'aber',  \+ member('<-comma<-', OF).

head('ADJD','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- coordinated_element(OF), \+ member('<-comma<-', OF).

head('PTKNEG','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head('ADJA','$,',l,comma,'KON_ADJA',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- coordinated_element(OF), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PRONOUN',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,'KON_PRONOUN'), coordinated_element(OF), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PRONOUN_REL',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,'KON_PRONOUN_REL'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PPER',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,'KON_PPER'), coordinated_element(OF), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PRF',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- kon_mapping(Tag,'KON_PRF'), coordinated_element(OF), \+ member('<-comma<-', OF).

head('V*FIN','$,',l,comma,'KON_FINVERB',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- member('->kon->', OF), \+ member('<-comma<-', OF), \+ member('->subj->', OF).

head('V*IMP','$,',l,comma,'KON_IMPVERB',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('KOMPX','$,',l,comma,'KON_KOMPX',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('V*PP','$,',l,comma,'KON_PPVERB',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-comma<-', OF).

head('V*INF','$,',l,comma,'KON_INFVERB',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-comma<-', OF).

head('VVIZU','$,',l,comma,'KON_VVIZU',[FC,_,_,_,OF,_,_,_],_,MH,_,MH) :- in_coordination(FC,OF), \+ member('<-comma<-', OF), \+ member('->subj->', OF).


%for some word classes, the same is allowed even if there is no conjunction at the end "der hochgefährliche, giftige baustoff".
head('ADJA','$,',l,comma,'KON_ADJA',[_,_,_,_,OF,_,_,_],_,MH,_,MH) :- \+ member('<-comma<-', OF).


%noun + noun/pronoun
head('NN','KON_PRONOUN',r,kon,'NN', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('NE','KON_PRONOUN',r,kon,'NE', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('FM','KON_PRONOUN',r,kon,'FM', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('NN','KON_PPER',r,kon,'NN', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('NE','KON_PPER',r,kon,'NE', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('FM','KON_PPER',r,kon,'FM', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('NN','KON_PRF',r,kon,'NN', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('NE','KON_PRF',r,kon,'NE', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('FM','KON_PRF',r,kon,'FM', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('NN','KON_NOUN',r,kon,'NN', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('NE','KON_NOUN',r,kon,'NE', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('FM','KON_NOUN',r,kon,'FM', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


%pronoun + noun/pronoun
head('PDS','KON_PRONOUN',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDS',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIS','KON_PRONOUN',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPER','KON_PRONOUN',r,kon,'PPER', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PRF','KON_PRONOUN',r,kon,'PRF', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PRF',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWS','KON_PRONOUN',r,kon,'PWS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWS',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSS','KON_PRONOUN',r,kon,'PPOSS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSS',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


%attributive pronouns: "mein und dein Freund"
head('PWAT','KON_PRONOUN_AT',r,kon,'PWAT', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWAT',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIAT','KON_PRONOUN_AT',r,kon,'PIAT', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIAT',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PDAT','KON_PRONOUN_AT',r,kon,'PDAT', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDAT',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIDAT','KON_PRONOUN_AT',r,kon,'PIDAT', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIDAT',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSAT','KON_PRONOUN_AT',r,kon,'PPOSAT', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSAT',MF,'PDS',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('PRELS','KON_PRONOUN_REL',r,kon,'PRELS',  _,_,MG,MF,MNew) :- unify_case(MG,'PRELS',MF,'PRELS',MNew).

head('PRELAT','KON_PRONOUN_REL',r,kon,'PRELS',  _,_,MG,MF,MNew) :- unify_case(MG,'PRELAT',MF,'PRELS',MNew).


head('PDS','KON_PPER',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDS',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIS','KON_PPER',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPER','KON_PPER',r,kon,'PPER', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PRF','KON_PPER',r,kon,'PRF', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PRF',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWS','KON_PPER',r,kon,'PWS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWS',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWAT','KON_PPER',r,kon,'PWS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWAT',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIAT','KON_PPER',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIAT',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PDAT','KON_PPER',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDAT',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIDAT','KON_PPER',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIDAT',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSS','KON_PPER',r,kon,'PPOSS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSS',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSAT','KON_PPER',r,kon,'PPOSS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSAT',MF,'PPER',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('PDS','KON_PRF',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDS',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIS','KON_PRF',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPER','KON_PRF',r,kon,'PPER', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PRF','KON_PRF',r,kon,'PRF', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PRF',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWS','KON_PRF',r,kon,'PWS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWS',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWAT','KON_PRF',r,kon,'PWS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PWAT',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIAT','KON_PRF',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIAT',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PDAT','KON_PRF',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDAT',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIDAT','KON_PRF',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIDAT',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSS','KON_PRF',r,kon,'PPOSS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSS',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPOSAT','KON_PRF',r,kon,'PPOSS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPOSAT',MF,'PRF',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


head('PDS','KON_NOUN',r,kon,'PDS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PDS',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PIS','KON_NOUN',r,kon,'PIS', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPER','KON_NOUN',r,kon,'PPER', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PRF','KON_NOUN',r,kon,'PRF', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MG,MF,MNew) :- unify_case(MG,'PRF',MF,'NN',MNew), prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


%ein und derselbe Job
head('ART','KON_PRONOUN_AT',r,kon,'ART',  _,_,_,MF,MNew) :- convertMorphList('PIS',MF,'ART',MNew).

%der oder die nächste
head('ART','KON_ART',r,kon,'ART',  _,_,_,MF,MF).


%truncated conjunction. special: morphological information of conjoined object is used.
head('TRUNC','KON_NOUN',r,kon,'NN',  [_,_,_,_,_,_,_,_],_,_,MD,MD).

%jahre- bis jahrzehntelange Haft
head('TRUNC','KON_ADJA',r,kon,'ADJA', [_,_,_,_,_,_,_,_],_,_,MD,MD).

head('TRUNC','KON_ADV',r,kon,'ADJD', [_,_,_,_,_,_,_,_],_,_,MD,MD).

%er ist hin- und hergefahren
head('TRUNC','KON_FINVERB',r,kon,'VVFIN', [_,_,_,_,_,_,_,_],F-G,_,MD,MD) :- 1 is F-G.

head('TRUNC','KON_INFVERB',r,kon,'VVINF', [_,_,_,_,_,_,_,_],F-G,_,MD,MD) :- 1 is F-G.

head('TRUNC','KON_PPVERB',r,kon,'VVPP', [_,_,_,_,_,_,_,_],F-G,_,MD,MD) :- 1 is F-G.

head('TRUNC','KON_VVIZU',r,kon,'VVIZU', [_,_,_,_,_,_,_,_],F-G,_,MD,MD) :- 1 is F-G.



%card + card
head('CARD','KON_CARD',r,kon,'CARD', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).


%adv/adjd/pp/adja + adv/adjd/pp
head('ADV','KON_ADV',r,kon,'ADV', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PWAV','KON_ADV',r,kon,'PWAV', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('ADJD','KON_ADV',r,kon,'ADJD', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PTKNEG','KON_ADV',r,kon,'PTKNEG', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PP','KON_ADV',r,kon,'PP', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPQ','KON_ADV',r,kon,'PPQ', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('PPREL','KON_ADV',r,kon,'PPREL', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

head('ADJA','KON_ADV',r,kon,'ADJA', [_,_,_,_,HeadRels,DepRels,_,DepID],_,MH,_,MH) :- prevent_clause_crossing_kon(HeadRels, DepRels, DepID).

%für und mit X
head('APPR','KON_ADV',r,kon,'PP',  _,_,MH,_,MH).

head('APPRART','KON_ADV',r,kon,'PP',  _,_,MH,_,MH).


%adjd/adja + /adja
head('ADJD','KON_ADJA',r,kon,'ADJD',  _,_,MH,_,MH).

head('ADJA','KON_ADJA',r,kon,'ADJA',  _,_,MH,_,MH).


%kokom + kokom
head('KOMPX','KON_KOMPX',r,kon,'KOMPX',  _,_,MH,_,MH).


%v*fin + v*fin
head('V*FIN','KON_FINVERB',r,kon,'V*FIN',  _,_,MH,_,MH).



%v*pp + v*pp
head('V*PP','KON_PPVERB',r,kon,'V*PP',  _,_,MH,_,MH).




%v*inf + v*inf
head('V*INF','KON_INFVERB',r,kon,'V*INF',  _,_,MH,_,MH) .

head('VVIZU','KON_VVIZU',r,kon,'VVIZU',_,_,MH,_,MH).


%vvimp + vvimp
head('V*IMP','KON_IMPVERB',r,kon,'V*IMP',  _,_,MH,_,MH).

%coordinated subclauses
head('RC','KON_RC',r,kon,'RC',  _,_,MH,_,MH).

head('QC','KON_QC',r,kon,'QC',  _,_,MH,_,MH).

head('OBJC','KON_OBJC',r,kon,'OBJC',  _,_,MH,_,MH).

head('NEB','KON_NEB',r,kon,'NEB',  _,_,MH,_,MH).


% mix of relative and interrogative pronouns indicates thate the whole thing is relative:
% "ein Standard, der akzeptiert wird und womit man die Qualität überprüfen kann."
head('RC','KON_QC',r,kon,'RC',  _,_,MH,_,MH).
head('QC','KON_RC',r,kon,'RC',  _,_,MH,_,MH).

%X, Y und so weiter
head(Tag,'KON_ANY',r,kon,Tag,  _,_,MH,_,MH).


%Der 9./10. Mai
head(_,'$(',l,bracket,'KON_ANY',  [_,_,_,'/',_,_,_,_],F-G,HM,_,HM) :- 1 is F-G.
head('CARD','$(',l,bracket,'KON_CARD',  [_,_,_,Bracket,_,_,_,_],F-G,HM,_,HM) :- 1 is F-G, rightbracket(Bracket,'21').

%Die Gross- / Kleinschreibung
head('TRUNC','KON_ANY',r,kon,'NN',  [_,_,_,_,_,_,_,_],_,_,MD,MD).

%two clauses that are not in a subordinated relationship can belong together.

head('V*FIN','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],H-D,HM,_,HM) :- \+ member('<-comma<-', HeadRels), (1 is H-D->(\+ member('->subj->', HeadRels), \+ member('->subjc->', HeadRels));true).

head('V*IMP','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],H-D,HM,_,HM) :- \+ member('<-comma<-', HeadRels), (1 is H-D->(\+ member('->subj->', HeadRels), \+ member('->subjc->', HeadRels));true).

%v*fin + v*fin: doesn't require conjunction.
head('V*FIN','KONC',r,konc,'V*FIN',[GC,FC,_,_,_,_,_,_],_,MH,_,MH) :- member('mainclause',FC),member('mainclause',GC).

head('V*IMP','KONC',r,konc,'V*IMP',[GC,FC,_,_,_,_,_,_],_,MH,_,MH) :- member('mainclause',FC),member('mainclause',GC).




%what looks like loose apposition could also be part of coordination chain (without conjunction). (Peter, Susi, Mark)
%strategy: label two elements as apposition, three or more as coordination
head('NN','APP',r,kon,'NN',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'NN',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->kon->', HRels), \+ member('->app_loose->', HRels), \+ member('->app_loose->', DRels).

head('NE','APP',r,kon,'NE',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'NE',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->kon->', HRels), \+ member('->app_loose->', HRels), \+ member('->app_loose->', DRels).

head('FM','APP',r,kon,'FM',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'FM',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->kon->', HRels), \+ member('->app_loose->', HRels), \+ member('->app_loose->', DRels).

head('PIS','APP',r,kon,'PIS',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'PIS',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->kon->', HRels), \+ member('->app_loose->', HRels), \+ member('->app_loose->', DRels).

head('PPER','APP',r,kon,'PPER',[_,_,_,_,HRels,DRels,_,_],_,MG,MF,MNew) :- unify_case(MG,'PPER',MF,'NN',MNew), \+ member('->bracket->', HRels), \+ member('->kon->', HRels), \+ member('->app_loose->', HRels), \+ member('->app_loose->', DRels).


% Er ist gegen Gewalt, gegen Krieg (repetition of preposition indicates that they are coordinated)
head('PP','PP',r,kon,'PP',[_,_,HWord,DWord,_,DRels,_,_],_,HeadMorph,_,HeadMorph) :- member('<-comma<-', DRels), splitappr(HWord,Word,_), splitappr(DWord,Word,_).

% Von A nach B.
head('PP','PP',r,kon,'PP',[_,_,HWord,DWord,HRels,_,_,_],_,HeadMorph,_,HeadMorph) :- splitappr(HWord,HWordBare,_), splitappr(DWord,DWordBare,_), valid_pp_coord_start(HWordBare), valid_pp_coord_end(DWordBare), \+ member('->kon->', HRels).


kon_mapping('NN','KON_NOUN') :- !.
kon_mapping('FM','KON_NOUN') :- !.
kon_mapping('NE','KON_NOUN') :- !.

kon_mapping('CARD','KON_CARD') :- !.

kon_mapping('PP','KON_ADV') :- !.
kon_mapping('PPREL','KON_ADV') :- !.
kon_mapping('PPQ','KON_ADV') :- !.
kon_mapping('PAV','KON_ADV') :- !.
kon_mapping('ADV','KON_ADV') :- !.
kon_mapping('ADJD','KON_ADV') :- !.
kon_mapping('PWAV','KON_ADV') :- !.

kon_mapping('ADJA','KON_ADJA') :- !.

%needs its own class because of different morphology style
kon_mapping('PPER','KON_PPER') :- !.
kon_mapping('PRF','KON_PRF') :- !.


kon_mapping('PDS','KON_PRONOUN') :- !.
kon_mapping('PIS','KON_PRONOUN') :- !.
kon_mapping('PWS','KON_PRONOUN') :- !.
kon_mapping('PPOSS','KON_PRONOUN') :- !.

kon_mapping('PWAT','KON_PRONOUN_AT') :- !.
kon_mapping('PIAT','KON_PRONOUN_AT') :- !.
kon_mapping('PDAT','KON_PRONOUN_AT') :- !.
kon_mapping('PIDAT','KON_PRONOUN_AT') :- !.
kon_mapping('PPOSAT','KON_PRONOUN_AT') :- !.

kon_mapping('ART','KON_ART') :- !.

kon_mapping('PRELS','KON_PRONOUN_REL') :- !.
kon_mapping('PRELAT','KON_PRONOUN_REL') :- !.

kon_mapping('VVFIN','KON_FINVERB') :- !.
kon_mapping('VMFIN','KON_FINVERB') :- !.
kon_mapping('VAFIN','KON_FINVERB') :- !.

kon_mapping('VVIMP','KON_IMPVERB') :- !.
kon_mapping('VAIMP','KON_IMPVERB') :- !.

kon_mapping('VVPP','KON_PPVERB') :- !.
kon_mapping('VMPP','KON_PPVERB') :- !.
kon_mapping('VAPP','KON_PPVERB') :- !.

kon_mapping('VVINF','KON_INFVERB') :- !.
kon_mapping('VMINF','KON_INFVERB') :- !.
kon_mapping('VAINF','KON_INFVERB') :- !.

kon_mapping('VVIZU','KON_VVIZU') :- !.

kon_mapping('RC','KON_RC') :- !.
kon_mapping('QC','KON_QC') :- !.
kon_mapping('NEB','KON_NEB') :- !.
kon_mapping('OBJC','KON_OBJC') :- !.


%make sure NPs (or similar) are not coordinated with NP in a subclause (by requiring end of subclause to be marked with comma).
prevent_clause_crossing_kon(HeadRels, DepRels, DepID) :- (subclauseToRight(HeadRels)->(commaToLeft(DepID); member('<-comma<-',DepRels));true).

subclauseToRight(HeadRels) :- intersection(['->objc->',
                                            '->obji->',
                                            '->subjc->',
                                            '->rel->',
                                            '->neb->'],HeadRels, Intersection), Intersection \= [].

%======================================================================================
%quotes/(in)direct speech: tag 's'

%quotes are separated from the main clause by a comma (full stops will initialize a new sentence in the parser, so they're not recognized anyway).
head('V*FIN','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head('V*IMP','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).



head('V*FIN','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head('V*IMP','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).


%normally, verbs depend on coordination; to properly analyse a verb in a main clause as speech, invert relationship.
% und das ist wahr, sagte er.
head('QUOTE','KON',l,koord,'QUOTE',[_,_,_,_,_,_,_,DepID],_,HM,_,HM) :- stopToLeft(DepID).

%v*fin + v*fin: head word needs to be word of speech (sagen, meinen, bekräftigen...) --> statisics module

%quote after head clause
head('V*FIN','QUOTE',r,s,'V*FIN',[GC,FC,_,_,OG,DepRels,_,_],_,MH,_,MH) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',DepRels), \+ member('<-subjc<-',DepRels), \+ member('<-s<-',DepRels), (member('passive',GC)->(\+ member('<-subj<-',OG), \+ member('->subj->',OG));true).

head('V*IMP','QUOTE',r,s,'V*IMP',[GC,FC,_,_,OG,DepRels,_,_],_,MH,_,MH) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',DepRels), \+ member('<-subjc<-',DepRels), \+ member('<-s<-',DepRels), (member('passive',GC)->(\+ member('<-subj<-',OG), \+ member('->subj->',OG));true).


%quote before head clause -> we want a subject to make sure it isn't something like "A tat B, sagte aber C"
head('V*FIN','QUOTE',l,s,'V*FIN',[FC,GC,_,_,OF,_,_,_],_,MH,_,MH) :- member('mainclause',FC),member('mainclause',GC), restrict_vorfeld(FC,OF), member('->subj->',OF), \+ member('->s->',OF), \+ member('<-s<-',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF), \+ member('<-obja<-',OF), \+ member('->obja->',OF),\+ member('->kon->',OF), (member('passive',FC)->(\+ member('<-subj<-',OF), \+ member('->subj->',OF));true).


%======================================================================================
%parenthetical structures: tag 'par'

head('QUOTE','$,',r,comma,'PAR',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HeadRels).

head('PPNEB','$,',r,comma,'PPNEBX',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).
head('PPNEBX','$,',l,comma,'PPNEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('->comma->', HeadRels).

head('KOMPXWITHCOMMATOLEFT','$,',r,comma,'KOMPXWITHCOMMA',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HeadRels), \+  member('->comma->', HeadRels).

head('KOMPX','$,',l,comma,'KOMPXWITHCOMMA',[_,_,_,_,HeadRels,_,_,DepID],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels), stopToRight(DepID).
head('KOMPX','$,',l,comma,'KOMPXWITHCOMMATOLEFT',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head('QUOTE','$,',r,comma,'PARSO',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HeadRels), \+ member('->comma->', HeadRels), nth1(RelPos, HeadRels, '<-adv<-'), AdvPos is RelPos-1, nth1(AdvPos, HeadRels, AdvStruct), AdvStruct =.. [Adv,_], atom_concat(so,_,Adv).

%quotes on both sides: "Er sei", sagte der Angeklagte, "völlig unschuldig."
head(Tag,'PAR',r,par,Tag,[_,FD,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F), member('mainclause',FD), restrict_coord(DepRels), \+ member('->s->',DepRels), \+ member('<-s<-',DepRels), \+ member('<-objc<-',DepRels), \+ member('->objc->',DepRels), \+ member('<-obja<-',DepRels), \+ member('->obja->',DepRels).

%quotes on both sides: "Ich bin hier", sagte der Angeklagte, "sie nicht."
head(Tag,'QUOTE',r,par,Tag,[_,FD,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-To), lastlexical(From,F), RightPos is To + 1, checkPos(RightPos,_,'$,',_,_), member('mainclause',FD), restrict_coord(DepRels), \+ member('->s->',DepRels), \+ member('<-s<-',DepRels), \+ member('<-objc<-',DepRels), \+ member('->objc->',DepRels), \+ member('<-obja<-',DepRels), \+ member('->obja->',DepRels).

%"Verantwortlich, so Peter, ist Hans"
head(Tag,'APP',r,par,Tag,[_,_,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F), member('<-comma<-', DepRels), member('->comma->', DepRels), nth1(RelPos, DepRels, '<-adv<-'), AdvPos is RelPos-1, nth1(AdvPos, DepRels, AdvStruct), AdvStruct =.. [Adv,_], atom_concat(so,_,Adv).
%"Verantwortlich, so scheint es, ist Hans"
head(Tag,'PARSO',r,par,Tag,[_,_,_,_,_,_,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F).


%"Ich bin - wie versprochen - gekommen."
head(Tag,'PPNEB',r,par,Tag,[_,_,_,_,_,_,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F).
head(Tag,'VVPP',r,par,Tag,[_,DC,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- verbchunklength(DC,1), length(DepRels,DepLen), LastRelPos is DepLen-1, ((nth1(LastRelPos, DepRels,'->bracket->'), nth1(1, DepRels, '<-bracket<-'));(nth1(LastRelPos, DepRels,'->comma->'), nth1(1, DepRels, '<-comma<-'))), getRange(DID,From-_), lastlexical(From,F).


head(Tag,'KOMPXWITHCOMMA',r,par,Tag,[_,_,_,_,_,_,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F).

%expressions in parentheses (like this one). pretty complicated rule, since we do not want all brackets (we exclude quotation marks, for instance)
head(Tag,_Tag2,r,par,Tag,[_,_,_,_,_,[StartBracket,'<-bracket<-'|DepRels],_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F), append(_,['->bracket->',EndBracket],DepRels), StartBracket =.. [_,[X]], EndBracket =.. [_,[Y]], splittag(X,Word,'$('), splittag(Y,Word2,'$('), leftbracket(Word,Class),rightbracket(Word2,Class), atom_number(Class, Int), Int > 10.

%don't attach parenthetical expressions to commas or parentheses, but to the last word before that.
lastlexical(From,Last) :- LeftPos is From - 1,
			  checkPos(LeftPos,_,Tag,_,_),
			  ((Tag='$(';Tag='$,')->lastlexical(LeftPos,Last); LeftPos=Last).


%======================================================================================
%'zeit' - temporal modifiers - only allow cardinal numbers for now (90% of all cases), noun phrases are theoretically possible (letzten dienstag ging er nach hause) - lexical disambiguation?

%'zeit' before verb
head('V*FIN','CARD',l,zeit,'V*FIN',[FC,_,_,DWord,UG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), restrict_vorfeld(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('V*INF', 'CARD',l,zeit,'V*INF',[FC,_,_,DWord,UG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('V*PP', 'CARD',l,zeit,'V*PP',[FC,_,_,DWord,UG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'CARD',l,zeit,'VVIZU',[FC,_,_,DWord,UG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

%Letztes Jahr regnete es.
head('V*FIN','NN',l,zeit,'V*FIN',[FC,_,_,Lemma,UG,_,_,_],_,MF,MG,MF) :- zeitcand(Lemma), restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('V*INF', 'NN',l,zeit,'V*INF',[FC,_,_,Lemma,UG,_,_,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('V*PP', 'NN',l,zeit,'V*PP',[FC,_,_,Lemma,UG,_,_,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NN',l,zeit,'VVIZU',[FC,_,_,Lemma,UG,_,_,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).


%Eines Tages regnete es.
head('V*FIN','NN',l,zeit,'V*FIN',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], restrict_vorfeld(FC,UG), case_gen(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('V*INF', 'NN',l,zeit,'V*INF',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('V*PP', 'NN',l,zeit,'V*PP',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NN',l,zeit,'VVIZU',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), in_coordination(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).


%Anfang Oktober regnete es.
head('V*FIN','NZEIT',l,zeit,'V*FIN',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('V*INF', 'NZEIT',l,zeit,'V*INF',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- in_coordination(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('V*PP', 'NZEIT',l,zeit,'V*PP',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- in_coordination(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NZEIT',l,zeit,'VVIZU',[FC,_,_,_,UG,_,_,_],_,MF,MG,MF) :- in_coordination(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

%der 1995 verstorbene Künstler
head('ADJA', 'CARD',l,zeit,'ADJA',[_,_,_,Lemma,UG,_,_,_],_,MH,_,MH) :-  (derived_from_ppres(MH,'ADJA');derived_from_ppast(MH,'ADJA')), card_is_zeit_cand(Lemma), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('ADJA', 'NN',l,zeit,'ADJA',[_,_,_,Lemma,UG,_,_,_],_,MF,MG,MF) :- (derived_from_ppres(MF,'ADJA');derived_from_ppast(MF,'ADJA')), zeitcand(Lemma), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('ADJA', 'NZEIT',l,zeit,'ADJA',[_,_,_,_,UG,_,_,_],_,MF,MG,MF) :-  (derived_from_ppres(MF,'ADJA');derived_from_ppast(MF,'ADJA')), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).


%'zeit' after verb
head('V*FIN','CARD',r,zeit,'V*FIN',[_,_,_,DWord,OG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).

head('V*IMP','CARD',r,zeit,'V*IMP',[_,_,_,DWord,OG,_,_,_],_,MH,_,MH) :- card_is_zeit_cand(DWord), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).


head('V*FIN','NN',r,zeit,'V*FIN',[_,_,_,Lemma,OG,_,_,_],_,MG,MF,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).

head('V*IMP','NN',r,zeit,'V*IMP',[_,_,_,Lemma,OG,_,_,_],_,MG,MF,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).


head('V*FIN','NZEIT',r,zeit,'V*FIN',[_,_,_,_,OG,_,_,_],_,MG,MF,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).

head('V*IMP','NZEIT',r,zeit,'V*IMP',[_,_,_,_,OG,_,_,_],_,MG,MF,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord_and_nachfeld(OG).


head('V*FIN', 'NN',r,zeit,'V*FIN',[_,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), restrict_coord_and_nachfeld(UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('V*INF', 'NN',r,zeit,'V*INF',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('V*PP', 'NN',r,zeit,'V*PP',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NN',r,zeit,'VVIZU',[FC,_,_,Lemma,UG,[DET,Rel|_],_,_],_,MF,MG,MF) :- zeitgen(Lemma, Chunk, Rel), DET =.. [_,[Chunk]], case_gen(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

%allow four-digit numbers ("1985"), but not other numbers ("13") to have temporal function.
card_is_zeit_cand(Word) :- atom_length(Word,4), name(Word,X), name(Num,X), number(Num).

%======================================================================================
%'grad'

%fünf jahre alt
head('ADJA','NN',l,grad,'ADJA',[_,_,_,DWord,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).

head('ADJD','NN',l,grad,'ADJD',[_,_,_,DWord,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).

head('PIAT','NN',l,grad,'PIAT',[_,_,_,DWord,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).

head('PP','NN',l,grad,'PP',[_,_,_,DWord,_,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), 1 is HPos-DPos, (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).

head('PAV','NN',l,grad,'PAV',[_,_,_,DWord,_,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), 1 is HPos-DPos, (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).

head('ADV','NN',l,grad,'ADV',[_,_,_,DWord,_,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), 1 is HPos-DPos, (among_dependents(DRels, '_CARD', 1);measure(DWord);zeitcand(DWord)).


%ein wenig zu alt
head('ADJA','PIS',l,grad,'ADJA',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head('ADJD','PIS',l,grad,'ADJD',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head('PIAT','PIS',l,grad,'PIAT',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head('PP','PIS',l,grad,'PP',[_,_,_,_,_,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), 1 is HPos-DPos.

head('PAV','PIS',l,grad,'PAV',[_,_,_,_,_,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), 1 is HPos-DPos.

head('ADV','PIS',l,grad,'ADV',[_,_,_,_,_,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), 1 is HPos-DPos.


%voller Liebe
head('ADJD','NN',r,grad,'ADJD',[_,_,voll,_,_,_,_,_],_,HMorph,_,HMorph).
head('ADJD','NN',r,grad,'ADJD',[_,_,voller,_,_,_,_,_],_,HMorph,_,HMorph).


%======================================================================================
%vocative

% sentence-initial vocative: "Peter, das ist nicht lustig!"
head('NE','$,',r,comma,'VOK',[_,_,_,_,HeadRels,_,HeadID,_],_,HMorph,_,HMorph) :- stopToLeft(HeadID), \+ member('->comma->', HeadRels).
head('NN','$,',r,comma,'VOK',[_,_,_,_,HeadRels,_,HeadID,_],_,HMorph,_,HMorph) :- stopToLeft(HeadID), \+ member('->comma->', HeadRels).

head('V*FIN','VOK',l,vok,'V*FIN',[_,_,_,_,_,DepRels,_,_],_,HMorph,_,HMorph) :- \+ member('<-adv<-', DepRels), \+ member('<-adv_kon<-', DepRels), \+ member('<-pp<-', DepRels), \+ member('<-bad_det<-', DepRels), \+ member('<-det<-', DepRels).

% sentence-final vocative: "Das ist nicht lustig, Peter!"
head('NE','$,',l,comma,'VOK',[_,_,_,_,HeadRels,_,HeadID,_],_,HMorph,_,HMorph) :- stopToRight(HeadID), \+ member('<-comma<-', HeadRels), \+ member('<-bad_det<-', HeadRels), \+ member('<-det<-', HeadRels), \+ member('<-adv<-', HeadRels), \+ member('<-adv_kon<-', HeadRels), \+ member('<-pp<-', HeadRels).
head('NN','$,',l,comma,'VOK',[_,_,_,_,HeadRels,_,HeadID,_],_,HMorph,_,HMorph) :- stopToRight(HeadID), \+ member('<-comma<-', HeadRels), \+ member('<-bad_det<-', HeadRels), \+ member('<-det<-', HeadRels), \+ member('<-adv<-', HeadRels), \+ member('<-adv_kon<-', HeadRels), \+ member('<-pp<-', HeadRels).

head('V*FIN','VOK',r,vok,'V*FIN',[_,_,_,_,_,_,_,_],_,HMorph,_,HMorph).



%======================================================================================
%brackets can enclose any structure


%different opening and closing brackets
head(Tag,'$(',l,bracket,NewTag,[_,_,_,Lex,_,_,HID,_],_,HM,_,HM) :- leftbracket(Lex,Class), bracketToRight(HID,Lex2), rightbracket(Lex2,Class), atom_concat('BRACKET',Class,TempTag), atom_concat(TempTag,Tag,NewTag).

head(OldTag,'$(',r,bracket,Tag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- rightbracket(Lex,Class), atom_concat('BRACKET',Class,TempTag), atom_concat(TempTag,Tag,OldTag).

head(Tag,'$(',l,bracket,NewTag,[_,_,_,Lex,_,_,HID,_],_,HM,_,HM) :- bracketToRight(HID,Lex), atom_concat('BRACKET2',Tag,NewTag), \+ leftbracket(Lex,_), \+ rightbracket(Lex,_).

head(OldTag,'$(',r,bracket,Tag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- atom_concat('BRACKET2',Tag,OldTag), \+ leftbracket(Lex,_), \+ rightbracket(Lex,_).



leftbracket('«','1') :- !.
leftbracket('»','2') :- !.
leftbracket('„','3') :- !.
leftbracket('"','4') :- !.
leftbracket('\'','5') :- !. %'
leftbracket('(','11') :- !.
leftbracket('[','12') :- !.
leftbracket('{','13') :- !.

%minus signs, hypens and dashes
%for syntactic analysis, we don't differentiate between them, since their use may be inconsistent
leftbracket('-','21') :- !.
leftbracket('‐​','21') :- !.
leftbracket('‑','21') :- !.
leftbracket('‒','21') :- !.
leftbracket('–','21') :- !.
leftbracket('—','21') :- !.
leftbracket('―','21') :- !.

%unknown characters
leftbracket(_,'0') :- !.

rightbracket('»','1') :- !.
rightbracket('«','2') :- !.
rightbracket('“','3') :- !.
rightbracket('"','4') :- !.
rightbracket('\'','5') :- !. %'
rightbracket(')','11') :- !.
rightbracket(']','12') :- !.
rightbracket('}','13') :- !.

rightbracket('-','21') :- !.
rightbracket('‐​','21') :- !.
rightbracket('‑','21') :- !.
rightbracket('‒','21') :- !.
rightbracket('–','21') :- !.
rightbracket('—','21') :- !.
rightbracket('―','21') :- !.

rightbracket(_,'0') :- !.

%if hyphen/dash is at a clause boundary, don't require them to be paired.
head('KOUI','$(',l,badbracket,'KOUI',[_,_,_,Lex,_,_,_,_],F-G,HM,_,HM) :- 1 is F-G, leftbracket(Lex,'21').
head('KOUS','$(',l,badbracket,'KOUS',[_,_,_,Lex,_,_,_,_],F-G,HM,_,HM) :- 1 is F-G, leftbracket(Lex,'21').
head('KON','$(',l,badbracket,'KON',[_,_,_,Lex,_,_,_,_],F-G,HM,_,HM) :- 1 is F-G, leftbracket(Lex,'21').
head('V*FIN','$(',l,badbracket,'KONC',[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- leftbracket(Lex,'21').

head(Tag,'$(',r,badbracket,Tag,[_,_,_,Lex,_,_,_,_],F-G,HM,_,HM) :- -1 is F-G, member(Lex,['"', '\'']). %'

%PP may be sentence-final and separated from its head by a dash
head(Tag,'$(',l,badbracket,Tag,[_,_,_,Lex,_,_,HID,_],_,HM,_,HM) :- leftbracket(Lex,'21'), member(Tag,['PP']), stopToRight(HID).


%======================================================================================
% Deal with non-words (tag XY)

% this means that non-words will be skipped over
head(Tag,'XY',r,unknown,Tag,[_,_,_,_,_,_,_,_],F-G,HM,_,HM) :- -1 is F-G.

% Correctly parse some abbreviated units of measurement, even if they are unknown to tagger.
head('XY','CARD',l,attr,'NN',[_,_,HW,_,_,_,_,_],F-G,HM,_,HM) :- member(HW,[m,cm,km,mm,ft,h,min,s,g,kg,mg]), 1 is F-G.


% attach interjections (will be removed in postprocessing)
head(Tag,'ITJ',r,unknown,Tag,[_,_,_,_,_,_,_,_],F-G,HM,_,HM) :- -1 is F-G.


%======================================================================================
%catchall included for backwards compatibility

%======================================================================================
%tag transformations; allows us to map different tags to one for rule-lookup, which makes rules more maintainable.

head('VAINF', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*INF', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*INF'->Transtag2='VAINF';Transtag2=Transtag).
head('VVINF', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*INF', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*INF'->Transtag2='VVINF';Transtag2=Transtag).
head('VMINF', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*INF', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*INF'->Transtag2='VMINF';Transtag2=Transtag).

head('VAPP', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*PP', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*PP'->Transtag2='VAPP';Transtag2=Transtag).
head('VVPP', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*PP', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*PP'->Transtag2='VVPP';Transtag2=Transtag).
head('VMPP', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*PP', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*PP'->Transtag2='VMPP';Transtag2=Transtag).


head('VAFIN', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*FIN', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*FIN'->Transtag2='VAFIN';Transtag2=Transtag).
head('VVFIN', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*FIN', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*FIN'->Transtag2='VVFIN';Transtag2=Transtag).
head('VMFIN', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*FIN', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*FIN'->Transtag2='VMFIN';Transtag2=Transtag).

head('VAIMP', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*IMP', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*IMP'->Transtag2='VAIMP';Transtag2=Transtag).
head('VVIMP', OBJ,Dir,Rel,Transtag2,L,D,MA,MB,MC) :- head('V*IMP', OBJ,Dir,Rel,Transtag,L,D,MA,MB,MC), (Transtag='V*IMP'->Transtag2='VVIMP';Transtag2=Transtag).

head(Tag, 'VVINF',Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'VVPP' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'VAINF',Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'VAPP' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'VMINF',Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'VMPP' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'V*INF/PP',Dir,Rel,Transtag,L,D,MA,MB,MC).


%there is some inconsistency concerning the tag for pronominal adverbs. This maps 'PROP' and 'PROAV' to 'PAV' internally (doesn't affect output)
head(Tag, 'PROP' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'PAV',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'PROAV',Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'PAV',Dir,Rel,Transtag,L,D,MA,MB,MC).

head('PROP', Tag ,Dir,Rel,'PAV',L,D,MA,MB,MC) :- head('PAV',Tag,Dir,Rel,'PAV',L,D,MA,MB,MC).
head('PROAV', Tag,Dir,Rel,'PAV',L,D,MA,MB,MC) :- head('PAV',Tag,Dir,Rel,'PAV',L,D,MA,MB,MC).

%tags that can be clausal subject/object
head(Tag, 'OBJC' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'OBJC/SUBJC',Dir,Rel,Transtag,L,D,MA,MB,MC).
head(Tag, 'QC' ,Dir,Rel,Transtag,L,D,MA,MB,MC) :- head(Tag, 'OBJC/SUBJC',Dir,Rel,Transtag,L,D,MA,MB,MC).

%======================================================================================
%morphological rules

%is used to distinguish between different morphology layouts in gertwol

%Gen,Case,Num,?
morph_noun('FM').
morph_noun('NN').
morph_noun('NE').


%Gen,Case,Num
morph_pronoun('PIS').
morph_pronoun('PIAT').
morph_pronoun('PDS').
morph_pronoun('PDAT').
morph_pronoun('PIDAT').
morph_pronoun('PPOSS').
morph_pronoun('PPOSAT').
morph_pronoun('PRELS').
morph_pronoun('PRELAT').
morph_pronoun('PWS').
morph_pronoun('PWAT').


morph_finverb('VVFIN').
morph_finverb('VAFIN').
morph_finverb('VMFIN').


%if there is no morphological information available, don't unify anything.

case_nom([A],_) :- var(A), !.
case_acc([A],_) :- var(A), !.
case_dat([A],_) :- var(A), !.
case_gen([A],_) :- var(A), !.

case_nom([[A]],_) :- var(A), !.
case_acc([[A]],_) :- var(A), !.
case_dat([[A]],_) :- var(A), !.
case_gen([[A]],_) :- var(A), !.

gender_neut([A],_) :- var(A), !.
gender_neut([[A]],_) :- var(A), !.

%case identifiers for tueba-style morphology
:- if(morphology(tueba)).
case_nom(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'n',tueba), !.
case_acc(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'a',tueba), !.
case_dat(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'d',tueba), !.
case_gen(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'g',tueba), !.

gender_neut(List,Tag) :- member(Morph,List), get_gender(Morph,Tag,'n',tueba), !.

degree_comp(_,_) :- !.

derived_from_ppres(_,_) :- !.
derived_from_ppast(_,_) :- !.
derived_from_vpart(_,_) :- !.

:- endif.


%case identifiers for gertwol-style morphology
:- if(morphology(gertwol)).
case_nom(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'Nom',gertwol), !.
case_acc(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'Akk',gertwol), !.
case_acc(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'Acc',gertwol), !.
case_dat(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'Dat',gertwol), !.
case_gen(List,Tag) :- member(Morph,List), get_case(Morph,Tag,'Gen',gertwol), !.

gender_neut(List,Tag) :- member(Morph,List), get_gender(Morph,Tag,'Neut',gertwol), !.

degree_comp(List,Tag) :- member(Morph,List), get_degree(Morph,Tag,'Comp',gertwol), !.

derived_from_ppres(List,Tag) :- derived_from_vpart(List,Tag).
derived_from_ppres(List,Tag) :- !, member(Morph,List), get_derived_from(Morph,Tag,'<PPRES',gertwol).
derived_from_ppast(List,Tag) :- derived_from_vpart(List,Tag).
derived_from_ppast(List,Tag) :- !, member(Morph,List), get_derived_from(Morph,Tag,'<PPAST',gertwol).
derived_from_vpart(List,Tag) :- !, member(Morph,List), get_derived_from(Morph,Tag,'<VPART',gertwol).

:- endif.


%placeholders in case morphology is turned off.
:- if(morphology(off)).
case_nom(_,_) :- !.
case_acc(_,_) :- !.
case_dat(_,_) :- !.
case_gen(_,_) :- !.

gender_neut(_,_) :- !.

degree_comp(_,_) :- !.

derived_from_ppres(_,_) :- !.
derived_from_ppast(_,_) :- !.
derived_from_vpart(_,_) :- !.

:- endif.


check_agreement(_,_,_,_,_) :- morphology(off), !.

check_agreement(List1,Tag1,List2,Tag2,ListOut) :- \+ var(List1), \+ var(List2),
	findall(SingleList1, (member(SingleList1,List1), member(SingleList2,List2),unify_morph(SingleList1,Tag1,SingleList2,Tag2)),ListTemp), 
	sort(ListTemp,ListTemp2), my_remove_duplicates(ListTemp2,ListOut), !, \+ ListOut = [].

%if first arg is not instantiated, use the instantiated list and convert it.
check_agreement(_,Tag,List2,Tag2,OutList) :- \+ var(List2), !, convertMorphList(Tag2,List2,Tag,OutList).

%if both args are uninstantiated, return true. otherwise, unification will fail.
check_agreement(List1,_,List2,_,_) :-  var(List1), var(List2), !.


%convertMorphList(+InTag,+InList,+OutTag,?OutList). Convert the morphological format to that of another word class.

convertMorphList(_,In,_,Out) :- var(In), var(Out), !.

convertMorphList(_,[In],_,[Out]) :- var(In), var(Out), !.

convertMorphList(_,_,_,_) :- morphology(off), !.

convertMorphList(_,List,_,List) :- morphology(tueba), !.

convertMorphList(Tag,In,Tag2,OutUniq) :- convertMorphList2(Tag,In,Tag2,Out), !, my_remove_duplicates(Out,OutUniq).


convertMorphList2(_,[],_,[]) :- morphology(gertwol), !.

convertMorphList2('APPRART',[List|RestIn],'APPR',[[Case]|RestOut]) :-
	morphology(gertwol), 
	get_case(List,'APPRART',Case,gertwol),
	!,
	convertMorphList2('APPRART', RestIn, 'APPR',RestOut).


convertMorphList2(Tag,[List|RestIn],Tag2,[ListOut|RestOut]) :-
	morphology(gertwol), 
	get_case(List,Tag,Case,gertwol),
	get_number(List,Tag,Number,gertwol),
	get_gender(List,Tag,Gen,gertwol),
	createMorph(Tag2,Gen,Case,Number,ListOut),
	!,
	convertMorphList2(Tag, RestIn, Tag2,RestOut).


%not complete, just those word classes that are needed. Takes information and creates a list in the desired format.
createMorph('ADJA',Gen,Case,Number,[_,Gen,Case,Number,_,_]) :- morphology(gertwol), !.
createMorph('ART',Gen,Case,Number,[_,Gen,Case,Number]) :- morphology(gertwol).
createMorph(Tag,Gen,Case,Number,[Gen,Case,Number]) :- morphology(gertwol),
		(morph_noun(Tag);morph_pronoun(Tag)), !.


unify_morph(_,_,_,_) :- morphology(off), !.

unify_morph(List1,_Tag1,List2,_Tag2) :- var(List1), var(List2), !.

unify_morph(List1,_Tag1,[A,B,C,D,E,_F],'ADJA') :- var(List1), var(A), var(B), var(C), var(D), var(E), !.

%unify_morphs(?List1,+Tag1,?List2,+Tag2).
%tests for number-person agreement
unify_morph(List1,Tag1,List2,Tag2) :- morphology(MorphType), morph_finverb(Tag1), !,
				(MorphType=gertwol->get_case(List2,Tag2,'Nom',MorphType);MorphType=tueba->get_case(List2,Tag2,'n',MorphType)), !,
				get_number(List2,Tag2,Number,MorphType), get_number(List1,Tag1,Number,MorphType), %number
				get_person(List2,Tag2,Person,MorphType), get_person(List1,Tag1,Person,MorphType). %person



%tests for case-number-gender agreement
unify_morph(List1,Tag1,List2,Tag2) :- morphology(MorphType), get_case(List2,Tag2,Case,MorphType), get_case(List1,Tag1,Case,MorphType), %case
				get_number(List2,Tag2,Number,MorphType), get_number(List1,Tag1,Number,MorphType), %number
				get_gender(List2,Tag2,Genus,MorphType), get_gender(List1,Tag1,Genus,MorphType), !. %gender


unify_case(_,_,_,_,_) :- morphology(off), !.


%if first arg is not instantiated, return second list
unify_case(List1,Tag1,List2,Tag2,OutList) :- var(List1), convertMorphList(Tag2,List2,Tag1,OutList), !.
unify_case([E],Tag1,List2,Tag2,OutList) :- var(E), convertMorphList(Tag2,List2,Tag1,OutList), !.

%unify_case(?List1,+Tag1,?List2,+Tag2,?ListOut).
unify_case(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_case(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_case(Morph2,Tag2,Case,MorphType)), ListTemp), 
	sort(ListTemp,ListTemp2), my_remove_duplicates(ListTemp2,ListOut), !, \+ ListOut = [].


unify_number(_,_,_,_,_) :- morphology(off), !.

%if first arg is not instantiated, do nothing.
unify_number(List1,Tag1,List2,Tag2,OutList) :- var(List1), convertMorphList(Tag2,List2,Tag1,OutList), !.
unify_number([E],Tag1,List2,Tag2,OutList) :- var(E), convertMorphList(Tag2,List2,Tag1,OutList), !.

%unify_number(?List1,+Tag1,?List2,+Tag2,?ListOut).
unify_number(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_number(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_number(Morph2,Tag2,Case,MorphType)), ListTemp), 
	sort(ListTemp,ListTemp2), my_remove_duplicates(ListTemp2,ListOut), !, \+ ListOut = [].




unify_gender(_,_,_,_,_) :- morphology(off), !.

%if first arg is not instantiated, return second list
unify_gender(List1,Tag1,List2,Tag2,OutList) :- var(List1), convertMorphList(Tag2,List2,Tag1,OutList), !.
unify_gender([E],Tag1,List2,Tag2,OutList) :- var(E), convertMorphList(Tag2,List2,Tag1,OutList), !.

unify_gender(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_gender(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_gender(Morph2,Tag2,Case,MorphType)), ListTemp), 
	sort(ListTemp,ListTemp2), my_remove_duplicates(ListTemp2,ListOut), !, \+ ListOut = [].


get_case(_,'CARD',_,_) :- !.
get_case(_,'ADV',_,_) :- !.
get_case(_,'ADJD',_,_) :- !.
get_case([Case|_],_,Case,tueba) :- !.

get_case([_,_,Case,_,_],'ADJA',Case,gertwol) :- !.
get_case([_,_,Case,_,_,_],'ADJA',Case,gertwol) :- !.
get_case([_,_,_,Case],'PPER',Case,gertwol) :- !.
get_case([_,_,Case,_],'ART',Case,gertwol) :- !.
get_case([_,Case],'APPRART',Case,gertwol) :- !.
get_case([Case],'APPR',Case,gertwol) :- !.
get_case([Case],'APPO',Case,gertwol) :- !.
get_case([_,_,Case],'PRF',Case,gertwol) :- !.
get_case([_,Case,_],Tag,Case,gertwol) :- (morph_noun(Tag);morph_pronoun(Tag)), !.

get_case(_,_,_,off) :- !.



get_number([_,Number|_],_,Number,tueba) :- !.
get_number(_,'APPRART',s,tueba) :- !.
get_number(_,'CARD',p,tueba) :- !.

get_number([_,_,_,Number,_],'ADJA',Number,gertwol) :- !.
get_number([_,_,_,Number,_,_],'ADJA',Number,gertwol) :- !.
get_number([_,Number,_,_],'PPER',Number,gertwol) :- !.
get_number([_,_,_,Number],'ART',Number,gertwol) :- !.
get_number([_,Number,_],'PRF',Number,gertwol) :- !.
get_number(_List,'APPRART','Sg',gertwol) :- !.
get_number(_,'CARD','Pl',gertwol) :- !.
get_number([_,Number,_,_],Tag,Number,gertwol) :- morph_finverb(Tag), !.
get_number([_,_,Number],Tag,Number,gertwol) :- (morph_noun(Tag);morph_pronoun(Tag);Tag = 'PRF'), !.

get_number(_,_,_,off) :- !.


get_gender(_,'CARD',_,_) :- !.
get_gender([_,_,Gender],_,Gender,tueba) :- !.

get_gender([_,Gender,_,_,_],'ADJA',Gender,gertwol) :- !.
get_gender([_,Gender,_,_,_,_],'ADJA',Gender,gertwol) :- !.
get_gender([_,_,Gender,_],'PPER',Gender,gertwol) :- !.
get_gender([_,Gender,_,_],'ART',Gender,gertwol) :-  !.
get_gender([Gender,_],'APPRART',Gender,gertwol) :- !.
get_gender([Gender,_,_],Tag,Gender,gertwol) :- (morph_noun(Tag);morph_pronoun(Tag)), !.

get_gender(_,_,_,off) :- !.



get_person([_,_,_,Person],'PPER',Person,tueba) :- !.
get_person([Person,_,_,_],Tag,Person,tueba) :- morph_finverb(Tag), !.
get_person(_,Tag,'3',tueba) :- \+ morph_finverb(Tag).
get_person(_,Tag,3,tueba) :- \+ morph_finverb(Tag), !.

get_person([Person,_,_,_],'PPER',Person,gertwol) :- !.
get_person([Person,_,_,_],Tag,Person,gertwol) :- morph_finverb(Tag), !.
get_person(_,Tag,'3',gertwol) :- \+ morph_finverb(Tag).
get_person(_,Tag,3,gertwol) :- \+ morph_finverb(Tag), !.

get_person(_,_,_,off) :- !.


get_degree([Degree,_,_,_,_],'ADJA',Degree,gertwol) :- !.
get_degree([Degree,_,_,_,_,_],'ADJA',Degree,gertwol) :- !.
get_degree([Degree],'ADJD',Degree,gertwol) :- !.
get_degree([Degree,_],'ADJD',Degree,gertwol) :- !.


get_degree(_,_,_,tueba) :- !.
get_degree(_,_,_,off) :- !.


%for some derived words, the PoS of the original word is included in the morphology info.
get_derived_from(Morph,_Tag,Origin,gertwol) :- last(Morph,Last), !, \+ var(Last), Last = Origin.


%used for format conversion
case_tueba('Nom',n) :- !.
case_tueba('Akk',a) :- !.
case_tueba('Acc',a) :- !.
case_tueba('Dat',d) :- !.
case_tueba('Gen',g) :- !.
case_tueba(X,X) :- !.


:- morphology(gertwol)->assert(singular('Sg'));true.
:- morphology(gertwol)->assert(plural('Pl'));true.

:- morphology(tueba)->assert(singular('s'));true.
:- morphology(tueba)->assert(plural('p'));true.

:- morphology(gertwol)->assert(nominative('Nom'));true.
:- morphology(gertwol)->assert(accusative('Acc'));true.
:- morphology(gertwol)->assert(accusative('Akk'));true.
:- morphology(gertwol)->assert(dative('Dat'));true.
:- morphology(gertwol)->assert(genitive('Gen'));true.

:- morphology(tueba)->assert(nominative('n'));true.
:- morphology(tueba)->assert(accusative('a'));true.
:- morphology(tueba)->assert(dative('d'));true.
:- morphology(tueba)->assert(genitive('g'));true.


% cardinals require a plural head (with a few exceptions)
% Uhr stays singular
% units of measurement may be uninflected ("20 Grad")
unify_card(_,_,_,_) :- morphology(off), !.
unify_card(HeadWord,DepWord,HeadMorph,MorphOut) :- ((DepWord = '1';HeadWord='Uhr')->singular(Number);plural(Number)),
    (measure(HeadWord)->createMeasureMorph(HeadMorph,Number,MorphOut);
                    check_agreement(HeadMorph,'NN',[[_,_,Number]],'NN',MorphOut)), !.

%unit of measure; use gender from morphological analysis, but allow all cases, and correct number
createMeasureMorph(MorphIn,Number,MorphOut) :- morphology(MorphType),
            findall(Gender, (member(Morph,MorphIn),get_gender(Morph,'NN',Gender,MorphType)),GenderList),
            nominative(Nom),
            accusative(Acc),
            dative(Dat),
            genitive(Gen),
            findall([Gender,Case,Number],(member(Gender,GenderList), member(Case,[Nom,Acc,Dat,Gen])),MorphTemp), my_remove_duplicates(MorphTemp,MorphOut), !.

createMorphOutput(Head,Dep,MyRel) :- (getChartInfo(Head,HPos,HWord,HLemma,_,HMorph);true),
      getChartInfo(Dep,DPos,DWord,DLemma,_,DMorph),
      (call(output(HPos,HWord,HLemma,_HTag,_,_,_HMorph2))->true;
          var(HPos)->true;
            ((chart(HPos,HPos,HPos,_,Lemma,HTag,_,_,_,[Word|_]),
            assert(output(HPos,Word,Lemma,HTag,root,0,HMorph))))),
      (call(output(DPos,DWord,DLemma,DTag,MyRel,_,DMorph2));
          (checkPos(DPos,_,DTag,_,_),
          DMorph2 = DMorph)), 
      (retract(output(DPos,_,_,_,_,_,_));true),
      assert(output(DPos,DWord,DLemma,DTag,MyRel,0,DMorph2)), !.

createMorphOutput(_,_,_) :- !.

createRelOutput(Head,Dep,MyRel) :- lexic(Head,HLemma,HPos), 
      lexic(Dep,DLemma,DPos), 
      (call(output(HPos,_,HLemma,_,_,_,_));(getChartInfo(_,HPos,_,HLemma,_,_),checkPos(HPos,_,_,_,_))),
      (retract(output(DPos,DWord,DLemma,DTag,_,_,DMorph));(getChartInfo(_,DPos,DWord,DLemma,_,DMorph),checkPos(DPos,_,DTag,_,_))),
      assert(output(DPos,DWord,DLemma,DTag,MyRel,HPos,DMorph)), !.

createRelOutput(_,_,_) :- !.

getChartInfo(Head,Pos,Word,Lemma,Tag,Morph) :- chart(_,_,_,[Pos,_,_,_],Lemma,Tag,_,Head,_,[Word|Morph]), !.
getChartInfo(Head,Pos,Word,Lemma,Tag,Morph) :- chart(Pos,Pos,Pos,_,Lemma,Tag,_,Head,_,[Word|Morph]), !.



%start unification from the top:
morph_cleanup2(Rel,HMorph,HTag,DMorph,DTag,DPos,HPos,DMorphOut) :- 
              output(HPos,_,_,HTag,HRel,HHPos,HMorph),
              output(HHPos,_,_,HHTag,_,HHHPos,HHMorph), !,
              morph_cleanup2(HRel,HHMorph,HHTag,HMorph,HTag,HHPos,HHHPos,HMorphOut),
              morph_cleanup(Rel,HMorphOut,HTag,DMorph,DTag,DPos,HPos,DMorphOut).

morph_cleanup2(Rel,HMorph,HTag,DMorph,DTag,DPos,HPos,DMorphOut) :- morph_cleanup(Rel,HMorph,HTag,DMorph,DTag,DPos,HPos,DMorphOut), !.


%fixes some ugly output because of the bracket rules in the grammar.
morph_cleanup(Class,HMorph,HTag,DMorph,DTag,DPos,HPos,DMorphOut) :- nth1(2,HMorph,X), atomic(X), HMorph = [NewList|_], !, morph_cleanup(Class,NewList,HTag,DMorph,DTag,DPos,HPos,DMorphOut). 
morph_cleanup(Class,HMorph,HTag,DMorph,DTag,DPos,HPos,DMorphOut) :- nth1(2,DMorph,X), atomic(X), DMorph = [NewList|_], !, morph_cleanup(Class,HMorph,HTag,NewList,DTag,DPos,HPos,DMorphOut).


% morph_cleanup(root,_,_,DMorph,_,_,DMorph) :- !.


%don't print morphological analyses that are only used internally.
morph_cleanup(_,_,_,_,'KON',_,_,[_]) :- !.

%Some grammatical functions are bound to a specific case. Restrict output morphology to syntactically valid analyses.
morph_cleanup(subj,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Nom']],'APPR',DMorphTemp), (unify_number(DMorphTemp,DTag,HMorph,HTag,DMorphOut);DMorphOut=DMorphTemp), !.
morph_cleanup(pred,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Nom']],'APPR',DMorphOut), !.
morph_cleanup(obja,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk'], ['Acc']],'APPR',DMorphOut), !.
morph_cleanup(obja2,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk'], ['Acc']],'APPR',DMorphOut), !.
morph_cleanup(grad,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk'], ['Acc']],'APPR',DMorphOut), !.
morph_cleanup(zeit,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk'], ['Acc']],'APPR',DMorphOut), !.
morph_cleanup(objd,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Dat']],'APPR',DMorphOut), !.
morph_cleanup(objg,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Gen']],'APPR',DMorphOut), !.
morph_cleanup(gmod,_,_,DMorph,DTag,_,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Gen']],'APPR',DMorphOut), !.

%Agreement is checked in grammar, but result is only stored for head. Produce output for dependent here.
morph_cleanup(attr,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- check_agreement(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(det,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- check_agreement(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(pn,HMorph,_HTag,DMorph,DTag,_,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,'APPR',DMorphOut), !.
morph_cleanup(app_loose,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(app_close,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(kon,HMorph,HTag,DMorph,DTag,_,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.

%for the latest member in a coordinative chain, unification with the head doesn't work. Find first member of the chain instead.
morph_cleanup(cj,_,_,DMorph,DTag,_,HPos,DMorphOut) :- findkonchainhead(HPos,HMorph,HTag), unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.


morph_cleanup(_,_,HTag,HMorph,'VAFIN',HPos,_,HMorphOut) :- output(_,_,_,DTag,subj,HPos,DMorph), check_agreement(HMorph,HTag,DMorph,DTag,HMorphOut), !.
morph_cleanup(_,_,HTag,HMorph,'VMFIN',HPos,_,HMorphOut) :- output(_,_,_,DTag,subj,HPos,DMorph), check_agreement(HMorph,HTag,DMorph,DTag,HMorphOut), !.
morph_cleanup(_,_,HTag,HMorph,'VVFIN',HPos,_,HMorphOut) :- output(_,_,_,DTag,subj,HPos,DMorph), check_agreement(HMorph,HTag,DMorph,DTag,HMorphOut), !.


%catchall
morph_cleanup(_,_,_,DMorph,_,_,_,DMorph) :- !.


%for the latest member in a coordinative chain, unification with the head doesn't work. Find first member of the chain instead.
findkonchainhead(DPos,HMorph,HTag) :- output(DPos,_,_,_,kon,HPos,_),
        findkonchainhead(HPos,HMorph,HTag).

findkonchainhead(HPos,HMorph,HTag) :- output(HPos,_,_,HTag,_,_,HMorph), (var(HTag); \+ kon_mapping(_,HTag), \+ HTag = 'KON'), !.


% X , KON Y: comma is sometimes allowed, sometimes not.
% good: nicht Peter , sondern Jan
% bad: Peter , und Jan (this is more likely a coordination of verbs, as in "Zuerst kam Peter , und Jan folgte kurz darauf")
restrict_comma_for_kon(_Transtag, sondern, _HeadRels).
restrict_comma_for_kon(_Transtag, aber, _HeadRels).
restrict_comma_for_kon(_Transtag, jedoch, _HeadRels).
restrict_comma_for_kon('KON_FINVERB', _HeadWord, _HeadRels).
restrict_comma_for_kon(_Transtag, _HeadWord, HeadRels) :- \+ member('<-comma<-',HeadRels).


%======================================================================================
%topological rules

%in Verbzweitstellung, only one complement (even adjunct?) can precede the finite verb.
restrict_vorfeld(Chunk,Dependents) :- member('mainclause',Chunk), 
                !, %cut the catchall
                intersection(Dependents,['<-subj<-',
                                         '<-obja<-',
                                         '<-objd<-',
                                         '<-objg<-',
                                         '<-pred<-',
                                         '<-objp<-',
                                         '<-obji<-',
                                         '<-pp<-',
                                         '<-subjc<-',
                                         '<-s<-',
                                         '<-zeit<-',
                                         '<-kom<-',
                                         '<-explsubj<-',
                                         '<-adv<-',
                                         '<-objc<-',
                                         '<-neb<-',
                                         '<-explobja<-'],[]). %only succeed if intersection is empty


restrict_vorfeld(_,_) :- !. %catchall


%restrict_coord/1: makes sure that subjects, objects etc. are not attached to a finite verb if there is a verb coordination in between:
%example: susi denkt und peter sieht laura. -> 'laura' can't be object of 'denkt'.
restrict_coord(RelList) :- intersection(RelList,['->konc->','->kon->','->s->','->objc->','->subjc->','->neb->','->obji->','->rel->'],[]). %only succeed if intersection is empty

% like restrict_coord, but also don't allow any attachment if we're in the nachfeld (we've seen a separable verb prefix or full verb)
restrict_coord_and_nachfeld(RelList) :- intersection(RelList,['->konc->','->kon->','->s->','->objc->','->subjc->','->neb->','->obji->','->rel->','->aux->','->avz->'],[]). %only succeed if intersection is empty


% we relax our rule "attach everything to the finite verb" (which has the purpose of making most structure parsable with a context-free grammar) for coordinations of multiple non-finite verbs:
% Er darf weder einen Beruf ausüben noch Dritte vor Gericht vertreten
in_coordination(Chunk,_Relations) :- verbchunklength(Chunk, 1), !.
in_coordination(_Chunk,Relations) :- member('->kon->', Relations), !.

%======================================================================================
%checks on positions outside of normal scope.

%check if construction either ends with a comma, a bracket or the end of the sentence.
commaToRight(ID) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,_,Tag,_,_), (Tag = 'NONE';Tag='$,').

commaToLeft(ID) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$,').


stopToRight(ID) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,_,Tag,_,_), (Tag = 'NONE';Tag='$.';sentdelim(Tag)).

stopToLeft(ID) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$.';sentdelim(Tag)).


bracketToRight(ID,Word) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,Word,Tag,_,_), (Tag = 'NONE';Tag='$(').

bracketToLeft(ID,Word) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,Word,Tag,_,_), (Tag = 'NONE';Tag='$(').


getRange(ID, From-To) :- chart(ID,From,To,_,_,_,_,_,_,_).

%check any terminal symbol and its features.
checkPos(Pos,Word,Tag,Chunk,Morph) :- chart(Pos,Pos,Pos,[_,_,Chunk,_],Word,Tag,_,_,_,Morph), !.

%catchall (sentence position does not exist: beginning or end of sentence). 
checkPos(_,'NONE','NONE','NONE','NONE') :- !.

%used to distinguish between "der kleine ist niedlich" und "der kleine eisbär ist niedlich".
%this is just an approximation: "der kleine im wasser schwimmende eisbär" will be misclassified.

endOfNP(Pos) :- NewPos is Pos + 1,
        checkPos(NewPos,_,'$(',_,_), !,
        endOfNP(NewPos).

endOfNP(Pos) :- NewPos is Pos + 1,
        checkPos(NewPos,_,Tag,_,_),
        \+ (Tag = 'NN';Tag = 'NE';Tag = 'FM';Tag = 'CARD';Tag = 'ADJA';Tag = 'TRUNC').

%======================================================================================


%objcandidate: all word classes that can be objects
objcandidate('NN',_).
objcandidate('NE',_).
objcandidate('FM',_).
objcandidate('PDS',_).
objcandidate('PIS',_).
objcandidate('PPER',_).
objcandidate('PPOSS',_).
objcandidate('PRF',_).
objcandidate('ADJA',Pos) :- endOfNP(Pos).
objcandidate('CARD',Pos) :- endOfNP(Pos).
objcandidate('PIAT',Pos) :- endOfNP(Pos).

%subjcandidate: all word classes that can be subjects
subjcandidate('NN',_).
subjcandidate('NE',_).
subjcandidate('FM',_).
subjcandidate('PDS',_).
subjcandidate('PIS',_).
subjcandidate('PPER',_).
subjcandidate('PPOSS',_).
subjcandidate('ADJA',Pos) :- endOfNP(Pos).
subjcandidate('CARD',Pos) :- endOfNP(Pos).
subjcandidate('PIAT',Pos) :- endOfNP(Pos).

%validgmod: all word classes that can constitue a genitive modifier.
validgmod('NN').
validgmod('NE').
validgmod('FM').
validgmod('PDS').
validgmod('PIS').

%valid preposition complements
prepcompl('NN',_).
prepcompl('NE',_).
prepcompl('FM',_).
prepcompl('CARD',Pos) :- endOfNP(Pos).
prepcompl('ADJA',Pos) :- endOfNP(Pos).
%including some rare (or non-existing) dependencies.
prepcompl('PDS',_).
prepcompl('PIS',_).
prepcompl('PPER',_).
prepcompl('PRF',_).
prepcompl('PPOSS',_).
%
prepcompl('PIDAT',Pos) :- endOfNP(Pos), RightPos is Pos + 1, \+ checkPos(RightPos,_,'ART',_,_).
prepcompl('PIAT',Pos) :- endOfNP(Pos).
prepcompl('PDAT',Pos) :- endOfNP(Pos).
prepcompl('PPOSAT',Pos) :- endOfNP(Pos).
%false positives with unchunked NPs. commented out.
% prepcompl('ADV',Pos) :- endOfNP(Pos).
% prepcompl('TRUNC',Pos) :- endOfNP(Pos).
% prepcompl('PP',Pos) :- endOfNP(Pos).


attributive_pronoun('PIDAT').
attributive_pronoun('PDAT').
attributive_pronoun('PIAT').
attributive_pronoun('PPOSAT').
attributive_pronoun('PWAT').
attributive_pronoun('PRELAT').


nonfinite('VAINF').
nonfinite('VAPP').
nonfinite('VMINF').
nonfinite('VMPP').
nonfinite('VVINF').
nonfinite('VVPP').
nonfinite('VVIZU').


%determiner candidates.
detcan('ART',_).
detcan('PIDAT',Pos) :- LeftPos is Pos - 1, checkPos(LeftPos,_,Tag,_,_), Tag \= 'ART', Tag \= 'ADJA', \+ attributive_pronoun(Tag).
detcan('PIAT',_).
detcan('PPOSAT',_).
detcan('PDAT',_).

adverbial_pronoun('PROP').
adverbial_pronoun('PAV').
adverbial_pronoun('PROAV').

%two indicators that token can be part of coordination chain; either it has coordinated element, or adverb indicating coordination.
coordinated_element(Dependents) :- member('->kon->',Dependents), !.
coordinated_element(Dependents) :- member('<-adv<-',Dependents), among_dependents(Dependents,Dep,1), !, conjunctive_adverb(Dep).

%check each member of a list.
among_dependents([Item|Rest], Tag, LVL) :- !, (among_dependents(Item,Tag, LVL);
					   among_dependents(Rest,Tag, LVL)), !.

%searches the arguments of a complex term. Recursion is marked to go one level deeper.
%use positive number to limit number of recursions, use -1 for no limit
among_dependents(Term, Tag, LVL) :- compound(Term),
				LVL \= 0,
			        NewLVL is LVL - 1,
			       Term =.. [_|ChunkList], !,
			       among_dependents(ChunkList,Tag, NewLVL).

%check atom.
among_dependents(Elem, Tag, _) :- atom(Elem), atom_concat(_,Tag,Elem), !.


%if there are several conjoined auxiliary verbs, disregard the mutual exclusion of obja and objc:
%"ich habe ihn gesehen und gedacht, dass er schläft"
conjoined_vp(Struct) :- ((nth1(Pos, Struct, '<-aux<-'),
                        LastPos is Pos-1,
                        nth1(LastPos,Struct,DepStruct),
                        DepStruct =.. DepList, !,
                        member('->kon->',DepList));
                        (nth1(Pos, Struct, '->aux->'),
                        NextPos is Pos+1,
                        nth1(NextPos,Struct,DepStruct),
                        DepStruct =.. DepList, !,
                        member('->kon->',DepList))).


%when calculating length of verb chunk, ignore 'mainclause' and 'passive'
verbchunklength(['mainclause'|Rest],Len) :- verbchunklength(Rest,Len).
verbchunklength(['passive'|Rest],Len) :- verbchunklength(Rest,Len).
verbchunklength(List,Len) :- length(List,Len), !.


splittag(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'_'),
	sub_atom(WordI,0,Before,_,Word), 
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa), !.

splittag(WordI,WordI,_) :- !.


%standard sort only removes duplicates if no variables are involved.
%we want [[_,'Akk'],[_,'Akk']] to be reduced to [[_,'Akk']]
my_remove_duplicates(L,Unique) :- duplicate_check(L,[],UniqueTmp), (is_all_var(UniqueTmp)->Unique=_;(UniqueTmp=[UniqInner],is_all_var(UniqInner))->Unique=[_];Unique=UniqueTmp).

duplicate_check([],Acc,Acc) :- !.
duplicate_check([H|T],Acc,Unique) :- \+ member(H,Acc), !, duplicate_check(T,[H|Acc],Unique).
duplicate_check([_|T],Acc,Unique) :- duplicate_check(T,Acc,Unique).

is_all_var([]).
is_all_var([Element|Rest]) :- var(Element), is_all_var(Rest).
