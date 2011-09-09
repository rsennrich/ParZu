% Grammar Rules: head(+Head,+Dependent,?Direction,+Type,_)
%% head(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FF,FG,OF,OG],FPos-GPos,MorphF,MorphG,TransMorph)
%% head(Gtag,Ftag,r,Type,Transtag,[FChunk,GChunk,FF,FG,[SF|OF],OG],FPos-GPos,MorphF,MorphG,TransMorph)

%%head2 causes less headaches than head, because the order of arguments does not depend on the direction of the relation.
%%use this if you write new rules.
%%head2(HeadTag,DepTag,Dir,Type,Transtag,[HeadChunk,DepChunk,HeadWord,DepWord,HeadRels,DepRels,HeadID,DepID],HeadPos-DepPos,HeadMorph,DepMorph,TransMorph)

:- style_check(-discontiguous).

%======================================================================================
%determiners

head('NN',DET,l,det,'NN', [_,_,_,_,OF,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'NN',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('NE',DET,l,det,'NE', [_,_,_,_,OF,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'NE',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('FM',DET,l,det,'FM', [_,_,_,_,OF,_],_-G,MF,MG,MNew) :- detcan(DET,G), check_agreement(MF,'FM',MG,DET,MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

%ein paar Leute: Take morphology from noun; no agreement necessary
head('NIDEF',DET,l,det,'NN', [_,_,_,_,OF,_],_-G,MF,_,MF) :- detcan(DET,G), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).


%solch eine Friedenstruppe: double determiner possible with PIDAT.
head('NN','PIDAT',l,det,'NN', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).

head('NE','PIDAT',l,det,'NE', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).

head('FM','PIDAT',l,det,'FM', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).


head('NN','PIAT',l,det,'NN', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).

head('NE','PIAT',l,det,'NE', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).

head('FM','PIAT',l,det,'FM', [_,_,_,_,OF,_],_-G,MF,_,MF) :- \+ member('<-gmod<-',OF), OldPos is G - 1, \+ checkPos(OldPos,_,'ART',_,_).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA',DET,l,det,'NN', [_,_,_,_,OF,_],F-G,MF,MG,MNew) :- detcan(DET,G), endOfNP(F), check_agreement(MF,'ADJA',MG,DET,MTemp), convertMorphList('ADJA',MTemp,'NN',MNew) ,\+ member('<-det<-',OF).

head('CARD',DET,l,det,'NN', [_,_,_,_,OF,_],F-G,MF,MG,MNew) :- detcan(DET,G), endOfNP(F), check_agreement(MF,'NN',MG,DET,MNew), \+ member('<-det<-',OF).


%new transtag (PRELS) to identify subordinated clauses - interfering with other rules? perhaps new rules for appositions etc. needed.
head('NN','PRELAT',l,det,'PRELS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'NN',MG,'PRELAT',MNew).

head('NE','PRELAT',l,det,'PRELS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'NE',MG,'PRELAT',MNew).

head('FM','PRELAT',l,det,'PRELS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-gmod<-',OF), check_agreement(MF,'FM',MG,'PRELAT',MNew).

%ungrammatical in theory, but tagging errors possible
head('NN','PRELS',l,det,'PRELS', [_,_,_,_,OF,OG],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'NN',MG,'PRELS',MNew).

head('NE','PRELS',l,det,'PRELS', [_,_,_,_,OF,OG],_,MF,MG,MNew) :- \+ member('<-det<-',OF),\+ member('<-det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'NE',MG,'PRELS',MNew).

head('FM','PRELS',l,det,'PRELS', [_,_,_,_,OF,OG],_,MF,MG,MNew) :- \+ member('<-det<-',OF), \+ member('<-det<-',OG), \+ member('<-gmod<-',OF), check_agreement(MF,'FM',MG,'PRELS',MNew).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA','PRELAT',l,det,'PRELS', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F),\+ member('<-det<-',OF).

head('CARD','PRELAT',l,det,'PRELS', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F),\+ member('<-det<-',OF).



head('NN','PWAT',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'NN',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('NE','PWAT',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'NE',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('FM','PWAT',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'FM',MG,'PWAT',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

%ungrammatical in theory, but tagging errors possible
head('NN','PWS',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'NN',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('NE','PWS',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'NE',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).

head('FM','PWS',l,det,'PWS', [_,_,_,_,OF,_],_,MF,MG,MNew) :- check_agreement(MF,'FM',MG,'PWS',MNew), \+ member('<-det<-',OF), \+ member('<-gmod<-',OF).


%some word classes can be head of noun phrase if noun is missing. 
head('ADJA','PWAT',l,det,'PWS', [_,_,_,_,OF,_],F-_,MF,MG,MNew) :- endOfNP(F), check_agreement(MF,'ADJA',MG,'PWAT',MTemp), convertMorphList('ADJA',MTemp,'NN',MNew) ,\+ member('<-det<-',OF).

head('CARD','PWAT',l,det,'PWS', [_,_,_,_,OF,_],F-_,MF,MG,MNew) :- endOfNP(F), check_agreement(MF,'NN',MG,'PWAT',MNew), \+ member('<-det<-',OF).


%======================================================================================
%attributes

head('NN','ADJA',l,attr,'NN', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), check_agreement(MF,'NN',MG,'ADJA',MNew).

head('NE','ADJA',l,attr,'NE', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), check_agreement(MF,'NE',MG,'ADJA',MNew).

head('FM','ADJA',l,attr,'FM', [_,_,_,_,OF,_],_,MF,MG,MNew) :- \+ member('<-det<-',OF), check_agreement(MF,'FM',MG,'ADJA',MNew).


%this rule only applies if there is another article left of the pronoun. "Ein paar Leute". Special transtag if article morphology is to be ignored in 'det' rules.
head('NN','PIDAT',l,attr,TransTag, [_,_,_,GWord,OF,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), LPos is G - 1, checkPos(LPos,LWord,'ART',_,_), check_agreement(MF,'NN',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),LWord=ein)->TransTag='NIDEF';TransTag='NN').

head('NE','PIDAT',l,attr,TransTag, [_,_,_,GWord,OF,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), LPos is G - 1, checkPos(LPos,LWord,'ART',_,_), check_agreement(MF,'NE',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),LWord=ein)->TransTag='NIDEF';TransTag='NE').

head('FM','PIDAT',l,attr,TransTag, [_,_,_,GWord,OF,_],_-G,MF,MG,MNew) :- \+ member('<-det<-',OF), LPos is G - 1, checkPos(LPos,LWord,'ART',_,_), check_agreement(MF,'FM',MG,'PIDAT',MNew), ((pidat_anymorph(GWord),LWord=ein)-TransTag='NIDEF';TransTag='FM').

%ein paar Leute - no morphology check
pidat_anymorph('paar').
pidat_anymorph('bisschen').
pidat_anymorph('bißchen').
pidat_anymorph('wenig').

head('NN','CARD',l,attr,'NN', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).

head('NE','CARD',l,attr,'NE', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).

head('FM','CARD',l,attr,'FM', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).


%Der Ex- Aussenminister. Might be treated as two words (for instance on line breaks)
head('NN','TRUNC',l,attr,'NN', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).

head('NE','TRUNC',l,attr,'NE', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).

head('FM','TRUNC',l,attr,'FM', [_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-det<-',OF).


%CARD or ADJA as head of NP if noun is missing.
head('ADJA','CARD',l,attr,'NN', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F), \+ member('<-det<-',OF).

head('CARD','CARD',l,attr,'NN', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F), \+ member('<-det<-',OF).

head('ADJA','ADJA',l,attr,'NN', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F), \+ member('<-det<-',OF).

head('CARD','ADJA',l,attr,'NN', [_,_,_,_,OF,_],F-_,MF,_,MF) :- endOfNP(F), \+ member('<-det<-',OF).


%exception: '2 mal', '5 mal' etc.
head('ADV','CARD',l,attr,'ADV', [_,_,'mal',_,OF,_],_,MF,_,MF) :- \+ member('<-attr<-',OF).
head('ADV','CARD',l,attr,'ADV', [_,_,'Mal',_,OF,_],_,MF,_,MF) :- \+ member('<-attr<-',OF).



%'drei andere', 'viele mehr' etc.
head('PIS','CARD',l,attr,'PIS', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.
head('PIS','ADJD',l,attr,'PIS', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.
head('PIS','PIDAT',l,attr,'PIS', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.
head('PIS','ADJA',l,attr,'PIS', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

%======================================================================================
%prep(osition)

%use prepcompl/1 to list all valid dependents of prepositions.
head('APPR',PN,r,pn,'PP',[_,_,_,_,_,_],F-_,MF,MG,MNew) :- prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew).

%bis auf weiteres - may be mistagged.
head(_,'PP',r,pn,'PP',[_,_,_,bis,_,_],F-G,_,MG,MG) :- 1 is F-G.
head(_,'PP',r,pn,'PP',[_,_,_,'Bis',_,_],F-G,_,MG,MG) :- 1 is F-G.


%zu might be mistagged as PTKA/PTKZU/PTKVZ
head(_,PN,r,pn,'PP',[_,_,_,zu,_,_],F-_,MF,MG,MNew) :- prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew).
head(_,PN,r,pn,'PP',[_,_,_,'Zu',_,_],F-_,MF,MG,MNew) :- prepcompl(PN,F), unify_case(MG,'APPR',MF,PN,MNew).


%"mit mehr als x" - no distance restriction. (inconsistency in gold standard: pn or kom?)
head('APPR','KOMPX',r,kom,'PP',[_,_,_,_,_,_],_,_,MG,MG).



%relative clause
head('APPR','PRELAT',r,pn,'PPREL',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew).

head('APPR','PRELS',r,pn,'PPREL',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew).

head('APPR','PWS',r,pn,'PPQ',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'APPR',MF,'PRELS',MNew).




%use prepcompl/1 to list all valid dependents of prepositions.
head('APPRART',PN,r,pn,'PP',[_,_,_,_,_,_],F-_,_,MG,MNew) :- prepcompl(PN,F), convertMorphList('APPRART',MG,'APPR',MNew).


%relative clause
head('APPRART','PRELAT',r,pn,'PPREL',[_,_,_,_,_,_],_,_,MG,MNew) :- convertMorphList('APPRART',MG,'APPR',MNew).

head('APPRART','PRELS',r,pn,'PPREL',[_,_,_,_,_,_],_,_,MG,MNew) :- convertMorphList('APPRART',MG,'APPR',MNew).

head('APPRART','PWS',r,pn,'PPQ',[_,_,_,_,_,_],_,_,MG,MNew) :- convertMorphList('APPRART',MG,'APPR',MNew).



%======================================================================================
%postposition


%use prepcompl/1 to list all valid dependents of prepositions.
head('APPO',PN,l,pn,'PP',[_,_,_,_,_,_],_-G,MF,_,MF) :- prepcompl(PN,G).

%relative clause
head('APPO','PRELS',l,pn,'PPREL',[_,_,_,_,_,_],_,MF,_,MF).

head('APPO','PRELAT',l,pn,'PPREL',[_,_,_,_,_,_],_,MF,_,MF).

head('APPO','PWS',l,pn,'PPQ',[_,_,_,_,_,_],_,MF,_,MF).

%======================================================================================
%Subject, only one is allowed    

%subject before finite verb
head('VVFIN',SUBJ,l,subj,'VVFIN',[FC,_,_,_,UG,OG],_-G,MF,MG,MNew) :- subjcandidate(SUBJ,G), case_nom(MG,SUBJ), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF;check_agreement(MF,'VVFIN',MG,SUBJ,MNew)).

head('VMFIN',SUBJ,l,subj,'VMFIN',[FC,_,_,_,UG,OG],_-G,MF,MG,MNew) :- subjcandidate(SUBJ,G), case_nom(MG,SUBJ), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF;check_agreement(MF,'VMFIN',MG,SUBJ,MNew)).


head('VAFIN',SUBJ,l,subj,'VAFIN',[FC,_,_,_,UG,OG],_-G,MF,MG,MNew) :- subjcandidate(SUBJ,G), case_nom(MG,SUBJ), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF;check_agreement(MF,'VAFIN',MG,SUBJ,MNew)).


%subject before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
% head('VVIZU',OBJ,l,obja,'VVIZU',[FC,_,_,_,UG,_],__-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%allow subject before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
% head('VVPP', SUBJ,l,subj,'NEB',[FC,_,_,_,UG,_],F-G,MF,MG,MF) :- subjcandidate(SUBJ,G), \+ SUBJ = 'CARD', verbchunklength(FC,1), RightPos is F + 1, \+ checkPos(RightPos,_,'KON',_,_), case_nom(MG,SUBJ), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).

%relative pronoun (new transtag 'RC')
head('VVFIN','PRELS',l,subj,'RC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VVFIN',MG,'PRELS',MNew)).

head('VMFIN','PRELS',l,subj,'RC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VMFIN',MG,'PRELS',MNew)).

head('VAFIN','PRELS',l,subj,'RC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VAFIN',MG,'PRELS',MNew)).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,subj,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_nom(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWS',l,subj,'QC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VVFIN',MG,'PWS',MNew)).

head('VMFIN','PWS',l,subj,'QC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VMFIN',MG,'PWS',MNew)).

head('VAFIN','PWS',l,subj,'QC',[FC,_,_,_,UG,OG],_,MF,MG,MNew) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG), (member('->kon->',OG)->MNew=MF; check_agreement(MF,'VAFIN',MG,'PWS',MNew)).

%only necessary in case of tagging errors
head('VVPP','PWS',l,subj,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_nom(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-subj<-',UG), \+ member('->subj->',UG), \+ member('<-subjc<-',UG), \+ member('->subjc->',UG), \+ member('<-explsubj<-',UG), \+ member('->explsubj->',UG).


%subject after finite verb
head('VVFIN',SUBJ,r,subj,'VVFIN',[_,_,_,_,OF,OG],F-_,MF,MG,MNew)  :- subjcandidate(SUBJ,F), case_nom(MF,SUBJ), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG), \+ member('<-explsubj<-',OG), \+ member('->explsubj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG), (member('->kon->',OF)->MNew=MF; check_agreement(MG,'VVFIN',MF,SUBJ,MNew)).

head('VAFIN',SUBJ,r,subj,'VAFIN',[_,_,_,_,OF,OG],F-_,MF,MG,MNew)  :- subjcandidate(SUBJ,F), case_nom(MF,SUBJ), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG), \+ member('<-explsubj<-',OG), \+ member('->explsubj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG), (member('->kon->',OF)->MNew=MF; check_agreement(MG,'VAFIN',MF,SUBJ,MNew)).

head('VMFIN',SUBJ,r,subj,'VMFIN',[_,_,_,_,OF,OG],F-_,MF,MG,MNew)  :- subjcandidate(SUBJ,F), case_nom(MF,SUBJ), restrict_coord(OG),  \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG), \+ member('<-explsubj<-',OG), \+ member('->explsubj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG), (member('->kon->',OF)->MNew=MF; check_agreement(MG,'VMFIN',MF,SUBJ,MNew)).


%======================================================================================
%Accusative object, only one allowed (for verbs using two accusative objects, use OBJA2)

%object before finite verb.
head('VVFIN',OBJ,l,obja,'VVFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN',OBJ,l,obja,'VMFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN',OBJ,l,obja,'VAFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,obja,'VVIZU',[FC,_,_,_,UG,_],__-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%allow OBJAs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
head('VAINF', OBJ,l,obja,'VAINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VAPP', OBJ,l,obja,'VAPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VMINF', OBJ,l,obja,'VMINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VMPP', OBJ,l,obja,'VMPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VVINF', OBJ,l,obja,'VVINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VVPP', OBJ,l,obja,'VVPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).


%relative pronoun (new transtag 'RC')
head('VVFIN','PRELS',l,obja,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN','PRELS',l,obja,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN','PRELS',l,obja,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,obja,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWS',l,obja,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja<-',UG), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN','PWS',l,obja,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN','PWS',l,obja,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,obja,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('->obja->',UG), \+ member('<-objc<-',UG), \+ member('->objc->',UG), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object after finite verb.
head('VVFIN',OBJ,r,obja,'VVFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VAFIN',OBJ,r,obja,'VAFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VMFIN',OBJ,r,obja,'VMFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VVIMP',OBJ,r,obja,'VVIMP',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

%rollkragenpullover tragende brillenträger
head('ADJA', OBJ,l,obja,'ADJA',[_,_,_,_,OF,_],F-G,MF,MG,MF) :- 1 is F-G, derived_from_vpart(MF,'ADJA'), objcandidate(OBJ,F), case_acc(MG,OBJ), \+ endOfNP(F), \+ member('<-obja<-',OF), \+ member('->obja->',OF).

head('ADJA', OBJ,l,obja,'ADJD',[_,_,_,_,OF,_],F-G,MF,MG,MF) :- 1 is F-G, derived_from_vpart(MF,'ADJD'), objcandidate(OBJ,F), case_acc(MG,OBJ), \+ endOfNP(F), \+ member('<-obja<-',OF), \+ member('->obja->',OF).


%======================================================================================
%2nd accusative object. Mirror rules for the first, but without uniqueness requirement

%object before finite verb.
head('VVFIN',OBJ,l,obja2,'VVFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN',OBJ,l,obja2,'VMFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN',OBJ,l,obja2,'VAFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,obja2,'VVIZU',[FC,_,_,_,UG,_],__-G,MF,MG,MF) :- objcandidate(OBJ,G), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%allow OBJAs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
head('VAINF', OBJ,l,obja2,'VAINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VAPP', OBJ,l,obja2,'VAPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VMINF', OBJ,l,obja2,'VMINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VMPP', OBJ,l,obja2,'VMPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VVINF', OBJ,l,obja2,'VVINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).
head('VVPP', OBJ,l,obja2,'VVPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_acc(MG,OBJ), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%relative pronoun (new transtag 'RC')
head('VVFIN','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,obja2,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWS',l,obja2,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('<-obja2<-',UG), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VMFIN','PWS',l,obja2,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

head('VAFIN','PWS',l,obja2,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('passive',FC), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,obja2,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_acc(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('passive',FC), \+ member('->obja2->',UG), \+ member('<-explobja<-',UG), \+ member('->explobja->',UG).



%object after finite verb.
head('VVFIN',OBJ,r,obja2,'VVFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VAFIN',OBJ,r,obja2,'VAFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VMFIN',OBJ,r,obja2,'VMFIN',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).

head('VVIMP',OBJ,r,obja2,'VVIMP',[_,GC,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_acc(MF,OBJ), \+ member('passive',GC), restrict_coord(OG), \+ member('<-obja2<-',OG), \+ member('->obja2->',OG), \+ member('->objp->',OG), \+ member('<-explobja<-',OG), \+ member('->explobja->',OG).


%======================================================================================
%Dative object, only one allowed    

%object before finite verb. 
head('VVFIN',OBJ,l,objd,'VVFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VMFIN',OBJ,l,objd,'VMFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VAFIN',OBJ,l,objd,'VAFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,objd,'VVIZU',[_,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%allow OBJDs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
head('VAINF', OBJ,l,objd,'VAINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('VAPP', OBJ,l,objd,'VAPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('VMINF', OBJ,l,objd,'VMINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('VMPP', OBJ,l,objd,'VMPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('VVINF', OBJ,l,objd,'VVINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).
head('VVPP', OBJ,l,objd,'VVPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_dat(MG,OBJ), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

%relative pronoun (new transtag 'RC')
head('VVFIN','PRELS',l,objd,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VMFIN','PRELS',l,objd,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VAFIN','PRELS',l,objd,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,objd,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objd<-',UG), \+ member('->objd->',UG).



%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWS',l,objd,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VMFIN','PWS',l,objd,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).

head('VAFIN','PWS',l,objd,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%only necessary in case of tagging errors
head('VVPP','PWS',l,objd,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_dat(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objd<-',UG), \+ member('->objd->',UG).


%die der partei nahestehenden wähler
head('ADJA', OBJ,l,objd,'ADJA',[_,_,_,_,OF,_],F-G,MF,MG,MF) :-  1 is F-G, derived_from_vpart(MF,'ADJA'), objcandidate(OBJ,F), case_dat(MG,OBJ), \+ endOfNP(F), \+ member('<-objd<-',OF), \+ member('->objd->',OF).

head('ADJA', OBJ,l,objd,'ADJD',[_,_,_,_,OF,_],F-G,MF,MG,MF) :-  1 is F-G, derived_from_vpart(MF,'ADJD'), objcandidate(OBJ,F), case_dat(MG,OBJ), \+ endOfNP(F), \+ member('<-objd<-',OF), \+ member('->objd->',OF).



%object after finite verb.
head('VVFIN',OBJ,r,objd,'VVFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).

head('VAFIN',OBJ,r,objd,'VAFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).

head('VMFIN',OBJ,r,objd,'VMFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).

head('VVIMP',OBJ,r,objd,'VVIMP',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_dat(MF,OBJ), restrict_coord(OG), \+ member('<-objd<-',OG), \+ member('->objd->',OG), \+ member('->objp->',OG).


%======================================================================================
%predicate noun (pred), only one allowed.


%predicate noun before finite verb.
head('VVFIN',Dtag,l,pred,'VVFIN',[FC,_,_,_,UG,_],_,MF,MG,MF)  :- predcand(Dtag), case_nom(MG,Dtag), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG).

head('VAFIN',Dtag,l,pred,'VAFIN',[FC,_,_,_,UG,_],_,MF,MG,MF)  :- predcand(Dtag), case_nom(MG,Dtag), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG).

head('VMFIN',Dtag,l,pred,'VMFIN',[FC,_,_,_,UG,_],_,MF,MG,MF)  :- predcand(Dtag), case_nom(MG,Dtag), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('<-pred<-',UG), \+ member('->pred->',UG).



head('VVFIN','ADJD',l,pred,'VVFIN',[FC,_,_,_,UG,_],_,MF,_,MF)  :- \+ member('<-pred<-',UG), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('->pred->',UG), \+ member('<-objd<-',UG).

head('VAFIN','ADJD',l,pred,'VAFIN',[FC,_,_,_,UG,_],_,MF,_,MF)  :- \+ member('<-pred<-',UG), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('->pred->',UG), \+ member('<-objd<-',UG).

head('VMFIN','ADJD',l,pred,'VMFIN',[FC,_,_,_,UG,_],_,MF,_,MF)  :- \+ member('<-pred<-',UG), restrict_vorfeld(FC,UG), \+ member('<-subj<-',UG), \+ member('<-adv<-',UG), \+ member('->pred->',UG), \+ member('<-objd<-',UG).


%predicate noun before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',Dtag,l,pred,'VVIZU',[_,_,_,_,UG,_],_,MF,MG,MF) :- predcand(Dtag), case_nom(MG,Dtag), \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG).

head('VVIZU','ADJD',l,pred,'VVIZU',[_,_,_,_,UG,_],_,MF,_,MF) :- \+ member('<-pred<-',UG), \+ member('->pred->',UG), \+ member('<-adv<-',UG), \+ member('<-subj<-',UG), \+ member('<-objd<-',UG).



%predicate noun after finite verb.
head('VVFIN',Dtag,r,pred,'VVFIN',[_,_,_,_,_,OG],_,MF,MG,MG)  :- predcand(Dtag), case_nom(MF,Dtag), restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG).

head('VAFIN',Dtag,r,pred,'VAFIN',[_,_,_,_,_,OG],_,MF,MG,MG)  :- predcand(Dtag), case_nom(MF,Dtag), restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG).

head('VMFIN',Dtag,r,pred,'VMFIN',[_,_,_,_,_,OG],_,MF,MG,MG)  :- predcand(Dtag), case_nom(MF,Dtag), restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG).

head('VVIMP',Dtag,r,pred,'VVIMP',[_,_,_,_,_,OG],_,MF,MG,MG)  :- predcand(Dtag), case_nom(MF,Dtag), restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG), \+ member('->objp->',OG).



head('VVFIN','ADJD',r,pred,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).

head('VAFIN','ADJD',r,pred,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).

head('VMFIN','ADJD',r,pred,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).

head('VVIMP','ADJD',r,pred,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-pred<-',OG), \+ member('->pred->',OG).


predcand('NN').
predcand('NE').
predcand('PIS').

%======================================================================================
%Genitive object, only one allowed    

%object before finite verb. 
head('VVFIN',OBJ,l,objg,'VVFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VMFIN',OBJ,l,objg,'VMFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VAFIN',OBJ,l,objg,'VAFIN',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%object before infinitive verb with 'zu' (there is no finite verb in infinitive clauses)
head('VVIZU',OBJ,l,objg,'VVIZU',[_,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%allow OBJDs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
head('VAINF', OBJ,l,objg,'VAINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VAPP', OBJ,l,objg,'VAPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VMINF', OBJ,l,objg,'VMINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VMPP', OBJ,l,objg,'VMPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VVINF', OBJ,l,objg,'VVINF',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VVPP', OBJ,l,objg,'VVPP',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).
head('VVIZU', OBJ,l,objg,'VVIZU',[FC,_,_,_,UG,_],_-G,MF,MG,MF) :- objcandidate(OBJ,G), verbchunklength(FC,1), case_gen(MG,OBJ), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%relative pronoun (new transtag 'RC')
head('VVFIN','PRELS',l,objg,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VMFIN','PRELS',l,objg,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VAFIN','PRELS',l,objg,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

%only necessary in case of tagging errors
head('VVPP','PRELS',l,objg,'RC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PRELS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWS',l,objg,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VMFIN','PWS',l,objg,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

head('VAFIN','PWS',l,objg,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), \+ member('<-objg<-',UG), \+ member('->objg->',UG).

%only necessary in case of tagging errors
head('VVPP','PWS',l,objg,'QC',[FC,_,_,_,UG,_],_,MF,MG,MF) :- case_gen(MG,'PWS'), restrict_vorfeld(FC,UG), verbchunklength(FC,1), \+ member('<-objg<-',UG), \+ member('->objg->',UG).


%object after finite verb.
head('VVFIN',OBJ,r,objg,'VVFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

head('VAFIN',OBJ,r,objg,'VAFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

head('VMFIN',OBJ,r,objg,'VMFIN',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

head('VVIMP',OBJ,r,objg,'VVIMP',[_,_,_,_,_,OG],F-_,MF,MG,MG)  :- objcandidate(OBJ,F), case_gen(MF,OBJ), restrict_coord(OG), \+ member('<-objg<-',OG), \+ member('->objg->',OG), \+ member('->objp->',OG).

%======================================================================================
%g(enitive) mod(ifier). only one on each side of the head allowed.

%Genitive modifier after head noun.

head('NN', GEN, r, gmod, 'NN',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'NN'))), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app->',OG).

head('NE', GEN, r, gmod, 'NE',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'NE'))), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app->',OG).

head('FM', GEN, r, gmod, 'FM',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'FM'))), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app->',OG).

head('PDS', GEN, r, gmod, 'PDS',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'PDS'))), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app->',OG).

head('PIS', GEN, r, gmod, 'PIS',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- (F-G > 1; (GEN = 'NE', \+ case_gen(MG,'PIS'))), validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG), \+ member('->app->',OG).


%no occurrences of these in exploration corpus. still leave in? 
%Yes for 'PWS': "Welcher der beiden ist der Mörder?" No for the others.
head('PWS', GEN, r, gmod, 'PWS',[_,_,_,_,OF,OG],F-G,MF,MG,MG) :- F-G > 1, validgmod(GEN), case_gen(MF,GEN), \+ member('->gmod->',OG), \+ member('<-gmod<-',OF), \+ member('->pp->',OG), \+ member('->kon->',OG).

%head('PPER', GEN, r, gmod, 'PPER',[_,_,_,_,_,OG],_,MF,MG,MG) :- validgmod(GEN), case_gen(MF), \+ member('->gmod->',OG).

%head('PRELS', GEN, r, gmod, 'PRELS',[_,_,_,_,_,OG],_,MF,MG,MG) :- validgmod(GEN), case_gen(MF), \+ member('->gmod->',OG).



%Genitive modfier before head noun. Pronoun heads seem to be ungrammatical: "Einer der Fischer" vs. *"Bremens einer".

head('NN', 'NE', l, gmod, 'NN',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('NE', 'NE', l, gmod, 'NE',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('FM', 'NE', l, gmod, 'FM',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NE'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).


head('NN', 'NN', l, gmod, 'NN',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('NE', 'NN', l, gmod, 'NE',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('FM', 'NN', l, gmod, 'FM',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'NN'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).


head('NN', 'FM', l, gmod, 'NN',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('NE', 'FM', l, gmod, 'NE',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).

head('FM', 'FM', l, gmod, 'FM',[_,_,_,_,UG,OG],_,MF,MG,MF) :- case_gen(MG,'FM'), \+ member('<-gmod<-',UG), \+ member('<-det<-',UG), \+ member('<-det<-',OG).


%======================================================================================
%EXPLetive 'es'


%EXPL after finite verb - takes position of OBJA or SUBJ
head('VVFIN','PPER',r,explsubj,'VVFIN',[_,_,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG).

head('VAFIN','PPER',r,explsubj,'VAFIN',[_,_,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG).

head('VMFIN','PPER',r,explsubj,'VMFIN',[_,_,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('->objp->',OG), \+ member('->pred->',OG).


head('VVFIN','PPER',r,explobja,'VVFIN',[_,GC,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('passive',GC), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG).

head('VAFIN','PPER',r,explobja,'VAFIN',[_,GC,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('passive',GC), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG).

head('VMFIN','PPER',r,explobja,'VMFIN',[_,GC,'es',_,_,OG],_,_,MG,MG)  :- restrict_coord(OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('passive',GC), \+ member('<-s<-',OG), \+ member('->s->',OG), \+ member('->objp->',OG).


%EXPL before finite verb - takes position of SUBJ
head('VVFIN','PPER',l,explsubj,'VVFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), (member('->subjc->',UG);member('->obji->',UG)), \+ member('<-subj<-',UG), \+ member('->subj->',UG).

head('VMFIN','PPER',l,explsubj,'VMFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), (member('->subjc->',UG);member('->obji->',UG)), \+ member('<-subj<-',UG), \+ member('->subj->',UG).

head('VAFIN','PPER',l,explsubj,'VAFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), (member('->subjc->',UG);member('->obji->',UG)), \+ member('<-subj<-',UG), \+ member('->subj->',UG).


%same, but with upper-case 'Es'
head('VVFIN','PPER',l,explsubj,'VVFIN',[FC,_,_,'Es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), member('->subjc->',UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG).

head('VMFIN','PPER',l,explsubj,'VMFIN',[FC,_,_,'Es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), member('->subjc->',UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG).


head('VAFIN','PPER',l,explsubj,'VAFIN',[FC,_,_,'Es',UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), member('->subjc->',UG), \+ member('<-subj<-',UG), \+ member('->subj->',UG).



%EXPL before finite verb - takes position of OBJA (only possible in Verbletztstellung)
head('VVFIN','PPER',l,explobja,'VVFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- \+ member('mainclause',FC), (member('->objc->',UG);member('->obji->',UG)), \+ member('passive',FC), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-obja<-',UG), \+ member('->obja->',UG).

head('VMFIN','PPER',l,explobja,'VMFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- \+ member('mainclause',FC), (member('->objc->',UG);member('->obji->',UG)), \+ member('passive',FC), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-obja<-',UG), \+ member('->obja->',UG).

head('VAFIN','PPER',l,explobja,'VAFIN',[FC,_,_,'es',UG,_],_,MF,_,MF) :- \+ member('mainclause',FC), (member('->objc->',UG);member('->obji->',UG)), \+ member('passive',FC), \+ member('<-s<-',UG), \+ member('->s->',UG), \+ member('<-obja<-',UG), \+ member('->obja->',UG).


%======================================================================================
%pp

%allows prepositional phrases to be enclosed by commas
head2('PP','$,',l,comma,'PP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- commaToRight(HID), \+ member('<-comma<-', HRels).

head2('PP','$,',r,comma,'PP',[_,_,_,_,HRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HRels), \+ member('->comma->', HRels).


%prepositional phrases postmodifying a np or pronoun
head('NN', 'PP',r,pp,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('->pp->',OG).

head('NE', 'PP',r,pp,'NE',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('->pp->',OG).

head('FM', 'PP',r,pp,'FM',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('->pp->',OG).

head('PIS', 'PP',r,pp,'PIS',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('->pp->',OG).

head('ADJD', 'PP',r,pp,'ADJD',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

%rare heads; little impact on performance
head('PPER', 'PP',r,pp,'PPER',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

head('PDS', 'PP',r,pp,'PDS',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

head('CARD', 'PP',r,pp,'CARD',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

head('PTKANT', 'PP',r,pp,'PTKANT',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

head('PWS', 'PP',r,pp,'PWS',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).

%included in case of tagging errors
head('ART', 'PP',r,pp,'PIS',[_,_,_,_,_,OG],F-G,_,MG,MG) :- 1 is F-G, \+ member('->pp->',OG).


%needs disambiguation, otherwise too many FPs
head('NN', PRO,r,pp,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), \+ member('->pp->',OG).

head('NE', PRO,r,pp,'NE',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), \+ member('->pp->',OG).

head('FM', PRO,r,pp,'FM',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), \+ member('->pp->',OG).

head('PIS', PRO,r,pp,'PIS',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), \+ member('->pp->',OG).

head('CARD', PRO,r,pp,'CARD',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), \+ member('->pp->',OG).



%prepositional phrase after verb
head('VVFIN', 'PP',r,pp,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).

head('VAFIN', 'PP',r,pp,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).

head('VMFIN', 'PP',r,pp,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).

head('VVIMP', 'PP',r,pp,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ (nth1(Pos,OG,'->pred->'), PosPred is Pos + 1, nth1(PosPred,OG,Pred), Pred =.. [Head|_], lexic(Head,_,HeadPos), checkPos(HeadPos,_,Tag,_,_), member(Tag,['NN','NE','ADV'])).



%there is some inconsistency concerning the tag for pronominal adverbs. I'm including all three possibilities I've encountered.
head('VVFIN', PRO,r,pp,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG).

head('VAFIN', PRO,r,pp,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG).

head('VMFIN', PRO,r,pp,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG).

head('VVIMP', PRO,r,pp,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG).



%other direction, prepositional phrase before verb
head('VVFIN', 'PP',l,pp,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VAFIN', 'PP',l,pp,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VMFIN', 'PP',l,pp,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VVIZU', 'PP',l,pp,'VVIZU',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).


%relative pronoun (new transtag 'RC')
head('VVFIN', 'PPREL',l,pp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VAFIN', 'PPREL',l,pp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VMFIN', 'PPREL',l,pp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).


%interrogative pronoun (new transtag 'QC')
head('VVFIN', 'PPQ',l,pp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VAFIN', 'PPQ',l,pp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VMFIN', 'PPQ',l,pp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).




%there is some inconsistency concerning the tag for pronominal adverbs. I'm including all three possibilities I've encountered.
head('VVFIN', PRO,l,pp,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF).

head('VAFIN', PRO,l,pp,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF).

head('VMFIN', PRO,l,pp,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF).


%allow PPs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
%example/motivation: das kind, 1999 in cottbus geboren, konnte schon klavier spielen.
head(NONFIN, 'PP',l,pp,NONFIN,[FC,_,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), verbchunklength(FC,1).
head(NONFIN, 'PROP',l,pp,NONFIN,[FC,_,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), verbchunklength(FC,1).
head(NONFIN, 'PAV',l,pp,NONFIN,[FC,_,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), verbchunklength(FC,1).
head(NONFIN, 'PROAV',l,pp,NONFIN,[FC,_,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), verbchunklength(FC,1).



%PP premodifying participial adjective (der auf dem boden liegende mann)
head('ADJA', 'PP',l,pp,'ADJA',[_,_,_,_,_,_],F-_,MF,_,MF) :- \+ endOfNP(F).
head('ADJD', 'PP',l,pp,'ADJD',[_,_,_,_,_,_],F-_,MF,_,MF) :- \+ endOfNP(F).

%daran angedockt
head('ADJA', PRO,l,pp,'ADJA',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G, proadv(PRO).
%head('ADJD', PRO,l,pp,'ADJD',[_,_,_,_,_,_],_,MF,_,MF) :- proadv(PRO).


%complex prepositions:
%bis auf weiteres
head('APPR', 'PP',r,pp,'PP',[_,_,_,_,_,_],F-G,_,MG,MG) :-  1 is F-G.

%bis dahin
head('APPR', PRO,r,pp,'PP',[_,_,_,_,_,_],F-G,_,MG,MG) :-  proadv(PRO),1 is F-G.


%======================================================================================
%objp (prepositions as complements)


%prepositional object after verb
head('VVFIN', 'PP',r,objp,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VAFIN', 'PP',r,objp,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VMFIN', 'PP',r,objp,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VVIMP', 'PP',r,objp,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).


%there is some inconsistency concerning the tag for pronominal adverbs. I'm including all three possibilities I've encountered.
head('VVFIN', PRO,r,objp,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VAFIN', PRO,r,objp,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VMFIN', PRO,r,objp,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).

head('VVIMP', PRO,r,objp,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- proadv(PRO), restrict_coord(OG), \+ member('<-objp<-',OG), \+ member('->objp->',OG).



%other direction, prepositional object before verb
head('VVFIN', 'PP',l,objp,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VAFIN', 'PP',l,objp,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VMFIN', 'PP',l,objp,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VVIZU', 'PP',l,objp,'VVIZU',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


%relative pronoun (new transtag 'RC')
head('VVFIN', 'PPREL',l,objp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VAFIN', 'PPREL',l,objp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VMFIN', 'PPREL',l,objp,'RC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


%interrogative pronoun (new transtag 'QC')
head('VVFIN', 'PPQ',l,objp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VAFIN', 'PPQ',l,objp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VMFIN', 'PPQ',l,objp,'QC',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


%OBJP modifying participial adjective (der auf dem boden liegende mann)
head('ADJA', 'PP',l,objp,'ADJA',[_,_,_,_,OF,_],F-_,MF,_,MF) :- \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF),\+ endOfNP(F).

head('ADJD', 'PP',l,objp,'ADJD',[_,_,_,_,OF,_],F-_,MF,_,MF) :- \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF),\+ endOfNP(F).



%there is some inconsistency concerning the tag for pronominal adverbs. I'm including all three possibilities I've encountered.
head('VVFIN', PRO,l,objp,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VAFIN', PRO,l,objp,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VMFIN', PRO,l,objp,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), restrict_vorfeld(FC,OF), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


%allow PPs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
%example/motivation: das kind, 1999 in cottbus geboren, konnte schon klavier spielen.
head('VAINF', 'PP',l,objp,'VAINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VAPP', 'PP',l,objp,'VAPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VMINF', 'PP',l,objp,'VMINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VMPP', 'PP',l,objp,'VMPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVINF', 'PP',l,objp,'VVINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVPP', 'PP',l,objp,'VVPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVIZU', 'PP',l,objp,'VVIZU',[FC,_,_,_,OF,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).

head('VAINF', PRO,l,objp,'VAINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VAPP', PRO,l,objp,'VAPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VMINF', PRO,l,objp,'VMINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VMPP', PRO,l,objp,'VMPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVINF', PRO,l,objp,'VVINF',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVPP', PRO,l,objp,'VVPP',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).
head('VVIZU', PRO,l,objp,'VVIZU',[FC,_,_,_,OF,_],_,MF,_,MF) :- proadv(PRO), verbchunklength(FC,1), \+ member('<-objp<-',OF), \+ member('->objp->',OF), \+ member('<-pp<-',OF).


%======================================================================================
%appositions


%noun with comma on its left could be apposition.
head('NN','$,',l,comma,'APPX',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).

head('NE','$,',l,comma,'APPX',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).

head('FM','$,',l,comma,'APPX',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).



head2('NN','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).

head2('NE','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).

head2('FM','$,',l,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :- (commaToRight(HID);stopToRight(HID)), \+ member('<-comma<-', HRels).


%comma at end of apposition allowed/included (but only if there is one on its left).

head('APPX','$,',r,comma,'APP',[_,_,_,_,_,OG],_,_,MG,MG) :- member('<-comma<-', OG), \+ member('->comma->', OG).

%head2('APP','$,',r,comma,'APP',[_,_,_,_,HRels,_,HID,_],_,HM,_,HM) :-  member('<-comma<-', HRels), \+ member('->comma->', HRels).


%appositions that are enclosed by comma are bound to noun on their left.
head('NN','APP',r,app,'NN',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NN',MF,'NN',MNew).

head('NE','APP',r,app,'NE',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NE',MF,'NN',MNew).

head('FM','APP',r,app,'FM',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'FM',MF,'NN',MNew).

head('PIS','APP',r,app,'PIS',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIS',MF,'NN',MNew).

head('PPER','APP',r,app,'PPER',[_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPER',MF,'NN',MNew).


%sentence-final appositions.
%scores go down slightly
% head2('NN','APPX',r,app,'NN',[_,_,_,_,_,_,_,DepID],_,HeadMorph,DepMorph,TransMorph) :- stopToRight(DepID), unify_case(HeadMorph,'NN',DepMorph,'NN',TransMorph).
% 
% head2('NE','APPX',r,app,'NE',[_,_,_,_,_,_,_,DepID],_,HeadMorph,DepMorph,TransMorph) :- stopToRight(DepID), unify_case(HeadMorph,'NE',DepMorph,'NN',TransMorph).
% 
% head2('FM','APPX',r,app,'FM',[_,_,_,_,_,_,_,DepID],_,HeadMorph,DepMorph,TransMorph) :- stopToRight(DepID), unify_case(HeadMorph,'FM',DepMorph,'NN',TransMorph).
% 
% head2('PIS','APPX',r,app,'PIS',[_,_,_,_,_,_,_,DepID],_,HeadMorph,DepMorph,TransMorph) :- stopToRight(DepID), unify_case(HeadMorph,'PIS',DepMorph,'NN',TransMorph).
% 
% head2('PPER','APPX',r,app,'PPER',[_,_,_,_,_,_,_,DepID],_,HeadMorph,DepMorph,TransMorph) :- stopToRight(DepID), unify_case(HeadMorph,'PPER',DepMorph,'NN',TransMorph).



%Anfang Oktober etc.: can be adverbial expression -> special metatag
head2('NN','NN',r,app,'NZEIT',[_,_,HeadWord,DepWord,HeadRels,DepRels,_,_],HeadPos-DepPos,HeadMorph,DepMorph,TransMorph) :- member(HeadWord,['Anfang','Mitte','Ende']), zeitcand(DepWord), -1 is HeadPos-DepPos, \+ member('<-det<-', DepRels), \+ member('<-attr<-', DepRels), \+ member('<-det<-', HeadRels), \+ member('<-attr<-', HeadRels), unify_case(HeadMorph,'NN',DepMorph,'NN',TransMorph).

head2('NN','CARD',r,app,'NZEIT',[_,_,HeadWord,_,HeadRels,DepRels,_,_],HeadPos-DepPos,HeadMorph,DepMorph,TransMorph) :- member(HeadWord,['Anfang','Mitte','Ende']), -1 is HeadPos-DepPos, \+ member('<-det<-', DepRels), \+ member('<-attr<-', DepRels), \+ member('<-det<-', HeadRels), \+ member('<-attr<-', HeadRels), unify_case(HeadMorph,'NN',DepMorph,'NN',TransMorph).


%normal case. either apposition is enclosed in brackets, or it is a narrow apposition.
head(HTag,'NE',r,app,HTag,[_,_,_,_,OF,_],_,MF,MG,MNew) :- apphead(HTag), ((member('<-bracket<-', OF),member('->bracket->', OF));(\+ member('<-det<-', OF), \+ member('<-attr<-', OF))), unify_case(MG,HTag,MF,'NE',MNew).

%der bürgermeister meier vs. der internet browser: if last element is nn (but not if ne), use it for np agreement.
head(HTag,'NN',r,app,HTag,[_,_,_,_,OF,_],_,MF,MG,MNew) :- apphead(HTag), ((member('<-bracket<-', OF),member('->bracket->', OF));(\+ member('<-det<-', OF), \+ member('<-attr<-', OF))), unify_case(MF,'NN',MG,HTag,MNew).

head(HTag,'FM',r,app,HTag,[_,_,_,_,OF,_],_,MF,MG,MNew) :- apphead(HTag), ((member('<-bracket<-', OF),member('->bracket->', OF));(\+ member('<-det<-', OF), \+ member('<-attr<-', OF))), unify_case(MF,'FM',MG,HTag,MNew).

head(HTag,'CARD',r,app,HTag,[_,_,_,_,_,_],_,_,MG,MG) :- apphead(HTag).



apphead('PPER').
apphead('PIS').
apphead('FM').
apphead('NN').
apphead('NE').


%======================================================================================
%relative clauses


%allows relative clauses to be enclosed by commas
head2('RC','$,',r,comma,'RC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head2('RC','$,',l,comma,'RC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).


%allows question clauses to be enclosed by commas
head2('QC','$,',l,comma,'QC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('QC','$,',r,comma,'QC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).


%attaches relative clause to last noun, pronoun or verb. better attachment strategies only available through morphological information and in postprocessing ("er hat den Mann aus Zürich gekannt, der gestorben ist")

head('NN','RC',r,rel,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('NE','RC',r,rel,'NE',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('FM','RC',r,rel,'FM',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PDS','RC',r,rel,'PDS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PIS','RC',r,rel,'PIS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PPER','RC',r,rel,'PPER',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PPOSS','RC',r,rel,'PPOSS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVFIN','RC',r,rel,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','RC',r,rel,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','RC',r,rel,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','RC',r,rel,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('RC','RC',r,rel,'RC',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('NEB','RC',r,rel,'NEB',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


%question clauses can be relative ones
head('NN','QC',r,rel,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('NE','QC',r,rel,'NE',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('FM','QC',r,rel,'FM',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PDS','QC',r,rel,'PDS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PIS','QC',r,rel,'PIS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PPER','QC',r,rel,'PPER',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PPOSS','QC',r,rel,'PPOSS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVFIN','QC',r,rel,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','QC',r,rel,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','QC',r,rel,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','QC',r,rel,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('RC','QC',r,rel,'RC',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('NEB','QC',r,rel,'NEB',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


%======================================================================================
%subordinating conjunctions

%conjunctions: wenn, obwohl etc.
head('VVFIN','KOUS',l,konjneb,'NEB',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','KOUS',l,konjneb,'NEB',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','KOUS',l,konjneb,'NEB',[_,_,_,_,_,_],_,MF,_,MF).

head('ADJD','KOUS',l,konjneb,'NEB',[_,_,_,_,_,_],_,MF,_,MF).


%consider possibility of tagging error
head('VVPP','KOUS',l,konjneb,'PPNEB',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVPP','KOKOM',l,konjneb,'PPNEB',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).

%um/statt/ohne + inf
head('VVIZU','KOUI',l,konjneb,'NEB',[_,_,_,_,_,_],_,MF,_,MF).


%ohne daß: whole construction is finite.
head('KOUS','KOUI',l,konjneb,'KOUS',[_,_,'daß','Ohne',_,_],F-G,MF,_,MF) :- 1 is F-G.
head('KOUS','KOUI',l,konjneb,'KOUS',[_,_,'daß',ohne,_,_],F-G,MF,_,MF) :- 1 is F-G.
head('KOUS','KOUI',l,konjneb,'KOUS',[_,_,dass,'Ohne',_,_],F-G,MF,_,MF) :- 1 is F-G.
head('KOUS','KOUI',l,konjneb,'KOUS',[_,_,dass,ohne,_,_],F-G,MF,_,MF) :- 1 is F-G.

% als ob: same idea as "ohne dass"
head2('KOUS','KOUS',l,konjneb,'KOUS',[_,_,ob,als,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.
head2('KOUS','KOUS',l,konjneb,'KOUS',[_,_,ob,'Als',_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.


%======================================================================================
%subordinated clauses

%allow commas to enclose a subordinated clause
head2('NEB','$,',l,comma,'NEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('NEB','$,',r,comma,'NEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

%allow commas to enclose a subordinated clause
%head(NONFIN,'$,',r,comma,'NEB',[_,GC,_,_,_,_],_,_,MG,MG) :- nonfinite(NONFIN), length(GC,1).


%subordinated clause before main clause
head('VVFIN','NEB',l,neb,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VMFIN','NEB',l,neb,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).

head('VAFIN','NEB',l,neb,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- restrict_vorfeld(FC,OF).


%subordinated clause after main clause
head('VVFIN','NEB',r,neb,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','NEB',r,neb,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','NEB',r,neb,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','NEB',r,neb,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).



%======================================================================================
%clausal object
%dass, ob, indirect questions, quotes.
%TODO: quotes. subjc

%clausal objects. mostly dass
head('VVFIN','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,_],_,MF,_,MF).

head('ADJD','KOUS',l,konjobjc,'OBJC',[_,_,_,_,_,_],_,MF,_,MF).


%consider possibility of tagging error
head('VVPP','KOUS',l,konjobjc,'OBJC',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).


% als ob: same idea as "ohne dass"
head2('KOUS','KOUS',l,konjobjc,'KOUS',[_,_,ob,als,_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.
head2('KOUS','KOUS',l,konjobjc,'KOUS',[_,_,ob,'Als',_,_,_,_],H-D,MH,_,MH) :- 1 is H-D.


%clausal object before main clause. Takes the place of the accusative object (except for reflexive ones. Exception could be implemented, but causes new problems)

head('VVFIN',OBJC,l,objc,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('passive',FC), \+ member('<-obja<-',OF), \+ member('->obja->',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF).

head('VMFIN',OBJC,l,objc,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('passive',FC), \+ member('<-obja<-',OF), \+ member('->obja->',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF).

head('VAFIN',OBJC,l,objc,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('passive',FC), \+ member('<-obja<-',OF), \+ member('->obja->',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF).


%clausal object after main clause
head('VVFIN',OBJC,r,objc,'VVFIN',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('VMFIN',OBJC,r,objc,'VMFIN',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('VAFIN',OBJC,r,objc,'VAFIN',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG),  (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('VVIMP',OBJC,r,objc,'VVIMP',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG),  (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).


%clausal object doesn't have to depend on main clause:
head('RC',OBJC,r,objc,'RC',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).

head('NEB',OBJC,r,objc,'NEB',[_,GC,_,_,_,OG],_,_,MG,MG) :-  objcsubjc(OBJC), \+ member('passive',GC), restrict_coord(OG), (\+ member('<-obja<-',OG), \+ member('->obja->',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG);conjoined_vp(OG)).


%die Möglichkeit, dass...
head('NN',OBJC,r,objc,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), \+ member('<-objc<-',OG), \+ member('->objc->',OG).


%darauf, dass...
head('PROP',OBJC,r,objc,'PROP',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), \+ member('<-objc<-',OG), \+ member('->objc->',OG).
head('PAV',OBJC,r,objc,'PAV',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), \+ member('<-objc<-',OG), \+ member('->objc->',OG).
head('PROAV',OBJC,r,objc,'PROAV',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), \+ member('<-objc<-',OG), \+ member('->objc->',OG).

%allow commas to enclose a clausal object
head2('OBJC','$,',r,comma,'OBJC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head2('OBJC','$,',l,comma,'OBJC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).


%=====================================================================================
%clausal subjects

%clausal subject before main clause. Takes the place of the subject

head('VVFIN',OBJC,l,subjc,'VVFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('<-subj<-',OF), \+ member('->subj->',OF), \+ member('<-subjc<-',OF), \+ member('->subjc->',OF).

head('VMFIN',OBJC,l,subjc,'VMFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('<-subj<-',OF), \+ member('->subj->',OF), \+ member('<-subjc<-',OF), \+ member('->subjc->',OF).

head('VAFIN',OBJC,l,subjc,'VAFIN',[FC,_,_,_,OF,_],_,MF,_,MF) :- objcsubjc(OBJC), restrict_vorfeld(FC,OF), \+ member('<-subj<-',OF), \+ member('->subj->',OF), \+ member('<-subjc<-',OF), \+ member('->subjc->',OF).


%clausal subject after main clause
head('VVFIN',OBJC,r,subjc,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).

head('VMFIN',OBJC,r,subjc,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).

head('VAFIN',OBJC,r,subjc,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).


%clausal subject doesn't have to depend on main clause:
head('RC',OBJC,r,subjc,'RC',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).

head('NEB',OBJC,r,subjc,'NEB',[_,_,_,_,_,OG],_,_,MG,MG) :- objcsubjc(OBJC), restrict_coord(OG), \+ member('<-subj<-',OG), \+ member('->subj->',OG), \+ member('<-subjc<-',OG), \+ member('->subjc->',OG).




%======================================================================================
%infinitive object

%infinitive objects are bound by comma and need the infinitive particle 'zu'.
head('VVIZU','$,',l,comma,'VVIZU',[_,_,_,_,OF,_],_,MF,_,MF)  :- \+ member('<-comma<-', OF), \+ member('->comma->', OF).


%infinitive objects depend on finite verb on their left. exception: topicalisation - implement?
head('VVFIN','VVIZU',r,obji,'VVFIN',[FC,GC,_,_,OF,OG],_,_,MG,MG) :- (\+ FC = GC; member('<-comma<-',OF)), restrict_coord(OG).

head('VAFIN','VVIZU',r,obji,'VAFIN',[FC,GC,_,_,OF,OG],_,_,MG,MG) :- (\+ FC = GC; member('<-comma<-',OF)), restrict_coord(OG).

head('VMFIN','VVIZU',r,obji,'VMFIN',[FC,GC,_,_,OF,OG],_,_,MG,MG) :- (\+ FC = GC; member('<-comma<-',OF)), restrict_coord(OG).

head('VVIMP','VVIZU',r,obji,'VVIMP',[FC,GC,_,_,OF,OG],_,_,MG,MG) :- (\+ FC = GC; member('<-comma<-',OF)), restrict_coord(OG).

%Noun can have infinitive object, but should be separated by comma -> competition with other functions of NNs
head('NN','VVIZU',r,obji,'NN',[_,_,_,_,OF,_],_,_,MG,MG) :- member('<-comma<-',OF).


%ein nicht enden wollender Krieg
head('ADJA', 'VVINF',l,obji,'ADJA',[_,_,'wollend',_,_,_],F-G,MF,_,MF) :- 1 is F-G.


%experimentieren lassen, kommen sehen usw.
%TüBa sometimes gives the tag "aux". We follow Foth in using obji
head('VVFIN',NONFIN,l,obji,'VVFIN', [_,GChunk,FF,_,_,OG],_,MF,_,MF) :- modallike(FF), nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('->comma->', OG)), verbchunklength(GChunk,1).
head('VVINF',NONFIN,l,obji,'VVINF', [_,GChunk,FF,_,_,_],_,MF,_,MF) :- modallike(FF), nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(GChunk,1).
head('VVPP',NONFIN,l,obji,'VVPP', [_,GChunk,FF,_,_,_],_,MF,_,MF) :- modallike(FF), nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(GChunk,1).
head('VVFIN',NONFIN,r,obji,'VVFIN', [FChunk,_,_,FG,OF,_],_,_,MG,MG) :- modallike(FG), nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('<-comma<-', OF)), verbchunklength(FChunk,1).


%full verbs that act like modal verbs
modallike('lassen') :- !.
modallike('bleiben') :- !.
modallike('gehen') :- !.
modallike('helfen') :- !.
modallike('lehren') :- !.
modallike('lernen') :- !.
modallike('sehen') :- !.
modallike('hören') :- !.

%======================================================================================
%adverbs. needs to be more permissive later on.


%adverb before finite verb
head('VVFIN','ADV',l,adv,'VVFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','ADV',l,adv,'VAFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','ADV',l,adv,'VMFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VVIZU','ADV',l,adv,'VVIZU',[_,_,_,_,_,_],_,MF,_,MF).


head('VVFIN','ADJD',l,adv,'VVFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','ADJD',l,adv,'VAFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','ADJD',l,adv,'VMFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VVIZU','ADJD',l,adv,'VVIZU',[_,_,_,_,_,_],_,MF,_,MF).


head('VVFIN','PTKNEG',l,adv,'VVFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','PTKNEG',l,adv,'VAFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','PTKNEG',l,adv,'VMFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VVIZU','PTKNEG',l,adv,'VVIZU',[_,_,_,_,_,_],_,MF,_,MF).


%answer particle. Included because of tagging errors:
%example: das ist ja toll
head('VVFIN','PTKANT',l,adv,'VVFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','PTKANT',l,adv,'VAFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','PTKANT',l,adv,'VMFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VVIZU','PTKANT',l,adv,'VVIZU',[_,_,_,_,_,_],_,MF,_,MF).



%weil ich ein wenig schüchtern bin
head('VVFIN','PIS',l,adv,'VVFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','PIS',l,adv,'VAFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','PIS',l,adv,'VMFIN',[_,_,_,_,_,_],_,MF,_,MF).

head('VVIZU','PIS',l,adv,'VVIZU',[_,_,_,_,_,_],_,MF,_,MF).



%interrogative pronoun (new transtag 'QC')
head('VVFIN','PWAV',l,adv,'QC',[_,_,_,_,_,_],_,MF,_,MF).

head('VAFIN','PWAV',l,adv,'QC',[_,_,_,_,_,_],_,MF,_,MF).

head('VMFIN','PWAV',l,adv,'QC',[_,_,_,_,_,_],_,MF,_,MF).

%only necessary in case of tagging errors
head('VVPP','PWAV',l,adv,'QC',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).



%allow adverbs before nonfinite verb if no matching finite verb is found in preprocessing (-> chunk has only one element).
%example/motivation: das kind, 1999 geboren, konnte schon klavier spielen.
head('VAINF','ADV',l,adv,'VAINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VAPP','ADV',l,adv,'VAPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMINF','ADV',l,adv,'VMINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMPP','ADV',l,adv,'VMPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVINF','ADV',l,adv,'VVINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVPP','ADV',l,adv,'VVPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVIZU','ADV',l,adv,'VVIZU',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).

head('VAINF','ADJD',l,adv,'VAINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VAPP','ADJD',l,adv,'VAPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMINF','ADJD',l,adv,'VMINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMPP','ADJD',l,adv,'VMPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVINF','ADJD',l,adv,'VVINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVPP','ADJD',l,adv,'VVPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVIZU','ADJD',l,adv,'VVIZU',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).

head('VAINF','PTKNEG',l,adv,'VAINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VAPP','PTKNEG',l,adv,'VAPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMINF','PTKNEG',l,adv,'VMINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMPP','PTKNEG',l,adv,'VMPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVINF','PTKNEG',l,adv,'VVINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVPP','PTKNEG',l,adv,'VVPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVIZU','PTKNEG',l,adv,'VVIZU',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).

head('VAINF','PTKANT',l,adv,'VAINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VAPP','PTKANT',l,adv,'VAPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMINF','PTKANT',l,adv,'VMINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VMPP','PTKANT',l,adv,'VMPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVINF','PTKANT',l,adv,'VVINF',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVPP','PTKANT',l,adv,'VVPP',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).
head('VVIZU','PTKANT',l,adv,'VVIZU',[FC,_,_,_,_,_],_,MF,_,MF) :- verbchunklength(FC,1).


%adverb after finite verb
head('VVFIN','ADV',r,adv,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','ADV',r,adv,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','ADV',r,adv,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','ADV',r,adv,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).



head('VVFIN','ADJD',r,adv,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','ADJD',r,adv,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','ADJD',r,adv,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','ADJD',r,adv,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).



head('VVFIN','PTKNEG',r,adv,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','PTKNEG',r,adv,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','PTKNEG',r,adv,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','PTKNEG',r,adv,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


%sie sind alle tot.

head('VVFIN','PIS',r,adv,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','PIS',r,adv,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','PIS',r,adv,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','PIS',r,adv,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('NN','PIS',r,adv,'NN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PDS','PIS',r,adv,'PDS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PPER','PIS',r,adv,'PPER',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PRELS','PIS',r,adv,'PRELS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('PWS','PIS',r,adv,'PWS',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).




%answer particle. Included because of tagging errors:
%example: das ist ja toll
head('VVFIN','PTKANT',r,adv,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','PTKANT',r,adv,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','PTKANT',r,adv,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','PTKANT',r,adv,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


%'nonverbal' adverbials: inclusion of tag pairs based on bigram statistics (only tag pairs with high probability of 'adv' included). other possibility: include more/all pairs here and give them lower probability value.


%adverbial particles: "am besten"..
head('ADJD', 'PTKA', l, adv, 'ADJD',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA', 'PTKA', l, adv, 'ADJA',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADV', 'PTKA', l, adv, 'ADV',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIAT', 'PTKA', l, adv, 'PIAT',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIDAT', 'PTKA', l, adv, 'PIDAT',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIS', 'PTKA', l, adv, 'PIS',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.


%'zu' might be mistagged as preposition
head('ADJD', _, l, adv, 'ADJD',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA', _, l, adv, 'ADJA',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADV', _, l, adv, 'ADV',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIAT', _, l, adv, 'PIAT',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIDAT', _, l, adv, 'PIDAT',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIS', _, l, adv, 'PIS',[_,_,_,zu,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJD', _, l, adv, 'ADJD',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA', _, l, adv, 'ADJA',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADV', _, l, adv, 'ADV',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIAT', _, l, adv, 'PIAT',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIDAT', _, l, adv, 'PIDAT',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIS', _, l, adv, 'PIS',[_,_,_,'Zu',_,_],F-G,MF,_,MF) :- 1 is F-G.


%dealing with mistaggings of 'am' as in 'am besten' (at the moment, pn/pp usually has better prob than adv/adv).
head('ADJD','APPR', l, adv, 'ADJD',[_,_,_,am,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJD','APPR', l, adv, 'ADJD',[_,_,_,'an-der',_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA','APPR', l, adv, 'ADJD',[_,_,_,am,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA','APPR', l, adv, 'ADJD',[_,_,_,'an-der',_,_],F-G,MF,_,MF) :- 1 is F-G.



%"mehr als x"
head('KOMPX','ADV',l,adv,'KOMPX', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.


%sehr gut
head('ADJD', 'ADV', l, adv, 'ADJD',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADJA', 'ADV', l, adv, 'ADJA',[_,_,_,_,_,_],_,MF,_,MF).

head('KOUS', 'ADV', l, adv, 'KOUS',[_,_,_,_,_,_],_,MF,_,MF).

head('NN', 'ADV', l, adv, 'NN',[_,_,_,_,_,_],_,MF,_,MF).

head('NE', 'ADV', l, adv, 'NE',[_,_,_,_,_,_],_,MF,_,MF).

head('APPR', 'ADV', l, adv, 'APPR',[_,_,_,_,_,_],_,MF,_,MF).

head('APPRART', 'ADV', l, adv, 'APPRART',[_,_,_,_,_,_],_,MF,_,MF).

head('ADV', 'ADV', l, adv, 'ADV',[_,_,_,_,_,_],_,MF,_,MF).

head('PTKNEG', 'ADV', l, adv, 'PTKNEG',[_,_,_,_,_,_],_,MF,_,MF).

head('CARD', 'ADV', l, adv, 'CARD',[_,_,_,_,_,_],_,MF,_,MF).

head('PDS', 'ADV', l, adv, 'PDS',[_,_,_,_,_,_],_,MF,_,MF).

head('PIAT', 'ADV', l, adv, 'PIAT',[_,_,_,_,_,_],_,MF,_,MF).

head('PIDAT', 'ADV', l, adv, 'PIDAT',[_,_,_,_,_,_],_,MF,_,MF).

head('PIS', 'ADV', l, adv, 'PIS',[_,_,_,_,_,_],_,MF,_,MF).

head('PPER', 'ADV', l, adv, 'PPER',[_,_,_,_,_,_],_,MF,_,MF).

% head(Tag, 'ADV', l, adv, Tag,[_,_,_,_,_,_],_,MF,_,MF).
% head(Tag, 'ADV', r, adv, Tag,[_,_,_,_,_,_],_,MF,_,MF).



head('ADJA', 'PTKNEG', l, adv, 'ADJA',[_,_,_,_,_,_],_,MF,_,MF).

head('ADJD', 'PTKNEG', l, adv, 'ADJD',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('ADV', 'PTKNEG', l, adv, 'ADV',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('KOUS', 'PTKNEG', l, adv, 'KOUS',[_,_,_,_,_,_],_,MF,_,MF).

head('CARD', 'PTKNEG', l, adv, 'CARD',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('NN', 'PTKNEG', l, adv, 'NN',[_,_,_,_,_,_],F-G,MF,_,MF) :- 2 >= F-G.

head('NE', 'PTKNEG', l, adv, 'NE',[_,_,_,_,_,_],F-G,MF,_,MF) :- 2 >= F-G.

head('APPR', 'PTKNEG', l, adv, 'APPR',[_,_,_,_,_,_],F-G,MF,_,MF) :- 2 >= F-G.

head('APPRART', 'PTKNEG', l, adv, 'APPRART',[_,_,_,_,_,_],F-G,MF,_,MF) :- 2 >= F-G.


%little hack: use PWAV as transtag to make sure whole thing gets recognised as question
head('ADJD', 'PWAV', l, adv, 'PWAV',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('PIDAT', 'PWAV', l, adv, 'PWAV',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.


%betriebswirtschaftlich günstig
head('ADJA', 'ADJD', l, adv, 'ADJA',[_,_,_,_,_,_],_,MF,_,MF).

head('ADJD', 'ADJD', l, adv, 'ADJD',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

%most of these are very rare, but there are possibly tagging errors (and not allowing these blocks analysis of non-local dependencies)
head('KOUS', 'ADJD', l, adv, 'KOUS',[_,_,_,_,_,_],_,MF,_,MF).

head('NN', 'ADJD', l, adv, 'NN',[_,_,_,_,_,_],_,MF,_,MF).

head('NE', 'ADJD', l, adv, 'NE',[_,_,_,_,_,_],_,MF,_,MF).

head('APPR', 'ADJD', l, adv, 'APPR',[_,_,_,_,_,_],_,MF,_,MF).

head('APPRART', 'ADJD', l, adv, 'APPRART',[_,_,_,_,_,_],_,MF,_,MF).

head('ADV', 'ADJD', l, adv, 'ADV',[_,_,_,_,_,_],_,MF,_,MF).

head('PTKNEG', 'ADJD', l, adv, 'PTKNEG',[_,_,_,_,_,_],_,MF,_,MF).

head('CARD', 'ADJD', l, adv, 'CARD',[_,_,_,_,_,_],_,MF,_,MF).

head('PDS', 'ADJD', l, adv, 'PDS',[_,_,_,_,_,_],_,MF,_,MF).

head('PIAT', 'ADJD', l, adv, 'PIAT',[_,_,_,_,_,_],_,MF,_,MF).

head('PIDAT', 'ADJD', l, adv, 'PIDAT',[_,_,_,_,_,_],_,MF,_,MF).

head('PIS', 'ADJD', l, adv, 'PIS',[_,_,_,_,_,_],_,MF,_,MF).

head('PPER', 'ADJD', l, adv, 'PPER',[_,_,_,_,_,_],_,MF,_,MF).


%seit wann
%little hack: use PWAV as transtag to make sure whole thing gets recognised as question
head('APPR','PWAV',r,adv,'PWAV',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.


%'seit heute'.
head('APPR','ADV',r,adv,'PP',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G, endOfNP(F).
head('APPRART','ADV',r,adv,'PP',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G, endOfNP(F).

head('PTKNEG', 'ADV', r, adv, 'PTKNEG',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.
head('PWAV', 'ADV', r, adv, 'PWAV',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.
head('PWS', 'ADV', r, adv, 'PWS',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.

%sowohl als auch...
head(_,'ADV',r,adv,'KON',[_,_,auch,als,_,_],F-G,_,MG,MG) :- 1 is F-G.


%nur, weil ich es sage.
head('PTKNEG', '$,', r, comma, 'ADVKOUS',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).
head('ADV', '$,', r, comma, 'ADVKOUS',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).
head('KOUS', 'ADVKOUS', l, adv, 'KOUS',[_,_,_,_,_,_],_,MF,_,MF).

%======================================================================================
%auxiliary verbs



%rules only work if verb chunking done in preprocessing.

head('VAFIN',NONFIN,r,aux,'VAFIN', [FChunk,GChunk,_,_,OF,_],_,_,MG,MG) :- nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('<-comma<-', OF)), FChunk = GChunk.

head('VMFIN',NONFIN,r,aux,'VMFIN', [FChunk,GChunk,_,_,OF,_],_,_,MG,MG) :- nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('<-comma<-', OF)), FChunk = GChunk.


head('VAFIN',NONFIN,l,aux,'VAFIN', [FChunk,GChunk,_,_,_,OG],_,MF,_,MF) :- nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('->comma->', OG)), FChunk = GChunk.

head('VMFIN',NONFIN,l,aux,'VMFIN', [FChunk,GChunk,_,_,_,OG],_,MF,_,MF) :- nonfinite(NONFIN), \+ (NONFIN = 'VVIZU', member('->comma->', OG)), FChunk = GChunk.


% FChunk = GChunk (preprocessing) may fail in cases such as "er muss erreichbar sein und *telefonieren können*"
head('VAINF',NONFIN,l,aux,'VAINF', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('VAPP',NONFIN,l,aux,'VAPP', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('VMINF',NONFIN,l,aux,'VMINF', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('VMPP',NONFIN,l,aux,'VMPP', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('VVINF',NONFIN,l,aux,'VVINF', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).
head('VVPP',NONFIN,l,aux,'VVPP', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- nonfinite(NONFIN), \+ NONFIN = 'VVIZU', verbchunklength(FChunk,1), verbchunklength(GChunk,1).


%"um geprüft zu werden / um haben zu wollen".
head('VVIZU','VAINF',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.
head('VVIZU','VAPP',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.
head('VVIZU','VMINF',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.
head('VVIZU','VMPP',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.
head('VVIZU','VVINF',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.
head('VVIZU','VVPP',l,aux,'VVIZU', [FChunk,GChunk,_,_,_,_],_,MF,_,MF) :- FChunk = GChunk.



%======================================================================================
%verb particles

head('VVFIN','PTKVZ',l,avz,'VVFIN', [_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('VVFIN','PTKVZ',r,avz,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


%new transtag to simplify other attachment rules. 
head('VVINF','PTKZU',l,part,'VVIZU',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('VAINF','PTKZU',l,part,'VVIZU',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

head('VMINF','PTKZU',l,part,'VVIZU',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.


%die zu erwartenden Störmanöver
head('ADJA','PTKZU',l,part,'ADJA',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G.

%might be useful in case of tagging errors
%head('ADJA','APPR',l,part,'ADJA',[_,_,_,'zu',_,_],F-G,MF,_,MF) :- 1 is F-G.



%======================================================================================
%circumposition particle

%(doesn't exist in exploration corpus).

head('PP','APZR',r,part,'PP',[_,_,_,_,_,_],_,_,MG,MG).


%======================================================================================
%ART + PIS ("ein wenig"; "ein anderer") marked as particle

head('PIS','ART',l,part,'PIS',[_,_,_,_,_,_],F-G,MF,_,MF) :- 2 >= F-G.


%in because of tagging errors
head('PIDAT','ART',l,part,'PIS',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G, endOfNP(F).

head('ADV','ART',l,part,'PIS',[_,_,_,_,_,_],F-G,MF,_,MF) :- 1 is F-G, endOfNP(F).


%======================================================================================
%comparatives



head('KOKOM','NN',r,cj,'KOMPX', [_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','NE',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','FM',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','ADJA',r,cj,'KOMPX',[_,_,_,_,_,_],_-G,_,MG,MG) :- endOfNP(G).

head('KOKOM','ADJD',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','PP',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','VVFIN',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

head('KOKOM','PIS',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).

%too many FPs

% head('KOKOM','PPER',r,cj,'KOMPX',[_,_,_,_,_,_],_,_,MG,MG).


%härter als stahl; kraftvoll wie immer
head('ADJA','KOMPX',r,kom,'ADJA',[_,_,Komp,_,_,_],_,_,MG,MG) :- Komp=als -> degree_comp(MG,'ADJA');true.

head('ADJD','KOMPX',r,kom,'ADJD',[_,_,Komp,_,_,_],_,_,MG,MG) :- Komp=als -> degree_comp(MG,'ADJD');true.

head('ADV','KOMPX',r,kom,'ADV',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.


%ein Mann wie stahl
head('NN','KOMPX',r,kom,'NN',[_,_,_,_,_,_],_,_,MG,MG).

head('NE','KOMPX',r,kom,'NE',[_,_,_,_,_,_],_,_,MG,MG).

head('FM','KOMPX',r,kom,'FM',[_,_,_,_,_,_],_,_,MG,MG).


%so etwas wie Würde
head('PIS','KOMPX',r,kom,'PIS',[_,_,_,_,_,_],F-G,_,MG,MG) :- 1 is F-G.

%Er als Tierfreund
head('PPER','KOMPX',r,kom,'PPER',[_,_,_,_,_,_],F-G,_,MG,MG) :- case_nom(MG,'PPER'), 1 is F-G.



%sich als Unschuldig ausgeben
head('VVFIN','KOMPX',r,kom,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VAFIN','KOMPX',r,kom,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VMFIN','KOMPX',r,kom,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).

head('VVIMP','KOMPX',r,kom,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- restrict_coord(OG).


head('VVFIN','KOMPX',l,kom,'VVFIN',[FC,_,_,_,_,UG],_,MF,_,MF) :- restrict_vorfeld(FC,UG).

head('VAFIN','KOMPX',l,kom,'VAFIN',[FC,_,_,_,_,UG],_,MF,_,MF) :- restrict_vorfeld(FC,UG).

head('VMFIN','KOMPX',l,kom,'VMFIN',[FC,_,_,_,_,UG],_,MF,_,MF) :- restrict_vorfeld(FC,UG).

head('VAINF','KOMPX',l,kom,'VAINF',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VAPP','KOMPX',l,kom,'VAPP',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VMINF','KOMPX',l,kom,'VMINF',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VMPP','KOMPX',l,kom,'VMPP',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VVINF','KOMPX',l,kom,'VVINF',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VVPP','KOMPX',l,kom,'VVPP',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).
head('VVIZU','KOMPX',l,kom,'VVIZU',[FC,_,_,_,_,UG],_,MF,_,MF) :- verbchunklength(FC,1), restrict_vorfeld(FC,UG).

%======================================================================================
%conjunction


%(peter) 'und' ->cj-> 'mark'. special: new morphological information is that of dependent, not that of head
head('KON',Tag,r,cj,Transtag,[_,_,_,Word,_,_],_,MF,_,MF) :- kon_mapping(Tag,Transtag), \+ member(Word,['Sowohl',sowohl,'so|wohl',weder,'Weder',entweder,'Entweder']).


%allows comma before conjunction.
head('KON', '$,', l, comma, 'KON',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).


%X, Y etc.
head(Tag,'ADV',r,kon,Tag,[_,_,'etc.',_,_,_],_,MF,_,MF).
head(Tag,'ADV',r,kon,Tag,[_,_,'usw.',_,_,_],_,MF,_,MF).

%'und so weiter'
head2(_,_,l,adv,'CJ',[_,_,weiter,so,_,_,_,_],F-_,_,_,_) :- LeftPos is F-1, checkPos(LeftPos,und,_,_,_).
head2(_,'CJ',r,cj,'KON_ANY',[_,_,und,weiter,_,_,_,_],_,_,_,_).

%'sondern auch'
head2(_,_,r,cj,'KON',[_,_,sondern,auch,_,_,_,_],_,_,_,_).


%"sowohl als auch" ; "entweder oder"; "weder noch"
head(Tag, 'KON', l, kon, Tag,[_,_,_,sowohl,OF,_],_,MF,_,MF) :- member('->kon->', OF).
head(Tag, 'KON', l, kon, Tag,[_,_,_,'Sowohl',OF,_],_,MF,_,MF) :- member('->kon->', OF).
head(Tag, 'KON', l, kon, Tag,[_,_,_,entweder,OF,_],_,MF,_,MF) :- member('->kon->', OF).
head(Tag, 'KON', l, kon, Tag,[_,_,_,'Entweder',OF,_],_,MF,_,MF) :- member('->kon->', OF).
head(Tag, 'KON', l, kon, Tag,[_,_,_,weder,OF,_],_,MF,_,MF) :- member('->kon->', OF).
head(Tag, 'KON', l, kon, Tag,[_,_,_,'Weder',OF,_],_,MF,_,MF) :- member('->kon->', OF).


%comma can join two elements if there is a conjunction after them: "ich kam, sah und siegte"
head(Tag,'$,',l,comma,'KON_NOUN',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_NOUN'), member('->kon->', OF), \+ member('<-comma<-', OF).

head('CARD','$,',l,comma,'KON_CARD',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head('PP','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('PPREL','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('PPQ','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- member('->kon->',OF), \+ member('<-comma<-', OF).

head('ADV','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head('ADJD','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head('ADJA','$,',l,comma,'KON_ADJA',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PRONOUN',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_PRONOUN'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PPER',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_PPER'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_FINVERB',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_FINVERB'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF), \+ member('->subj->', OF).

head('VVIZU','$,',l,comma,'KON_VVIZU',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF), \+ member('->subj->', OF).

head('VVIMP','$,',l,comma,'KON_IMPVERB',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_PPVERB',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_PPVERB'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head(Tag,'$,',l,comma,'KON_INFVERB',[_,_,_,_,OF,_],_,MF,_,MF) :- kon_mapping(Tag,'KON_INFVERB'), nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).

head('VVIZU','$,',l,comma,'KON_VVIZU',[_,_,_,_,OF,_],_,MF,_,MF) :- nth1(2,OF,'->kon->'), \+ member('<-comma<-', OF).


%with adjectives, the same is possible even if there is no conjunction at the end "der hochgefährliche, giftige baustoff".
head('ADJA','$,',l,comma,'KON_ADV',[_,_,_,_,OF,_],_,MF,_,MF) :- \+ member('<-comma<-', OF).



%noun + noun/pronoun
head('NN','KON_PRONOUN',r,kon,'NN',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NN',MF,'PDS',MNew).

head('NE','KON_PRONOUN',r,kon,'NE',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NE',MF,'PDS',MNew).

head('FM','KON_PRONOUN',r,kon,'FM',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'FM',MF,'PDS',MNew).


head('NN','KON_PPER',r,kon,'NN',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NN',MF,'PPER',MNew).

head('NE','KON_PPER',r,kon,'NE',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NE',MF,'PPER',MNew).

head('FM','KON_PPER',r,kon,'FM',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'FM',MF,'PPER',MNew).


head('NN','KON_NOUN',r,kon,'NN',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NN',MF,'NN',MNew).

head('NE','KON_NOUN',r,kon,'NE',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'NE',MF,'NN',MNew).

head('FM','KON_NOUN',r,kon,'FM',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'FM',MF,'NN',MNew).


%pronoun + noun/pronoun
head('PDS','KON_PRONOUN',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PDS',MF,'PDS',MNew).

head('PIS','KON_PRONOUN',r,kon,'PIS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIS',MF,'PDS',MNew).

head('PPER','KON_PRONOUN',r,kon,'PPER',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPER',MF,'PDS',MNew).

head('PWS','KON_PRONOUN',r,kon,'PWS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWS',MF,'PDS',MNew).

head('PWAT','KON_PRONOUN',r,kon,'PWS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWAT',MF,'PDS',MNew).

head('PRELS','KON_PRONOUN',r,kon,'PRELS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PRELS',MF,'PDS',MNew).

head('PRELAT','KON_PRONOUN',r,kon,'PRELS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PRELAT',MF,'PDS',MNew).

head('PIAT','KON_PRONOUN',r,kon,'PIS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIAT',MF,'PDS',MNew).

head('PDAT','KON_PRONOUN',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PDAT',MF,'PDS',MNew).

head('PIDAT','KON_PRONOUN',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIDAT',MF,'PDS',MNew).

head('PPOSS','KON_PRONOUN',r,kon,'PPOSS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPOSS',MF,'PDS',MNew).

head('PPOSAT','KON_PRONOUN',r,kon,'PPOSS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPOSAT',MF,'PDS',MNew).


head('PDS','KON_PPER',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PDS',MF,'PPER',MNew).

head('PIS','KON_PPER',r,kon,'PIS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIS',MF,'PPER',MNew).

head('PPER','KON_PPER',r,kon,'PPER',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPER',MF,'PPER',MNew).

head('PWS','KON_PPER',r,kon,'PWS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWS',MF,'PPER',MNew).

head('PWAT','KON_PPER',r,kon,'PWS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWAT',MF,'PPER',MNew).

head('PRELS','KON_PPER',r,kon,'PRELS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PRELS',MF,'PPER',MNew).

head('PRELAT','KON_PPER',r,kon,'PRELS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PRELAT',MF,'PPER',MNew).

head('PIAT','KON_PPER',r,kon,'PIS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIAT',MF,'PPER',MNew).

head('PDAT','KON_PPER',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PDAT',MF,'PPER',MNew).

head('PIDAT','KON_PPER',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIDAT',MF,'PPER',MNew).

head('PPOSS','KON_PPER',r,kon,'PPOSS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPOSS',MF,'PPER',MNew).

head('PPOSAT','KON_PPER',r,kon,'PPOSS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPOSAT',MF,'PPER',MNew).


head('PDS','KON_NOUN',r,kon,'PDS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PDS',MF,'NN',MNew).

head('PIS','KON_NOUN',r,kon,'PIS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PIS',MF,'NN',MNew).

head('PPER','KON_NOUN',r,kon,'PPER',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PPER',MF,'NN',MNew).

% head('PWS','KON_NOUN',r,kon,'PWS',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWS',MF,'NN',MNew).
% 
% head('PWAT','KON_NOUN',r,kon,'PWAT',  [_,_,_,_,_,_],_,MF,MG,MNew) :- unify_case(MG,'PWAT',MF,'NN',MNew).

%ein und derselbe Job
head('ART','KON_NOUN',r,kon,'NN',  [_,_,_,_,_,_],_,MF,_,MF).



%truncated conjunction. special: morphological information of conjoined object is used.
head('TRUNC','KON_NOUN',r,kon,'NN',  [_,_,_,_,_,_],_,MF,_,MF).

%jahre- bis jahrzehntelange Haft
head2('TRUNC','KON_ADJA',r,kon,'ADJA', [_,_,_,_,_,_,_,_],_,HM,_,HM).

head2('TRUNC','KON_ADV',r,kon,'ADJD', [_,_,_,_,_,_,_,_],_,HM,_,HM).

%er ist hin- und hergefahren
head2('TRUNC','KON_FINVERB',r,kon,'VVFIN', [_,_,_,_,_,_,_,_],_,HM,_,HM).

head2('TRUNC','KON_INFVERB',r,kon,'VVINF', [_,_,_,_,_,_,_,_],_,HM,_,HM).

head2('TRUNC','KON_PPVERB',r,kon,'VVPP', [_,_,_,_,_,_,_,_],_,HM,_,HM).

head2('TRUNC','KON_VVIZU',r,kon,'VVIZU', [_,_,_,_,_,_,_,_],_,HM,_,HM).



%card + card
head('CARD','KON_CARD',r,kon,'CARD',  [_,_,_,_,_,_],_,_,MG,MG).


%adv/adjd/pp/adja + adv/adjd/pp
head('ADV','KON_ADV',r,kon,'ADV',  [_,_,_,_,_,_],_,_,MG,MG).

head('PWAV','KON_ADV',r,kon,'PWAV',  [_,_,_,_,_,_],_,_,MG,MG).

head('ADJD','KON_ADV',r,kon,'ADJD',  [_,_,_,_,_,_],_,_,MG,MG).

head('PP','KON_ADV',r,kon,'PP',  [_,_,_,_,_,_],_,_,MG,MG).

head('PPQ','KON_ADV',r,kon,'PPQ',  [_,_,_,_,_,_],_,_,MG,MG).

head('PPREL','KON_ADV',r,kon,'PPREL',  [_,_,_,_,_,_],_,_,MG,MG).

head('ADJA','KON_ADV',r,kon,'ADJA',  [_,_,_,_,_,_],_,_,MG,MG).


%adjd/adja + /adja
head('ADJD','KON_ADJA',r,kon,'ADJD',  [_,_,_,_,_,_],_,_,MG,MG).

head('ADJA','KON_ADJA',r,kon,'ADJA',  [_,_,_,_,_,_],_,_,MG,MG).



%v*fin + v*fin
head('VVFIN','KON_FINVERB',r,kon,'VVFIN',  [_,_,_,_,_,_],_,_,MG,MG).

head('VAFIN','KON_FINVERB',r,kon,'VAFIN',  [_,_,_,_,_,_],_,_,MG,MG).

head('VMFIN','KON_FINVERB',r,kon,'VMFIN',  [_,_,_,_,_,_],_,_,MG,MG).



%v*pp + v*pp
head('VVPP','KON_PPVERB',r,kon,'VVPP',  [_,_,_,_,_,_],_,_,MG,MG).

head('VAPP','KON_PPVERB',r,kon,'VAPP',  [_,_,_,_,_,_],_,_,MG,MG).

head('VMPP','KON_PPVERB',r,kon,'VMPP',  [_,_,_,_,_,_],_,_,MG,MG).



%v*inf + v*inf
head('VVINF','KON_INFVERB',r,kon,'VVINF',  [_,_,_,_,_,_],_,_,MG,MG) .

head('VAINF','KON_INFVERB',r,kon,'VAINF',  [_,_,_,_,_,_],_,_,MG,MG).

head('VMINF','KON_INFVERB',r,kon,'VMINF',  [_,_,_,_,_,_],_,_,MG,MG).

head('VVIZU','KON_VVIZU',r,kon,'VVIZU',[_,_,_,_,_,_],_,_,MG,MG).


%vvimp + vvimp
head('VVIMP','KON_IMPVERB',r,kon,'VVIMP',  [_,_,_,_,_,_],_,_,MG,MG).


%X, Y und so weiter
head(Tag,'KON_ANY',r,kon,Tag,  [_,_,_,_,_,_],_,_,MG,MG).


%Der 9./10. Mai
head2(_,'$(',l,bracket,'KON_ANY',  [_,_,_,'/',_,_,_,_],F-G,HM,_,HM) :- 1 is F-G.

%two clauses that are not in a subordinated relationship can belong together.

head2('VVFIN','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VAFIN','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VMFIN','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VVIMP','$,',l,comma,'KONC',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).



%v*fin + v*fin: doesn't require conjunction.
head('VVFIN','KONC',r,kon,'VVFIN',[FC,GC,_,_,_,_],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC).

head('VAFIN','KONC',r,kon,'VAFIN',[FC,GC,_,_,_,_],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC).

head('VMFIN','KONC',r,kon,'VMFIN',[FC,GC,_,_,_,_],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC).

head('VVIMP','KONC',r,kon,'VVIMP',[FC,GC,_,_,_,_],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC).


kon_mapping('NN','KON_NOUN') :- !.
kon_mapping('FM','KON_NOUN') :- !.
kon_mapping('NE','KON_NOUN') :- !.

kon_mapping('CARD','KON_CARD') :- !.

kon_mapping('PP','KON_ADV') :- !.
kon_mapping('PPREL','KON_ADV') :- !.
kon_mapping('PPQ','KON_ADV') :- !.
kon_mapping('ADV','KON_ADV') :- !.
kon_mapping('ADJD','KON_ADV') :- !.

kon_mapping('ADJA','KON_ADJA') :- !.

%needs its own class because of different morphology style
kon_mapping('PPER','KON_PPER') :- !.

kon_mapping('PDS','KON_PRONOUN') :- !.
kon_mapping('PIS','KON_PRONOUN') :- !.
kon_mapping('PWS','KON_PRONOUN') :- !.
kon_mapping('PWAT','KON_PRONOUN') :- !.
kon_mapping('PRELS','KON_PRONOUN') :- !.
kon_mapping('PRELAT','KON_PRONOUN') :- !.
kon_mapping('PIAT','KON_PRONOUN') :- !.
kon_mapping('PDAT','KON_PRONOUN') :- !.
kon_mapping('PIDAT','KON_PRONOUN') :- !.
kon_mapping('PPOSS','KON_PRONOUN') :- !.
kon_mapping('PPOSAT','KON_PRONOUN') :- !.

kon_mapping('VVFIN','KON_FINVERB') :- !.
kon_mapping('VMFIN','KON_FINVERB') :- !.
kon_mapping('VAFIN','KON_FINVERB') :- !.

kon_mapping('VVIMP','KON_IMPVERB') :- !.

kon_mapping('VVPP','KON_PPVERB') :- !.
kon_mapping('VMPP','KON_PPVERB') :- !.
kon_mapping('VAPP','KON_PPVERB') :- !.

kon_mapping('VVINF','KON_INFVERB') :- !.
kon_mapping('VMINF','KON_INFVERB') :- !.
kon_mapping('VAINF','KON_INFVERB') :- !.

kon_mapping('VVIZU','KON_VVIZU') :- !.

%======================================================================================
%quotes/(in)direct speech: tag 's'

%quotes are separated from the main clause by a comma (full stops will initialize a new sentence in the parser, so they're not recognized anyway).
head2('VVFIN','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VAFIN','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VMFIN','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).

head2('VVIMP','$,',l,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('<-comma<-', HeadRels).



head2('VVFIN','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head2('VAFIN','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head2('VMFIN','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).

head2('VVIMP','$,',r,comma,'QUOTE',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- \+ member('->comma->', HeadRels).



%v*fin + v*fin: head word needs to be word of speech (sagen, meinen, bekräftigen...) --> statisics module

%quote after head clause
head('VVFIN','QUOTE',r,s,'VVFIN',[FC,GC,_,_,_,OG],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG).

head('VAFIN','QUOTE',r,s,'VAFIN',[FC,GC,_,_,_,OG],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG).

head('VMFIN','QUOTE',r,s,'VMFIN',[FC,GC,_,_,_,OG],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG).

head('VVIMP','QUOTE',r,s,'VVIMP',[FC,GC,_,_,_,OG],_,_,MG,MG) :- member('mainclause',FC),member('mainclause',GC), restrict_coord(OG),\+ member('->s->',OG), \+ member('<-s<-',OG), \+ member('<-objc<-',OG), \+ member('->objc->',OG), \+ member('<-obja<-',OG), \+ member('->obja->',OG).


%quote before head clause -> we want a subject to make sure it isn't something like "A tat B, sagte aber C"
head('VVFIN','QUOTE',l,s,'VVFIN',[FC,GC,_,_,OF,_],_,MF,_,MF) :- member('mainclause',FC),member('mainclause',GC), member('->subj->',OF), \+ member('->s->',OF), \+ member('<-s<-',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF), \+ member('<-obja<-',OF), \+ member('->obja->',OF),\+ member('->kon->',OF).

head('VAFIN','QUOTE',l,s,'VAFIN',[FC,GC,_,_,OF,_],_,MF,_,MF) :- member('mainclause',FC),member('mainclause',GC), member('->subj->',OF),\+ member('->s->',OF), \+ member('<-s<-',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF), \+ member('<-obja<-',OF), \+ member('->obja->',OF),\+ member('->kon->',OF).

head('VMFIN','QUOTE',l,s,'VMFIN',[FC,GC,_,_,OF,_],_,MF,_,MF) :- member('mainclause',FC),member('mainclause',GC), member('->subj->',OF),\+ member('->s->',OF), \+ member('<-s<-',OF), \+ member('<-objc<-',OF), \+ member('->objc->',OF), \+ member('<-obja<-',OF), \+ member('->obja->',OF),\+ member('->kon->',OF).


%======================================================================================
%parenthetical structures: tag 'par'

head2('QUOTE','$,',r,comma,'PAR',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HeadRels).

head2('PPNEB','$,',r,comma,'PPNEBX',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('<-comma<-', HeadRels).
head2('PPNEBX','$,',l,comma,'PPNEB',[_,_,_,_,HeadRels,_,_,_],_,HM,_,HM) :- member('->comma->', HeadRels).

%quotes on both sides: "Er sei", sagte der Angeklagte, "völlig unschuldig."
head2(Tag,'PAR',r,par,Tag,[_,FD,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F), member('mainclause',FD), restrict_coord(DepRels), \+ member('->s->',DepRels), \+ member('<-s<-',DepRels), \+ member('<-objc<-',DepRels), \+ member('->objc->',DepRels), \+ member('<-obja<-',DepRels), \+ member('->obja->',DepRels).

%quotes on both sides: "Ich bin hier", sagte der Angeklagte, "sie nicht."
head2(Tag,'QUOTE',r,par,Tag,[_,FD,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-To), lastlexical(From,F), RightPos is To + 1, checkPos(RightPos,_,'$,',_,_), member('mainclause',FD), restrict_coord(DepRels), \+ member('->s->',DepRels), \+ member('<-s<-',DepRels), \+ member('<-objc<-',DepRels), \+ member('->objc->',DepRels), \+ member('<-obja<-',DepRels), \+ member('->obja->',DepRels).

%"Verantwortlich, so Peter, ist Hans"
head2(Tag,'APP',r,par,Tag,[_,_,_,_,_,DepRels,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F), member('<-comma<-', DepRels), member('->comma->', DepRels), member('<-adv<-', DepRels).

%"Ich bin - wie versprochen - gekommen."
head2(Tag,'PPNEB',r,par,Tag,[_,_,_,_,_,_,_,DID],F-_,HM,_,HM) :- getRange(DID,From-_), lastlexical(From,F).

%expressions in parentheses (like this one). pretty complicated rule, since we do not want all brackets (we exclude quotation marks, for instance)
head2(Tag,Tag2,r,par,Tag,[_,_,_,_,_,[StartBracket,'<-bracket<-'|DepRels],_,DID],F-_,HM,_,HM) :- \+ member(Tag2,['NN','NE','FM']), getRange(DID,From-_), lastlexical(From,F), append(_,['->bracket->',EndBracket],DepRels), StartBracket =.. [_,[X]], EndBracket =.. [_,[Y]], splittag(X,Word,'$('), splittag(Y,Word2,'$('), leftbracket(Word,Class),rightbracket(Word2,Class), Class > 10.

%don't attach parenthetical expressions to commas or parentheses, but to the last word before that.
lastlexical(From,Last) :- LeftPos is From - 1,
			  checkPos(LeftPos,_,Tag,_,_),
			  ((Tag='$(';Tag='$,')->lastlexical(LeftPos,Last); LeftPos=Last).


%======================================================================================
%'zeit' - temporal modifiers - only allow cardinal numbers for now (90% of all cases), noun phrases are theoretically possible (letzten dienstag ging er nach hause) - lexical disambiguation?

%'zeit' before verb
head('VVFIN','CARD',l,zeit,'VVFIN',[FC,_,_,_,UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VMFIN','CARD',l,zeit,'VMFIN',[FC,_,_,_,UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAFIN','CARD',l,zeit,'VAFIN',[FC,_,_,_,UG,_],_,MF,_,MF) :- restrict_vorfeld(FC,UG), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAINF', 'CARD',l,zeit,'VAINF',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VAPP', 'CARD',l,zeit,'VAPP',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMINF', 'CARD',l,zeit,'VMINF',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMPP', 'CARD',l,zeit,'VMPP',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVINF', 'CARD',l,zeit,'VVINF',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVPP', 'CARD',l,zeit,'VVPP',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'CARD',l,zeit,'VVIZU',[FC,_,_,_,UG,_],_,MF,_,MF) :- verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

%Letztes Jahr regnete es.
head('VVFIN','NN',l,zeit,'VVFIN',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VMFIN','NN',l,zeit,'VMFIN',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAFIN','NN',l,zeit,'VAFIN',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAINF', 'NN',l,zeit,'VAINF',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VAPP', 'NN',l,zeit,'VAPP',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMINF', 'NN',l,zeit,'VMINF',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMPP', 'NN',l,zeit,'VMPP',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVINF', 'NN',l,zeit,'VVINF',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVPP', 'NN',l,zeit,'VVPP',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NN',l,zeit,'VVIZU',[FC,_,_,Lemma,UG,_],_,MF,MG,MF) :- zeitcand(Lemma), case_acc(MG,'NN'), verbchunklength(FC,1), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).


%Anfang Oktober regnete es.
head('VVFIN','NZEIT',l,zeit,'VVFIN',[FC,_,_,_,UG,_],_,MF,MG,MF) :- restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VMFIN','NZEIT',l,zeit,'VMFIN',[FC,_,_,_,UG,_],_,MF,MG,MF) :- restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAFIN','NZEIT',l,zeit,'VAFIN',[FC,_,_,_,UG,_],_,MF,MG,MF) :- restrict_vorfeld(FC,UG), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

head('VAINF', 'NZEIT',l,zeit,'VAINF',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VAPP', 'NZEIT',l,zeit,'VAPP',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMINF', 'NZEIT',l,zeit,'VMINF',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VMPP', 'NZEIT',l,zeit,'VMPP',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVINF', 'NZEIT',l,zeit,'VVINF',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVPP', 'NZEIT',l,zeit,'VVPP',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('VVIZU', 'NZEIT',l,zeit,'VVIZU',[FC,_,_,_,UG,_],_,MF,MG,MF) :- verbchunklength(FC,1), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).

%der 1995 verstorbene Künstler
head('ADJA', 'CARD',l,zeit,'ADJA',[_,_,_,_,UG,_],F-_,MF,_,MF) :-  derived_from_vpart(MF,'ADJA'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG), \+ endOfNP(F).
head('ADJA', 'CARD',l,zeit,'ADJA',[_,_,_,Lemma,UG,_],F-_,MF,MG,MF) :-  derived_from_vpart(MF,'ADJA'), \+ endOfNP(F), zeitcand(Lemma), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).
head('ADJA', 'NZEIT',l,zeit,'ADJA',[_,_,_,_,UG,_],F-_,MF,MG,MF) :-  derived_from_vpart(MF,'ADJA'), \+ endOfNP(F), case_acc(MG,'NN'), \+ member('<-zeit<-',UG), \+ member('->zeit->',UG).


%'zeit' after verb
head('VVFIN','CARD',r,zeit,'VVFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VMFIN','CARD',r,zeit,'VMFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VAFIN','CARD',r,zeit,'VAFIN',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VVIMP','CARD',r,zeit,'VVIMP',[_,_,_,_,_,OG],_,_,MG,MG) :- \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).


head('VVFIN','NN',r,zeit,'VVFIN',[_,_,Lemma,_,_,OG],_,MF,MG,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VMFIN','NN',r,zeit,'VMFIN',[_,_,Lemma,_,_,OG],_,MF,MG,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VAFIN','NN',r,zeit,'VAFIN',[_,_,Lemma,_,_,OG],_,MF,MG,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VVIMP','NN',r,zeit,'VVIMP',[_,_,Lemma,_,_,OG],_,MF,MG,MG) :- zeitcand(Lemma), case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).


head('VVFIN','NZEIT',r,zeit,'VVFIN',[_,_,_,_,_,OG],_,MF,MG,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VMFIN','NZEIT',r,zeit,'VMFIN',[_,_,_,_,_,OG],_,MF,MG,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VAFIN','NZEIT',r,zeit,'VAFIN',[_,_,_,_,_,OG],_,MF,MG,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).

head('VVIMP','NZEIT',r,zeit,'VVIMP',[_,_,_,_,_,OG],_,MF,MG,MG) :- case_acc(MF,'NN'), \+ member('<-zeit<-',OG), \+ member('->zeit->',OG), restrict_coord(OG).


zeitcand('Montag').
zeitcand('Dienstag').
zeitcand('Mittwoch').
zeitcand('Donnerstag').
zeitcand('Freitag').
zeitcand('Samstag').
zeitcand('Sonntag').

zeitcand('Januar').
zeitcand('Februar').
zeitcand('März').
zeitcand('April').
zeitcand('Mai').
zeitcand('Juni').
zeitcand('Juli').
zeitcand('August').
zeitcand('September').
zeitcand('Oktober').
zeitcand('November').
zeitcand('Dezember').

zeitcand('Sekunde').
zeitcand('Minute').
zeitcand('Stunde').
zeitcand('Tag').
zeitcand('Woche').
zeitcand('Monat').
zeitcand('Jahr').
zeitcand('Jahrzehnt').
zeitcand('Jahrhundert').
zeitcand('Jahrtausend').

zeitcand('Morgen').
zeitcand('Mittag').
zeitcand('Nachmittag').
zeitcand('Vormittag').
zeitcand('Abend').
zeitcand('Nacht').

zeitcand('Frühling').
zeitcand('Sommer').
zeitcand('Herbst').
zeitcand('Winter').

zeitcand('Quartal').
zeitcand('Trimester').
zeitcand('Semester').
zeitcand('Saison').
zeitcand('Zeit').

zeitcand('Mal').


%======================================================================================
%'grad'

%fünf jahre alt
head2('ADJA','NN',l,grad,'ADJA',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), among_dependents(DRels, '_CARD', 1).

head2('ADJD','NN',l,grad,'ADJD',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), among_dependents(DRels, '_CARD', 1).

head2('PIAT','NN',l,grad,'PIAT',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), among_dependents(DRels, '_CARD', 1).

head2('PP','NN',l,grad,'PP',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,DMorph,HMorph) :- case_acc(DMorph,'NN'), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)), among_dependents(DRels, '_CARD', 1).


%ein wenig zu alt
head2('ADJA','PIS',l,grad,'ADJA',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head2('ADJD','PIS',l,grad,'ADJD',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head2('PIAT','PIS',l,grad,'PIAT',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).

head2('PP','PIS',l,grad,'PP',[_,_,_,_,HRels,DRels,_,_],HPos-DPos,HMorph,_,HMorph) :- member('<-part<-',DRels), (1 is HPos-DPos;among_dependents(HRels, '_PTKA', 1)).



%======================================================================================
%brackets can enclose any structure


%different opening and closing brackets
head2(Tag,'$(',l,bracket,NewTag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- leftbracket(Lex,Class), concat('BRACKET',Class,TempTag), concat(TempTag,Tag,NewTag).

head2(OldTag,'$(',r,bracket,Tag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- rightbracket(Lex,Class), concat('BRACKET',Class,TempTag), concat(TempTag,Tag,OldTag).

head2(Tag,'$(',l,bracket,NewTag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- concat('BRACKET2',Tag,NewTag), \+ leftbracket(Lex,_), \+ rightbracket(Lex,_).

head2(OldTag,'$(',r,bracket,Tag,[_,_,_,Lex,_,_,_,_],_,HM,_,HM) :- concat('BRACKET2',Tag,OldTag), \+ leftbracket(Lex,_), \+ rightbracket(Lex,_).



leftbracket('«',1).
leftbracket('»',2).
leftbracket('„',3).
leftbracket('(',11).
leftbracket('[',12).
leftbracket('{',13).

%minux signs, hypens and dashes
%for syntactic analysis, we don't differentiate between them, since their use may be inconsistent
leftbracket('-',21).
leftbracket('‐​',21).
leftbracket('‑',21).
leftbracket('‒',21).
leftbracket('–',21).
leftbracket('—',21).
leftbracket('―',21).

rightbracket('»',1).
rightbracket('«',2).
rightbracket('“',3).
rightbracket(')',11).
rightbracket(']',12).
rightbracket('}',13).

rightbracket('-',21).
rightbracket('‐​',21).
rightbracket('‑',21).
rightbracket('‒',21).
rightbracket('–',21).
rightbracket('—',21).
rightbracket('―',21).



head2(Tag,'$(',r,badbracket,Tag,[_,_,_,Lex,_,_,_,_],F-G,HM,_,HM) :- -1 is F-G, member(Lex,['"', '\'']).

%good: ", \'

%======================================================================================
%catchall included for backwards compatibility

head2(Gtag,Ftag,r,Type,Transtag,[GChunk,FChunk,FGh,FFh,OG,OF,_,_],GPos-FPos,MORPHG,MORPHF,MORPH) :- head(Gtag,Ftag,r,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG],FPos-GPos, MORPHF,MORPHG,MORPH).

head2(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG,_,_],FPos-GPos,MORPHF,MORPHG,MORPH) :- head(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG],FPos-GPos, MORPHF,MORPHG, MORPH).


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


%case identifiers for tueba-style morphology
case_nom(List,Tag) :- morphology(tueba), member(Morph,List), get_case(Morph,Tag,'n',tueba), !.
case_acc(List,Tag) :- morphology(tueba), member(Morph,List), get_case(Morph,Tag,'a',tueba), !.
case_dat(List,Tag) :- morphology(tueba), member(Morph,List), get_case(Morph,Tag,'d',tueba), !.
case_gen(List,Tag) :- morphology(tueba), member(Morph,List), get_case(Morph,Tag,'g',tueba), !.

%case identifiers for gertwol-style morphology
case_nom(List,Tag) :- morphology(gertwol), member(Morph,List), get_case(Morph,Tag,'Nom',gertwol), !.
case_acc(List,Tag) :- morphology(gertwol), member(Morph,List), get_case(Morph,Tag,'Akk',gertwol), !.
case_dat(List,Tag) :- morphology(gertwol), member(Morph,List), get_case(Morph,Tag,'Dat',gertwol), !.
case_gen(List,Tag) :- morphology(gertwol), member(Morph,List), get_case(Morph,Tag,'Gen',gertwol), !.


%placeholders in case morphology is turned off.
case_nom(_,_) :- morphology(off), !.
case_acc(_,_) :- morphology(off), !.
case_dat(_,_) :- morphology(off), !.
case_gen(_,_) :- morphology(off), !.




degree_comp(List,Tag) :- morphology(gertwol), !, member(Morph,List), get_degree(Morph,Tag,'Comp',gertwol).

degree_comp(_,_) :- morphology(tueba), !.
degree_comp(_,_) :- morphology(off), !.


derived_from_vpart(List,Tag) :- morphology(gertwol), !, member(Morph,List), get_derived_from(Morph,Tag,'<VPART',gertwol).

derived_from_vpart(_,_) :- morphology(tueba), !.
derived_from_vpart(_,_) :- morphology(off), !.


check_agreement(_,_,_,_,_) :- morphology(off), !.

check_agreement(List1,Tag1,List2,Tag2,ListOut) :- \+ var(List1), \+ var(List2),
	findall(SingleList1, (member(SingleList1,List1), member(SingleList2,List2),unify_morph(SingleList1,Tag1,SingleList2,Tag2)),ListTemp), 
	list_to_set(ListTemp,ListOut), !, \+ ListTemp = [].

%if first arg is not instantiated, use the instantiated list and convert it.
check_agreement(_,Tag,List2,Tag2,OutList) :- \+ var(List2), !, convertMorphList(Tag2,List2,Tag,OutList).

%if both args are uninstantiated, return true. otherwise, unification will fail.
check_agreement(List1,_,List2,_,_) :-  var(List1), var(List2), !.


%convertMorphList(+InTag,+InList,+OutTag,?OutList). Convert the morphological format to that of another word class.

convertMorphList(_,In,_,Out) :- var(In), var(Out), !.

convertMorphList(_,_,_,_) :- morphology(off), !.

convertMorphList(_,List,_,List) :- morphology(tueba), !.

convertMorphList(_,[],_,[]) :- morphology(gertwol), !.


convertMorphList('APPRART',[List|RestIn],'APPR',[[Case]|RestOut]) :-	
	morphology(gertwol), 
	get_case(List,'APPRART',Case,gertwol),
	!,
	convertMorphList('APPRART', RestIn, 'APPR',RestOut).


convertMorphList(Tag,[List|RestIn],Tag2,[ListOut|RestOut]) :-	
	morphology(gertwol), 
	get_case(List,Tag,Case,gertwol),
	get_number(List,Tag,Number,gertwol),
	get_gender(List,Tag,Gen,gertwol),
	createMorph(Tag2,Gen,Case,Number,ListOut),
	!,
	convertMorphList(Tag, RestIn, Tag2,RestOut).


%not complete, just those word classes that are needed. Takes information and creates a list in the desired format.
createMorph('ADJA',Gen,Case,Number,[_,Gen,Case,Number,_,_]) :- morphology(gertwol), !.
createMorph(Tag,Gen,Case,Number,[Gen,Case,Number]) :- morphology(gertwol),
		(morph_noun(Tag);morph_pronoun(Tag)), !.



unify_morph(_,_,_,_) :- morphology(off), !.


%unify_morphs(?List1,+Tag1,?List2,+Tag2).
%tests for number-person agreement
unify_morph(List1,Tag1,List2,Tag2) :- morphology(MorphType), morph_finverb(Tag1), !,
				get_number(List2,Tag2,Number,MorphType), get_number(List1,Tag1,Number,MorphType), %number
				get_person(List2,Tag2,Person,MorphType), get_person(List1,Tag1,Person,MorphType). %person



%tests for case-number-gender agreement
unify_morph(List1,Tag1,List2,Tag2) :- morphology(MorphType), get_case(List2,Tag2,Case,MorphType), get_case(List1,Tag1,Case,MorphType), %case
				get_number(List2,Tag2,Number,MorphType), get_number(List1,Tag1,Number,MorphType), %number
				get_gender(List2,Tag2,Genus,MorphType), get_gender(List1,Tag1,Genus,MorphType), !. %gender


unify_case(_,_,_,_,_) :- morphology(off), !.


%if first arg is not instantiated, do nothing.
unify_case(List1,_,_,_,List1) :- var(List1), !.
unify_case([E],_,_,_,[E]) :- var(E), !.

%unify_case(?List1,+Tag1,?List2,+Tag2,?ListOut).
unify_case(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_case(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_case(Morph2,Tag2,Case,MorphType)), ListTemp), 
	list_to_set(ListTemp,ListOut), !, \+ ListTemp = [].


unify_number(_,_,_,_,_) :- morphology(off), !.

%if first arg is not instantiated, do nothing.
unify_number(List1,_,_,_,List1) :- var(List1), !.
unify_number([E],_,_,_,[E]) :- var(E), !.

%unify_number(?List1,+Tag1,?List2,+Tag2,?ListOut).
unify_number(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_number(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_number(Morph2,Tag2,Case,MorphType)), ListTemp), 
	list_to_set(ListTemp,ListOut), !, \+ ListTemp = [].




unify_gender(_,_,_,_,_) :- morphology(off), !.

%if first arg is not instantiated, do nothing.
unify_gender(List1,_,_,_,List1) :- var(List1), !.
unify_gender([E],_,_,_,[E]) :- var(E), !.

%unify_gender(?List1,+Tag1,?List2,+Tag2,?ListOut).
unify_gender(List1,Tag1,List2,Tag2,ListOut) :- morphology(MorphType),
	findall(Morph1, (member(Morph1,List1), get_gender(Morph1,Tag1,Case,MorphType),member(Morph2,List2), get_gender(Morph2,Tag2,Case,MorphType)), ListTemp), 
	list_to_set(ListTemp,ListOut), !, \+ ListTemp = [].




get_case(_,'CARD',_,_) :- !.
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

get_number([_,_,_,Number,_],'ADJA',Number,gertwol) :- !.
get_number([_,_,_,Number,_,_],'ADJA',Number,gertwol) :- !.
get_number([_,Number,_,_],'PPER',Number,gertwol) :- !.
get_number([_,_,_,Number],'ART',Number,gertwol) :- !.
get_number([_,Number,_],'PRF',Number,gertwol) :- !.
get_number([_,Number,_,_],Tag,Number,gertwol) :- morph_finverb(Tag), !.
get_number([_,_,Number],Tag,Number,gertwol) :- (morph_noun(Tag);morph_pronoun(Tag);Tag = 'PRF'), !.

get_number(_,_,_,off) :- !.



get_gender([_,_,Gender],_,Gender,tueba) :- !.

get_gender([_,Gender,_,_,_],'ADJA',Gender,gertwol) :- !.
get_gender([_,Gender,_,_,_,_],'ADJA',Gender,gertwol) :- !.
get_gender([_,_,_,Gender,_],'PPER',Gender,gertwol) :- !.
get_gender([_,Gender,_,_],'ART',Gender,gertwol) :-  !.
get_gender([Gender,_],'APPRART',Gender,gertwol) :- !.
get_gender([Gender,_,_],Tag,Gender,gertwol) :- (morph_noun(Tag);morph_pronoun(Tag)), !.

get_gender(_,_,_,off) :- !.



get_person([_,_,_,Person],'PPER',Person,tueba) :- !.
get_person([Person,_,_,_],Tag,Person,tueba) :- morph_finverb(Tag), !.
get_person(_,_,3,tueba) :- !.


get_person([Person,_,_,_],'PPER',Person,gertwol) :- !.
get_person([Person,_,_,_],Tag,Person,gertwol) :- (morph_finverb(Tag)), !.
get_person(_,_,3,gertwol) :- !.

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
case_tueba('Dat',d) :- !.
case_tueba('Gen',g) :- !.
case_tueba(X,X) :- !.


createMorphOutput(Head,Dep,MyRel) :- (getChartInfo(Head,HPos,HWord,HLemma,_,HMorph);true),
      getChartInfo(Dep,DPos,DWord,DLemma,_,DMorph),
      spyme(HPos,DPos,morph),
      (call(output(HPos,HWord,HLemma,HTag,_,_,HMorph2))->true;
          (chart(HPos,HPos,HPos,[[Lemma,HTag,_,_,_]],_,_,_,_,[Word|HMorph]),
          assert(output(HPos,Word,Lemma,HTag,root,0,HMorph)), 
          HMorph2 = HMorph)),
      (call(output(DPos,DWord,DLemma,DTag,MyRel,_,DMorph2));
          (checkPos(DPos,_,DTag,_,_),
          DMorph2 = DMorph)), 
      (retract(output(DPos,_,_,_,_,_,_));true),
      assert(output(DPos,DWord,DLemma,DTag,MyRel,0,DMorph2)), !.

createMorphOutput(_,_,_) :- !.

spyme(_,_,_).

createRelOutput(Head,Dep,MyRel) :- lexic(Head,HLemma,HPos), 
      lexic(Dep,DLemma,DPos), 
      (call(output(HPos,HWord,HLemma,HTag,_,_,HMorph));(getChartInfo(_,HPos,HWord,HLemma,_,HMorph),checkPos(HPos,_,HTag,_,_))),
      (retract(output(DPos,DWord,DLemma,DTag,_,_,DMorph));(getChartInfo(_,DPos,DWord,DLemma,_,DMorph),checkPos(DPos,_,DTag,_,_))),
      assert(output(DPos,DWord,DLemma,DTag,MyRel,HPos,DMorph)), !.

createRelOutput(_,_,_) :- !.

getChartInfo(Head,Pos,Word,Lemma,Tag,Morph) :- chart(_,_,_,[[LemmaPos,Tag,_,_,_],_],_,_,Head,_,[Word|Morph]), Head =.. [LemmaPos|_], lexic(LemmaPos,Lemma,Pos), !.
getChartInfo(Head,Pos,Word,Lemma,Tag,Morph) :- chart(_,_,_,[_,[LemmaPos,Tag,_,_,_]],_,_,Head,_,[Word|Morph]), Head =.. [LemmaPos|_], lexic(LemmaPos,Lemma,Pos), !.
getChartInfo(Head,Pos,Word,Lemma,Tag,Morph) :- chart(Pos,Pos,Pos,[[Lemma,Tag,_,_,_]],_,_,Head,_,[Word|Morph]), !.



%start unification from the top:
morph_cleanup2(Rel,HMorph,HTag,DMorph,DTag,HPos,DMorphOut) :- 
              output(HPos,_,_,HTag,HRel,HHPos,HMorph),
              output(HHPos,_,_,HHTag,_,HHHPos,HHMorph), !,
              morph_cleanup2(HRel,HHMorph,HHTag,HMorph,HTag,HHHPos,HMorphOut),
              morph_cleanup(Rel,HMorphOut,HTag,DMorph,DTag,HPos,DMorphOut).

morph_cleanup2(Rel,HMorph,HTag,DMorph,DTag,HPos,DMorphOut) :- morph_cleanup(Rel,HMorph,HTag,DMorph,DTag,HPos,DMorphOut), !.


%fixes some ugly output because of the bracket rules in the grammar.
morph_cleanup(Class,HMorph,HTag,DMorph,DTag,HPos,DMorphOut) :- nth1(2,HMorph,X), atomic(X), HMorph = [NewList|_], !, morph_cleanup(Class,NewList,HTag,DMorph,DTag,HPos,DMorphOut). 
morph_cleanup(Class,HMorph,HTag,DMorph,DTag,HPos,DMorphOut) :- nth1(2,DMorph,X), atomic(X), DMorph = [NewList|_], !, morph_cleanup(Class,HMorph,HTag,NewList,DTag,HPos,DMorphOut).


morph_cleanup(root,_,_,DMorph,_,_,DMorph) :- !.

%Some grammatical functions are bound to a specific case. Restrict output morphology to syntactically valid analyses.
morph_cleanup(subj,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Nom']],'APPR',DMorphTemp), (unify_number(DMorphTemp,DTag,HMorph,HTag,DMorphOut);DMorphOut=DMorphTemp), !.
morph_cleanup(pred,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Nom']],'APPR',DMorphOut), !.
morph_cleanup(obja,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk']],'APPR',DMorphOut), !.
morph_cleanup(obja2,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk']],'APPR',DMorphOut), !.
morph_cleanup(grad,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk']],'APPR',DMorphOut), !.
morph_cleanup(zeit,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Akk']],'APPR',DMorphOut), !.
morph_cleanup(objd,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Dat']],'APPR',DMorphOut), !.
morph_cleanup(objg,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Gen']],'APPR',DMorphOut), !.
morph_cleanup(gmod,_,_,DMorph,DTag,_,DMorphOut) :- morphology(gertwol), unify_case(DMorph,DTag,[['Gen']],'APPR',DMorphOut), !.

%Agreement is checked in grammar, but result is only stored for head. Produce output for dependent here.
morph_cleanup(attr,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- check_agreement(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(det,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- check_agreement(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(pn,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(app,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.
morph_cleanup(kon,HMorph,HTag,DMorph,DTag,_,DMorphOut) :- unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.

%for the latest member in a coordinative chain, unification with the head doesn't work. Find first member of the chain instead.
morph_cleanup(cj,_,_,DMorph,DTag,HPos,DMorphOut) :- findkonchainhead(HPos,HMorph,HTag), unify_case(DMorph,DTag,HMorph,HTag,DMorphOut), !.

%catchall
morph_cleanup(_,_,_,DMorph,_,_,DMorph) :- !.


%for the latest member in a coordinative chain, unification with the head doesn't work. Find first member of the chain instead.
findkonchainhead(DPos,HMorph,HTag) :- output(DPos,_,_,_,kon,HPos,_),
        findkonchainhead(HPos,HMorph,HTag).

findkonchainhead(HPos,HMorph,HTag) :- output(HPos,_,_,HTag,_,_,HMorph), \+ kon_mapping(_,Htag), \+ Htag = 'KON', !.

%======================================================================================
%topological rules

%in Verbzweitstellung, only one complement (even adjunct?) can precede the finite verb.
restrict_vorfeld(Chunk,Dependents) :- member('mainclause',Chunk), 
				!, %cut the catchall
				\+ member('<-subj<-', Dependents),
				\+ member('<-obja<-', Dependents),
				\+ member('<-objd<-', Dependents),
				\+ member('<-objg<-', Dependents),
				\+ member('<-pred<-', Dependents),
				\+ member('<-objp<-', Dependents),
% 				\+ member('<-objc<-', Dependents),
% 				\+ member('<-neb<-', Dependents),
				\+ member('<-obji<-', Dependents),
				\+ member('<-pp<-', Dependents),
				\+ member('<-subjc<-', Dependents),
				\+ member('<-s<-', Dependents),
				\+ member('<-zeit<-',Dependents),
				\+ member('<-kom<-',Dependents),
				\+ member('<-explsubj<-',Dependents),
				\+ member('<-explobja<-',Dependents).


restrict_vorfeld(_,_) :- !. %catchall


%restrict_coord/1: makes sure that subjects, objects etc. are not attached to a finite verb if there is a verb coordination in between:
%example: susi denkt und peter sieht laura. -> 'laura' can't be object of 'denkt'.
restrict_coord(RelList) :- \+ member('->kon->',RelList),
	\+ member('->s->',RelList),
	\+ member('->objc->',RelList),
	\+ member('->subjc->',RelList),
	\+ member('->neb->',RelList).


%======================================================================================
%checks on positions outside of normal scope.

%check if construction either ends with a comma, a bracket or the end of the sentence.
commaToRight(ID) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,_,Tag,_,_), (Tag = 'NONE';Tag='$,').

commaToLeft(ID) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$,').


stopToRight(ID) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,_,Tag,_,_), (Tag = 'NONE';Tag='$.';sentdelim(Tag)).

stopToLeft(ID) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$.';sentdelim(Tag)).


bracketToRight(ID) :- getRange(ID,_-To), RightPos is To + 1, checkPos(RightPos,_,Tag,_,_), (Tag = 'NONE';Tag='$(').

bracketToLeft(ID) :- getRange(ID,From-_), LeftPos is From - 1, checkPos(LeftPos,_,Tag,_,_), (Tag = 'NONE';Tag='$(').


getRange(ID, From-To) :- chart(ID,From,To,_,_,_,_,_,_).

%check any terminal symbol and its features.
checkPos(Pos,Word,Tag,Chunk,Morph) :- chart(Pos,Pos,Pos,[[Word,Tag,Chunk,_,_]],_,_,_,_,Morph), !.

%catchall (sentence position does not exist: beginning or end of sentence). 
checkPos(_,'NONE','NONE','NONE','NONE') :- !.

%used to distinguish between "der kleine ist niedlich" und "der kleine eisbär ist niedlich".
%this is just an approximation: "der kleine im wasser schwimmende eisbär" will be misclassified.

endOfNP(Pos) :- NewPos is Pos + 1,
        checkPos(NewPos,_,'$(',_,_), !,
        endOfNP(NewPos).

endOfNP(Pos) :- NewPos is Pos + 1,
        checkPos(NewPos,_,Tag,_,_),
        \+ (Tag = 'NN';Tag = 'NE';Tag = 'FM';Tag = 'CARD';Tag = 'ADJA').

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
%too rare: commented out for now
%validgmod('PDS').
%validgmod('PIS').

%valid preposition complements
prepcompl('NN',_).
prepcompl('NE',_).
prepcompl('FM',_).
prepcompl('CARD',Pos) :- endOfNP(Pos).
prepcompl('ADJA',Pos) :- endOfNP(Pos).
prepcompl('ADJD',Pos) :- endOfNP(Pos).
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
prepcompl('ADV').
%prepcompl('TRUNC').
prepcompl('PP').



nonfinite('VAINF').
nonfinite('VAPP').
nonfinite('VMINF').
nonfinite('VMPP').
nonfinite('VVINF').
nonfinite('VVPP').
nonfinite('VVIZU').


%determiner candidates.
detcan('ART',_).
detcan('PIDAT',Pos) :- LeftPos is Pos - 1, \+ checkPos(LeftPos,_,'ART',_,_).
detcan('PIAT',_).
detcan('PPOSAT',_).
detcan('PDAT',_).

%sentences that can be clausal subject/object
objcsubjc('OBJC').
objcsubjc('RC').
objcsubjc('QC').

%pronominal adverbs (inconsistent)
proadv('PROP').
proadv('PAV').
proadv('PROAV').


%check each member of a list.
among_dependents([Item|Rest], Tag, LVL) :- !, (among_dependents(Item,Tag, LVL);
					   among_dependents(Rest,Tag, LVL)), !.

%searches the arguments of a complex term. Recursion is marked to go one level deeper.
among_dependents(Term, Tag, LVL) :- compound(Term),
				LVL > 0,
			        NewLVL is LVL - 1,
			       Term =.. [_|ChunkList], !,
			       among_dependents(ChunkList,Tag, NewLVL).

%check atom.
among_dependents(Elem, Tag, _) :- atom(Elem), ending_member([Elem],Tag), !.


ending_member(FC,Tag) :-
    ((FC=_-_-RChunk) -> true; (RChunk = FC)),
    ending_member2(RChunk,Tag).
    
ending_member2([F|_],X) :-
    name(F,FList),
    name(X,XList),
    ttmember(FList,XList),
    !.

ending_member2([_F|R],X) :- 
    ending_member2(R,X).

ttmember(X,X).
ttmember([_F|R],X) :- ttmember(R,X).


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