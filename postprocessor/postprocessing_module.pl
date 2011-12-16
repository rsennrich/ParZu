%	postprocessing.pl
%   Rico Sennrich
%   take the parser output, convert it into the right format and do some postprocessing (non-projective dependencies)

:- dynamic morphclean/1.

postprocess(_,raw) :- retractall(output(_,_,_,_,_,_,_)), !.

%all other output formats.
%first round
postprocess(Pos,Outputformat) :-
             (output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)->true;(chart(Pos,Pos,Pos,[[Lemma,Tag,_]],_,_,_,_,_,_,[Word|Morph]),Rel=root,HeadPos=0,assert(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)))),
             ((output(HeadPos,_,_,HTag,_,_,HMorph)->
             morph_cleanup2(Rel,HMorph,HTag,Morph,Tag,Pos,HeadPos,Morph2));morph_cleanup2(root,_,_,Morph,Tag,Pos,0,Morph2)),
             retract(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)),
             assert(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph2)),
             fixAttachment(Rel,Pos,HeadPos,HeadPos2),
             transcodeRel(Rel,NewRel),
             (NewRel=root->HeadPos3=0;HeadPos3=HeadPos2),
%              printresult(Outputformat,Pos,Word,Lemma,Tag,NewRel,OutputPos,MorphOut),
             retract(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph3)),
             assert(output(Pos,Word,Lemma,Tag,NewRel,HeadPos3,Morph3)),
             NewPos is Pos + 1, !,
             postprocess(NewPos,Outputformat).


%second round: secondary edges
postprocess(_,_) :- secedges(yes),
              findall(Pos,output(Pos,_,_,_,_,_,_),List),length(List,Len),
              between(1,Len,Pos),
              fail.

%third round: print
postprocess(_,Outputformat) :- findall(Pos,output(Pos,_,_,_,_,_,_),List),length(List,Len),
              between(1,Len,Pos),
              output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph),
              printresult(Outputformat,Pos,Word,Lemma,Tag,Rel,HeadPos,Morph),
              fail.

%fourth round: cleanup
postprocess(_,_) :- nl,retractall(output(_,_,_,_,_,_,_)), !.
                                 


%print results in pseudo-conll format.
printresult(oldconll,Pos,DepWord,DepLemma,DepTag,Class,HeadPos,Morph) :- 
    transformMorph(conll,Morph,MorphOut),
    (nbestmode(NBEST),NBEST > 0->NewHeadPos is max(0,HeadPos-1);NewHeadPos is HeadPos),
    write(Pos),
    tab(3),
    write(DepWord),
    tab(3),
    write(DepLemma),
    tab(3),
    write(DepTag),
    tab(3),
    write(MorphOut),
    tab(3),
    write(NewHeadPos),
    tab(3),
    write(Class),
    nl.

%print results in actual conll format.
printresult(conll,Pos,DepWord,DepLemma,DepTag,Class,HeadPos,Morph) :- 
    transformMorph(conll,Morph,MorphOut),
    (nbestmode(NBEST),NBEST > 0->NewHeadPos is max(0,HeadPos-1);NewHeadPos is HeadPos),
    coarsetag(DepTag,CoarseTag),
    (secedges(no)->(SecEdgeHead='_',SecEdgeRel='_');(secedge(Pos,SecEdgeHead,SecEdgeRel))),
    write(Pos),
    write('\t'),
    write(DepWord),
    write('\t'),
    write(DepLemma),
    write('\t'),
    write(CoarseTag),
    write('\t'),
    write(DepTag),
    write('\t'),
    write(MorphOut),
    write('\t'),
    write(NewHeadPos),
    write('\t'),
    write(Class),
    write('\t'),
    write(SecEdgeHead),
    write('\t'),
    write(SecEdgeRel),
    nl.


%print results in prolog format.
printresult(prolog,Pos,Word,Lemma,Tag,Rel,HeadPos,Morph) :- 
    transformMorph(prolog,Morph,MorphOut),
    (nbestmode(NBEST),NBEST > 0->NewHeadPos is max(0,HeadPos-1);NewHeadPos is HeadPos),
%     Output =.. [Rel,Word,Lemma,Tag,Pos,HeadPos,MorphOut],
%     writeq(Output), write('.'),nl.
     writeq(word(Pos,Word,Lemma,Tag,Rel,NewHeadPos,MorphOut)), write('.'),nl.

%print results in moses format. default.
printresult(moses,_,Word,_,Tag,Rel,HeadPos,_) :- 
    (HeadPos=0->HeadWord=root;chart(HeadPos,HeadPos,HeadPos,_,_,_,_,_,_,_,[HeadWord|_])),
    write(Word),
    write('|'),
    write(Tag),
    write('|'),
    write(Rel),
    write('|'),
    write(HeadWord),
    write(' ').


transformMorph(prolog,[Var],_) :- var(Var),!.

transformMorph(conll,[Var],'_') :- var(Var),!.

transformMorph(conll,List,Out) :- is_list(List), member(List2,List),is_list(List2), !, length(List2,Len), (Len=0->Out='_';(End is Len + 1, conll_morph(List,1,End,'',Out))).

transformMorph(_,Morph,Morph) :- !.


conll_morph(_,Start,Start,In,Out) :- atom_concat(Out,'|',In), !.

conll_morph(List,Start,End,In,Out) :- findall(Val,(member(List2,List),nth1(Start,List2,Val)),Listout), 
        (member(Var,Listout),var(Var)->Val='_';(sort(Listout,Set),(length(Set,1)->Set = [Val];Val='_'))),
        atom_concat(In,Val,OutTemp),
        atom_concat(OutTemp,'|',OutTemp2),
        Next is Start + 1,
        conll_morph(List,Next,End,OutTemp2,Out).




%given the "Lemma_Tag" information, get the position of the word (and check if it is possible in case there are several identical ones)
getPosition(Lemma_Tag,OldPos,Pos) :-
	name(Lemma_Tag, Lemma_TagChars),
	append(LemmaChars, [95|TagChars], Lemma_TagChars),
	name(Tag, TagChars),
	name(Lemma, LemmaChars),
        chart(Pos,Pos,Pos,[[Lemma,Tag,_]],_,_,_,_,_,_,_),
	output(Pos,_,_,_,aux,OldPos,_), !.


%fix expletives. 
transcodeRel(explsubj,expl) :- !.
transcodeRel(explobja,expl) :- !.

transcodeRel(konjneb,konj) :- !.
transcodeRel(konjobjc,konj) :- !.

transcodeRel(obja2,obja) :- !.
transcodeRel(objc2,objc) :- !.

transcodeRel(Class,root) :- \+ relclass(Class), !.

transcodeRel(Class,Class) :- !.


%fix auxiliary relationships: head of auxiliary relationship is not always finite verb, but verb one level higher in the hierarchy.
fixAttachment(aux,DepPos,HeadPos,Pos) :-
    chart(HeadPos,HeadPos,HeadPos,[[HeadWord,HeadTag,HC]],_,_,_,_,_,_,_),
	atom_concat(HeadWord,'_',HeadTemp),
	atom_concat(HeadTemp,HeadTag,HeadX),
    chart(DepPos,DepPos,DepPos,[[DepWord,DepTag,_]],_,_,_,_,_,_,_),
	atom_concat(DepWord,'_',DepTemp),
	atom_concat(DepTemp,DepTag,Dep),
	nth1(DPos,HC,Dep),
	HPos is DPos - 1,
	nth1(HPos,HC,Head),
	(Head=HeadX->Pos=HeadPos;getPosition(Head,HeadPos,Pos)), !.


%fix attachment for prepositional phrases and adverbs (attached to finite verb in vorfeld, but to full verb in mittelfeld)
fixAttachment(Class,DepPos,HeadPos,Pos) :-
	(Class=pp;Class=adv),
        chart(HeadPos,HeadPos,HeadPos,[[_,Tag,HC]],_,_,_,_,_,_,_),
	(Tag='VAFIN';Tag='VMFIN';Tag='VAINF';Tag='VMINF';Tag='VAPP';Tag='VMPP'),
	(append(mainclause,HC,HCTemp);HCTemp=HC),
	(append(passive,HCTemp,HCTemp2);HCTemp2=HCTemp),
	length(HCTemp2,Length),
	Length > 1,
	last(HC,FullVerb),
	getPosition(FullVerb,HeadPos,Pos),
	%either our token is in the mittelfeld (DepPos > HeadPos) or the full verb itself comes before the finite verb (verbletztstellung or topicalisation of full verb)
	(DepPos > HeadPos;Pos < HeadPos), !.

%fix explsubj attachment. strange rule in gold standard, but oh well...
fixAttachment(explsubj,_,HeadPos,Pos) :-
    output(Pos,_,_,_,subjc,HeadPos,_), !.

%relative clauses are often attached to the finite verb of the superordinate clause, especially if attaching it to the real head would result in non-projective structures. fix this here.
fixAttachment(rel,DepPos,HeadPos,CandPos) :- output(DepPos,_,_,_,rel,HeadPos,_),
      relative_morphology(DepPos,RelPos,RelTag,RelMorph),
          between(1,RelPos,Dist),
          CandPos is RelPos-Dist,
          output(CandPos,_,_,CandTag,_,_,CandMorph),
          (morph_noun(CandTag)->true;morph_pronoun(CandTag)),
          \+ is_dependent(CandPos,DepPos),
          (RelTag='PRELS'->(
          unify_number(CandMorph,CandTag,RelMorph,RelTag,CandMorph2),
          unify_number(RelMorph,RelTag,CandMorph,CandTag,RelMorph2),
          unify_gender(CandMorph2,CandTag,RelMorph2,RelTag,CandMorph3),
          unify_gender(RelMorph2,RelTag,CandMorph2,CandTag,RelMorph3), !,
          retract(output(CandPos,CandWord,CandLemma,CandTag,CandRel,CandHead,_)),
          assert(output(CandPos,CandWord,CandLemma,CandTag,CandRel,CandHead,CandMorph3)),
          retract(output(RelPos,RelWord,RelLemma,RelTag,RelRel,RelHead,_)),
          assert(output(RelPos,RelWord,RelLemma,RelTag,RelRel,RelHead,RelMorph3))
          );RelTag='PRELAT').

%all other cases: dependents that are attached to finite verb should be attached to full verb instead.
fixAttachment(Class,_,HeadPos,Pos) :- 
	\+ leavealone(Class),
        chart(HeadPos,HeadPos,HeadPos,[[_,Tag,HC]],_,_,_,_,_,_,_),
	(Tag='VAFIN';Tag='VMFIN';Tag='VAINF';Tag='VMINF';Tag='VAPP';Tag='VMPP'),
	(append(mainclause,HC,HCTemp);HCTemp=HC),
	(append(passive,HCTemp,HCTemp2);HCTemp2=HCTemp),
	length(HCTemp2,Length),
	Length > 1,
	last(HC,FullVerb),
	getPosition(FullVerb,HeadPos,Pos),!.

%catchall: leave head unchanged.
fixAttachment(_,_,Pos,Pos) :- !.

% In AcI, accusative of matrix clause is subject of infinitive clause
secedge(Pos,SecHeadPos,subj) :- output(Pos,_,_,_,obja,HeadPos,_),
            output(SecHeadPos,_,_,_,obji,HeadPos,_), !.

%catchall: no secondary edge found
secedge(_,'_','_') :- !.


relative_morphology(HeadPos,RelPos,Tag,Morph) :- output(RelPos,_,_,Tag,_,_,Morph), 
              member(Tag,['PRELS','PRELAT']),
              is_dependent(RelPos,HeadPos),
              RelPos < HeadPos.

is_dependent(Pos,Pos) :- !.

is_dependent(Pos,HeadPos) :- output(Pos,_,_,_,_,BetweenPos,_),
        is_dependent(BetweenPos,HeadPos).


%ignore all relations not listed here (commas, brackets)
relclass(subj).
relclass(obja).
relclass(obja2).
relclass(objd).
relclass(gmod).
relclass(app).
relclass(pp).
relclass(pn).
relclass(attr).
relclass(det).
relclass(rel).
relclass(konj).
relclass(cj).
relclass(kon).
relclass(aux).
relclass(adv).
relclass(pred).
relclass(objc).
relclass(objc2).
relclass(objp).
relclass(obji).
relclass(neb).
relclass(avz).
relclass(part).
relclass(kom).
relclass(subjc).
relclass(expl).
relclass(s).
relclass(zeit).
relclass(grad).
relclass(explsubj).
relclass(explobja).
relclass(konjobjc).
relclass(konjneb).
relclass(par).
relclass(objg).
relclass(koord).


%subjects etc. are attached to the finite verb in the gold standard, so there's no need to modify these dependency relations.
leavealone(subj).
leavealone(subjc).
leavealone(aux).
leavealone(konj).
leavealone(kon).
leavealone(neb).
leavealone(pp).
leavealone(adv).
leavealone(konjneb).
leavealone(konjobjc).




coarsetag('ADJD','ADV') :- !.
coarsetag('PDAT','ART') :- !.
coarsetag('PIAT','ART') :- !.
coarsetag('PIDAT','ART') :- !.
coarsetag('PPOSAT','ART') :- !.
coarsetag('PRELAT','ART') :- !.
coarsetag('PWAT','ART') :- !.
coarsetag('NE','N') :- !.
coarsetag('NN','N') :- !.
coarsetag('APPO','PREP') :- !.
coarsetag('APPR','PREP') :- !.
coarsetag('APPRART','PREP') :- !.
coarsetag('PDS','PRO') :- !.
coarsetag('PIS','PRO') :- !.
coarsetag('PPER','PRO') :- !.
coarsetag('PPOSS','PRO') :- !.
coarsetag('PROP','PROP') :- !.
coarsetag('PRELS','PRO') :- !.
coarsetag('PRF','PRO') :- !.
coarsetag('PWS','PRO') :- !.
coarsetag('VAFIN','V') :- !.
coarsetag('VAIMP','V') :- !.
coarsetag('VAINF','V') :- !.
coarsetag('VAPP','V') :- !.
coarsetag('VMFIN','V') :- !.
coarsetag('VMINF','V') :- !.
coarsetag('VMPP','V') :- !.
coarsetag('VVFIN','V') :- !.
coarsetag('VVIMP','V') :- !.
coarsetag('VVINF','V') :- !.
coarsetag('VVIZU','V') :- !.
coarsetag('VVPP','V') :- !.
coarsetag(Tag,Tag) :- !.