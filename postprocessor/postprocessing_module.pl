%	postprocessing.pl
%   Rico Sennrich
%   take the parser output, convert it into the right format and do some postprocessing (non-projective dependencies)

:- dynamic morphclean/1, oldhead/2.

:- ensure_loaded('../core/helper_predicates').

postprocess(_,raw) :- retractall(output(_,_,_,_,_,_,_)), !.

%first round
postprocess(Pos,Outputformat) :-
             (output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)->true;(chart(Pos,Pos,Pos,_,Lemma,Tag,_,_,_,[Word|Morph]),Rel=root,HeadPos=0,assert(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)))),
             ((output(HeadPos,_,_,HTag,_,_,HMorph)->
             morph_cleanup2(Rel,HMorph,HTag,Morph,Tag,Pos,HeadPos,Morph2));morph_cleanup2(root,_,_,Morph,Tag,Pos,0,Morph2)),
             retract(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph)),
             assert(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph2)),
             fixAttachment(Rel,Pos,HeadPos,HeadPos2),
             transcodeRel(Rel,NewRel),
             (NewRel=root->HeadPos3=0;HeadPos3=HeadPos2),
             retract(output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph3)),
             assert(output(Pos,Word,Lemma,Tag,NewRel,HeadPos3,Morph3)),
             assert(oldhead(Pos,HeadPos)),
             NewPos is Pos + 1, !,
             postprocess(NewPos,Outputformat).


%second round: print
postprocess(_,Outputformat) :- findall(Pos,output(Pos,_,_,_,_,_,_),List),length(List,Len),
              between(1,Len,Pos),
              output(Pos,Word,Lemma,Tag,Rel,HeadPos,Morph),
              printresult(Outputformat,Pos,Word,Lemma,Tag,Rel,HeadPos,Morph),
              fail.

%third round: cleanup
postprocess(_,_) :- nl,retractall(output(_,_,_,_,_,_,_)), retractall(oldhead(_,_)), !.


%print results in pseudo-conll format.
printresult(oldconll,Pos,DepWord,DepLemma,DepTag,Class,HeadPos,Morph) :- 
    transformMorph(conll,Morph,MorphOut),
    nbest_fixhead(HeadPos,HeadPosOut),
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
    write(HeadPosOut),
    tab(3),
    write(Class),
    nl.

%print results in actual conll format.
printresult(conll,Pos,DepWord,DepLemma,DepTag,Class,HeadPos,Morph) :- 
    transformMorph(conll,Morph,MorphOut),
    coarsetag(DepTag,CoarseTag),
    getExtraHead(Pos,HeadPos,Class,ExtraHead,ExtraRel,conll),
    nbest_fixHead(HeadPos,HeadPosOut),
    nbest_fixHead(ExtraHead,ExtraHeadOut),
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
    write(HeadPosOut),
    write('\t'),
    write(Class),
    write('\t'),
    write(ExtraHeadOut),
    write('\t'),
    write(ExtraRel),
    nl.

%print results in prolog format.
printresult(prolog,Pos,Word,Lemma,Tag,Rel,HeadPos,Morph) :- 
    transformMorph(prolog,Morph,MorphOut),
    getExtraHead(Pos,HeadPos,Rel,ExtraHead,ExtraRel,prolog),
    nbest_fixHead(HeadPos,HeadPosOut),
    nbest_fixHead(ExtraHead,ExtraHeadOut),
    (extrainfo(no)->writeq(word(Pos,Word,Lemma,Tag,Rel,HeadPosOut,MorphOut));
    writeq(word(Pos,Word,Lemma,Tag,Rel,HeadPosOut,MorphOut,ExtraRel,ExtraHeadOut))), write('.'),nl.

%print results as Moses factors.
printresult(moses,_,Word,_,Tag,Rel,HeadPos,_) :- 
    (HeadPos=0->HeadWord=root;chart(HeadPos,HeadPos,HeadPos,_,_,_,_,_,_,[HeadWord|_])),
    write(Word),
    write('|'),
    write(Tag),
    write('|'),
    write(Rel),
    write('|'),
    write(HeadWord),
    write(' ').


% if either the option --secedges (for secondary edges) or --projective (for projective edges) is active, get correct edge/label information
getExtraHead(_Pos, _HeadPos, _Rel, '_', '_', conll) :- extrainfo(no), !.
getExtraHead(_Pos, _HeadPos, _Rel, -, -, prolog) :- extrainfo(no), !.

getExtraHead(Pos, _HeadPos, _Rel, ExtraHead, ExtraRel, Format) :- extrainfo(secedges), secedge(Pos,ExtraHead,ExtraRel,Format), !.
getExtraHead(Pos, HeadPos, Rel, ExtraHead, Rel, _Format) :- extrainfo(projective), (is_projective(Pos,HeadPos)->ExtraHead = HeadPos;oldhead(Pos,ExtraHead)), !.

%n-best tagging uses an extra token at the start of the sentence to pass through tagging probability.
%shift all positions one to the left.
nbest_fixHead(0, 0) :- !.
nbest_fixHead(-, -) :- !.
nbest_fixHead('_', '_') :- !.
nbest_fixHead(PosIn, PosOut) :- nbestmode(NBEST), NBEST > 0, PosOut is PosIn-1, !.
nbest_fixHead(Pos, Pos) :- !.


%only use pseudo-projective head (found by parser) if post-processed one (found by fixAttachment) violates projectivity.
is_projective(_,0) :- !, fail.

is_projective(DepPos, HeadPos) :- DepPos < HeadPos,
    First is DepPos + 1,
    Last is HeadPos - 1,
    between(First, Last, Pos),
    output(Pos,_,_,_,_,TempHeadPos,_),
    ((TempHeadPos=0->(oldhead(Pos,TempHeadPos2),between(DepPos, HeadPos, TempHeadPos2)); (is_dependent(Pos,HeadPos), \+ projective_conflict(left, Pos, DepPos)))->true; (!, fail)),
    fail.

is_projective(DepPos, HeadPos) :- DepPos > HeadPos,
    First is HeadPos + 1,
    Last is DepPos - 1,
    between(First, Last, Pos),
    output(Pos,_,_,_,_,TempHeadPos,_),
    ((TempHeadPos=0->(oldhead(Pos,TempHeadPos2),between(HeadPos, DepPos, TempHeadPos2)); (is_dependent(Pos,HeadPos), \+ projective_conflict(right, Pos, DepPos)))->true; (!, fail)),
    fail.

is_projective(_,_) :- !.

%check if attachment would conflict with earlier/later attachments.
%a token X between the head Y and dependent Z of a projective arc, and that does not depend on the dependent of the arc,
%may not have any dependent A for which Z lies between X and A.
projective_conflict(_,Pos, DepPos) :- is_dependent(Pos, DepPos), !, fail.

projective_conflict(right,Pos, DepPos) :- oldhead(X,Pos), X > DepPos, !.
projective_conflict(left,Pos, DepPos) :- oldhead(X,Pos), X < DepPos, !.

%%a token X between the head Y and dependent Z of a projective arc, and that does not depend on the dependent of the arc,
%%may not depend on a head A that lies outside the range Y,Z
projective_conflict(right,Pos, DepPos) :- oldhead(Pos,X), X > DepPos, !.

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
	chart(Pos,Pos,Pos,_,Lemma,Tag,_,_,_,_),
	call_with_depth_limit(is_dependent(Pos,OldPos,aux),1000,Depth), Depth \= 'depth_limit_exceeded', !.


%fix expletives. 
transcodeRel(explsubj,expl) :- !.
transcodeRel(explobja,expl) :- !.

transcodeRel(konjneb,konj) :- !.
transcodeRel(konjobjc,konj) :- !.

transcodeRel(obja2,obja) :- !.
transcodeRel(objc2,objc) :- !.

transcodeRel(app_loose,app) :- !.
transcodeRel(app_close,app) :- !.

transcodeRel(bad_det,det) :- !.
transcodeRel(bad_attr,attr) :- !.
transcodeRel(bad_pn,pn) :- !.

transcodeRel(konc,kon) :- !.

transcodeRel(adv_kon,adv) :- !.

transcodeRel(Class,root) :- \+ relclass(Class), !.

transcodeRel(Class,Class) :- !.


%fix auxiliary relationships: head of auxiliary relationship is not always finite verb, but verb one level higher in the hierarchy.
fixAttachment(aux,DepPos,HeadPos,Pos) :-
    chart(HeadPos,HeadPos,HeadPos,[_,_,HC,_],HeadWord,HeadTag,_,_,_,_),
	atom_concat(HeadWord,'_',HeadTemp),
	atom_concat(HeadTemp,HeadTag,HeadX),
	chart(DepPos,DepPos,DepPos,_,DepWord,DepTag,_,_,_,_),
	atom_concat(DepWord,'_',DepTemp),
	atom_concat(DepTemp,DepTag,Dep),
	nth1(DPos,HC,Dep),
	((ZuPos is DPos - 1,
	nth1(ZuPos,HC,'zu_PTKZU'),
	HPos is DPos - 2,
	nth1(HPos,HC,Head));
	(HPos is DPos - 1,
	nth1(HPos,HC,Head))),
	(Head=HeadX->Pos=HeadPos;getPosition(Head,HeadPos,Pos)), !.


%fix attachment for prepositional phrases and adverbs (attached to finite verb in vorfeld, but to full verb in mittelfeld)
fixAttachment(Class,DepPos,HeadPos,Pos) :-
	topology_dependant(Class),
        chart(HeadPos,HeadPos,HeadPos,[_,_,HC,_],_,Tag,_,_,_,_),
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
        \+ topology_dependant(Class),
        chart(HeadPos,HeadPos,HeadPos,[_,_,HC,_],_,Tag,_,_,_,_),
	(Tag='VAFIN';Tag='VMFIN';Tag='VAINF';Tag='VMINF';Tag='VAPP';Tag='VMPP'),
	(append(mainclause,HC,HCTemp);HCTemp=HC),
	(append(passive,HCTemp,HCTemp2);HCTemp2=HCTemp),
	length(HCTemp2,Length),
	Length > 1,
	last(HC,FullVerb),
	getPosition(FullVerb,HeadPos,Pos),!.

%catchall: leave head unchanged.
fixAttachment(_,_,Pos,Pos) :- !.

%control verbs (subject/object of matrix clause is subject of infinitive clause)

%subject may have auxiliary verb as head; use corresponding full verb to check for control
secedge(Pos,SecHeadPos,subj,_Format) :- output(Pos,_,_,_,subj,HeadPos,_),
            output(HeadPos,_,_,_,_,_,_),
            chart(HeadPos,HeadPos,HeadPos,[_,_,HC,_],_,_,_,_,_,_),
            (member(passive,HC)->ControlType=obja;ControlType=subj), % treat passive subject as accusative object for purpose of control (sie zwingt ihn -> er wurde gezwungen)
            last(HC,FullVerb),
            getPosition(FullVerb,HeadPos,TrueHeadPos),
            output(TrueHeadPos,_,HeadLemma,_,_,_,_),
            (output(_PTKVZPos,_,PTKVZWord,_,avz,_,_)->atom_concat(PTKVZWord,HeadLemma,TrueHeadLemma);TrueHeadLemma=HeadLemma),
            control(ControlType,TrueHeadLemma),
            output(SecHeadPos,_,_,_,obji,TrueHeadPos,_), !.


%general case (subj/obja/objd depending directly on full verb)
secedge(Pos,SecHeadPos,subj,_Format) :- output(Pos,_,_,_,Rel,HeadPos,_),
             output(HeadPos,_,HeadLemma,_,_,_,_),
             (output(_PTKVZPos,_,PTKVZWord,_,avz,_,_)->atom_concat(PTKVZWord,HeadLemma,TrueHeadLemma);TrueHeadLemma=HeadLemma),
             control(Rel,TrueHeadLemma),
             output(SecHeadPos,_,_,_,obji,HeadPos,_), !.

%catchall: no secondary edge found
secedge(_, '_', '_', conll) :- !.
secedge(_, -, -, _Format) :- !.

relative_morphology(HeadPos,RelPos,Tag,Morph) :- output(RelPos,_,_,Tag,_,_,Morph), 
              member(Tag,['PRELS','PRELAT']),
              is_dependent(RelPos,HeadPos),
              RelPos < HeadPos.

%is direct or indirect dependent
is_dependent(Pos,Pos) :- !.

is_dependent(Pos,HeadPos) :- output(Pos,_,_,_,_,BetweenPos,_),
        is_dependent(BetweenPos,HeadPos).

%is direct or indirect dependent, allowing only specific class label
is_dependent(Pos,Pos,_) :- !.

is_dependent(Pos,HeadPos,Class) :- output(Pos,_,_,_,Class,BetweenPos,_),
        is_dependent(BetweenPos,HeadPos,Class).

%ignore all relations not listed here (commas, brackets)
relclass(subj).
relclass(obja).
relclass(obja2).
relclass(objd).
relclass(gmod).
relclass(app_loose).
relclass(app_close).
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
relclass(vok).


%subjects etc. are attached to the finite verb in the gold standard, so there's no need to modify these dependency relations.
leavealone(subj).
leavealone(subjc).
leavealone(aux).
leavealone(konj).
leavealone(kon).
leavealone(konc).
leavealone(neb).
leavealone(konjneb).
leavealone(konjobjc).
leavealone(koord).
leavealone(par).
leavealone(part).
leavealone(vok).

%these classes have topology-dependant attachment rule (attached to finite verb in vorfeld, but to full verb in mittelfeld)
%comment these out if you want to always attach these relations to full verb, or change to 'leavealone' to always attach them to finite verb
topology_dependant(pp).
topology_dependant(adv).
topology_dependant(zeit).


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