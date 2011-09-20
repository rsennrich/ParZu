%	postprocessing.pl
%   Rico Sennrich
%   take the parser output, convert it into the right format and do some postprocessing (non-projective dependencies)
%   deprecated; functionality is now integrated into parser through postprocessing_module.py

:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- use_module(library(lists)).

:- dynamic relationIn/5,relationOut/5,sencounter/1, sent/2, analyses/6.


start(Outputformat) :-  assert(outputformat(Outputformat)),
	    assert(sencounter(1)),
            retractall(relationIn(_,_,_,_,_)),
            retractall(relationOut(_,_,_,_,_)),
			readsentences.

do_loop(Sentence) :- sent(Sentence,_),
            postprocess(Sentence),
	    outputformat(Format),
% 	    nl,
            printresult(Format,Sentence),
            retractall(relationIn(Sentence,_,_,_,_)),
            retractall(relationOut(Sentence,_,_,_,_)),
			retractall(sent(Sentence,_)),
			!.

%the parser chooses the finite verb instead of the full verb as head for all (most) relations. revert this, if possible.
postprocess(Sentence) :-
    relationIn(Sentence,Class,Head,Dep,Dir),
    lexic(Head,_,HeadPos),
    lexic(Dep,_,DepPos),
	fixVerbAttachment(Sentence,Class,DepPos,HeadPos,OutputPos),
	transcodeRel(Sentence,Class,NewClass,DepPos,HeadPos),
    assert(relationOut(Sentence,NewClass,OutputPos,DepPos,Dir)),
    fail.


postprocess(_) :- !.

%reads a line and calls addnumber/1 to identify the sentence and position number.
readsentences :- repeat, 
				read(X),
				assertrelation(X),
				X == end_of_file,
			    !.


%just a small format conversion for convenience's sake.
assertrelation(X) :- (X =.. [Class,Head,Dep,_,Dir,Sentence],relclass(Class)->assert(relationIn(Sentence,Class,Head,Dep,Dir));(X = sent(_,_),assert(X),fail)),
				   sencounter(Sen),
				   ((\+ Sen = Sentence)->(retract(sencounter(_)),
							  assert(sencounter(Sentence)),
							  OldSen is Sentence - 1,
							  between(Sen,OldSen,DoSen),
							  do_loop(DoSen),
							  fail
							  );fail).

%end_of_file has to succeed.
assertrelation(X) :- X == end_of_file, sencounter(Sen),do_loop(Sen),!.

%print results in conll format. disabled by default.
printresult(conll,Sentence) :- 
    sent(Sentence,WordList),
    between(1,100,Pos),
    (relationOut(Sentence,Class,HeadPos,Pos,_Dir)->true;(Class = 'NONE',HeadPos = '0')),
%    nth1(HeadPos,WordList,[_,HeadTag,_,[HeadWord|_]]),
    nth1(Pos,WordList,[_,DepTag,_,[DepWord|_]]),
    write(Sentence),
    write('_'),
    write(Pos),
    tab(3),
    write(DepWord),
    tab(3),
    write(DepTag),
    tab(3),
    write(HeadPos),
    tab(3),
    write(Class),
    nl,
    fail.


%print results in prolog format.
printresult(prolog,Sentence) :- 
    sent(Sentence,WordList),
    between(1,100,Pos),
    relationOut(Sentence,Class,HeadPos,Pos,_Dir),
%    nth1(HeadPos,WordList,[_,HeadTag,_,[HeadWord|_]]),
    nth1(Pos,WordList,[_,DepTag,_,[DepWord|_]]),
	downcase_atom(Class,LowerClass),
    Outline =.. [LowerClass,DepWord,DepTag,Pos,HeadPos,Sentence],
    writeq(Outline),
    write('.'),
    nl,
    fail.

%print results in moses format. default.
printresult(moses,Sentence) :- 
    between(1,100,Pos),
    getOutput(Sentence,Pos,DepWord,DepTag,Class,Head),
    write(DepWord),
    write('|'),
    write(DepTag),
    write('|'),
    write(Class),
    write('|'),
    write(Head),
    write(' '),
    fail.


printresult(_,_) :- nl,!.

getOutput(Sentence,Pos,Dep,Tag,LowerClass,Head) :- sent(Sentence,WordList), 
  nth1(Pos,WordList,[_,Tag,_,[Dep|_]]),
  ((relationOut(Sentence,Class,HeadPos,Pos,_Dir),
  downcase_atom(Class,LowerClass),
  nth1(HeadPos,WordList,[_,_HeadTag,_,[Head|_]]))
  ;(LowerClass=root,Head=none)), !.


lexic(WordI,Word,I) :-
    atomic(WordI), !,
    sub_atom(WordI,Before,1,After,'#'),
    sub_atom(WordI,     0,Before,_,Word),
    Before1 is Before+1,
    sub_atom(WordI,Before1,After,_,Iaaa),
    name(Iaaa,Iaa), name(I,Iaa),
        (number(I) -> true ; I=1), !.

lexic(WordI,WordI,_) :- !.


%given the "Lemma_Tag" information, get the position of the word (and check if it is possible in case there are several identical ones)
getPosition(Sentence,Lemma_Tag,OldPos,Pos) :-
    sent(Sentence,WordList),
	name(Lemma_Tag, Lemma_TagChars),
	append(LemmaChars, [95|TagChars], Lemma_TagChars),
	name(Tag, TagChars),
	name(Lemma, LemmaChars),
    nth1(Pos,WordList,[Lemma,Tag,_,_]), 
	isSameVerbcomplex(Sentence,OldPos,Pos), !.


isSameVerbcomplex(Sentence,HeadPos,DepPos) :- relationIn(Sentence,aux,Head,Dep,_),
    											lexic(Head,_,HeadPos),
    											lexic(Dep,_,DepPos).


%fix expletives. 
transcodeRel(_,explsubj,expl,_,_) :- !.
transcodeRel(_,explobja,expl,_,_) :- !.

transcodeRel(_,konjneb,konj,_,_) :- !.
transcodeRel(_,konjobjc,konj,_,_) :- !.

transcodeRel(_,obja2,obja,_,_) :- !.
transcodeRel(_,objc2,objc,_,_) :- !.

transcodeRel(_,Class,Class,_,_) :- !.


%fix auxiliary relationships: head of auxiliary relationship is not always finite verb, but verb one level higher in the hierarchy.
fixVerbAttachment(Sentence,aux,DepPos,HeadPos,Pos) :-
    sent(Sentence,WordList),
    nth1(HeadPos,WordList,[HeadWord,HeadTag,HC,_]),
	atom_concat(HeadWord,'_',HeadTemp),
	atom_concat(HeadTemp,HeadTag,HeadX),
    nth1(DepPos,WordList,[DepWord,DepTag,_,_]),
	atom_concat(DepWord,'_',DepTemp),
	atom_concat(DepTemp,DepTag,Dep),
	nth1(DPos,HC,Dep),
	HPos is DPos - 1,
	nth1(HPos,HC,Head),
	(Head=HeadX->Pos=HeadPos;getPosition(Sentence,Head,HeadPos,Pos)), !.


%fix attachment for prepositional phrases and adverbs (attached to finite verb in vorfeld, but to full verb in mittelfeld)
fixVerbAttachment(Sentence,Class,DepPos,HeadPos,Pos) :-
	(Class=pp;Class=adv),
    sent(Sentence,WordList),
    nth1(HeadPos,WordList,[_,Tag,HC,_]),
	(Tag='VAFIN';Tag='VMFIN';Tag='VAINF';Tag='VMINF';Tag='VAPP';Tag='VMPP'),
	(append(mainclause,HC,HCTemp);HCTemp=HC),
	(append(passive,HCTemp,HCTemp2);HCTemp2=HCTemp),
	length(HCTemp2,Length),
	Length > 1,
	last(HC,FullVerb),
	getPosition(Sentence,FullVerb,HeadPos,Pos),
	%either our token is in the mittelfeld (DepPos > HeadPos) or the full verb itself comes before the finite verb (verbletztstellung or topicalisation of full verb)
	(DepPos > HeadPos;Pos < HeadPos), !.

%fix explsubj attachment. strange rule in gold standard, but oh well...
fixVerbAttachment(Sentence,explsubj,DepPos,HeadPos,Pos) :-
    sent(Sentence,WordList),
    relationIn(Sentence,subjc,Head,Dep,_Dir),
    lexic(Head,_,HeadPos),
    lexic(Dep,_,Pos), !.

%all other cases: dependents that are attached to finite verb should be attached to full verb instead.
fixVerbAttachment(Sentence,Class,_,HeadPos,Pos) :- 
    sent(Sentence,WordList),
	\+ leavealone(Class),
    nth1(HeadPos,WordList,[_,Tag,HC,_]),
	(Tag='VAFIN';Tag='VMFIN';Tag='VAINF';Tag='VMINF';Tag='VAPP';Tag='VMPP'),
	(append(mainclause,HC,HCTemp);HCTemp=HC),
	(append(passive,HCTemp,HCTemp2);HCTemp2=HCTemp),
	length(HCTemp2,Length),
	Length > 1,
	last(HC,FullVerb),
	getPosition(Sentence,FullVerb,HeadPos,Pos),!.

%catchall: leave head unchanged.
fixVerbAttachment(_,_,_,Pos,Pos) :- !.



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
