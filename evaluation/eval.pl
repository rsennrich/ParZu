%	eval.pl	: takes a gold standard and the result of the parsing and evaluates the latter (true positives, false positives, false negatives, precision, recall). more precise than eval.pl (because it takes word positions into account).

:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- use_module(library(lists)).

:- dynamic result/5, mydebug/0, unattachedcount/1.

start :- start('tueba_eval0-1.pl', 'parser_output.pl').

debug(Class) :- debug('tueba_eval0-1.pl', 'parser_output.pl',Class).

unattached(Class) :- unattached('tueba_eval0-1.pl', 'parser_output.pl',Class).

start(Gold,Test) :- retractall(result(_,_,_,_,_)),
			ensure_loaded(Gold),
			ensure_loaded(Test),
			retractall(result(_,_,_,_,_,_)),
			asserta(result(total,0,0,0,0,0)),
			write('Relation: true positive, false positive, false negative, wrong attachment (should be subset of false positives)'),
			evalclass(Class),
			evalloop(Class),
			printresult(Class),
			fail.

start(_,_) :- compute_unknown_relations,
	      printresult(total), !.

%same as normal eval, but gives debug information about every parsing error.
start(Gold,Test,Class) :- retractall(result(_,_,_,_,_)),
            assert(mydebug),
            ensure_loaded(Gold),
            ensure_loaded(Test),
            retractall(result(_,_,_,_,_,_)),
            asserta(result(total,0,0,0,0,0)),
            write('Relation: true positive, false positive, false negative, wrong attachment (should be subset of false positives)'),
            evalclass(Class),
            evalloop(Class),
            printresult(Class),
            retract(mydebug).

%finds all words that are not assigned a head.
unattached(Gold,Test,Class) :- ensure_loaded(Gold),
            ensure_loaded(Test),
            retractall(unattachedcount(_)),
            assertz(unattachedcount(0)),
	    evalclass(Class),
            unattached(_,Class),
            fail.

unattached(_,_,_) :- !.

compute_unknown_relations :-% between(1,235,Sentence),
			    fn(_, total),
			    fail.

compute_unknown_relations :- !.

evalloop(Class) :-%	between(1,235,Sentence),
			doeval(_, Class),
			fail.

evalloop(_) :- !.

doeval(Sentence, Class) :- tp(Sentence, Class), fail.

doeval(Sentence, Class) :- fp(Sentence, Class), fail.

doeval(Sentence, Class) :- fn(Sentence, Class), fail.

doeval(Sentence, Class) :- wa(Sentence, Class), fail.

doeval(_,_) :- !.

%true positives
tp(Sentence, Class) :-	test(Class,Sentence,_,HeadPos,_,DepPos),
	w(Sentence, DepPos,_, _, Class, HeadPos),
	w(Sentence,HeadPos,_,_,_,_),
	result(Class,TP,FP,FN,WA),
	TPNew is TP + 1,
	retract(result(Class,_,_,_,_)),
	asserta(result(Class,TPNew,FP,FN,WA)),
	retract(result(total,TPT,FPT,FNT,WAT,NA)),
	TPTNew is TPT + 1,
	asserta(result(total,TPTNew,FPT,FNT,WAT,NA)).

%false positives
fp(Sentence, Class) :-	test(Class,Sentence,_,HeadPos,_,DepPos),
	\+ (	w(Sentence, DepPos,_, _, Class, HeadPos),
		w(Sentence,HeadPos,_,_,_,_)
	   ),
    ((mydebug ->
    w(Sentence, DepPos, _, DepTag, ClassTrue, _),
    w(Sentence,HeadPos,_,HeadTag,_,_),
    write('false class: '),
    write(Sentence),
    write(': '),
    write(DepPos),
    write(' ('),
    write(DepTag),
    write(')'),
    write(' - '),
    write(HeadPos),
    write(' ('),
    write(HeadTag),
    write(') gold class: '),
    write(ClassTrue),
    nl);true),
	result(Class,TP,FP,FN,WA),
	FPNew is FP + 1,
	retract(result(Class,_,_,_,_)),
	asserta(result(Class,TP,FPNew,FN,WA)),
	retract(result(total,TPT,FPT,FNT,WAT,NA)),
	FPTNew is FPT + 1,
	asserta(result(total,TPT,FPTNew,FNT,WAT,NA)).

%false negatives
fn(Sentence, Class) :-	w(Sentence, DepPos,_, DepTag, Class, HeadPos),
		w(Sentence,HeadPos,_,HeadTag,_,_),
	\+ test(Class,Sentence,_,HeadPos,_,DepPos),
    ((mydebug ->
    (test(WrongClass,Sentence,_,_,_,DepPos)->true;WrongClass='root'),
    write('not found: '),
    write(Sentence),
    write(': '),
    write(DepPos),
    write(' ('),
    write(DepTag),
    write(')'),
    write(' - '),
    write(HeadPos),
    write(' ('),
    write(HeadTag),
    write(') test class: '),
    write(WrongClass),
    nl);true),
	result(Class,TP,FP,FN,WA),
	FNNew is FN + 1,
	retract(result(Class,_,_,_,_)),
	asserta(result(Class,TP,FP,FNNew,WA)),
	retract(result(total,TPT,FPT,FNT,WAT,NA)),
	FNTNew is FNT + 1,
	asserta(result(total,TPT,FPT,FNTNew,WAT,NA)).


%false negatives (relation unknown)
fn(Sentence, total) :-	w(Sentence, _, _, _, Class, _),
	\+ result(Class,_,_,_,_),
	\+ Class = '-punct-',
	\+ Class = 'root',
	\+ Class = '-unknown-',
	print(Class),
	nl,
	retract(result(total,TPT,FPT,FNT,WAT,NA)),
	NANew is NA + 1,
	asserta(result(total,TPT,FPT,FNT,WAT,NANew)).

%wrong attachment: triggers both an fp and an fn, thus should be subset of both.
wa(Sentence, Class) :-	test(Class,Sentence,_,HeadPos,_,DepPos),
	w(Sentence, DepPos,_, _, Class, NewPos),
	\+ NewPos = HeadPos,
    ((mydebug ->
    w(Sentence, NewPos, _, TrueTag, _, _),
    w(Sentence, HeadPos, _, FalseTag, _, _),
    write('wrong attachment: '),
    write(Sentence),
    write(', '),
    write(DepPos),
    write(': '),
    write(HeadPos),
    write(' ('),
    write(FalseTag),
    write(')'),
    write(' instead of '),
    write(NewPos),
    write(' ('),
    write(TrueTag),
    write(')'),
    nl);true),
	result(Class,TP,FP,FN,WA),
	WANew is WA + 1,
	retract(result(Class,_,_,_,_)),
	asserta(result(Class,TP,FP,FN,WANew)),
	retract(result(total,TPT,FPT,FNT,WAT,NA)),
	WATNew is WAT + 1,
	asserta(result(total,TPT,FPT,FNT,WATNew,NA)).

%no head found, but should have head
unattached(Sentence, Class) :- w(Sentence,DepPos,_,Tag,Class,HeadPos),
    \+ (evalclass(X),
    test(X,Sentence,_,_,_,DepPos)),
    w(Sentence, HeadPos, _, Tag2, _, _),
    retract(unattachedcount(Count)),
    NewCount is Count + 1,
    assertz(unattachedcount(NewCount)),
    write(Count),
    write(' unattached: '),
    write(Sentence),
    write(': '),
    write(DepPos),
    write(' ('),
    write(Tag),
    write(')'),
    write(' - '),
    write(HeadPos),
    write(' ('),
    write(Tag2),
    write(') '),
    write(Class),
    nl.

printresult(total) :- result(total,TP,FP,FN,WA,NA),
		nl,
		write(total),
		write(': '),
		write(TP),
		write(', '),
		write(FP),
		write(', '),
		write(FN),
		write(', '),
		write(WA),
		write(', '),
		write(NA),
		nl,
		write('precision: '),
		(	(	(TP + FP) > 0,
				Precision is TP / (TP + FP),
				format('~4f', Precision)
			)
			;
			write('division by 0 error')
			
		),
		nl,
		write('recall: '),
		(	(	(TP + FN + NA) > 0,
				Recall is TP / (TP + FN + NA),
				format('~4f', Recall)
			)
			;
                        write('division by 0 error')
		),
		nl,
		write('f1: '),
		(	(	TP > 0,
				F1 is Precision*Recall*2 / (Precision + Recall),
                                format('~4f', F1)
			)
			;
                        write('division by 0 error')
		),
		nl, !.

printresult(Class) :- result(Class,TP,FP,FN,WA),
		nl,
		write(Class),
		write(': '),
		write(TP),
		write(', '),
		write(FP),
		write(', '),
		write(FN),
		write(', '),
		write(WA),
		nl,
		write('precision: '),
		(	(	(TP + FP) > 0,
				Precision is TP / (TP + FP),
                                format('~4f', Precision)
			)
			;
                        write('division by 0 error')
		),
		nl,
		write('recall: '),
		(	(	(TP + FN) > 0,
				Recall is TP / (TP + FN),
                                format('~4f', Recall)
			)
			;
                        write('division by 0 error')
		),
% 		nl,
% 		write('f1: '),
% 		(	(	TP > 0,
% 				F1 is Precision*Recall*2 / (Precision + Recall),
%                                 format('~4f', F1)
% 			)
% 			;
%                         write('division by 0 error')
% 		),
		nl, !.



lexic(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'#'),
	sub_atom(WordI,     0,Before,_,Word),
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa),
        (number(I) -> true ; I=1), !.

lexic(WordI,WordI,_) :- !.


%test(+Class, +Sentence, -HeadWord, -Depword)
%defines the mapping between the Class (functional dependency in tueba_dep) and the corresponding predicate in the progresde output.
%when adding new class, don't forget evalclass/1!


%format: relation(word,tag,position,headposition,sentence).

%This clause works if the relationship tag in the gold standard and the progresde output are identical. Otherwise, a new mapping can be defined.
test(Class, Sentence, HeadWord, HeadPos,_,DepPos) :- 	call(word(Sentence,DepPos,_,_,_,Class,HeadPos,_)),
          call(word(Sentence,HeadPos,HeadWord,_,_,_,_,_)).

% test('gmod-app', Sentence, HeadWord, HeadPos,_,DepPos) :- catch(call(gmod-app(HeadWord,_,DepPos,HeadPos,Sentence)),_,fail).




%use comments here to switch evaluation of a specific class on and off
%wehen adding new class, don't forget test/4!
evalclass(subj) :- asserta(result(subj,0,0,0,0)).

evalclass(obja) :- asserta(result(obja,0,0,0,0)).

evalclass(objd) :- asserta(result(objd,0,0,0,0)).

evalclass(gmod) :- asserta(result(gmod,0,0,0,0)).

evalclass(app) :- asserta(result(app,0,0,0,0)).

evalclass(pp) :- asserta(result(pp,0,0,0,0)).

evalclass(pn) :- asserta(result(pn,0,0,0,0)).

evalclass(attr) :- asserta(result(attr,0,0,0,0)).

evalclass(det) :- asserta(result(det,0,0,0,0)).

evalclass(rel) :- asserta(result(rel,0,0,0,0)).

evalclass(konj) :- asserta(result(konj,0,0,0,0)).

evalclass(cj) :- asserta(result(cj,0,0,0,0)).

evalclass(kon) :- asserta(result(kon,0,0,0,0)).

evalclass(aux) :- asserta(result(aux,0,0,0,0)).

evalclass(adv) :- asserta(result(adv,0,0,0,0)).

evalclass(pred) :- asserta(result(pred,0,0,0,0)).

evalclass(objc) :- asserta(result(objc,0,0,0,0)).

evalclass(objp) :- asserta(result(objp,0,0,0,0)).

evalclass(obji) :- asserta(result(obji,0,0,0,0)).

evalclass(neb) :- asserta(result(neb,0,0,0,0)).

evalclass(avz) :- asserta(result(avz,0,0,0,0)).

evalclass(part) :- asserta(result(part,0,0,0,0)).

evalclass(kom) :- asserta(result(kom,0,0,0,0)).

evalclass(subjc) :- asserta(result(subjc,0,0,0,0)).

evalclass(expl) :- asserta(result(expl,0,0,0,0)).

evalclass(s) :- asserta(result(s,0,0,0,0)).

evalclass(zeit) :- asserta(result(zeit,0,0,0,0)).

evalclass(grad) :- asserta(result(grad,0,0,0,0)).

evalclass(objg) :- asserta(result(objg,0,0,0,0)).

evalclass('gmod-app') :- asserta(result('gmod-app',0,0,0,0)).

evalclass(par) :- asserta(result(par,0,0,0,0)).

evalclass(koord) :- asserta(result(koord,0,0,0,0)).

evalclass(vok) :- asserta(result(vok,0,0,0,0)).