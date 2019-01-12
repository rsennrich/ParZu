%	preprocessing.pl : different functions: lemmatisation, chunking, adding information from morphological analysis

%search for 'uncomment' to see comments about different output formats.

%:- open_null_stream(Null),set_prolog_IO(user_input,user_output,Null).

:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- use_module(library(lists)).

:- dynamic sentno/1, posno/1, w/6, w/4, lvl/4, mainclause/1, completed/2, lemmatisation/1, morphology/1,sentdelim/1.

:- ensure_loaded('preprocessing_verbchunkmodule.pl').

:- assert(morphology(gertwol)).
:- assert(lemmatisation(gertwol)).
:- assert(sentdelim('$.')).

correct_mistagging(yes).

% disallow 'können + VVPP' and similar.
% Can decrease parsing quality if verbs are mistagged ("Er_PERS kann_VMFIN empfangen_VVPP"), but allows tagging errors to be fixed through n-best tagging.
enforce_aux_agreement(yes). 

%reads from stdin; writes to stdout
start(GERTWOL) :- start2(GERTWOL, user_input, user_output).

%reads from file, writes to stdout
start(GERTWOL, F) :- open(F, read, Stream, [encoding(utf8)]), start2(GERTWOL, Stream, user_output).

%reads from file, writes to file
start(GERTWOL, F, G) :- open(F, read, Stream, [encoding(utf8)]), open(G, write, OutStream, [encoding(utf8)]), start2(GERTWOL, Stream, OutStream),
close(Stream), close(OutStream).

start2(GERTWOL, Stream, OutStream) :- retractall(w(_,_,_,_,_,_)), retractall(w(_,_,_,_)), retractall(sentno(_)), retractall(posno(_)), retractall(completed(_,_)), retractall(mainclause(_)), retractall(lvl(_,_,_,_)),
		   ((morphology(none);morphology(keep))->true;consult(GERTWOL)),
		   assert(sentno(1)),
		   assert(posno(1)),
		   readsentences(Stream, OutStream).


%reads a line and calls addnumber/1 to identify the sentence and position number.
readsentences(Stream, OutStream) :- repeat,
				read(Stream,X),
				addnumber(X, OutStream),
				X == end_of_file,
			    !.

%addnumber/1: at the end of a sentence, posno is reset to 1 and sentno incremented by one. otherwise, posno is incremented by one.

%end of sentence
addnumber(X, OutStream) :- sentdelim(SentDelim),
            X = w(Word,SentDelim,_,_C),
			sentno(Sentence),
			posno(Pos),
            jointag(Word,SentDelim,Chunk),
			assert(w(Sentence, Pos, Word,SentDelim,[Chunk],[-,-])),
			retract(sentno(_)),
			NewSentence is Sentence + 1,
			assert(sentno(NewSentence)),
			retract(posno(_)),
			NewPos is 1,
			assert(posno(NewPos)),
		    dochunking(Sentence),
		    printwords(Sentence, OutStream),
		    retractall(w(Sentence,_,_,_,_,_)),
			!.

%new word within sentence.
addnumber(X, _) :-		X = w(Word,Tag,_,C),
			sentno(Sentence),
			posno(Pos),
			(lemmatisation(none) -> Lemma=Word, NewTag=Tag;getlemma(Word, Tag, Lemma,NewTag)),
			jointag(Lemma,NewTag,Chunk),
			assert(w(Sentence, Pos, Word,NewTag,[Chunk],C)),
			retract(posno(_)),
			NewPos is Pos + 1,
			assert(posno(NewPos)),
			!.

%end_of_file has to succeed.
addnumber(X, _) :- X == end_of_file, !.


printwords(Sentence, OutStream) :-
            sentdelim(SentDelim),
			w(Sentence,StopPos,_,SentDelim,_,_),
			nl(OutStream),
			between(1,StopPos,Pos),
			writedown(Sentence,Pos,OutStream),
			fail.

printwords(_, _) :- !.

%cycles through all the words (this method makes sure that the print-out is in the right order) and calls writedown every time.
printwords :- sentdelim(SentDelim),
            sentno(SenNum),
			between(1,SenNum,Sentence),
			w(Sentence,StopPos,_,SentDelim,_,_),
			nl,
			between(1,StopPos,Pos),
			writedown(Sentence,Pos),
			fail.

printwords :- !.

%writedown(+SenNr,+PosNr,+OutStream)
writedown(Sentence,Pos,OutStream) :-
			w(Sentence,Pos,Word,Tag,Chunk,MorphIn),
			(lemmatisation(none) -> Lemma=Word;getlemma(Word, Tag, Lemma,Tag)),
			(morphology(keep) -> MorphOut = MorphIn;buildmorphology(Word,Tag, MorphOut)), 
%Next line is commented for Progresde-compatible input. Uncommenting might be useful for debugging purposes
%                       Outline =.. [w,Sentence, Pos, Lemma,Tag,Chunk,MorphOut],
                        Outline =.. [w,Lemma,Tag,Chunk,MorphOut],
                        writeq(OutStream, Outline),
                        write(OutStream, '.'),
                        nl(OutStream),
                        !.


%cycles through all sentences and calls idstart/3.
dochunking  :- 	sentno(SenNum),
		between(1,SenNum,Sentence),
		retractall(mainclause(_)),
		retractall(completed(_,_)),
		retractall(lvl(_,_,_,_)),
		idstart(Sentence, 1,1),
		cleanup_chunking,
		fail.

dochunking.


dochunking(Sentence) :- retractall(mainclause(_)),
		retractall(completed(_,_)),
		retractall(lvl(_,_,_,_)),
		idstart(Sentence, 1,1).

%==============================================================================
%morphology and lemmatising


buildmorphology(Word,_, [Word|[_]]) :- morphology(none), !.

%exception: deren and dessen as attributive pronouns don't need to agree with their heads.
buildmorphology(dessen,'PRELAT',[dessen,_]) :-  !.
buildmorphology(deren,'PRELAT',[deren,_]) :- !.
buildmorphology(dessen,'PDAT',[dessen,_]) :-  !.
buildmorphology(deren,'PDAT',[deren,_]) :- !.

% Gertwol doesn't distinguish between modal/auxiliary/full verbs
buildmorphology(Word,'VAFIN',MorphOut) :- (\+ gertwol(Word,_,'VAFIN',_, _))-> buildmorphology(Word,'VVFIN',MorphOut).
buildmorphology(Word,'VMFIN',MorphOut) :- (\+ gertwol(Word,_,'VMFIN',_, _))-> buildmorphology(Word,'VVFIN',MorphOut).

%exception: nouns may be substantivized adjectives.
buildmorphology(Word,'NN',[Word|ListOut]) :- findall(Morph,gertwol(Word,_,'NN',Morph,_),ListTemp),
				      findall([Gender,Case,Number,_],gertwol(Word,_,'ADJA',[_,Gender,Case,Number,'Sw'],_),ListTemp2),
				      append(ListTemp,ListTemp2,ListTemp3),
				      (is_uninstantiated(ListTemp3) -> ListOut = [_] ; translatemorphs(ListTemp3,'NN', ListTemp4), sort(ListTemp4,ListTemp5), my_remove_duplicates(ListTemp5,ListOut)), !.

%exception: PIDAT/PIAT is inconsistent between tagger / morphology systems. Don't distinguish between them.
buildmorphology(Word,'PIDAT',MorphOut) :- (\+ gertwol(Word,_,'PIDAT',_,_), gertwol(Word,_,'PIAT',_,_)) -> buildmorphology(Word,'PIAT',MorphOut).
buildmorphology(Word,'PIAT',MorphOut) :- (\+ gertwol(Word,_,'PIAT',_,_), gertwol(Word,_,'PIDAT',_,_)) -> buildmorphology(Word,'PIDAT',MorphOut).

%exception: viele/wenige are PIS/PIDAT in TreeTagger, but ADJA in Gertwol
buildmorphology(Word,Tag,[Word|ListOut]) :- (Tag = 'PIS';Tag='PIDAT'),
				      \+ gertwol(Word,_,Tag,_,_),
				      findall([Gender,Case,Number],gertwol(Word,_,'ADJA',[_,Gender,Case,Number,_],_),ListTemp),
				      (is_uninstantiated(ListTemp) -> ListOut = [_] ; sort(ListTemp,ListOut)), !.

%exception: Wegen dem Dativ gibt es immer weniger Präpositionen, die nur mit Genitiv benutzt werden
buildmorphology(Word,'APPR',[Word|ListOut2]) :- findall(Morph,gertwol(Word,_,'APPR',Morph,_),ListTemp),
                                        (is_uninstantiated(ListTemp) -> ListOut = [_] ; translatemorphs(ListTemp,'APPR', ListOut)), (ListOut = [['Gen']]->append(ListOut,[['Dat']],ListOut2);ListOut2=ListOut), !.


%adjectives; include information on whether it's a participial one or not:
buildmorphology(Word,'ADJA',[Word|ListOut]) :- findall([Deg,Gender,Case,Number,Class,Original],gertwol(Word,_,'ADJA',[Deg,Gender,Case,Number,Class],Original),ListTemp),
                    (is_uninstantiated(ListTemp) -> ListOut = [_] ; translatemorphs(ListTemp,'ADJA', ListTemp2), sort(ListTemp2,ListTemp3), my_remove_duplicates(ListTemp3,ListOut)), !.

buildmorphology(Word,'ADJD',[Word|ListOut]) :- findall([Degree,Original],gertwol(Word,_,'ADJD',[Degree],Original),ListTemp),
                    (is_uninstantiated(ListTemp) -> ListOut = [_] ; translatemorphs(ListTemp,'ADJD', ListTemp2), sort(ListTemp2,ListTemp3), my_remove_duplicates(ListTemp3,ListOut)), !.

% general case:
buildmorphology(Word,Tag,[Word|ListOut]) :- findall(Morph,gertwol(Word,_,Tag,Morph,_),ListTemp),
                                        (is_uninstantiated(ListTemp) -> ListOut = [_] ; translatemorphs(ListTemp,Tag, ListTemp2), sort(ListTemp2,ListTemp3), my_remove_duplicates(ListTemp3,ListOut)), !.

translatemorphs([],_,[]) :- !. %break condition.


translatemorphs([In|Ins],'NN',[Out|Outs]) :- translatelist('NN',In,Out), !,
                                           translatemorphs(Ins,'NN',Outs). %catchall.

translatemorphs([In|Ins],Tag,[Out|Outs]) :- translatelist(general,In,Out), !,
					    translatemorphs(Ins,Tag,Outs). %catchall.


translatelist(general,[],[]) :- !. %break condition


%throw last member of noun morphology away.
translatelist('NN',[_],[]) :- !.

translatelist('NN',[In|Ins],[Out|Outs]) :-  !,         morphmapping(In,Out), !,
                                               translatelist('NN',Ins,Outs).

translatelist(general,[In|Ins],[Out|Outs]) :- morphmapping(In,Out), !,
						translatelist(general,Ins,Outs).

%if all elements in the list are variable, output should be variable '_'
is_uninstantiated([]) :- !.

is_uninstantiated([Elem]) :- var(Elem), !.

is_uninstantiated([List]) :- is_uninstantiated(List).

is_uninstantiated([First|Rest]) :- var(First),
			is_uninstantiated(Rest).

morphmapping(*,_).

morphmapping(X,X). %catchall


%override some tagger analyses.

getlemma(Word,Tag,Word,Tag) :- morphology(none), !.
getlemma(Word,Tag,Word,Tag) :- morphology(keep), !.

%noch (in 'weder x noch y') is often mistagged as adverb
getlemma(noch,_,noch,'KON') :- correct_mistagging(yes), w(_,_,Word,_,_,_),member(Word,[weder,'Weder']), !.

% Sometimes, TreeTagger has problems with Ein.
getlemma('Ein',_,ein,'ART') :- correct_mistagging(yes), !.
getlemma('Einen',_,ein,'ART') :- correct_mistagging(yes), !.
getlemma('Einem',_,ein,'ART') :- correct_mistagging(yes), !.

%normal case: tagger and morphology system agree
getlemma(Word,Tag,Lemma,Tag) :- gertwol(Word,Lemma,Tag,_Analysis, _), \+ Lemma = '<unknown>', !.

%exception: PIDAT/PIAT is inconsistent between tagger / morphology systems. Allow look-up of lemma from other class.
getlemma(Word,'PIDAT',Lemma, 'PIDAT') :- \+ gertwol(Word,_,'PIDAT',_,_), gertwol(Word,Lemma,'PIAT',_Analysis, _), \+ Lemma = '<unknown>', !.
getlemma(Word,'PIAT',Lemma, 'PIAT') :- \+ gertwol(Word,_,'PIAT',_,_), gertwol(Word,Lemma,'PIDAT',_Analysis, _), \+ Lemma = '<unknown>', !.

% Gertwol doesn't distinguish between modal/auxiliary/full verbs
getlemma(Word,'VAFIN',Lemma,'VAFIN') :- morphology(gertwol), gertwol(Word,Lemma,'VVFIN',_Analysis,_), \+ Lemma = '<unknown>', !.
getlemma(Word,'VAINF',Lemma,'VAINF') :- morphology(gertwol), gertwol(Word,Lemma,'VVINF',_Analysis,_), \+ Lemma = '<unknown>', !.
getlemma(Word,'VAPP',Lemma,'VAPP') :- morphology(gertwol), gertwol(Word,Lemma,'VVPP',_Analysis,_), \+ Lemma = '<unknown>', !.
getlemma(Word,'VMFIN',Lemma,'VMFIN') :- morphology(gertwol), gertwol(Word,Lemma,'VVFIN',_Analysis,_), \+ Lemma = '<unknown>', !.
getlemma(Word,'VMINF',Lemma,'VMINF') :- morphology(gertwol), gertwol(Word,Lemma,'VVINF',_Analysis,_), \+ Lemma = '<unknown>', !.
getlemma(Word,'VMPP',Lemma,'VMPP') :- morphology(gertwol), gertwol(Word,Lemma,'VVPP',_Analysis,_), \+ Lemma = '<unknown>', !.


%catchall if all else fails.
getlemma(Word,Tag,Word,Tag) :- findall(Tag2, (gertwol(Word,Lemma,Tag2,_, _),\+ Lemma = '<unknown>'), List),
			    length(List,Len),
			    sort(List,AltList),
			    (Len > 0 -> (write('%word/tag combination not found in morph. analyzer:'), write(Word), write(' '),write(Tag),write(' - alternatives proposed by analyzer: '), write(AltList), write('\n'));true),
			    !. 


%standard sort only removes duplicates if no variables are involved.
%we want [[_,'Akk'],[_,'Akk']] to be reduced to [[_,'Akk']]
my_remove_duplicates(L,Unique) :- duplicate_check(L,[],UniqueTmp), (is_all_var(UniqueTmp)->Unique=_;(UniqueTmp=[UniqInner],is_all_var(UniqInner))->Unique=[_];Unique=UniqueTmp).

duplicate_check([],Acc,Acc) :- !.
duplicate_check([H|T],Acc,Unique) :- \+ member(H,Acc), !, duplicate_check(T,[H|Acc],Unique).
duplicate_check([_|T],Acc,Unique) :- duplicate_check(T,Acc,Unique).

is_all_var([]).
is_all_var([Element|Rest]) :- var(Element), is_all_var(Rest).

%==============================================================================


%sample call
%:- nl,write('copy & paste:'), nl, write('start(\'tueba_input.pl\', \'tueba_vchunk.pl\').'), nl, write(' ').
