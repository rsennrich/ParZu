% :- dynamic sentno/1, posno/1, w/6, w/4, lvl/4, completed/2.

:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.

:- ensure_loaded('../core/helper_predicates.pl').

%verbchunking, subclause detection, passive detection
%==============================================================================


%idstart/3. Open search strategy. No restrictions as to whether we are in a main or in a subclause, and no auxiliary or modal verb that requires a full verb.


% idstart(_,Pos,_) :- write(Pos),nl,fail.

%first case: finite full verb. no further search, but assert for mainclause recognition
idstart(Sentence,Pos,LVL) :- w(Sentence,Pos,_Word,'VVFIN',[String],_),
				assert(lvl(LVL,Pos,String,head)),
				NewPos is Pos + 1,
				findfreelvl(LVL, NewLVL),
				idstart(Sentence,NewPos,NewLVL), !.


%Exception: vvpp might be mistagged finite verb (leave out for now; too many false positives)
% idstart(Sentence,Pos,LVL) :-    w(Sentence,Pos,Word,'VVPP',[String],C),
% 				RightPos is Pos + 1,
% 				w(Sentence,RightPos,_,RightTag,_,_),
% 				\+(puncttag(RightTag);verbtag(RightTag);functtag2(RightTag);RightTag='APPR';RightTag='APPRART'),
% 				nofinkon(Sentence,Pos),
% 				forcetag(Word,'VVPP',Lemma,'VVFIN'),
% 				jointag(Lemma,'VVFIN',NewString),
% 				assert(lvl(LVL,Pos,NewString,head)),
% 				retract(w(Sentence,Pos,Word,'VVPP',[String],C)),
% 				assert(w(Sentence,Pos,Word,'VVFIN',[NewString],C)),
% 				findfreelvl(LVL, NewLVL), !,
% 				idstart(Sentence,RightPos,NewLVL), !.

%first case: finite auxiliary or modal verb. idmain is called to search for full verb.
idstart(Sentence,Pos,LVL) :- w(Sentence,Pos,_Word,Tag,[String],_),
%				finverb(Tag),
				(Tag = 'VAFIN' ; Tag = 'VMFIN'),
				assert(lvl(LVL,Pos,String,head)),
				NewPos is Pos + 1,
				idmain(Sentence, NewPos, LVL,EndPos, yes), 
				findfreelvl(LVL, NewLVL),
				idstart(Sentence,EndPos,NewLVL), !.

%second case: subordinated (or relative) clause is introduced with function word: idsub is called to find verb chunk.
idstart(Sentence,Pos,LVL) :- w(Sentence,Pos,_Word,Tag,_,_),
				functtag(Tag),
				assert(lvl(LVL,Pos,Tag,x)),
				NewPos is Pos + 1,
				idsub(Sentence, NewPos, LVL,EndPos),
				findfreelvl(LVL, NewLVL),
				idstart(Sentence,EndPos,NewLVL), !.

%third clause: topicalised full verb just before finite verb
%Remove this for better precision, but worse recall.
idstart(Sentence,Pos,LVL) :- w(Sentence,Pos,_Word,Tag,[String],_),
				fullverbcand(Tag),
				assert(lvl(LVL,Pos,String,full)),
				getverbgroupsub(Sentence,LVL,Pos,EndPos),
				headAuxiliar(Sentence,LVL),
				findfreelvl(LVL, NewLVL), !,
				idstart(Sentence,EndPos,NewLVL).


%fourth clause: fixes some problems introduced in third clause. Namely, a sentence-initial infinitive object ("ihn zu kennen war eine Ehre") will be treated like a subordinated clause (finishing the search after encountering an infinitive).
idstart(Sentence,Pos,LVL) :- w(Sentence,Pos,_Word,'PTKZU',_,_),
				assert(lvl(LVL,Pos,y,ptkzu)),
				assert(lvl(LVL,Pos,'KOUS',x)),
				NewPos is Pos + 1,
				idsub(Sentence,NewPos,LVL,EndPos),
				retract(lvl(LVL,Pos,'KOUS',x)),
				findfreelvl(LVL, NewLVL), !,
				idstart(Sentence,EndPos,NewLVL).

%verbal particle found. A bit of a guess, since we cannot be sure that LastLVL is LVL - 1, but it should work most of the time.
idstart(Sentence, Pos, LVL) :- w(Sentence,Pos,_Word,'PTKVZ',[String],_),
			  LastLVL is LVL - 1,
			  assert(lvl(LastLVL,Pos,String,ptkvz)),
			  NewPos is Pos + 1, !,
			  idstart(Sentence,NewPos,LVL).

%sentence finished
idstart(Sentence,Pos,_) :- sentdelim(SentDelim),
              w(Sentence,Pos,_,SentDelim,_,_), 
			   \+ (w(Sentence,NewPos,_,SentDelim,_,_), NewPos > Pos),
			docomplete(Sentence), !.

%sentence finished
idstart(Sentence,Pos,_) :- \+ (w(Sentence,NewPos,_,_,_,_), NewPos >= Pos),
            docomplete(Sentence), !.

%nothing found - repeat search with next position.
idstart(Sentence,Pos,LVL) :- sentdelim(SentDelim),
                w(Sentence,_,_,SentDelim,_,_),
			   NewPos is Pos + 1, !,
			   idstart(Sentence,NewPos,LVL).

%catchall. needed for non-existing sentences (which don't have a full stop).
idstart(_,_,_) :- !.


%idmain(+Sentence,+Pos,+LVL,-EndPos, +ExpectFullVerb): if a finite verb is found in Verbzweitstellung, this clauses searches for non-finite verbs or verbal particles dependent from it. 

%non-finite full verb found. make sure that the finite verb is not a full verb itself and enter the third search strategy (getverbgroupmain).
idmain(Sentence, Pos, LVL, EndPos, ExpectFullVerb) :- w(Sentence,Pos,_Word,Tag,[String],_),
			  fullverbcand(Tag),
			  headAuxiliar(Sentence,LVL),
			  (finverb_follows(Sentence,Pos)->ExpectFullVerb=yes;true),
			  assert(lvl(LVL,Pos,String,full)), !,
			  getverbgroupmain(Sentence,LVL,Pos, EndPos).

%non-finite full verb found. exploring possibility that it is topicalised full verb. (Das ist zwar super, doch sehen will das niemand)
idmain(Sentence, Pos, LVL, EndPos, no) :- w(Sentence,Pos,_Word,Tag,[String],_),
                          fullverbcand(Tag),
                          headAuxiliar(Sentence,LVL),
                          finverb_follows(Sentence,Pos),
                          findfreelvl(LVL, NewLVL), !,
                          assert(lvl(NewLVL,Pos,String,full)),
                          getverbgroupsub(Sentence,NewLVL,Pos,EndPos).

%verbal particle found. If full verb follows, enter third search strategy (getverbgroupmain); else, stop search.
idmain(Sentence, Pos, LVL, EndPos, ExpectFullVerb) :- w(Sentence,Pos,_Word,Tag,[String],_),
			  Tag = 'PTKVZ',
			  assert(lvl(LVL,Pos,String,ptkvz)),
			  ((NewPos is Pos + 1,
			  w(Sentence,NewPos,_Word2,Tag2,[_String2],_),
			  (fullverbcand(Tag2);Tag2='PTKZU'))->idmain(Sentence,NewPos,LVL,EndPos,ExpectFullVerb);EndPos is Pos + 1), !.


%other finite verb found. Check for possibility of tagging error.
idmain(Sentence, Pos, LVL, EndPos, yes) :- w(Sentence,Pos,Word,Tag,_,_),
			  finverb(Tag), 
			  headAuxiliar(Sentence,LVL),
			  nofinkon(Sentence,Pos),
			  forcetag(Word,Tag,Lemma,NewTag),
			  fullverbcand(NewTag), 
			  jointag(Lemma,NewTag,NewString),
			  retract(w(Sentence,Pos,Word,Tag,[_String],C)),
			  assert(w(Sentence,Pos,Word,NewTag,[NewString],C)),
			  assert(lvl(LVL,Pos,NewString,full)), !,
  			  getverbgroupmain(Sentence,LVL,Pos, EndPos).


%other finite verb found. This stops the search for more dependents.
idmain(Sentence, Pos, _LVL, Pos, _) :- w(Sentence,Pos,_Word,Tag,_,_),
			  finverb(Tag), !.

%sentence ends. This stops the search for more dependents.
idmain(Sentence,Pos,_,Pos, _) :- w(Sentence,Pos,_,'$.',_,_), !.

%sentence ends. This stops the search for more dependents.
idmain(Sentence,Pos,_,Pos, _) :- sentdelim(SentDelim),
                        w(Sentence,Pos,_,SentDelim,_,_), !.

%embedded subordinated clause found. The algorithm first completes this embedded clause and then continues searching for dependents.
idmain(Sentence, Pos, LVL, EndPos, ExpectFullVerb) :- w(Sentence,Pos,_Word,Tag,_,_),
			  functtag(Tag),
		    	  findfreelvl(LVL, NewLVL),
			  assert(lvl(NewLVL,Pos,Tag,x)),
			  NewPos is Pos + 1, !,
			  idsub(Sentence,NewPos, NewLVL,SubEnd),
			  idmain(Sentence,SubEnd,LVL,EndPos, ExpectFullVerb).


%embedded infinitive clause found. The algorithm first completes this embedded clause and then continues searching for dependents.
idmain(Sentence, Pos, LVL, EndPos, _) :- w(Sentence,Pos,_,'PTKZU',_,_),
			  RightPos is Pos+1,
			  \+ (w(Sentence,RightPos,_,Tag2,_,_), (Tag2 = 'ADJA';Tag2='ADJD')), %ein ernst zu nehmendes Urteil: don't search for infinite verb
			  findfreelvl(LVL, NewLVL),
			  NewPos is Pos + 1, !,
			  assert(lvl(NewLVL,Pos,'PTKZU',ptkzu)),
			  idsub(Sentence,NewPos,NewLVL, EndSub),
			  idmainzu(Sentence,EndSub, LVL,NewLVL,EndPos).


%infinitive verbs. only accept if head lemma is "sein", and no comma is in between.
idmain(Sentence, Pos, LVL, EndPos, _) :- w(Sentence,Pos,_,'VVIZU',[String],_),
			      lvl(LVL,HeadPos,String2,head),
			      jointag(sein,'VAFIN',String2),
			  \+ (w(Sentence,XPos,_,'$,',_,_), HeadPos < XPos, XPos < Pos),
			  headAuxiliar(Sentence,LVL),
			  assert(lvl(LVL,Pos,String,full)), !,
			  getverbgroupmain(Sentence,LVL,Pos, EndPos).


%probably ist + predicate. ExpectFullVerb set to 'no', so if you find finite verb after this, don't consider it a possible tagging error.
idmain(Sentence,Pos,LVL,EndPos, _) :- w(Sentence,Pos,_,'$,',_,_), 
			      lvl(LVL,_,String,head),
			      jointag(sein,'VAFIN',String),
			      ((LeftPos is Pos - 1,
			      w(Sentence,LeftPos,_,Tag,_,_),
			      member(Tag,['ADV','ADJD','$(']));
			      (RightPos is Pos + 1,
			      w(Sentence,RightPos,_,Tag,_,_),
			      member(Tag,['ADV','KON','$(']))
			      ),
            NewPos is Pos + 1, !,
            idmain(Sentence,NewPos,LVL, EndPos, no).

%nothing found, but sentence continues. Increment by one and continue search recursively.
idmain(Sentence,Pos,LVL, EndPos, ExpectFullVerb) :- sentdelim(SentDelim),
                w(Sentence,XPos,_,SentDelim,_,_), 
                XPos > Pos,
                NewPos is Pos + 1, !,
                w(Sentence,Pos,_,Tag,_,_),
                (Tag = 'VVIZU'->ExpectFullVerb2=no;ExpectFullVerb2=ExpectFullVerb),
                idmain(Sentence,NewPos,LVL, EndPos, ExpectFullVerb2).

%catchall. should not be needed, since recursive clause should always succeed.
idmain(_,Pos,_,Pos, _) :- !.





%idmainzu(+Sentence,+Pos,+LVL,+LVL2,-EndPos): if a finite verb is found in Verbzweitstellung, and there is an embedded infinitive clause, check if it belongs to the main clause.
idmainzu(Sentence,Pos,LVL,LVL2,EndPos) :- w(Sentence,Pos,_,Tag,_,_),
					  (verbtag(Tag)->idmain(Sentence,Pos,LVL,EndPos, yes);
					  (((lvl(LVL,HeadPos,_,head),\+ (w(Sentence,XPos,_,'$,',_,_), HeadPos < XPos, XPos < Pos), headAuxiliar(Sentence, LVL)) % no comma allowed between head and infinitive verb (otherwise it probably is infinitive object)
					   ->shift_lvl(LVL2,LVL);true)
					  ,EndPos=Pos)).

shift_lvl(LVLIn,LVLOut) :- retract(lvl(LVLIn,Pos,Tag2,Type)),
			assert(lvl(LVLOut,Pos,Tag2,Type)),
			fail.

shift_lvl(_,_) :- !.
					  

%there are some signs that a finite verb is probably part of a new verb clause.
nofinkon(Sentence,Pos) :- LeftPos is Pos - 1,
			     w(Sentence,LeftPos,_,'KON',_,_), !, fail.

nofinkon(Sentence,Pos) :- LeftPos is Pos - 1,
			     w(Sentence,LeftPos,_,'$,',_,_), !, fail.

% nofinkon(Sentence,Pos) :- RightPos is Pos - 1,
% 			     w(Sentence,LeftPos,_,'$,',_,_), !, fail.

nofinkon(Sentence,Pos) :- LeftPos is Pos - 1,
			  w(Sentence,LeftPos,_,'$(',_,_), !,
			  nofinkon(Sentence,LeftPos).

nofinkon(_,_) :- !.


%if we see a non-finite verb immediately followed by a finite verb, this is an indication that they belong together, and that the non-finite verb is not completing an earlier verb matrix.
%"Sein Zustand ist miserabel, doch gesichert scheint"
%same for PTKZU: "Er war sich sicher, ihn schon früher gesehen zu haben"
finverb_follows(Sentence,Pos) :- NewPos is Pos+1,
                           w(Sentence,NewPos,_Word,Tag,_,_),
                           (finverb(Tag); Tag='PTKZU').

%idsub(+Sentence,+Pos,+LVL,-EndPos): Similar to idmain, but not initialised by finite verb, but by a word class that triggers a subordinated sentence.


%infinitive verb found (and expected because of conjuction). This stops the search for more dependents.
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,'VVIZU',[String],_),
                lvl(LVL,_KonjPos,'KOUI',x),
                assert(lvl(LVL,Pos,String,head)),
                EndPos is Pos + 1, !.

%non-finite verb found. Check for possibility of tagging error. (could be finite verb).
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,Word,Tag,_,_),
			  fullverbcand(Tag),
			  RightPos is Pos + 1,
			  w(Sentence,RightPos,_,Tag2,_,_),
              sentdelim(SentDelim),
			  member(Tag2,['$,','$.','KON','KOKOM',SentDelim]),
			  \+ lvl(LVL,_,_,ptkzu),
			  \+ lvl(LVL,_,'KOUI',x),
			  forcetag(Word,Tag,Lemma,NewTag),
			  finverb(NewTag), 
			  jointag(Lemma,NewTag,NewString),
			  retract(w(Sentence,Pos,Word,Tag,_,C)),
			  assert(w(Sentence,Pos,Word,NewTag,[NewString],C)),
			  assert(lvl(LVL,Pos,NewString,head)), 
			  EndPos is Pos + 1, !.



%non-finite verb found. Check for possibility of tagging error. (could be finite verb). special case: "dass wir haben beitragen können" -> tagger might tag "können" as finite and haben as infinite: switch this.
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,Word,'VAINF',_,_),
			  (Word = 'haben'; Word = 'werden'), %first word is haben or werden
			  RightPos is Pos + 1,
			  w(Sentence,RightPos,_,Tag2,[MiddleString],_), % second word is infinitive
			  fullverbcand(Tag2),
			  RightRightPos is Pos + 2,
			  w(Sentence,RightRightPos,Word2,Tag3,_,_), % third word is modal
			  (Tag3='VMINF';Tag3='VMFIN'),
 			  forcetag(Word,'VAINF',Lemma,'VAFIN'),
 			  forcetag(Word2,Tag3,Lemma2,'VMINF'),
			  jointag(Lemma,'VAFIN',NewString), % force tag 'VAFIN' for first word
			  retract(w(Sentence,Pos,Word,_,_,C)),
			  assert(w(Sentence,Pos,Word,'VAFIN',[NewString],C)),
			  assert(lvl(LVL,Pos,NewString,head)), 
			  jointag(Lemma2,'VMINF',NewString2), % force tag 'VMINF' for third word
			  retract(w(Sentence,RightRightPos,Word2,_,_,C2)),
			  assert(w(Sentence,RightRightPos,Word2,'VMINF',[NewString2],C2)),
			  assert(lvl(LVL,RightPos,MiddleString,full)),
			  assert(lvl(LVL,RightRightPos,NewString2,aux)), !,
			  EndPos is RightRightPos + 1, !.


%non-finite fullverb found. Other verbs should follow immediately.
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,Tag,[String],_),
			  fullverbcand(Tag),
			  assert(lvl(LVL,Pos,String,full)),
			  getverbgroupsub(Sentence,LVL,Pos, EndPos), !.


%embedded subordinated clause found. The algorithm first completes this embedded clause and then continues searching for dependents.
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,Tag,_,_),
			  functtag(Tag),
		    	  findfreelvl(LVL, NewLVL),
			  assert(lvl(NewLVL,Pos,Tag,x)),
			  NewPos is Pos + 1, !,
			  idsub(Sentence,NewPos, NewLVL,SubEnd),
			  idsub(Sentence,SubEnd,LVL,EndPos), !.


%same as above, with the difference that 'zu' only triggers a subordinated clause if the conjunction verb class isn't 'KOUI' ("um zu gehen").
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,Tag,_,_),
			  Tag = 'PTKZU',
			  \+ 	lvl(LVL,_KonjPos,'KOUI',x),
		    	  findfreelvl(LVL, NewLVL),
			  assert(lvl(NewLVL,Pos,y,ptkzu)),
			  NewPos is Pos + 1, !,
			  idsub(Sentence,NewPos, NewLVL,SubEnd),
			  (idsubzu(Sentence,SubEnd,LVL, NewLVL, EndPos)->true;idsub(Sentence,SubEnd,LVL,EndPos)), !.


%finite verb found. This rule deals with an exception: sometimes the finite verb does not come in last place as expected, but just before other non-finite verbs "weil er es nicht hätte tun sollen". 
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_,Tag,[String],_),
			  	finverb(Tag), 
				NewPos is Pos + 1,
				w(Sentence,NewPos,_,NewTag,[NewString],_),
				fullverbcand(NewTag),
				assert(lvl(LVL,Pos,String,head)),
				assert(lvl(LVL,NewPos,NewString,full)), !,
				getverbgroupmain(Sentence,LVL,NewPos,EndPos).


%2nd exception: there might be two verb complexes in a subclause (e.g. "weil er gekränkt ist und sich austoben muss").
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,Tag,[String],_),
			  	finverb(Tag), 
				possible_subordinated_coordination(Sentence, Pos, RightPos),
				assert(lvl(LVL,Pos,String,head)),
				findfreelvl(LVL, NewLVL),
				lvl(LVL,_,Type,x),
				assert(lvl(NewLVL,_,Type,x)), !,
				idsub(Sentence, RightPos, NewLVL,EndPos). %V2 Stellung ist possible in clauses beginning with PWS, for example



%finite verb found (and exceptions fail). This stops the search for more dependents.
idsub(Sentence, Pos, LVL, EndPos) :- w(Sentence,Pos,_Word,Tag,[String],_),
			  	finverb(Tag), 
				assert(lvl(LVL,Pos,String,head)),
				NewPos is Pos + 1, !,
                sentdelim(SentDelim),
 				(((Tag = 'VAFIN'; Tag ='VMFIN'),lvl(LVL,_,Type,x),member(Type,['PWS','PWAT','PWAV']),w(Sentence,NewPos,_,Tag2,_,_),\+ member(Tag2,['KON','$,','$.',SentDelim]))->idmain(Sentence, NewPos, LVL,EndPos, yes);EndPos=NewPos). %V2 Stellung ist possible in clauses beginning with PWS, for example.



%sentence ends. This stops the search for more dependents.
idsub(Sentence,Pos,_,Pos) :- w(Sentence,Pos,_,'$.',_,_), !.

%sentence ends. This stops the search for more dependents.
idsub(Sentence,Pos,_,Pos) :- sentdelim(SentDelim),
            w(Sentence,Pos,_,SentDelim,_,_), !.

%nothing found. Increment by one and continue search recursively.
idsub(Sentence,Pos,LVL, EndPos) :- sentdelim(SentDelim),
                    w(Sentence,XPos,_,SentDelim,_,_),
                  XPos > Pos,
				   NewPos is Pos + 1, !,
			           idsub(Sentence,NewPos,LVL, EndPos).


%catchall. should not be needed, since recursive clause should always succeed.
idsub(_,Pos,_,Pos) :- !.


%idsubzu(+Sentence,+Pos,+LVL,+LVL2,-EndPos): distinguish between embedded infinitive clause ("weil nach Hause zu gehen mühsam ist"), and infinitive constructions that are part of verb matrix (weil er nach Hause zu gehen hat")
idsubzu(_Sentence,Pos,LVL,LVL2,EndPos) :- lvl(LVL2,_,_,head),
					!,
					shift_lvl(LVL2,LVL),
					EndPos=Pos.


% check possibility that finite verb does not finish subordinated clause, but that there comes a coordinated verb after it:
% possible_subordinated_coordination(+Sentence, +Pos, -RightPos)

% "weil er gekränkt ist und sich austoben muss"
possible_subordinated_coordination(Sentence, Pos, RightPos) :- RightPos is Pos + 1,
            w(Sentence,RightPos,_,'KON',_,_), !.


%getverbgroupmain(+Sentence,+LVL,+Pos,-EndPos): most restrictive searching strategy. Search is stopped as soon as there is a gap (i.e. a word that doesn't belong to the verb chunk).




%Exception: finite verb in verb group might be mistagged infinitive
getverbgroupmain(Sentence, LVL, Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,Word,Tag,_,_),
				  finverb(Tag), 
				  nofinkon(Sentence,Pos),
				  forcetag(Word,Tag,Lemma,NewTag),
				  auxcand(NewTag), 
				  jointag(Lemma,NewTag,NewString),
				  retract(w(Sentence,NewPos,Word,Tag,[_String],C)),
				  assert(w(Sentence,NewPos,Word,NewTag,[NewString],C)),
				  assert(lvl(LVL,NewPos, NewString,aux)), !,
				  getverbgroupmain(Sentence,LVL,NewPos,EndPos).


%There can be any number of auxiliary verbs immediately following a full verb ("er wird gefahren worden sein wollen") 
getverbgroupmain(Sentence, LVL, Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,[String],_),
				  auxcand(Tag),
				  assert(lvl(LVL,NewPos, String,aux)), !,
				 getverbgroupmain(Sentence,LVL,NewPos,EndPos).

%exception: Er hat mich ihn befreien lassen: treat lassen as full verb, leave befreien alone; the parser will take care of the correct attachment 
%(it can't be treated as one verb complex because befreien and lassen have one object each.
getverbgroupmain(Sentence, LVL, Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,Word,Tag,[String],_),
				  fullverbcand(Tag),
				  getlemma(Word,Tag,Lemma,Tag),
				  modallike(Lemma),
				  retract(lvl(LVL,_, _,full)),
				  assert(lvl(LVL,NewPos, String,full)), !,
				 getverbgroupmain(Sentence,LVL,NewPos,EndPos).



%no auxiliary/modal verb found. Stopping search and instantiating EndPos.
getverbgroupmain(_,_,Pos,EndPos) :- EndPos is Pos + 1, !.


%parentheses and quotation mark don't stop search.
getverbgroupmain(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,_,_),
				  Tag = '$(',
				  assert(lvl(LVL,NewPos,'$(',z)), !,
				 getverbgroupmain(Sentence,LVL,NewPos, EndPos).



%getverbgroupsub(+Sentence,+LVL,+Pos,-EndPos): most restrictive searching strategy. Search is stopped as soon as there is a gap (i.e. a word that doesn't belong to the verb chunk). Verbletzstellung.


%if we are in an infinitive clause, the search is stopped not after finding a finite verb, but an infinitive one.
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,[String],_),
				  (Tag = 'VAINF'; Tag ='VMINF'),
			  	  (	lvl(LVL,_KonjPos,'KOUI',x)
				  ;
					lvl(LVL,Pos,y,ptkzu)
				  ),
				  assert(lvl(LVL,NewPos,String,head)),
				EndPos is Pos + 2,!.



%exception: there might be several coordinated auxiliary verbs in a subclause (e.g. "wenn gespuckt und hintereinander her gerannt wird.")
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,'KON',_,_),
				  w(Sentence,Pos,_,Tag,_,_),
				  verbtag(Tag),
				  findfreelvl(LVL, NewLVL),
				  lvl(LVL,_,Type,x),
				  assert(lvl(NewLVL,_,Type,x)),
				  ((ppverb(Tag),getkon(Sentence,NewLVL,NewPos,TempPos,sub,pp));
				  (infverb(Tag),getkon(Sentence,NewLVL,NewPos,TempPos,sub,inf))), !,
				  getverbgroupsub(Sentence,LVL,TempPos, EndPos).



%non-finite verb found. Check for possibility of tagging error.
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,Word,Tag,_,_),
				  auxcand(Tag),
				  RightPos is NewPos + 1,
				  w(Sentence,RightPos,_,Tag2,_,_),
                  sentdelim(SentDelim),
				  member(Tag2,['$,','$.',SentDelim]),
				  \+ lvl(LVL,_,_,ptkzu),
				  \+ lvl(LVL,_,'KOUI',x),
				  lvl(LVL,_,_,x),
				  forcetag(Word,Tag,Lemma,NewTag),
				  finverb(NewTag), 
				  jointag(Lemma,NewTag,NewString),
				  retract(w(Sentence,NewPos,Word,Tag,_,C)),
				  assert(w(Sentence,NewPos,Word,NewTag,[NewString],C)),
				  assert(lvl(LVL,NewPos,NewString,aux)), !,
				  getverbgroupsub(Sentence,LVL,NewPos, EndPos).


%There can be any number of auxiliary verbs immediately following a full verb ("weil er gefahren worden sein wollen wird") 
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,[String],_),
				  auxcand(Tag),
				  assert(lvl(LVL,NewPos,String,aux)), !,
				 getverbgroupsub(Sentence,LVL,NewPos, EndPos).


%exception: Weil ich ihn mich befreien lassen habe: treat lassen as full verb, leave befreien alone; the parser will take care of the correct attachment 
%(it can't be treated as one verb complex because befreien and lassen have one object each.
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
                                  w(Sentence,NewPos,Word,Tag,[String],_),
                                  fullverbcand(Tag),
                                  getlemma(Word,Tag,Lemma,Tag),
                                  modallike(Lemma),
                                  retract(lvl(LVL,_, _,full)),
                                  assert(lvl(LVL,NewPos, String,full)), !,
                                 getverbgroupsub(Sentence,LVL,NewPos, EndPos).

%exception: there might be two verb complexes in a subclause (e.g. "weil er gekränkt ist und sich austoben muss").
getverbgroupsub(Sentence,LVL,Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,[String],_),
				  (Tag = 'VAFIN'; Tag ='VMFIN'),
				  possible_subordinated_coordination(Sentence, NewPos, RightPos),
				  findfreelvl(LVL, NewLVL),
				  lvl(LVL,_,Type,x),
				  assert(lvl(NewLVL,_,Type,x)),
				  idsub(Sentence, RightPos, NewLVL,EndPos),
				  assert(lvl(LVL,NewPos, String,head)), !.



%since we are in a subordinated clause, we expect Verbletzstellung, stopping the search if a finite auxiliary/modal verb is found.
getverbgroupsub(Sentence,LVL,Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,[String],_),
				  (Tag = 'VAFIN'; Tag ='VMFIN'),
				  assert(lvl(LVL,NewPos, String,head)),
				  EndPos is Pos + 2,!.


%special case: "weil er mich gehen lässt": verb complex is not built up in preprocessing, but we want to recognize it as part of subclause (and stop the search)
getverbgroupsub(Sentence,_LVL,Pos, EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_,'VVFIN',_,_),
				  EndPos is Pos + 2,!.


%special case: "um mich gehen zu lassen"
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_,'VVFIN',_,_),
			  	  (	lvl(LVL,_KonjPos,'KOUI',x)
				  ;
					lvl(LVL,_,y,ptkzu)
				  ),
				EndPos is Pos + 2,!.


%'zu' does not stop search.
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,_Word,Tag,_,_),
				  Tag = 'PTKZU',
				  assert(lvl(LVL,NewPos,y,ptkzu)), !,
				 getverbgroupsub(Sentence,LVL,NewPos, EndPos).


%parentheses and quotation mark don't stop search.
getverbgroupsub(Sentence, LVL, Pos,EndPos) :- NewPos is Pos + 1,
				  w(Sentence,NewPos,Word,Tag,_,_),
				  Tag = '$(',
				  Word \= '-',
				  (Word = ')'->lvl(LVL,_,'$(',z);true),
				  assert(lvl(LVL,NewPos,'$(',z)), !,
				 getverbgroupsub(Sentence,LVL,NewPos, EndPos).


%no allowed word class found (for example in an elliptic subclause: "wie versprochen"). Stopping search and instantiating EndPos.
getverbgroupsub(_Sentence,_LVL,Pos,EndPos) :- EndPos is Pos + 1, !.




%wenn gespuckt und hintereinander her gerannt wird.
getkon(Sentence,_LVL,Pos,NewPos,sub,pp) :- NewPos is Pos + 1,
					  w(Sentence,NewPos,_,Tag,_,_),
					  ppverb(Tag), !.


%weil er kommen und mich treffen will..
getkon(Sentence,_LVL,Pos,NewPos,sub,inf) :- NewPos is Pos + 1,
					  w(Sentence,NewPos,_,Tag,_,_),
					  infverb(Tag), !.

%unexpected word found; aborting search
getkon(Sentence,_LVL,Pos,NewPos,_,_) :-  NewPos is Pos + 1,
				  w(Sentence,NewPos,_,Tag,_,_),
				  (verbtag(Tag);functtag(Tag)), !.

%sentence finished. whoops.
getkon(Sentence,_LVL,Pos,NewPos,_,_) :-  NewPos is Pos + 1,
				  w(Sentence,NewPos,_,'$.',_,_), !.

getkon(Sentence,_LVL,Pos,NewPos,_,_) :-  NewPos is Pos + 1,
                  sentdelim(SentDelim),
                  w(Sentence,NewPos,_,SentDelim,_,_), !.

%continue search with next item
getkon(Sentence,LVL,Pos,EndPos,ClauseType,VerbType) :- NewPos is Pos + 1, 
				getkon(Sentence,LVL,NewPos,EndPos,ClauseType,VerbType), !.



%initialises the replacement of the original w/6 rules with the modified ones.
docomplete(Sentence) :-    lvl(LVL,_,_,_),
			     complete(Sentence,LVL,[]),
			     fail.

docomplete(_) :- !.


%lvl is used to discriminate between different structures.
%if new structure is initialised, findfreelvl is used to give it a number that isn't yet in use.
findfreelvl(Result, Result) :- \+ lvl(Result,_,_,_), !.

findfreelvl(Try, Result) :- lvl(Try,_,_,_),
			NewTry is Try + 1,
			findfreelvl(NewTry, Result), !.

%complete(+Sentence, +LVL, +Rest)
%build the new chunk list. order of the rules determines order of the entries in chunk.

%verbal particles are always last.
complete(Sentence, LVL, Rest) :- lvl(LVL,Pos,String,ptkvz),
				\+ completed(LVL,Pos),
				 assert(completed(LVL,Pos)),!,
				     complete(Sentence,LVL,[String|Rest]).

%last or second to last is the full verb
complete(Sentence, LVL, Rest) :- lvl(LVL,Pos,String,full),
				 \+ completed(LVL,Pos),
				 assert(completed(LVL,Pos)), !,
				     complete(Sentence,LVL,[String|Rest]).

complete(Sentence, LVL, Rest) :- lvl(LVL,Pos,String,ptkzu),
                                \+ (lvl(LVL,Pos2,String,aux), Pos2 < Pos),
                                NextPos is Pos + 1,
                                completed(LVL, NextPos),
                                \+ completed(LVL,Pos),
                                assert(completed(LVL,Pos)), !,
                                complete(Sentence,LVL,['zu_PTKZU'|Rest]).

%this adds all auxiliary verb relationships to the chunk. 
complete(Sentence, LVL, Rest) :- lvl(LVL,Pos,String,aux),
				\+ (lvl(LVL,Pos2,String,aux), Pos2 < Pos),
					\+ completed(LVL,Pos),
			         assert(completed(LVL,Pos)), !,
				     complete(Sentence,LVL,[String|Rest]).


%finite verb
complete(Sentence, LVL, Rest) :- lvl(LVL,Pos,String,head),
					\+ completed(LVL,Pos),
					 assert(completed(LVL,Pos)), !,
				     complete2(Sentence,LVL,[String|Rest]).

%catchall
complete(Sentence,LVL,List) :- 	complete2(Sentence,LVL,List).


%if we find invalid pair of verbs (e.g. können + VVPP), abandon building verb complex.
complete2(Sentence,LVL,Rest) :-  chunkPair(Rest,Word,_,Word2,Tag2),
                                \+ valid_aux(Word,Tag2,Word2),
                                fillallfailed(Sentence,LVL).


%if we know that we're not in a subclause structure (no function words and no verbal particle 'zu'), mark the clause as a main clause
complete2(Sentence, LVL, Rest) :- \+lvl(LVL,_,_,x),
				nth1(1,Rest,Head),
				splittag(Head,_,Tag),
				\+ complete_possible_subordinated_coordination(Sentence, LVL),
				member(Tag,['VVFIN','VAFIN','VMFIN']),
				\+ member('mainclause',Rest),
				!,
				complete2(Sentence,LVL,['mainclause'|Rest]).

%passive recognition
complete2(Sentence,LVL,Rest) :-  chunkPair(Rest,'werden',_,_Word2,'VVPP'),
				fillallcompleted(Sentence,LVL,['passive'|Rest]).

complete2(Sentence,LVL,Rest) :-  chunkPair(Rest,'sein',_,_Word2,'VVPP'),
                fillallcompleted(Sentence,LVL,['passive'|Rest]).

complete2(Sentence,LVL,Rest) :-  chunkPair(Rest,'sein',_,_Word2,Tag),
				(Tag = 'VVIZU'; (Tag = 'VVINF', lvl(LVL,_,_,ptkzu))),
				fillallcompleted(Sentence,LVL,['passive'|Rest]).

%catchall
complete2(Sentence,LVL,List) :- 	fillallcompleted(Sentence,LVL,List).


%after new chunk has been built, all verbs are re-asserted recursively.
fillallcompleted(Sentence, LVL, ChunkList) :- (member('mainclause',ChunkList)->assert(mainclause(LVL));true),
				completed(LVL,Pos),
				retract(completed(LVL,Pos)),
				retract(w(Sentence,Pos,Word,Tag,_,C)),
				assert(w(Sentence,Pos,Word,Tag,ChunkList,C)), !,
				fillallcompleted(Sentence, LVL, ChunkList).

%if we abandon a verb complex, all verbs are re-asserted recursively.
fillallfailed(Sentence, LVL) :- completed(LVL,Pos),
                                retract(completed(LVL,Pos)),
                                retract(w(Sentence,Pos,Word,Tag,Chunk,C)),
                                assert(w(Sentence,Pos,Word,Tag,Chunk,C)), !,
                                fillallfailed(Sentence, LVL).

% clean dynamic predicates.
cleanup_chunking :- retractall(lvl(_,_,_,_)),
				 retractall(completed(_,_)),
				 retractall(mainclause(_)), !.

%checks if the head of a mutli-verb structure is valid (VVFIN isn't).
headAuxiliar(Sentence, LVL) :- lvl(LVL,Pos,_,head),
				w(Sentence,Pos,_,Tag,_,_),
				(Tag = 'VAFIN'; Tag ='VMFIN'), !.

headAuxiliar(Sentence, LVL) :- lvl(LVL,Pos,_,head),
				w(Sentence,Pos,_,Tag,_,_),
				(Tag = 'VAINF'; Tag ='VMINF'), 
			  	  (	lvl(LVL,_KonjPos,'KOUI',x)
				  ;
					lvl(LVL,_,y,ptkzu)
				  ),!.

headAuxiliar(_,LVL) :- retractall(lvl(LVL,_,_,_)), !.


%gets a pair of two adjacent verbs in a chunk.
chunkPair([Mb1,Mb2|_Rest], Word1,Tag1, Word2,Tag2) :- atomic(Mb1),
				atomic(Mb2),
				splittag(Mb1,Word1,Tag1),
				splittag(Mb2,Word2,Tag2),
				\+ Tag2 = 'PTKVZ'.

%recursive clause.
chunkPair([_Elem|Rest], Word1,Tag1, Word2,Tag2) :- chunkPair(Rest,Word1,Tag1,Word2,Tag2).


%if main clause follows subordinated clause, and ends in a verb, it possibly is not really a main clause,
%but a coordinated subordinated clause "Frauen, die attraktiv sind, aber mit ihrem Aussehen nicht zurechtkommen."
complete_possible_subordinated_coordination(Sentence, LVL) :- lvl(LVL, Pos, _, head),
	OldLVL is LVL -1,
	lvl(OldLVL, _, _, head),
	\+ mainclause(OldLVL),
	NewPos is Pos + 1,
	(w(Sentence,NewPos,_,Tag,_,_)->(sentdelim(Tag);member(Tag,['$,','$.','KON']));true), !.


splittag(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'_'),
	sub_atom(WordI,0,Before,_,Word), 
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa), !.

splittag(WordI,WordI,_) :- !.

jointag(Word,Tag,Out) :-
     name(Word,CW),
     name(Tag,CT),
     append(CW,[95],First),
     append(First,CT,CO),
     name(Out,CO).



%only needed for easier output filtering.
verbtag('VVINF').
verbtag('VAINF').
verbtag('VMINF').
verbtag('VVFIN').
verbtag('VAFIN').
verbtag('VMFIN').
verbtag('VVPP').
verbtag('VAPP').
verbtag('VMPP').
verbtag('VAIMP').
verbtag('VVIMP').
verbtag('VVIZU').

%all non-finite verb (except for VVIZU) can be a full verb depending on a finite auxiliary or modal verb
fullverbcand('VVINF').
fullverbcand('VAINF').
fullverbcand('VMINF').
fullverbcand('VVPP').
fullverbcand('VAPP').
fullverbcand('VMPP').

%precision/recall trade-off (recall up, precision down)
%fullverbcand('VVIZU').

%finite verbs
finverb('VVFIN').
finverb('VAFIN').
finverb('VMFIN').

%infinitive verbs
infverb('VVINF').
infverb('VAINF').
infverb('VMINF').

%participle verbs
ppverb('VVPP').
ppverb('VAPP').
ppverb('VMPP').

%these classes can join a finite verb and a full verb.
auxcand('VAINF').
auxcand('VMINF').
auxcand('VAPP').
auxcand('VMPP').

%punctuation tags. only needed for easier output filtering.
puncttag('$.').
puncttag('s').
puncttag('$,').
puncttag('$(').
puncttag(X) :- sentdelim(X).

%these conjuctions or pronouns signify that the finite verb must be in Verbletztstellung.
functtag('KOUS').
functtag('KOUI').
functtag('PRELS').
functtag('PRELAT').
functtag('PWS').
functtag('PWAV').
functtag('PWAT').

%some other functional tags. only needed for easier output filtering.
functtag2('PTKVZ').
functtag2('PTKZU').
functtag2('KON').
functtag2('KOKOM').


valid_aux('dürfen', Tag, _) :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux('haben', Tag, Word) :- enforce_aux_agreement(yes), !, (member(Tag,['VVPP','VAPP', 'VMPP', 'PTKZU', 'VVIZU', 'VMINF']); Word='lassen').
valid_aux('können', Tag, _) :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux('mögen', Tag, _)  :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux('müssen', Tag, _) :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux('sein', Tag, _)  :- enforce_aux_agreement(yes), !, member(Tag,['VVPP','VAPP', 'VMPP', 'PTKZU', 'VVIZU']).
valid_aux('sollen', Tag, _) :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux('wollen', Tag, _) :- enforce_aux_agreement(yes), !, member(Tag,['VVINF','VAINF','VMINF']).
valid_aux(_,_,_).

forcetag(_,_,_,_) :- morphology(none), !, fail.
forcetag(_,_,_,_) :- morphology(keep), !, fail.

%word form is ambiguous (same word form/same lemma), but because of syntactic context, we decide to overrule tagger.
forcetag(Word,Tag,Lemma,NewTag) :- correct_mistagging(yes), gertwol(Word,Lemma,NewTag,_, _), gertwol(Word,_,Tag,_, _), \+ Lemma = '<unknown>'.
