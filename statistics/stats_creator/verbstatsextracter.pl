%	verbstatsextracter.pl : extracts valency information for all verbs and stores them in a file that is used by the parser for statistical desambiguation.

:- dynamic w/7, store/16.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(store(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	write(Out, '%verb(Lemma,Occurs,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl).'),
	nl(Out),
	statinit,
	statloop,
	writedown(Out),
	close(Out).


%get the absolute frequency of a word and assert the template for its entry (values will be filled in later)
statinit :- bagof(Sen/Pos,searchgoal(Sen,Pos,all,Word),List),
		length(List, Length), 
		asserta(store(Word,Length,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
		fail.

statinit :- !. %catchall


%goes through all possible relations that are defined in searchgoal/4 and add it to the store/12 predicate of the corresponding verb.
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,Cat,Word),List), 
		length(List, Length), 
		store(Word,All,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl),
		retract(store(Word,All,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl)),
		addoccurs(Cat,Length, All,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,NSubj,NObja,NObjd,NObja2,NObjg,NObjp,NObjc,NSubjC,NPred,NObji,NRefl,NIntr,NQuote,NExpl),
		asserta(store(Word,All,NSubj,NObja,NObjd,NObja2,NObjg,NObjp,NObjc,NSubjC,NPred,NObji,NRefl,NIntr,NQuote,NExpl)),
		fail.


statloop :- !. %catchall


%add all occurrences of a class found for a given verb to the right place. There is one of these predicates for each place/class.
addoccurs(subj,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,NSubj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NSubj is Subj + Freq, !.

addoccurs(obja,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,NObja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObja is Obja + Freq, !.

addoccurs(objd,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,NObjd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObjd is Objd + Freq, !.

addoccurs(obja2,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,NObja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObja2 is Obja2 + Freq, !.

addoccurs(objg,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,NObjg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObjg is Objg + Freq, !.

addoccurs(objp,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,NObjp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObjp is Objp + Freq, !.

addoccurs(objc,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,NObjc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- NObjc is Objc + Freq, !.

addoccurs(subjc,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,Subjc,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,NSubjc,Pred,Obji,Refl,Intr,Quote,Expl) :- NSubjc is Subjc + Freq, !.

addoccurs(pred,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,NPred,Obji,Refl,Intr,Quote,Expl) :- NPred is Pred + Freq, !.

addoccurs(obji,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,NObji,Refl,Intr,Quote,Expl) :- NObji is Obji + Freq, !.

addoccurs(refl,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,NRefl,Intr,Quote,Expl) :- NRefl is Refl + Freq, !.

addoccurs(intr,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,NIntr,Quote,Expl) :- NIntr is Intr + Freq, !.

addoccurs(quote,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,NQuote,Expl) :- NQuote is Quote + Freq, !.

addoccurs(expl,Freq, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,NExpl) :- NExpl is Expl + Freq, !.

%catchall: if Class is not defined above, do nothing.
addoccurs(_,_, _,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl) :- !.



%searchgoal(?Sen,?Pos,?Class,?Word). Usually called by bagof.
%finds all verbs
searchgoal(Sen,Pos,all,Word) :- w(Sen,Pos,Word,Tag,Funct,_Dep,_Morph), 
			 verb(Tag), \+ Funct = 'aux'.

%finds all verbs with a reflexive pronoun
searchgoal(Sen,Pos,refl,Word2) :- w(Sen,Pos,_,'prf',_Funct,Dep,_Morph), 
				w(Sen,Dep,Word2,Tag2,Funct2,_,_Morph2), 
			 verb(Tag2), \+ Funct2 = 'aux'.

%finds all intransitive verbs
searchgoal(Sen,Pos,intr,Word) :- w(Sen,Dep,Word,Tag,Funct,_Dep2,_Morph), 
				\+ (	w(Sen,Pos,_,_Tag,Funct2,Dep,_Morph2),
					(Funct2 = 'obja'; Funct2 = 'objd';Funct2 = 'obja2';Funct2 = 'objg', Funct2 = 'objc', Funct2 = 'obji')),
			 		verb(Tag), \+ Funct = 'aux'.

%finds all quotes
searchgoal(Sen,Pos,quote,Word2) :- searchgoal(Sen,Pos,s,Word2).


%finds all verbs without subject (not used in the program, just for debugging purposes).
/*searchgoal(Sen,Pos,nosubj,Word) :- w(Sen,Dep,Word,Tag,Funct,_Dep2,_Morph), 
				\+ (	w(Sen,Pos,Word2,_Tag,Funct2,Dep,_Morph2),
					(Funct2 = 'subj')),
					verb(Tag), \+ Funct = 'aux'.*/


%Verbs that allow two objects (clausal or accusative)
searchgoal(Sen,Pos,'obja2',Word2) :- w(Sen,Pos,_,_,Class,Dep,_Morph), 
             (Class = 'obja'),
             w(Sen,Dep,Word2,Tag2,Funct2,_,_), 
             verb(Tag2), \+ Funct2 = 'aux',
              w(Sen,Pos3,_,_,Class3,Dep,_), 
             (Class3 = 'obja'; Class3 = 'objc'; Class3 = 's'),
             \+ Pos3 = Pos.

%predicates: don't count adverbial predicates, which have separate statistics.
searchgoal(Sen,Pos,pred,Word2) :- w(Sen,Pos,_,Tag,pred,Dep,_Morph), 
			 w(Sen,Dep,Word2,Tag2,Funct2,_,_Morph2),
			 verb(Tag2), \+ Funct2 = 'aux',
			 \+ predcand_adverb(Tag).

%catchall. Usually called with Class being uninitialised.
searchgoal(Sen,Pos,Class,Word2) :- w(Sen,Pos,_,_Tag,Class,Dep,_Morph),
			 Class \= pred,
			 w(Sen,Dep,Word2,Tag2,Funct2,_,_Morph2), 
			 verb(Tag2), \+ Funct2 = 'aux'.



writedown(Out) :- store(Word,All,Subj,Obja,Objd,Obja2,Objg,Objp,Objc,SubjC,Pred,Obji,Refl,Intr,Quote,Expl),
		All > 3,
		write(Out,'verb'),
		write(Out, '('),
		writeq(Out,Word),
		write(Out,','),
		write(Out,All), 
		write(Out,','),
		write(Out,Subj),
		write(Out,','),
		write(Out,Obja),
		write(Out,','),
		write(Out,Objd),
		write(Out,','),
		write(Out,Obja2),
		write(Out,','),
		write(Out,Objg),
		write(Out,','),
		write(Out,Objp),
		write(Out,','),
		write(Out,Objc),
		write(Out,','),
		write(Out,SubjC),
		write(Out,','),
		write(Out,Pred),
		write(Out,','),
		write(Out,Obji),
		write(Out,','),
		write(Out,Refl),
		write(Out,','),
		write(Out,Intr),
		write(Out,','),
		write(Out,Quote),
		write(Out,','),
		write(Out,Expl),
		write(Out, ').'),
		nl(Out),
		fail.

writedown(_) :- !.


verb('vvfin').
verb('vvinf').
verb('vvpp').
verb('vvimp').
verb('vvizu').
verb('vafin').
verb('vainf').
verb('vapp').
verb('vaimp').
verb('vmfin').
verb('vminf').
verb('vmpp').

predcand_adverb('adjd').
predcand_adverb('adv').
predcand_adverb('pwav').
