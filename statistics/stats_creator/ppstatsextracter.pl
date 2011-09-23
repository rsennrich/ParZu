%	ppstatsextractor.pl : extracts information from a training corpus about which head/preposition combinations are typically objp/pp. Also includes information about the absolute frequency of each word.

:- dynamic w/7, haspp/5,occurs/3,hasobjp/5,occurstemp/3,haspptemp/5,hasobjptemp/5, wordstotal/1,objptotal/1,pptotal/1,nounattach/3,verbattach/3.

%start(+In, +Out).
start(In, Outfile,Outfile2) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(occurs(_,_,_)),
	retractall(haspp(_,_,_,_,_)),
	retractall(hasobjp(_,_,_,_,_)),
	retractall(wordstotal(_)),
	retractall(pptotal(_)),
	retractall(objptotal(_)),
	consult(In),
	open(Outfile, write, Out),
    open(Outfile2, write, Out2),
	statloop,
    totalloop,
	combineloop,
	writedown(Out),
	close(Out),
    writedown2(Out2),
    close(Out2).


%get absolute frequency of each word
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,all,Tag,Word),List),
		length(List, Length), 
		assertz(occurstemp(Word,Tag,Length)),
		fail.

%get all head/preposition combinations that are tagged PP and their frequency.
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,pp,Word,Word2,Tag2,Case),List),
		length(List, Length), 
		assertz(haspptemp(Word2,Tag2,Word,Case,Length)),
		fail.

%get all head/preposition combinations that are tagged OBJP and their frequency.
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,objp,Word,Word2,Tag2,Case),List),
		length(List, Length), 
		assertz(hasobjptemp(Word2,Tag2,Word,Case,Length)),
		fail.

statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,nounattach,Word,Case),List),
		length(List, Length), 
		assertz(nounattach(Word,Case,Length)),
		fail.

statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,verbattach,Word,Case),List),
		length(List, Length), 
		assertz(verbattach(Word,Case,Length)),
		fail.


%get total number of words, objp and pp relations. Used for baseline probability.
statloop :- bagof(Sen/Pos,Word^Tag^searchgoal(Sen,Pos,all,Tag,Word),List),
		length(List, Length), 
		assertz(wordstotal(Length)),
		fail.

statloop :- bagof(Sen/Pos,Case^Word^Word2^Tag2^searchgoal(Sen,Pos,objp,Word,Word2,Tag2,Case),List),
		length(List, Length), 
		assertz(objptotal(Length)),
		fail.

statloop :- bagof(Sen/Pos,Case^Word^Word2^Tag2^searchgoal(Sen,Pos,pp,Word,Word2,Tag2,Case),List),
		length(List, Length), 
		assertz(pptotal(Length)),
		fail.


statloop. %catchall


%for full verbs, the frequencies are added and stored for every verb/tag combination. We do not want to disregard POS tags altogether though...
combineloop :- occurstemp(Word,_,_), 
	        combinetags(Word,all), fail.

combineloop :- occurs(Word,Tag,_), 
		(Tag = 'vvinf'; Tag = 'vvfin'; Tag = 'vvpp'; Tag = 'vvizu'; Tag = 'vvimp';Tag='v'),
		haspptemp(Word,_,_,Case,_),
		combinetags(Word,pp,Case), fail.

combineloop :-	occurs(Word,Tag,_), 
		(Tag = 'vvinf'; Tag = 'vvfin'; Tag = 'vvpp'; Tag = 'vvizu'; Tag = 'vvimp';Tag='v'),
		hasobjptemp(Word,_,_,Case,_),
		combinetags(Word,objp,Case), fail.

combineloop :- occurs(Word,Tag,_), 
		retract(haspptemp(Word,Tag,Word2,Case,Num)),
		assertz(haspp(Word,Tag,Word2,Case,Num)), fail.

combineloop :- occurs(Word,Tag,_), 
		retract(hasobjptemp(Word,Tag,Word2,Case,Num)),
		assertz(hasobjp(Word,Tag,Word2,Case,Num)), fail.

combineloop :- !.


totalloop :- gettotal(occurs),
         gettotal(haspp),
         gettotal(hasobjp), fail.

totalloop :- bagof(Word/Num,(occurstemp(Word,Tag,Num), \+ Tag = '*any*'),_List), 
        gettotal(occurs,Tag), 
        gettotal(haspp,Tag), 
        gettotal(hasobjp,Tag),
        fail.

totalloop :- !.

gettotal(occurs,HeadTag) :-
    findall(NumHead,occurstemp(_,HeadTag,NumHead),ListHead),
    sumlist(ListHead,NumHead), assert(occurstemp('*any*',HeadTag,NumHead)).

gettotal(haspp,HeadTag) :-
    bagof(NumHead,H^haspptemp(H,HeadTag,Prep,Case,NumHead),ListHead),
    sumlist(ListHead,NumHead), assert(haspptemp('*any*',HeadTag,Prep,Case,NumHead)), fail.

gettotal(haspp,_) :- !.

gettotal(hasobjp,HeadTag) :-
    bagof(NumHead,H^hasobjptemp(H,HeadTag,Prep,Case,NumHead),ListHead),
    sumlist(ListHead,NumHead), assert(hasobjptemp('*any*',HeadTag,Prep,Case,NumHead)), fail.

gettotal(hasobjp,_) :- !.



gettotal(occurs) :- findall(NumHead,occurstemp(_,_,NumHead),ListHead),
    sumlist(ListHead,NumHead), assert(occurstemp('*any*','*any*',NumHead)).

gettotal(haspp) :-
    bagof(NumHead,H^HeadTag^(haspptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(haspptemp('*any*','*any*',Prep,Case,NumHead)), fail.

gettotal(haspp) :-
    bagof(NumHead,H^HeadTag^Case^(haspptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*', \+ Case = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(haspptemp('*any*','*any*',Prep,'*any*',NumHead)), fail.

gettotal(haspp) :-
    bagof(NumHead,H^HeadTag^Prep^Case^(haspptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*', \+ Case = '*any*', \+ Prep = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(haspptemp('*any*','*any*','*any*','*any*',NumHead)), !.

gettotal(hasobjp) :-
    bagof(NumHead,H^HeadTag^(hasobjptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(hasobjptemp('*any*','*any*',Prep,Case,NumHead)), fail.

gettotal(hasobjp) :-
    bagof(NumHead,H^HeadTag^Case^(hasobjptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*', \+ Case = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(hasobjptemp('*any*','*any*',Prep,'*any*',NumHead)), fail.

gettotal(hasobjp) :-
    bagof(NumHead,H^HeadTag^Prep^Case^(hasobjptemp(H,HeadTag,Prep,Case,NumHead), \+ H = '*any*', \+ HeadTag = '*any*', \+ Case = '*any*', \+ Prep = '*any*'),ListHead),
    sumlist(ListHead,NumHead), assert(hasobjptemp('*any*','*any*','*any*','*any*',NumHead)), !.



combinetags(Word,all) :- occurstemp(Word,Tag,Num),
			\+ (Tag = 'vvinf'; Tag = 'vvfin'; Tag = 'vvpp'; Tag = 'vvizu'; Tag = 'vvimp'),
			 retract(occurstemp(Word,Tag,Num)),
			 assertz(occurs(Word,Tag,Num)), !.

combinetags(Word,all) :- (occurstemp(Word,'vvinf',Length1);Length1 is 0),
		    (occurstemp(Word,'vvfin',Length2);Length2 is 0),
		    (occurstemp(Word,'vvpp',Length3);Length3 is 0),
		    (occurstemp(Word,'vvizu',Length4);Length4 is 0),
		    (occurstemp(Word,'vvimp',Length5);Length5 is 0),
		    NewLength is Length1 + Length2 + Length3 +Length4 +Length5,
		    (retract(occurstemp(Word,'vvinf',_));true),
		    (retract(occurstemp(Word,'vvfin',_));true),
		    (retract(occurstemp(Word,'vvpp',_));true),
		    (retract(occurstemp(Word,'vvizu',_));true),
		    (retract(occurstemp(Word,'vvimp',_));true),
		    (NewLength = 0;
		    (assertz(occurs(Word,'v',NewLength)))), !.


combinetags(Word,pp,Case) :- (haspptemp(Word,'vvinf',Word2,Case,Length1);Length1 is 0),
		    (haspptemp(Word,'vvfin',Word2,Case,Length2);Length2 is 0),
		    (haspptemp(Word,'vvpp',Word2,Case,Length3);Length3 is 0),
		    (haspptemp(Word,'vvizu',Word2,Case,Length4);Length4 is 0),
		    (haspptemp(Word,'vvimp',Word2,Case,Length5);Length5 is 0),
		    NewLength is Length1 + Length2 + Length3 +Length4 +Length5,
		    (retract(haspptemp(Word,'vvinf',Word2,Case,_));true),
		    (retract(haspptemp(Word,'vvfin',Word2,Case,_));true),
		    (retract(haspptemp(Word,'vvpp',Word2,Case,_));true),
		    (retract(haspptemp(Word,'vvizu',Word2,Case,_));true),
		    (retract(haspptemp(Word,'vvimp',Word2,Case,_));true),
		    (NewLength = 0;
		    (assertz(haspp(Word,'v',Word2,Case,NewLength)))), !.


combinetags(Word,objp,Case) :- (hasobjptemp(Word,'vvinf',Word2,Case,Length1);Length1 is 0),
		    (hasobjptemp(Word,'vvfin',Word2,Case,Length2);Length2 is 0),
		    (hasobjptemp(Word,'vvpp',Word2,Case,Length3);Length3 is 0),
		    (hasobjptemp(Word,'vvizu',Word2,Case,Length4);Length4 is 0),
		    (hasobjptemp(Word,'vvimp',Word2,Case,Length5);Length5 is 0),
		    NewLength is Length1 + Length2 + Length3 +Length4 +Length5,
		    (retract(hasobjptemp(Word,'vvinf',Word2,Case,_));true),
		    (retract(hasobjptemp(Word,'vvfin',Word2,Case,_));true),
		    (retract(hasobjptemp(Word,'vvpp',Word2,Case,_));true),
		    (retract(hasobjptemp(Word,'vvizu',Word2,Case,_));true),
		    (retract(hasobjptemp(Word,'vvimp',Word2,Case,_));true),
		    (NewLength = 0;
		    (assertz(hasobjp(Word,'v',Word2,Case,NewLength)))), !.


%searchgoal(?Sen,?Pos,?Class,?Word). Usually called by bagof.

%find all words
searchgoal(Sen,Pos,all,Tag,Word) :- w(Sen,Pos,Word,Tag,_Funct,_Dep,_Morph).


%find all pp relations
searchgoal(Sen,Pos,pp,WordOut,Word2,Tag2,Case) :- w(Sen,Pos,Word,Tag,'pp',Dep,[Case|_]),
				     (Tag='apprart'->splitappr(Word,WordOut,_);WordOut=Word),
				     w(Sen,Dep,Word2,Tag2,_Funct2,_Dep2,_Morph).

%find all objp relations
searchgoal(Sen,Pos,objp,WordOut,Word2,Tag2,Case) :- w(Sen,Pos,Word,Tag,'objp',Dep,[Case|_]),
				     (Tag='apprart'->splitappr(Word,WordOut,_);WordOut=Word),
				     w(Sen,Dep,Word2,Tag2,_Funct2,_Dep2,_Morph).



searchgoal(Sen,Pos,verbattach,WordOut,Case) :- (w(Sen,Pos,Word,Tag,'pp',Dep,[Case|_]);w(Sen,Pos,Word,Tag,'objp',Dep,[Case|_])),
				     (Tag='apprart'->splitappr(Word,WordOut,_);WordOut=Word),
				     w(Sen,Dep,_,Tag2,_Funct2,_Dep2,_Morph),
				      verbtag(Tag2).

searchgoal(Sen,Pos,nounattach,WordOut,Case) :- (w(Sen,Pos,Word,Tag,'pp',Dep,[Case|_]);w(Sen,Pos,Word,Tag,'objp',Dep,[Case|_])),
				     (Tag='apprart'->splitappr(Word,WordOut,_);WordOut=Word),
				     w(Sen,Dep,_,Tag2,_Funct2,_Dep2,_Morph),
				      (Tag2='fm';Tag2='ne';Tag2='nn').


%morphisto-style APPRART
splitappr(am,an,_) :- !.
splitappr(ans,an,_) :- !.
splitappr(aufs,auf,_) :- !.
splitappr(beim,bei,_) :- !.
splitappr(durchs,durch,_) :- !.
splitappr(im,in,_) :- !.
splitappr(ins,in,_) :- !.
splitappr(übers,über,_) :- !.
splitappr(vom,von,_) :- !.
splitappr(zum,zu,_) :- !.
splitappr(zur,zu,_) :- !.


%gertwol-style APPRART
%'in-das' -> 'in' + 'das'
splitappr(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'-'),
	sub_atom(WordI,0,Before,_,Word), 
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa), !.

splitappr(WordI,WordI,_) :- !.

writedown2(Out) :- occurs(Word,Tag,Freq),
		Freq > 2,
		write(Out,'occurs'),
		write(Out, '('),
		writeq(Out,Word),
		write(Out,','),
		writeq(Out,Tag), 
		write(Out,','),
		writeq(Out,Freq),
		write(Out, ').'),
		nl(Out),
		fail.

writedown2(_) :- !.

writedown(Out) :- haspp(Word1,Tag,Word2,Case,Freq),
		Freq > 2,
		write(Out,'haspp'),
		write(Out, '('),
		writeq(Out,Word1),
		write(Out,','),
		writeq(Out,Tag), 
		write(Out,','),
		writeq(Out,Word2), 
		write(Out,','),
		writeq(Out,Case),
		write(Out,','),
		writeq(Out,Freq),
		write(Out, ').'),
		nl(Out),
		fail.

writedown(Out) :- hasobjp(Word1,Tag,Word2,Case,Freq),
		Freq > 2,
		write(Out,'hasobjp'),
		write(Out, '('),
		writeq(Out,Word1),
		write(Out,','),
		writeq(Out,Tag), 
		write(Out,','),
		writeq(Out,Word2), 
		write(Out,','),
		writeq(Out,Case),
		write(Out,','),
		writeq(Out,Freq),
		write(Out, ').'),
		nl(Out),
		fail.


writedown(_) :- !.


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
