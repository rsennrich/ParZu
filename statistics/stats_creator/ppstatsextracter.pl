%	ppstatsextractor.pl : extracts information from a training corpus about which head/preposition combinations are typically objp/pp. Also includes information about the absolute frequency of each word.

:- dynamic w/7, haspp/5,occurs/3,hasobjp/5,occurstemp/3,haspptemp/5,hasobjptemp/5, wordstotal/1,objptotal/1,pptotal/1,nounattach/3,verbattach/3.

:- ensure_loaded('../../core/morph_predicates.pl').

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
    writedown2(Out2),
        close(Out),
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

combineloop :- haspptemp(Word,_,_,Case,_),
		combinetags(Word,pp,Case), fail.

combineloop :-	hasobjptemp(Word,_,_,Case,_),
		combinetags(Word,objp,Case), fail.

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


combinetags(Word,all) :- occurstemp(Word,Tag,Length),
            (verbtag(Tag)->(NewTag=v,combinetags(Word,all,0,NewLength));(retract(occurstemp(Word,Tag,Length)),NewTag=Tag,NewLength=Length)), !,
            assertz(occurs(Word,NewTag,NewLength)).

combinetags(Word,all,AccuIn,AccuOut) :- occurstemp(Word,Tag,Length),
                         verbtag(Tag), !,
                         retract(occurstemp(Word,Tag,Length)),
                         NewLength is Length+AccuIn,
                         combinetags(Word,all,NewLength,AccuOut).

combinetags(_Word,all,Accu,Accu) :- !.


combinetags(Word,pp,Case) :- haspptemp(Word,Tag,Word2,Case,Length),
            (verbtag(Tag)->(NewTag=v,combinetags(Word,pp,Word2,Case,0,NewLength));(retract(haspptemp(Word,Tag,Word2,Case,Length)),NewTag=Tag,NewLength=Length)), !,
            assertz(haspp(Word,NewTag,Word2,Case,NewLength)).

combinetags(Word,pp,Word2,Case,AccuIn,AccuOut) :- haspptemp(Word,Tag,Word2,Case,Length),
                         verbtag(Tag), !,
                         retract(haspptemp(Word,Tag,Word2,Case,Length)),
                         NewLength is Length+AccuIn,
                         combinetags(Word,pp,Word2,Case,NewLength,AccuOut).

combinetags(_Word,pp,_Word2,_Case,Accu,Accu) :- !.



combinetags(Word,objp,Case) :- hasobjptemp(Word,Tag,Word2,Case,Length),
            (verbtag(Tag)->(NewTag=v,combinetags(Word,objp,Word2,Case,0,NewLength));(retract(hasobjptemp(Word,Tag,Word2,Case,Length)),NewTag=Tag,NewLength=Length)), !,
            assertz(hasobjp(Word,NewTag,Word2,Case,NewLength)).

combinetags(Word,objp,Word2,Case,AccuIn,AccuOut) :- hasobjptemp(Word,Tag,Word2,Case,Length),
                         verbtag(Tag), !,
                         retract(hasobjptemp(Word,Tag,Word2,Case,Length)),
                         NewLength is Length+AccuIn,
                         combinetags(Word,objp,Word2,Case,NewLength,AccuOut).

combinetags(_Word,objp,_Word2,_Case,Accu,Accu) :- !.


%searchgoal(?Sen,?Pos,?Class,?Word). Usually called by bagof.

%find all words
searchgoal(Sen,Pos,all,Tag,Word) :- w(Sen,Pos,Word,Tag,_Funct,_Dep,_Morph).


%find all pp relations
searchgoal(Sen,Pos,pp,WordOut2,Word2,Tag2,Case) :- w(Sen,Pos,Word,_Tag,'pp',Dep,[Case|_]),
                    (splitappr(Word,WordOut,_)->true;WordOut=Word),
                    (derive_prep_from_pav(WordOut,WordOut2)->true;WordOut2=WordOut),
                    w(Sen,Dep,Word2,Tag2,_Funct2,_Dep2,_Morph).

%find all objp relations
searchgoal(Sen,Pos,objp,WordOut2,Word2,Tag2,Case) :- w(Sen,Pos,Word,_Tag,'objp',Dep,[Case|_]),
                    (splitappr(Word,WordOut,_)->true;WordOut=Word),
                    (derive_prep_from_pav(WordOut,WordOut2)->true;WordOut2=WordOut),
                    w(Sen,Dep,Word2,Tag2,_Funct2,_Dep2,_Morph).



searchgoal(Sen,Pos,verbattach,WordOut2,Case) :- (w(Sen,Pos,Word,Tag,'pp',Dep,[Case|_]);w(Sen,Pos,Word,Tag,'objp',Dep,[Case|_])),
                    (splitappr(Word,WordOut,_)->true;WordOut=Word),
                    (derive_prep_from_pav(WordOut,WordOut2)->true;WordOut2=WordOut),
                    w(Sen,Dep,_,Tag2,_Funct2,_Dep2,_Morph),
                    verbtag(Tag2).

searchgoal(Sen,Pos,nounattach,WordOut,Case) :- (w(Sen,Pos,Word,Tag,'pp',Dep,[Case|_]);w(Sen,Pos,Word,Tag,'objp',Dep,[Case|_])),
                    (splitappr(Word,WordOut,_)->true;WordOut=Word),
                    (derive_prep_from_pav(WordOut,WordOut2)->true;WordOut2=WordOut),
                    w(Sen,Dep,_,Tag2,_Funct2,_Dep2,_Morph),
                    (Tag2='fm';Tag2='ne';Tag2='nn').

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