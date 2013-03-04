% extract statistics about whether an adjd/adv/pwav is more often pred or adv

:- dynamic w/7, predicative/4, adverbial/4.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(predicative(_,_,_,_)),
	retractall(adverbial(_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	statloop,
	combineloop,
	writedown(Out),
	close(Out).


%bilexical statistics
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,pred,ADJ,Tag,VERB),List),
        length(List, Length),
        assertz(predicative(VERB,ADJ,Tag,Length)),
        fail.

statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,adv,ADJ,Tag,VERB),List),
        length(List, Length),
        assertz(adverbial(VERB,ADJ,Tag,Length)),
        fail.

statloop. %catchall


searchgoal(Sen,Pos,adv,Word,Tag,Word2) :- w(Sen,Pos,Word,Tag,'adv',Head,_Morph),
    predcand_adv(Tag),
    w(Sen,Head,Word2,Tag2,_Fun,_Head2,_Morph2),
    verbtag(Tag2).

searchgoal(Sen,Pos,pred,Word,Tag,Word2) :- w(Sen,Pos,Word,Tag,'pred',Head,_Morph),
    predcand_adv(Tag),
    w(Sen,Head,Word2,Tag2,_Fun,_Head2,_Morph2),
    verbtag(Tag2).


% back-off statistics
combineloop :- bagof(Length,VERB^(predicative(VERB,ADJ,Tag,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(predicative('*any*',ADJ,Tag,Total)),
    fail.

combineloop :- bagof(Length,ADJ^(predicative(VERB,ADJ,Tag,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(predicative(VERB,'*any*',Tag,Total)),
    fail.

combineloop :- bagof(Length,VERB^(adverbial(VERB,ADJ,Tag,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adverbial('*any*',ADJ,Tag,Total)),
    fail.

combineloop :- bagof(Length,ADJ^(adverbial(VERB,ADJ,Tag,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adverbial(VERB,'*any*',Tag,Total)),
    fail.

combineloop.

writedown(Out) :- predicative(VERB,ADJ,Tag,Length),
    Length > 1,
    writeq(Out, predicative(VERB,ADJ,Tag,Length)),
    write(Out,'.'),
    nl(Out),
    fail.

writedown(Out) :- adverbial(VERB,ADJ,Tag,Length),
    Length > 1,
    writeq(Out, adverbial(VERB,ADJ,Tag,Length)),
    write(Out,'.'),
    nl(Out),
    fail.

writedown(_) :- !.

predcand_adv('adjd').
predcand_adv('adv').
predcand_adv('pwav').

verbtag('vvinf').
verbtag('vainf').
verbtag('vminf').
verbtag('vvfin').
verbtag('vvinf').
verbtag('vafin').
verbtag('vmfin').
verbtag('vvpp').
verbtag('vapp').
verbtag('vmpp').
verbtag('vaimp').
verbtag('vvimp').
verbtag('vvizu').