% extract statistics about whether an adjd is more often pred or adv

:- dynamic w/7, adjd_predicative/3, adjd_adverbial/3.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(adjd_predicative(_,_,_)),
	retractall(adjd_adverbial(_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	statloop,
	combineloop,
	writedown(Out),
	close(Out).


%bilexical statistics
statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,pred,ADJ,VERB),List),
        length(List, Length),
        assertz(adjd_predicative(VERB,ADJ,Length)),
        fail.

statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,adv,ADJ,VERB),List),
        length(List, Length),
        assertz(adjd_adverbial(VERB,ADJ,Length)),
        fail.

statloop. %catchall


searchgoal(Sen,Pos,adv,Word,Word2) :- w(Sen,Pos,Word,'adjd','adv',Head,_Morph),
    w(Sen,Head,Word2,Tag2,_Fun,_Head2,_Morph2),
    verbtag(Tag2).

searchgoal(Sen,Pos,pred,Word,Word2) :- w(Sen,Pos,Word,'adjd','pred',Head,_Morph),
    w(Sen,Head,Word2,Tag2,_Fun,_Head2,_Morph2),
    verbtag(Tag2).


% back-off statistics
combineloop :- bagof(Length,VERB^(adjd_predicative(VERB,ADJ,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adjd_predicative('*any*',ADJ,Total)),
    fail.

combineloop :- bagof(Length,ADJ^(adjd_predicative(VERB,ADJ,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adjd_predicative(VERB,'*any*',Total)),
    fail.

combineloop :- bagof(Length,VERB^(adjd_adverbial(VERB,ADJ,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adjd_adverbial('*any*',ADJ,Total)),
    fail.

combineloop :- bagof(Length,ADJ^(adjd_adverbial(VERB,ADJ,Length), VERB \= '*any*', ADJ \= '*any*' ), List),
    sumlist(List,Total),
    assertz(adjd_adverbial(VERB,'*any*',Total)),
    fail.

combineloop.

writedown(Out) :- adjd_predicative(VERB,ADJ,Length),
    Length > 1,
    writeq(Out, adjd_predicative(VERB,ADJ,Length)),
    write(Out,'.'),
    nl(Out),
    fail.

writedown(Out) :- adjd_adverbial(VERB,ADJ,Length),
    Length > 1,
    writeq(Out, adjd_adverbial(VERB,ADJ,Length)),
    write(Out,'.'),
    nl(Out),
    fail.

writedown(_) :- !.


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