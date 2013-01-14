% extract statistics about how often an adverb is attached to different POS (relative to number of times an attachment is theoretically possible)

:- dynamic w/7, advbigramleft/4, advbigramright/4, advbigramlefttemp/4, advbigramrighttemp/4.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(advbigramleft(_,_,_,_)),
    retractall(advbigramright(_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	statloop,
	combineloop,
	writedown(Out),
	close(Out).



statloop :- bagof(Sen/Pos/RightPos,searchgoal(Sen,Pos,advleftcan,Word,Tag2,RightPos),List),
		length(List, Length),
        bagof(Sen/Pos,searchgoal(Sen,Pos,advleft,Word,Tag2),List2),
        length(List2, Length2), 
		assertz(advbigramlefttemp(Word,Tag2,Length,Length2)),
		fail.

statloop :- bagof(Sen/Pos/LeftPos,searchgoal(Sen,Pos,advrightcan,Word,Tag2,LeftPos),List),
        length(List, Length), 
        bagof(Sen/Pos,searchgoal(Sen,Pos,advright,Word,Tag2),List2),
        length(List2, Length2), 
        assertz(advbigramrighttemp(Tag2,Word,Length,Length2)),
        fail.


statloop. %catchall


%for full verbs, the frequencies are added and stored for every verb/tag combination. We do not want to disregard POS tags altogether though...
combineloop :- advbigramlefttemp(Word,_,_,_),
                combinetags(Word,left), fail.

combineloop :-  advbigramrighttemp(_,Word,_,_),
                combinetags(Word,right), fail.

combineloop :- !.


searchgoal(Sen,Pos,advleftcan,Word,Tag2,RightPos) :- w(Sen,Pos,Word,_,'adv',_Dep,_Morph),
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2),
    RightPos > Pos,
    attach_candidate(Sen,Pos,RightPos).

searchgoal(Sen,Pos,advleft,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',Dep,_Morph),
    RightPos = Dep,
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2),
    RightPos > Pos,
    attach_candidate(Sen,Pos,RightPos).

searchgoal(Sen,Pos,advrightcan,Word,Tag2,LeftPos) :- w(Sen,Pos,Word,_,'adv',_Dep,_Morph),
    w(Sen,LeftPos,_,Tag2,_Funct2,_Dep2,_Morph2),
    LeftPos < Pos,
    attach_candidate(Sen,LeftPos,Pos).

searchgoal(Sen,Pos,advright,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',Dep,_Morph),
    LeftPos = Dep,
    w(Sen,LeftPos,_,Tag2,_Funct2,_Dep2,_Morph2),
    LeftPos < Pos,
    attach_candidate(Sen,LeftPos,Pos).


%check if two words are candidates for a dependency relation (defined by no projective relation being broken)
attach_candidate(Sen,First,Second) :- 
    Start is First+1,
    End is Second-1,
    ((between(Start,End,Pos),
    w(Sen,Pos,_,_,_,Head,_),
    (Head < First; Head > Second))->(!,fail); true).


combinetags(Word,left) :- advbigramlefttemp(Word,Tag,Length,Length2),
    (verbtag(Tag)->(NewTag=v,combinetags(Word,left,0,0,NewLength,NewLength2));(retract(advbigramlefttemp(Word,Tag,Length,Length2)),NewTag=Tag,NewLength=Length,NewLength2=Length2)), !,
    assertz(advbigramleft(Word,NewTag,NewLength,NewLength2)).

combinetags(Word,right) :- advbigramrighttemp(Tag,Word,Length,Length2),
    (verbtag(Tag)->(NewTag=v,combinetags(Word,right,0,0,NewLength,NewLength2));(retract(advbigramrighttemp(Tag,Word,Length,Length2)),NewTag=Tag,NewLength=Length,NewLength2=Length2)), !,
    assertz(advbigramright(NewTag,Word,NewLength,NewLength2)).

combinetags(Word,left,AccuIn,AccuIn2,AccuOut,AccuOut2) :- advbigramlefttemp(Word,Tag,Length,Length2),
    verbtag(Tag), !,
    retract(advbigramlefttemp(Word,Tag,Length,Length2)),
    NewLength is Length+AccuIn,
    NewLength2 is Length2+AccuIn2,
    combinetags(Word,left,NewLength,NewLength2,AccuOut,AccuOut2).

combinetags(Word,right,AccuIn,AccuIn2,AccuOut,AccuOut2) :- advbigramrighttemp(Tag,Word,Length,Length2),
    verbtag(Tag), !,
    retract(advbigramrighttemp(Tag,Word,Length,Length2)),
    NewLength is Length+AccuIn,
    NewLength2 is Length2+AccuIn2,
    combinetags(Word,right,NewLength,NewLength2,AccuOut,AccuOut2).

combinetags(_,_,AccuIn,AccuIn2,AccuIn,AccuIn2) :- !.



writedown(Out) :- advbigramleft(Tag,Tag2,Total,ADV),
        Total > 2,
		write(Out,'advbigramleft'),
		write(Out, '('),
		writeq(Out,Tag),
		write(Out,','),
		writeq(Out,Tag2), 
		write(Out,','),
		writeq(Out,Total),
        write(Out,','),
        writeq(Out,ADV),
		write(Out, ').'),
		nl(Out),
		fail.

writedown(Out) :- advbigramright(Tag,Tag2,Total,ADV),
        Total > 2,
        write(Out,'advbigramright'),
        write(Out, '('),
        writeq(Out,Tag),
        write(Out,','),
        writeq(Out,Tag2), 
        write(Out,','),
        writeq(Out,Total),
        write(Out,','),
        writeq(Out,ADV),
        write(Out, ').'),
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