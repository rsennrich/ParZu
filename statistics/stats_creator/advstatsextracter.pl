% extract statistics about how often an adverb is attached to different POS (relative to number of times an attachment is theoretically possible)

:- dynamic w/7, advbigramleft/4, advbigramright/4.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(advbigramleft(_,_,_,_)),
    retractall(advbigramright(_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	statloop,
	writedown(Out),
	close(Out).



statloop :- bagof(Sen/Pos/RightPos,searchgoal(Sen,Pos,advleftcan,Word,Tag2,RightPos),List),
		length(List, Length),
        bagof(Sen/Pos,searchgoal(Sen,Pos,advleft,Word,Tag2),List2),
        length(List2, Length2), 
		assertz(advbigramleft(Word,Tag2,Length,Length2)),
		fail.

statloop :- bagof(Sen/Pos/LeftPos,searchgoal(Sen,Pos,advrightcan,Word,Tag2,LeftPos),List),
        length(List, Length), 
        bagof(Sen/Pos,searchgoal(Sen,Pos,advright,Word,Tag2),List2),
        length(List2, Length2), 
        assertz(advbigramright(Tag2,Word,Length,Length2)),
        fail.


statloop. %catchall



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
