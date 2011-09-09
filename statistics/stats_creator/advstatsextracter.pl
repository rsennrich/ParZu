%	ppstatsextractor.pl : extracts information from a training corpus about which head/preposition combinations are typically objp/pp. Also includes information about the absolute frequency of each word.

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



statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,advleftcan,Word,Tag2),List),
		length(List, Length),
        bagof(Sen/Pos,searchgoal(Sen,Pos,advleft,Word,Tag2),List2),
        length(List2, Length2), 
		assertz(advbigramleft(Word,Tag2,Length,Length2)),
		fail.

statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,advrightcan,Word,Tag2),List),
        length(List, Length), 
        bagof(Sen/Pos,searchgoal(Sen,Pos,advright,Word,Tag2),List2),
        length(List2, Length2), 
        assertz(advbigramright(Tag2,Word,Length,Length2)),
        fail.


statloop. %catchall



searchgoal(Sen,Pos,advleftcan,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',_Dep,_Morph),
    RightPos is Pos + 1,
	w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2).

searchgoal(Sen,Pos,advleft,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',Dep,_Morph),
    RightPos is Pos + 1,
    RightPos = Dep,
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2).

searchgoal(Sen,Pos,advrightcan,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',_Dep,_Morph),
    RightPos is Pos - 1,
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2).

searchgoal(Sen,Pos,advright,Word,Tag2) :- w(Sen,Pos,Word,_,'adv',Dep,_Morph),
    RightPos is Pos - 1,
    RightPos = Dep,
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2,_Morph2).




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
