start(In, Outfile) :- 
    consult(In),
    open(Outfile, write, Out),
	bagof(Rel,Sen^Pos^Head^Word2^Tag2^Head2^Morph^Morph2^(w(Sen,Pos,Word,kous,konj,Head,Morph), w(Sen,Head,Word2,Tag2,Rel,Head2,Morph2)),List),length(List,Length),
	bagof(a,member(Rel,List),List2),length(List2,Length2),
	Length2 > Length/10,Length2 > 5,
	write(Out,'konjstats('),
    writeq(Out,Word),
    write(Out,','), 
    writeq(Out,Rel), 
    write(Out,','), 
    writeq(Out,Length2),
    write(Out,').'),
    nl(Out),
    fail.