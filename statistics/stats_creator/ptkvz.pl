%	ptkvz : adds all verbal particles to their head's lemma. Useful as preprocessing step before extracting statistics from the corpus (since the verbal particle may affect the verb's valency).

:- dynamic w/7.

%start(+In, +Out).
start(In, Outfile) :- 
	retractall(w(_,_,_,_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	writedown(Out),
	close(Out).


writedown(Out) :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
			getptkvz(Sentence, Pos, Word, Tag, WordNew),
			write(Out, 'w('),
			writeq(Out, Sentence),
			write(Out, ','),
			writeq(Out, Pos),
			write(Out, ','),
			writeq(Out, WordNew),
			write(Out, ','),
			writeq(Out, Tag),
			write(Out, ','),
			writeq(Out, Funct),
			write(Out,','),
			writeq(Out, Dep),
            write(Out,','),
            writeq(Out, Morph),
			write(Out, ').'),
			nl(Out),
            fail.

writedown(_) :- !.

getptkvz(Sentence,Pos,Word,Tag,WordNew) :-w(Sentence, Pos, Word, Tag, _Funct, _Dep, Morph),
					   w(Sentence,_Pos2,PTKVZ, 'PTKVZ', 'AVZ',Pos,Morph),
					   atom_concat(PTKVZ,Word,WordNew), !.

getptkvz(_Sentence,_Pos,Word,_Tag,Word) :- !.
