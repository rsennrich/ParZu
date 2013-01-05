/*	goldstandard_convert.pl : has two major purposes:
		 1) everything that is marked as an auxiliary verb (relation 'AUX') 
		passes its dependents up to its own head. Objects, predicates etc. are made direct dependents of the finite verb in the grammar to prevent crossing dependencies. This could be used for the extraction of distance statistics.

		2) non-finite full verb are made the head of complex verb phrases (instead of the finite auxiliary or modal verb). This is needed for the extraction of lexical statistics.

    currently, the script is run three times: first with mode 'raise', then with mode 'swap', then with mode 'raise' again
    */

:- dynamic w/7, x/7.

%start(+In, +Out, +Mode).
%mode is 'raise' or 'swap'
start(In, Outfile, Mode) :- retractall(w(_,_,_,_,_,_,_)),
	retractall(x(_,_,_,_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	conversionloop(Out, Mode).


conversionloop(_Out,swap) :- w(Sentence, Pos, _, _, _, _, _),
	convert(Sentence, Pos, 'swap'),
	fail.

conversionloop(_Out,raise) :- w(Sentence, Pos, _, _, _, _, _),
			convert(Sentence, Pos, 'raise'),
			fail.

conversionloop(Out, _) :- writedown(Out).

conversionloop(Out, _) :- close(Out), !.

writedown(Out) :- x(Sentence, NewPos, Word, Tag, Funct, Dep, Morph),
            write(Out, 'w('),
            writeq(Out, Sentence),
            write(Out, ','),
            writeq(Out, NewPos),
            write(Out, ','),
            writeq(Out, Word),
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


%Tauscht Abhängigkeit (1 abhängig von 2 --> 2 abhängig von 1)
convert(Sentence, Pos, 'swap') :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
	   swap(Tag2, Funct2, Tag, Funct),
	   w(Sentence, Dep, _, Tag2, Funct2, NewDep, _),
	   assertz(x(Sentence, Pos, Word, Tag, Funct2, NewDep, Morph)), !.

%Tauscht Abhängigkeit (1 abhängig von 2 --> 2 abhängig von 1)
convert(Sentence, Pos, 'swap') :- w(Sentence, Pos, Word, Tag, Funct, _Dep, Morph),
	   swap(Tag, Funct, _Tag2, Funct2),
	   w(Sentence, NewPos, _, _, Funct2, Pos,_),
	   assertz(x(Sentence, Pos, Word, Tag, Funct2, NewPos, Morph)), !.

%no modification if the first clauses fail.
convert(Sentence, Pos, 'swap') :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph), 
			assertz(x(Sentence, Pos, Word, Tag, Funct, Dep, Morph)), !.



%Raises a dependency as long as the conditions in raise/3 are met.
convert(Sentence, Pos, 'raise') :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
	   doraise(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
	   !.

%no modification if the first clause fails.
convert(Sentence, Pos, 'raise') :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
	   assertz(x(Sentence, Pos, Word, Tag, Funct, Dep, Morph)),
	   !.


doraise(Sentence, Pos, Word, Tag, Funct, Dep, Morph) :- w(Sentence, Pos, Word, Tag, Funct, Dep, Morph),
	   						w(Sentence, Dep, _, _, Funct2, NewDep,_),
							raise(Tag, Funct, Funct2), !,
							doraise(Sentence, Pos, Word, Tag, Funct, NewDep, Morph).

doraise(Sentence, Pos, Word, Tag, Funct, Dep, Morph) :- assertz(x(Sentence, Pos, Word, Tag, Funct, Dep, Morph)), !.



%Tag1/Funct1 are those of the dependent, Funct2 of the head
%raise(?Tag1, ?Funct1, ?Funct2)
raise(_, X, 'AUX') :- \+ X = 'PART'.

% Tag1/Funct1 are those of the dependent, Tag2/Funct2 of the head
% swap(?Tag1, ?Funct1, ?Tag2, ?Funct2)
swap(HTag, Head, 'VVINF', 'AUX') :- vpfin(HTag).
swap(HTag, Head, 'VVPP', 'AUX') :- vpfin(HTag).

%finite verbs that are usually in auxiliary function and can be swapped with non-finite fullverb
vpfin('VMFIN').
vpfin('VAFIN').
vpfin('VVFIN').