%	ppstatsextractor.pl : extracts information from a training corpus about which head/preposition combinations are typically objp/pp. Also includes information about the absolute frequency of each word.

:- dynamic w/6, advlexical/10, advbigramright/4.

%start(+In, +Out).
start(In, Outfile) :- retractall(w(_,_,_,_,_,_)),
	retractall(advbigramleft(_,_,_,_)),
	retractall(advbigramright(_,_,_,_)),
	consult(In),
	open(Outfile, write, Out),
	write(Out,'%advlexical(Word,POSofHead,LeftCan,Left,ImmLeftCan,ImmLeft,RightCan,Right,ImmRightCan,ImmRight)'),
	statloop(Out),
	close(Out).



% statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,advleftcan,Lex,Tag2),List),
% 		length(List, Length),
%         bagof(Sen/Pos,searchgoal(Sen,Pos,advleft,Lex,Tag2),List2),
%         length(List2, Length2), 
% 		assertz(advbigramleft(Lex,Tag2,Length,Length2)),
% 		fail.
% 
% statloop :- bagof(Sen/Pos,searchgoal(Sen,Pos,advrightcan,Lex,Tag2),List),
%         length(List, Length), 
%         bagof(Sen/Pos,searchgoal(Sen,Pos,advright,Lex,Tag2),List2),
%         length(List2, Length2), 
%         assertz(advbigramright(Tag2,Lex,Length,Length2)),
%         fail.


statloop(Out) :- bagof(Word/Tag,searchgoal(Word,Tag),List),
% 	    length(List,Len),
% 	    write(Len),
	    looplist(Out,List).


looplist(_,[]) :- !.

looplist(Out,[Word/Tag|Rest]) :-
	    statloop(Word,Tag),
	    writedown(Out,Word,Tag),
	    looplist(Out,Rest).

statloop(_). %catchall

statloop(Lex,Tag2) :- 
	    findall(Sen/Pos,searchgoal(Sen,Pos,Lex,occurs),ListX),
	    length(ListX,LenX),
	    LenX > 10,
	findall(Sen/Pos,searchgoal(Sen,Pos,advrightcan,Lex,Tag2),List),
        length(List, Length), 
% 	write(1),
        findall(Sen/Pos,searchgoal(Sen,Pos,advright,Lex,Tag2),List2),
        length(List2, Length2), 
% 	write(2),
	findall(Sen/Pos,searchgoal(Sen,Pos,advleftcan,Lex,Tag2),List3),
	length(List3, Length3),
% 	write(3),
        findall(Sen/Pos,searchgoal(Sen,Pos,advleft,Lex,Tag2),List4),
        length(List4, Length4), 
% 	write(4),
	findall(Sen/Pos,searchgoal(Sen,Pos,advimmrightcan,Lex,Tag2),List5),
        length(List5, Length5), 
% 	write(5),
        findall(Sen/Pos,searchgoal(Sen,Pos,advimmright,Lex,Tag2),List6),
        length(List6, Length6), 
% 	write(6),
	findall(Sen/Pos,searchgoal(Sen,Pos,advimmleftcan,Lex,Tag2),List7),
	length(List7, Length7),
% 	write(7),
        findall(Sen/Pos,searchgoal(Sen,Pos,advimmleft,Lex,Tag2),List8),
        length(List8, Length8), 
% 	write(8),
         assertz(advlexical(Lex,Tag2,Length3,Length4,Length7,Length8,Length,Length2,Length5,Length6)), !.

statloop(_,_) :- !.

searchgoal(Word,Tag2) :- w(Sen,Pos,Word,Tag,_,_Dep),
			  advcan(Tag),
			 w(Sen,RightPos,_,Tag2,_Funct2,_Dep2).

searchgoal(Sen,Pos,Word,occurs) :- w(Sen,Pos,Word,_Tag,_,_Dep).


searchgoal(Sen,Pos,advleftcan,Word,Tag2) :- 
      w(Sen,Pos,Word,Tag,_,_Dep),
    advcan(Tag),
	w(Sen,RightPos,_,Tag2,_Funct2,_Dep2),
    RightPos > Pos.

searchgoal(Sen,Pos,advleft,Word,Tag2) :- w(Sen,Pos,Word,Tag,'adv',Dep),
    advcan(Tag),
    Dep > Pos,
    w(Sen,Dep,_,Tag2,_Funct2,_Dep2).


searchgoal(Sen,Pos,advimmleftcan,Word,Tag2) :-
    w(Sen,Pos,Word,Tag,_,_Dep),
    advcan(Tag),
    RightPos is Pos + 1,
	w(Sen,RightPos,_,Tag2,_Funct2,_Dep2).

searchgoal(Sen,Pos,advimmleft,Word,Tag2) :- w(Sen,Pos,Word,Tag,'adv',Dep),
    advcan(Tag),
    Dep is Pos + 1,
    w(Sen,Dep,_,Tag2,_Funct2,_Dep2).



searchgoal(Sen,Pos,advrightcan,Word,Tag2) :-
    w(Sen,Pos,Word,Tag,_,_Dep),
    advcan(Tag),
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2),
    RightPos < Pos.

searchgoal(Sen,Pos,advright,Word,Tag2) :- w(Sen,Pos,Word,Tag,'adv',Dep),
    advcan(Tag),
    Dep < Pos,
    w(Sen,Dep,_,Tag2,_Funct2,_Dep2).


searchgoal(Sen,Pos,advimmrightcan,Word,Tag2) :-
    w(Sen,Pos,Word,Tag,_,_Dep),
    advcan(Tag),
    RightPos is Pos - 1,
    w(Sen,RightPos,_,Tag2,_Funct2,_Dep2).

searchgoal(Sen,Pos,advimmright,Word,Tag2) :- w(Sen,Pos,Word,Tag,'adv',Dep),
    advcan(Tag),
    Dep is Pos - 1,
    w(Sen,Dep,_,Tag2,_Funct2,_Dep2).


% writedown(Out) :- advbigramleft(Tag,Tag2,Total,ADV),
% 		write(Out,'advbigramleft'),
% 		write(Out, '(\''),
% 		write(Out,Tag),
% 		write(Out,'\',\''),
% 		write(Out,Tag2), 
% 		write(Out,'\','),
% 		write(Out,Total),
%         write(Out,','),
%         write(Out,ADV),
% 		write(Out, ').'),
% 		nl(Out),
% 		fail.
% 
% writedown(Out) :- advbigramright(Tag,Tag2,Total,ADV),
%         write(Out,'advbigramright'),
%         write(Out, '(\''),
%         write(Out,Tag),
%         write(Out,'\',\''),
%         write(Out,Tag2), 
%         write(Out,'\','),
%         write(Out,Total),
%         write(Out,','),
%         write(Out,ADV),
%         write(Out, ').'),
%         nl(Out),
%         fail.


writedown(Out,Word,POSofHead) :- advlexical(Word,POSofHead,LeftCan,Left,ImmLeftCan,ImmLeft,RightCan,Right,ImmRightCan,ImmRight),
        write(Out,'advlexical'),
        write(Out, '(\''),
        write(Out,Word),
        write(Out,'\',\''),
        write(Out,POSofHead), 
        write(Out,'\','),
        write(Out,LeftCan), 
        write(Out,','),
        write(Out,Left),
        write(Out,','),
        write(Out,ImmLeftCan),
        write(Out,','),
        write(Out,ImmLeft),
        write(Out,','),
        write(Out,RightCan),
        write(Out,','),
        write(Out,Right),
        write(Out,','),
        write(Out,ImmRightCan),
        write(Out,','),
        write(Out,ImmRight),
        write(Out, ').'),
        nl(Out), !.


writedown(_,_,_) :- !.

advcan(adv).
advcan(adjd).
advcan(ptka).
advcan(ptkneg).
advcan(pwav).

%sample call
:- nl,write('copy & paste:'), nl, write('start(\'tueba4_stats4-end.pl\', \'advstat_data.pl\').'), nl, write(' ').