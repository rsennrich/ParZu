:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8)).
:- set_stream(user_output, encoding(utf8)).
:- system:prompt(_, '').
:- style_check(-discontiguous).

save_me :- qsave_program('6549_SGML.po',[local=131072,global=131072,trail=65536,argument=65536]).

runtime_entry(start) :- go_textual('./chunkedtext.txt').  %% if providing sicstus standalone runtime version

start :- consult('ParZu_parameters.pl'), load_grammar_german, go_textual.

% USE THIS TO START CORE 
start_simplistic :- consult('ParZu_parameters.pl'), consult('grammar_german.pl'),
			write('A sample call is:'),nl, write('go_textual(\'preprocessed_input.pl\'), told.'), nl.

start_german :- consult('ParZu_parameters.pl'), load_grammar_german,
write('A sample call is:'),nl,  write('go_textual(\'preprocessed_input.pl\'), told.'), nl.


%% set some flags
intralex(no).
complement_or_adjunct(no).
conj_expand(no). %% expand conj in postprocessing
appos_expand(no). %% expand appos


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% END of PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic chart/9, min_len/1, inccount/1, inccount/2, lastpos/1, tops_chart/4, statschart/13, level/1, tried/3, commalast/1, commalongest/2, perlevel/1, graphical/1, sentno/1, output/7, outputformat/1,sentdelim/1,returnsentdelim/1,nbestmode/1, morphology/1, lemmatisation/1.

:- index(chart(1,1,1,0,0,0,0,0,0)). %% only has an effect in SWI

:- ensure_loaded('tree_textual.pl').
:- assert(outputformat(raw)).
:- assert(sentdelim('$.')).
:- assert(returnsentdelim(yes)).
:- assert(nbestmode(0)).
:- assert(morphology(gertwol)).
:- assert(lemmatisation(gertwol)).

% debug(2). % show new best res.
% debug(1). % show all reduce steps
debug(0).   % USE THIS

%%%%%%%%%%%%%%%%%% LOAD GRAMMAR 

load_grammar_german :-
    ensure_loaded('grammar_german.pl'),	
    ensure_loaded('../postprocessor/postprocessing_module.pl'),
    ensure_loaded('../statistics/statmodule_ger.pl'),
    ensure_loaded('../statistics/konjstat_data'),
    (lemmatisation(gertwol) -> (ensure_loaded('../statistics/ppstat_data'),
    ensure_loaded('../statistics/advstat_data'),
    ensure_loaded('../statistics/vstat_data'),
    ensure_loaded('../statistics/freq_data')); true),
    (lemmatisation(off) -> (ensure_loaded('../statistics/ppstat_data_nolemma'),
    ensure_loaded('../statistics/advstat_data_nolemma'),
    ensure_loaded('../statistics/vstat_data_nolemma'),
    ensure_loaded('../statistics/freq_data_nolemma')) ; true).



%%%%%%%%% END OF HEADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% \section{intrachunk}
%% CUT COMPLETELY

%% end of intrachunk

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%\section{parsing}

%%% sparse( -reduce&c-structure_stack, +input_sentence, -WordPosStack, +LastWordPos, -functional_structure).

% shift is allowed in firstparse, but not in backtrack %% in CYK, make chart entries
% sparse/6 reads in a sentence by shift/6ing, and nothing else
sparse(Stack,[[F,Ftag,Chunk,MORPH]|SRest],[Pos|PosList],LastPos,FStack,2) :-               % sparse/6 shifts and dose nothing else
    !,
    inc(ID),
    name(F,NF), name(ID,NID),
    append(NF,"#",NFhash),
    append(NFhash,NID,NFID),
    name(FID,NFID),
    appl(FID,Chunk,FChunk),
    %%writeq(FID), write(','),
    %% intrachunk0(Chunk,FID,Ftag,C),
    asserta(chart(ID,Pos,Pos,[[F,Ftag,Chunk,F,1]],[Pos,Pos,Pos-Pos,1,1],[[F,Ftag,w,ID]],FChunk,0,MORPH)), % last arg: F/FChunk?
    shift(Stack,[[F,Ftag,Chunk,MORPH]|SRest],[Pos|PosList],LastPos,FStack,2).  %% shift calls sparse again -> recursion
    


%% sparse/11 this is the core CYK parsing algorithm
%% main parsing step:
%% REDUCE LEFT 
sparse(FID,_,[FPos,_FPos1,_Ffrom-Fto,FScore,_FLen],[[_F,Ftag,_FType,FID]],FuncF,[WFormF|MORPHF],
       GID,_,[GPos,_GPos1,Gfrom-_Gto,GScore,_GLen],[[_G,Gtag,_GType,GID]],FuncG,[_|MORPHG], Level) :-  %% Ffrom \= Gfrom,
  %F(G) = dep to left, reduce-Stack reversed!
  commalast(Commalast),
  (tried(FID,GID,Commalast) -> !, fail; assert(tried(FID,GID,Commalast))), % already tried
  FuncF=..[FF|LMF],           % get F pred (FF)
%  (([MF|OF]=LMF) -> true ; (MF=nil,OF=nil)),  % get F first arg (SF) & L of deptypes
%  ([FChunk|_]=LMF ->true ; (Ffrom=Fto,FChunk=FuncF)), % terminal nodes
%  MF=..[SF|_],
  member([FC1|FC2],LMF), FChunk=[FC1|FC2],
  OF = LMF, SF = FF,
  FuncG=..[FG|LMG],           % get G pred (FG)
%  (([MG|OG]=LMG) -> true ; (MG=nil,OG=nil)),  % get G first arg (SG) & L of deptypes
%  ([GChunk|_]=LMG ->true ; (Gfrom=Gto,GChunk=FuncG)), % terminal nodes
%  MG=..[SG|_],
  member([GC1|GC2],LMG), GChunk=[GC1|GC2],
  OG = LMG, SG = FG,
  lexic(FF,FFh,_), lexic(FG,FGh,_),
%  head(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG],FPos-GPos, MORPHF,MORPHG, MORPH),
  head2(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG,FID,GID],FPos-GPos,MORPHF,MORPHG,MORPH),
  Dist is FPos - GPos,
  lexic(FG,FGW,_), lexic(FF,FFW,_),
  (statschart(SF,Ftag,FFW,MORPHF,Gtag,FGW,SG,MORPHG,Type,Prob,Percent,Dist,_-_) -> true ; (stats(Type,Ftag,FFW,SF,MORPHF,Gtag,FGW,SG,MORPHG,Prob,Percent,Dist,FChunk-OF), asserta(statschart(SF,Ftag,FFW,MORPHF,Gtag,FGW,SG,MORPHG,Type,Prob,Percent,Dist,_FChunk-_OG)))),
  constant(commit,COMMIT),
  constant(discard,DISCARD),
  constant(alter,ALTER),
  constant(alterlocal,ALTERL),
  (Prob < DISCARD -> (fail); true),
  %% assert stats
  name(Type,NS), name('<-',NA),
  append(NA,NS,ND1),append(ND1,NA,ND),name(DType,ND),   % <-func<-
  %% Build Func-Struc:
  appl_chunk_l(FuncF,FuncG,DType,FuncFTRes),
  inc(ID),
  OFID = FID, OGID = GID, OGfrom = Gfrom, OFto = Fto, % same values whether from chart or parsing
  (chart(_,OGfrom,OFto,_,[_,_,OGfrom-OFto,_,_],[[_,Transtag,_,_]],FuncFTRes,_,_) -> (fail);true), % alternative path joins in again
  OPScore is FScore * GScore * Percent, Len is OFto - OGfrom,
  constant(aggressive_start,MAXCHART),
  constant(aggressive_thresh,THRESH),
  (ID>MAXCHART -> (((OPScore / ((Len+(Len**sqrt(2)))+(ID/2))) < THRESH) -> (write(' TOO LOW!'),nl,fail);true); true),
  dlen(OGfrom-OFto,DLen),
  %do not assert if alternative is not among n best.
  (ALTERL>=0->(findall((PruneScore,PruneID), chart(PruneID,OGfrom,OFto,_,[_,_,OGfrom-OFto,PruneScore,_],_,_,_,_),PruneList),
  len(PruneList,PruneLen),
  ALTERN is ALTER + ALTERL,
  (PruneLen < ALTERN -> true ;
    (sort(PruneList,SList),
    reverse(SList,SList2),
    nth1(ALTERN,SList2,(NScore,_))),
    OPScore>NScore));true),
  %% OGfrom \= OFto,
  asserta(chart(ID,OGfrom,OFto,[[FF,Ftag,FChunk,OFID,FScore],[FG,Gtag,GChunk,OGID,GScore]],[FPos,GPos,OGfrom-OFto,OPScore,DLen],[[FF,Transtag,Type,ID]],FuncFTRes,Level,[WFormF|MORPH])),
  (debug(1) -> (write(ID),write(' LEFT'),nl,write_tree(FuncFTRes),nl);true),
  retract(perlevel(X)),
  X1 is X+1, assert(perlevel(X1)),
  ((Prob > COMMIT) -> !; true), %% early commitment
  fail.

%% main parsing step:
%% REDUCE RIGHT
sparse(FID,[[_,_,FChunk,_,_]],[FPos,_FPos1,_Ffrom-Fto,FScore,_FLen],[[_F,Ftag,_FType,FID]],FuncF,[_|MORPHF],
       GID,[[_,_,GChunk,_,_]],[GPos,_GPos1,Gfrom-_Gto,GScore,_GLen],[[_G,Gtag,_GType,GID]],FuncG,[WFormG|MORPHG],Level) :- %% Ffrom \= Gfrom,
  %G(F) = dep to right, reduce-Stack reversed!
  (tried(GID,FID,0) -> !, fail; assert(tried(GID,FID,0))), % already tried
  FuncF=..[FF|LMF],           % get F pred (FF)
%  (([MF|OF]=LMF) -> true ; (MF=nil,OF=nil)),  % get F first arg (SF) & L of deptypes
%  ([FChunk|_]=LMF ->true ; (Ffrom=Fto,FChunk=FuncF)), % terminal nodes
%  MF=..[SF|_],
  member([FC1|FC2],LMF), FChunk=[FC1|FC2],
  OF = LMF, SF = FF,
  FuncG=..[FG|LMG],           % get G pred (FG)
%  (([MG|OG]=LMG) -> true ; (MG=nil,OG=nil)),  % get G first arg (SG) & L of deptypes
%  ([GChunk|_]=LMG ->true ; (Gfrom=Gto,GChunk=FuncG)), % terminal nodes
%  MG=..[SG|_],
  member([GC1|GC2],LMG), GChunk=[GC1|GC2],
  OG = LMG, SG = FG,
  !,
  lexic(FF,FFh,_), lexic(FG,FGh,_),
%  head(Gtag,Ftag,r,Type,Transtag,[FChunk,GChunk,FFh,FGh,[SF|OF],OG],FPos-GPos, MORPHF,MORPHG,MORPH),
  head2(Gtag,Ftag,r,Type,Transtag,[GChunk,FChunk,FGh,FFh,OG,OF,GID,FID],GPos-FPos, MORPHG,MORPHF,MORPH),
  Dist is FPos - GPos,
  lexic(FG,FGW,_), lexic(FF,FFW,_),
  (statschart(SG,Gtag,FGW,MORPHG,Ftag,FFW,SF,MORPHF,Type,Prob,Percent,Dist,_-_) -> true; (stats(Type,Gtag,FGW,SG,MORPHG,Ftag,FFW,SF,MORPHF,Prob,Percent,Dist,GChunk-OG), asserta(statschart(SG,Gtag,FGW,MORPHG,Ftag,FFW,SF,MORPHF,Type,Prob,Percent,Dist,_GChunk-_OG)))),
  constant(commit,COMMIT),
  constant(discard,DISCARD),
  constant(alter,ALTER),
  constant(alterlocal,ALTERL),
  (Prob < DISCARD -> (fail); true),
  %% assert stats
  name(Type,NS), name('->',NA),
  append(NA,NS,ND1),append(ND1,NA,ND),name(DType,ND),   % ->func->
  %% Build Func-Struc:
  appl_chunk_r(FuncG,FuncF,DType,FuncGTRes),
  inc(ID),
  OFID = FID, OGID = GID, OGfrom = Gfrom, OFto = Fto, % same values whether from chart or parsing
 (chart(_,OGfrom,OFto,_,[_,_,OGfrom-OFto,_,_],[[_,Transtag,_,_]],FuncGTRes,_,_) -> (fail); true), % alternative paths join again
  OPScore is FScore * GScore * Percent, Len is OFto - OGfrom,
  constant(aggressive_start,MAXCHART),
  constant(aggressive_thresh,THRESH),
  (ID>MAXCHART -> (((OPScore / ((Len+(Len**sqrt(2)))+(ID/2))) < THRESH) -> (write(' TOO LOW!'),nl,!,fail);true); true),
  dlen(OGfrom-OFto,DLen),
  %do not assert if alternative is not among n best.
  (ALTERL>=0->(findall((PruneScore,PruneID), chart(PruneID,OGfrom,OFto,_,[_,_,OGfrom-OFto,PruneScore,_],_,_,_,_),PruneList),
  len(PruneList,PruneLen),
  ALTERN is ALTER + ALTERL,
  (PruneLen < ALTERN -> true ;
    (sort(PruneList,SList),
    reverse(SList,SList2),
    nth1(ALTERN,SList2,(NScore,_))),
    OPScore>NScore));true),
  %% OGfrom \= OFto,
  asserta(chart(ID,OGfrom,OFto,[[FF,Ftag,FChunk,OFID,FScore],[FG,Gtag,GChunk,OGID,GScore]],[GPos,FPos,OGfrom-OFto,OPScore,DLen],[[FG,Transtag,Type,ID]],FuncGTRes,Level,[WFormG|MORPH])),
  (debug(1) -> (write(ID),write(' RIGHT'),nl,write_tree(FuncGTRes),nl);true),
  retract(perlevel(X)),
  X1 is X+1, assert(perlevel(X1)),
  ((Prob > COMMIT) -> !; true), %% early commitment
  fail.


%% initiate the parsing process
%% sparse/6 -> nextlevel/2 -> sparse/9: sparse/9 reduces all it can on one CYK level
sparse(Stack,[],_,N,FStruc,_) :-     % NEW FUNCTION IN CYK ::: launch the parsing process (hitherto it was just shifting)
    % nl, write('END OF ROUND:'), write(Stack), nl,
    % ultiparse(Stack,[],_,N,FStruc,1),
    len(Stack,Length),
    %Length > 1,
    min_len(MinLen),
    Length =< MinLen,
    retractall(min_len(_)),
    retractall(sentrel(_)),
    assert(min_len(Length)),
    retract(level(L)),
    L1 is L+1,
    assert(level(L1)),
    retractall(lastpos(_)),
    LastPos is N-1,
    assert(lastpos(LastPos)),
    %% write(' s).'), %% if printing sentence heads
    write('</PROLOG_INTRACHUNK>'), nl, 
    nl, write('END OF ROUND '), write(L), write(' : '), write(LastPos), write(' chunks'), nl, !,
    (debug(2) -> (write_tree(FStruc),nl);true),    
    retractall(perlevel(_)),
    assert(perlevel(0)),
    %% TEST %% lddq(L1), %% long-distance dep.s such as questions
    %% NEXT LEVEL: SELECT CANDIDATES FROM CHART: foreach chart entry ...
    constant(levels,MAX),
    nextlevel(1,MAX).


% nextlevel -> bagof(sparse/9): reduces all it can on the given CYK level     
nextlevel(L,MAX) :-
    L < MAX,
    chart( FID,Ffrom,Fto,_,[FPos,FPos1,Ffrom-Fto,FScore,FLen],[[F,Ftag,FType,FID]],FuncF,_LA,MORPHF), % foreach chart entry
    % LA < L,
    (FID = 1 -> (!,true); (
     Gto is Ffrom-1,
     chart( GID,Gfrom,Gto,_,[GPos,GPos1,Gfrom-Gto,GScore,GLen],[[G,Gtag,GType,GID]],FuncG,LB,MORPHG),
     (commalast(0) -> true ; checkcommalongest(Gfrom,Gtag,LB,Ffrom,Fto,_Len,FID,GID)),
     % (LA is L-1 -> true; LB is L-1), %% experiment, see 6331,6332,6333
     %% findall only finds the first -> untauglich
     bagof(_,sparse(FID,_,[FPos,FPos1,Ffrom-Fto,FScore,FLen],[[F,Ftag,FType,FID]],FuncF, MORPHF,
                    GID,_,[GPos,GPos1,Gfrom-Gto,GScore,GLen],[[G,Gtag,GType,GID]],FuncG, MORPHG,
		    L),_)
    )),
    write('End of Level '), write(L), write(' reduced items X: '), perlevel(X), lastpos(LastPos), !, XFact is (X/LastPos), write(X), write('XChunks :'), write(XFact), nl,
    %% has anything managed to reduce at this level? IF NOT --> TRY COMMAJUMP; THEN END!
    % (chart(_UntilID,_,_,_,_,_,_,L) -> (retractall(commalast(_)), assert(commalast(0))) ; (commalast(0), retractall(commalast(0)), assert(commalast(1)))), !,
    (X>0 -> true ; (commalast(0), retractall(commalast(0)), assert(commalast(1)))),
     constant(alter,ALTER),
    prune(L,XFact,ALTER),
    L1 is L+1,
    retractall(perlevel(_)),
    assert(perlevel(0)),
    % retractall(commalongest(_,_)),
    !, % CUT, this level is done
    nextlevel(L1,MAX).


%% \subsection{pruning}
%% use this if you want no pruning ever, for developping
%prune(_,XFact,ALTER):- !. %% no pruning at all

%% no pruning: will be used as long as complexity low
%prune(_,XFact,ALTER) :- XFact < (ALTER-1), inccount(X), X < 200, !.

%% mild pruning
prune(_L,XFact,ALTER) :-
    XFact < 100,
    write('fixed pruning keep '),
    %% constant(alter,ALTER),
    %% Div is (XFact/10), Div > 0, 
    write(ALTER), nl,
    %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    chart(_,Ffrom,Fto,_,[_,_,Ffrom-Fto,_Score,_],_,_,_,_),
    findall((Score,ID), chart(ID,Ffrom,Fto,_,[_,_,Ffrom-Fto,Score,_],_,_,_,_),List),
    len(List,Len),
    (Len < ALTER -> fail ;
      (sort(List,SList),
       Till is Len-(ALTER+1),  %% fixed beam length
       %% Till is Len-(Len/Div),  write(Till), write('/'),write(Len), nl, %% variable beam length
       prunechart(0,Till,SList),
       fail)).

%% aggressive pruning
prune(_L,XFact,ALTER) :-
    XFact >= 100,
    write('variable pruning at '),
    %% constant(alter,ALTER),
    Div is (XFact/8), Div > 0, write(Div), nl,
   %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    chart(_,Ffrom,Fto,_,[_,_,Ffrom-Fto,_Score,_],_,_,_,_),
    findall((Score,ID), chart(ID,Ffrom,Fto,_,[_,_,Ffrom-Fto,Score,_],_,_,_,_),List),
    len(List,Len),
    (Len < ALTER -> fail ;
      (sort(List,SList),
       %% Till is Len-(ALTER+1),  %% fixed beam length
       Till is Len-(Len/Div),
       Till > 0,
       write(Till), write('/'),write(Len), nl, %% variable beam length
       prunechart(0,Till,SList),
       fail)).

 prune(_,_,_) :- !.

%% prunechart: removes as many chart items as told. stops at Till.
prunechart(_,_,[]). %eol

prunechart(C,Till,[(_Score,ID)|RList]) :-
    C < Till, !,
    %displaychart(ID),
    retract(chart(ID,_,_,_,_,_,_,_,_)),
    C1 is C+1,
    prunechart(C1,Till,RList).

prunechart(_,_,_). %eorec

checkcommalongest(Gfrom,Gtag,GA,Ffrom,Fto,Len,_FID,_GID) :-
    Gtag = 'COMMA',
    GA is 0,
    Len is Fto-Ffrom,
    (commalongest(Gfrom,OLen) -> true ; OLen is 0),
    Len >= OLen, retractall(commalongest(Gfrom,OLen)), assert(commalongest(Gfrom,Len)).


shift(Stack,[[F,Ftag,Chunk,C]|SRest],[Pos|PosList],LastPos,FStack,Shift):-
    NewPos is LastPos + 1,
    sparse([[F,Ftag,Chunk,C,1]|Stack],SRest,[NewPos,Pos|PosList],NewPos,[F|FStack],Shift).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end of parsing


%% \section{Statistics}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% use this if you want no stats at all: simply delivers probability = 1.
%stats(Type,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,1,1,Dist,HeadChunk-OG) :- !.

stats(Type,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,NP,Dist,HeadChunk-OG) :- stats2(Type,Htag,FH,SH,MORPHH,Dtag,FD,SD,MORPHD,P,NP,Dist,HeadChunk-OG), !.



lexic(WordI,Word,I) :-
	atomic(WordI), !,
	sub_atom(WordI,Before,1,After,'#'),
	sub_atom(WordI,     0,Before,_,Word),
	Before1 is Before+1,
	sub_atom(WordI,Before1,After,_,Iaaa),
	name(Iaaa,Iaa), name(I,Iaa),
        (number(I) -> true ; I=1).

lexic(WordI,WordI,_).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Routines for Building up Structures:

appl(Head,Chunk,NewStruc) :-
    is_list(Chunk),
    Head=..[Pred|Args],
    NewStruc=..[Pred,Chunk|Args].

appl_chunk_l(Head,Dep1,DType,NewStruc) :-
    Head=..[Pred|Args],
    NewStruc=..[Pred,Dep1,DType|Args].

appl_chunk_r(Head,Dep1,DType,NewStruc) :-
    Head=..[Pred|Args],
    %% NewStruc=..[Pred,Args,DType,Dep1] % what we want
    append(Args,[DType],DArgs),
    append(DArgs,[Dep1],NArgs),
    NewStruc=..[Pred|NArgs].


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

is_tag(F) :-
    name(F,NF),
    name('_',[U]),
    memberchk(U,NF).

is_arrow(F) :-
    name(F,NF),
    name('>',[U]),
    memberchk(U,NF).


prettyprint([],_).% end rec.

prettyprint(F,Indent) :-
    atomic(F), 
    (is_tag(F) ->
        (nl,LIndent is Indent-1,
        spaces(LIndent),write( ' ['), write(F), write(']'));
    is_arrow(F) ->
        (nl,spaces(Indent),
        write(F));
    (nl,spaces(Indent),write(F))
    ).

prettyprint([F|R],Indent) :- % traverse list
    prettyprint(F,Indent),
    prettyprint(R,Indent).

prettyprint(F,Indent) :-        %a(b,c(d)) convert to list:
    F=..[Pred|Args],            %[a,b,c(d)]
    NewIndent is Indent+1,
    prettyprint(Pred,Indent),
    prettyprint(Args,NewIndent).

spaces(0).    
spaces(X) :- 
    X > 0,   % important to stop recursion.
    write('|    '), 
    LessX is X-1, 
    spaces(LessX).
    
closebrackets(0).
closebrackets(X) :-
    X > 0,
    write('|'),
    LessX is X-1,
    closebrackets(LessX).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Driver Predicates:

go_textual(File) :-
    abolish(perlevel/1),
    retractall(graphical(_)), assert(graphical(false)),
    retractall(sentno(_)), assert(sentno(0)),
    open(File, read, Stream,[encoding(utf8)]), 
    set_input(Stream),
    collect_sents(no-end),
    seen.

go_textual :-
    abolish(perlevel/1),
    retractall(graphical(_)), assert(graphical(false)),
    retractall(sentno(_)), assert(sentno(0)),
    collect_sents(no-end),
    seen.


%% \begin{Input/Output}

%% read in sentence word by word
%% TODO: XML input (should be easy)
collect_sents(end-of-file).
collect_sents(no-end) :-
    garbage_collect,
    trim_stacks,
    retractall(statschart(_,_,_,_,_,_,_,_,_,_,_,_,_)),
    retractall(inccount(_,_)),
    collect_sent([],Sent,EOF,1),
    retractall(min_len(_)),
    retractall(chart(_,_,_,_,_,_,_,_,_)),
    retractall(tops_chart(_,_,_,_)),
    retractall(tried(_,_,_)),
    retractall(lastpos(_)),
    retractall(commalast(_)),
    retractall(commalongest(_,_)),
    assert(commalast(0)),
    assert(min_len(1000)),
    retractall(inccount(_)),
    assert(inccount(0)),
    retractall(level(_)),
    assert(level(0)),
    %write('NEW SENTENCE :: =============================='), nl,
    %% sparse([],Sent,[0],0,[],1),    %% this prints first solution only
    writeq(sent(Sent)), write('.'), nl,
    %write('sent_heads( '),
    write('<PROLOG_INTRACHUNK>'), nl,
    assert(lastpos(1)),
    findall(Sent,sparse([],Sent,[1],1,[],2),_),  %% this prints all solutions
    % listing(chart), %%%%%%%%%%%%%%%%%%%%%%% debugging
    !, %% no backtracking into parser
    pathfinder,
    write('%%END_OF_SENTENCE'),nl,nl,
    %% ((graphical(true),\+ object(@frame)) -> !, abort ;
    !, collect_sents(EOF). % cut, this sentence is done

collect_sents(fail) :- 
    write('NO COMPLETE STRUCTURE'), nl,
    collect_sents(_).


collect_sent(Acc,Sent,EOF,Position) :-
    read_term(T,[syntax_errors(dec10)]),
    process_sent(T,Acc,Sent,EOF,Position).

%%%%%%%%%%%%%%%%%%%%%% IF INPUT IS ABSOLUTELY ERROR-FREE
%collect_sent(Acc,Sent,EOF) :-
%    read(T),
%    process_sent(T,Acc,Sent,EOF).

%% file ends, everything is read in
process_sent(T,Sent,Sent,end-of-file,_):-
    T == end_of_file,
    write('%% ==END-OF-FILE'),nl.

%% sentence ends -> start parsing    
process_sent(T,Sent,Sent,no-end,_) :-
    (returnsentdelim(no);T=w('ENDOFDOC',SentDelim,_,_)),
    sentdelim(SentDelim),
     T = w(_,SentDelim,_,_),
    retract(sentno(S)),
    S1 is S+1,
    assert(sentno(S1)),
    nl.  % w('S','S',['S_S'],a),nl. for non-stemmed

%% sentence ends -> start parsing   
process_sent(T,Acc,Sent,no-end,_) :-
    returnsentdelim(yes),
    sentdelim(SentDelim),
    T = w(Punct,SentDelim,Chunk,C),
    append(Acc,[[Punct,SentDelim,Chunk,C]],Sent),
    retract(sentno(S)),
    S1 is S+1,
    assert(sentno(S1)),
    nl.  % w('S','S',['S_S'],a),nl. for non-stemmed

%% sgml predicate read in: thread through
process_sent(T,Acc,Sent,no-end,Position) :-
    T = sgml(SGML),
    % display(sgml(SGML)),
    writeq(sgml(SGML)), write('.'), nl, nl,
    collect_sent(Acc,Sent,no-end,Position).

process_sent(T,Acc,Sent,no-end,Position) :-
    T = sgml(SGML,Wo),
    % display(sgml(SGML)),
    writeq(sgml(SGML,Wo)), write('.'), nl, nl,
    collect_sent(Acc,Sent,no-end,Position).

process_sent(T,Acc,Sent,no-end,Position) :-
    T = sid(SGML),
    %display(sid(SGML)),
    writeq(sid(SGML)), write('.'), nl, nl,
    collect_sent(Acc,Sent,no-end,Position).

process_sent(T,Acc,Sent,no-end,Position) :-
    T = sid(SGML,SGML2),
    %display(sid(SGML)),
    writeq(sid(SGML,SGML2)), write('.'), nl, nl,
    collect_sent(Acc,Sent,no-end,Position).

process_sent(T,Acc,Sent,no-end,Position) :-
    T = id(_WSJ,SGML),
    % display(sid(SGML)),
    writeq(sid(SGML)), write('.'), nl, nl,
    collect_sent(Acc,Sent,no-end,Position).

%% read in an individual chunk, recursively
process_sent(w(Head,HeadTag,Chunk,C),Acc,Sent,no-end,Position) :-
    % T == w(Head,HeadTag,Chunk,C),
    append(Acc,[[Head,HeadTag,Chunk,C]],NewAcc),
     Position1 is Position+1,
    collect_sent(NewAcc,Sent,no-end,Position1).


%% Helper Predicates:
    
% Standard append/3: append(?L1,?L2,?L3).
%append([],L,L).
%append([X|L1],L2,[X|L3]) :-
%    append(L1,L2,L3).

% Length of a DiffList: dlen(+A-B,-Len).
dlen(A-B,Len) :-
    Len is B - A.

% Length of a List: len(+List,-Len).
len([],0).
len([_|R],Len) :-
    len(R,NewLen),
    Len is NewLen +1.

inc(Y) :-
    retract(inccount(X)),
    Y is X + 1,
    assert(inccount(Y)).

print_set_strucs([]).

print_set_strucs([F|R]) :-
  print_strucs(F),
  write('========================================================================'),nl,
  print_set_strucs(R).


%% \section{pathfinder}
%% pathfinder collects several incomplete parses into a most promising set


pathfinder :-
  lastpos(Z),
  (Z = 0 -> ! ;
   ( retractall(inccount(_)),
    assert(inccount(0)),
    longpathfinder(1-Z,[]-MC,1-FinalScore,[]-Strucs),%% new method
    %pathfinder_l(1-Z,[]-MC,1-FinalScore,[]-Strucs), %% old method
    inc(Count),
    assertz(tops_chart(FinalScore,MC,1-Z,Strucs)),
    %print_strucs2(Count,FinalScore,MC,1-Z,Strucs),
    %write(Count), nl,
    (Count > 1000 -> pathcollect([]) ; fail)
    )).%% cutoff at 1000

pathfinder :- % endrule
  % ultiparse,
  pathcollect([]).

pathcollect(L) :-
  retract(tops_chart(FinalScore,MC,1-Z,Strucs)),
  pathcollect([[FinalScore,MC,1-Z,Strucs]|L]).
pathcollect(Acc) :-
  sort(Acc,SAcc),
  reverse(SAcc,RSAcc),
  print_eachstruc(RSAcc,1).

print_eachstruc([],_).
print_eachstruc([[_FinalScore,_MC,1-_Z,_Strucs]|_R],Count) :- ambi(Ambi), Count >= Ambi. %%%%%%%%%% set to same !!
print_eachstruc([[FinalScore,MC,1-Z,Strucs]|R],Count) :- ambi(Ambi), Count < Ambi, !,
  writeq(analyses(Count,FinalScore,MC,1-Z,Strucs)), write('.'), nl,
  write('<PROLOGPREDS '), write(Count), write('>'), nl, process_strucs(Strucs),!,
  outputformat(Outputformat),
  nl, postprocess(1,Outputformat),
  write('</PROLOGPREDS>'), nl,
  (modus(showparses) -> print_strucs2(Count,FinalScore,MC,1-Z,Strucs) ; true),
  Count1 is Count + 1,
  print_eachstruc(R,Count1).


process_strucs([]).

process_strucs([F|R]) :-
    process_struc(F,_,_),
    process_strucs(R).

process_struc(Struc,Head,_) :-
    Struc =.. [Head,_Chunk]. %%% leaf

process_struc(Struc,Head,HeadOfDep) :-
    ((Struc =.. [Head,Chunk,Rel,Dep], name(Rel,[45,62|NRel])) -> Dir='(->)';         %% dep found TO THE RIGHT -> =[45,62]
     (Struc =.. [Head,Dep,Rel,Chunk], name(Rel,[60,45|NRel]),    Dir='(<-)')),       %% dep found TO THE LEFT  <- =[60,45]
    reverse(NRel,RNRel), append([_,_],RRel,RNRel),
    reverse(RRel,RRRel), name(MyRel,RRRel),
    createMorphOutput(Struc,Dep,MyRel),
    process_struc(Dep,HeadOfDep,HeadofDepDep),         %% Rel(Head,HeadOfDep).
    createRelOutput(Head,HeadOfDep,MyRel),
    % Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir], awriteq(Res), write('.'), nl, assert(Res).
    (outputformat(raw) -> (           Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir], awriteq(Res), write('.'), nl) ; true).


process_struc(Struc,Head,HeadOfDep) :-
    ((Struc =.. [Head,Chunk,Rel,Dep|Rest], name(Rel,[45,62|NRel])) -> Dir='(->)';    %% dep found TO THE RIGHT, but more deps to follow
     (Struc =.. [Head,Dep,Rel,Chunk|Rest], name(Rel,[60,45|NRel]),    Dir='(<-)')),  %% dep found TO THE LEFT , but more deps to follow
    reverse(NRel,RNRel), append([_,_],RRel,RNRel),
    reverse(RRel,RRRel), name(MyRel,RRRel),
    createMorphOutput(Struc,Dep,MyRel),
    process_struc(Dep,HeadOfDep,HeadofDepDep),        %% Rel(Head,HeadOfDep)
    createRelOutput(Head,HeadOfDep,MyRel),
    (outputformat(raw) -> (           Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir], awriteq(Res), write('.'), nl) ; true),
    MoreDeps =.. ([Head,Chunk|Rest]),
    process_struc(MoreDeps,_,_),
    !.


%check grammar for (language-specific) rules.
% createMorphOutput(_,_,_) :- !.
% createRelOutput(_,_,_) :- !.

awriteq(Res) :- writeq(Res), Res =.. ResList, assert(sentrel(ResList)).



%%% SORT PATHS: | ?- sort([[1.121,2,3],[1.105,1,0]],X).
%%% X = [[1.105,1,0],[1.121,2,3]]

%%%%longpathfinder(From-To,Missing,Score,Struc).
longpathfinder(Z-Z,M-M,Sc-Sc,S-S) :- !. 
longpathfinder(A-Z,MIn-MOut,ScIn-ScOut,SIn-SOut) :-
  dlen(A-Z,MaxLen),
  findlongest(MaxLen,A-Z,A-I-J-Z,MIn-MMid1,ScIn-ScMid1,SIn-SMid1), % find longest chart entry
  (I=A -> (I1 is A, MMid2=MMid1) ; (I1 is I-1, (I1=A -> MMid2=[A|MMid1];MMid2=MMid1) )),
  longpathfinder(A-I1,MMid2-MMid3,ScMid1-ScMid2,SMid1-SMid2),      % left  of found
  (J=Z -> (J1 is Z, MMid4=MMid3) ; (J1 is J+1, (J1=Z -> MMid4=[Z|MMid3];MMid4=MMid3) )),
  longpathfinder(J1-Z,MMid4-MOut,ScMid2-ScOut,SMid2-SOut).         % right of found

%findlongest(Len,From-To,From-ViaFrom-ViaTo-To,Missing,Score).
findlongest(0,A-Z,A-A-Z-Z,_MIn-[A-Z],S-S,Sc-Sc) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%SWI VERSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findlongest(MaxLen,A-Z,A-I-J-Z,MIn-MOut,ScIn-ScOut,SIn-SOut) :-
  (chart(_,I,J,_,[_,_,I-J,CScore,MaxLen],_,CStruc,_,_), \+ I=J, I>=A, J=<Z, append(SIn,[CStruc],SOut), ScOut is ScIn*CScore, MOut=MIn)
  *-> true;
     (NewMaxLen is MaxLen-1,findlongest(NewMaxLen,A-Z,A-I-J-Z,MIn-MOut,ScIn-ScOut,SIn-SOut)).

    

pathfinder_l(Z-Z,MCIn-MCIn,Score-Score,Struc-Struc) :- !. % end of recursion
pathfinder_l(A-Z,MCIn-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut) :-  % path from start
  pathfinder(A-Z,MCIn-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut).
pathfinder_l(A-Z,MCIn-[A|MCOut],ScoreIn-ScoreOut,StrucIn-StrucOut) :- % or path from start + 1
  A1 is A+1,
  (A1<Z ->
   pathfinder(A1-Z,MCIn-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut);
   pathfinder(A1-Z,[A1|MCIn]-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut)). 
pathfinder_l(A-Z,MCIn-[Z|MCOut],ScoreIn-ScoreOut,StrucIn-StrucOut) :- % or path to end - 1
  A<Z,
  Z1 is Z-1,
  (A<Z1 ->
   pathfinder(A-Z1,MCIn-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut);
   pathfinder(A-Z1,[Z1|MCIn]-MCOut,ScoreIn-ScoreOut,StrucIn-StrucOut)).
pathfinder(Z-Z,M-M,FS-FS,Strucs-Strucs) :- !.           % end of recursion
pathfinder(A-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :-   % direct path
  chart(_ID,A,Z,_,[_,_,A-Z,ZScore,Len],_,ZStruc,_,_),
  %dlen(A-Z,Len),
  ScoreMid is ScoreIn * ZScore * Len,
  append([ZStruc],StrucIn,StrucMid),
  pathfinder(Z-Z,MI-MO,ScoreMid-ScoreOut,StrucMid-StrucOut).
pathfinder(A-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :- % indirect path, decrease from right
  B is Z-1,
  A<B,
  pathfinder_r(A-B-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut).

pathfinder_r(A-B-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :-  % indirect path to end, long first
  chart(_ID,A,B,_,[_,_,A-B,BScore,Len],_,BStruc,_,_), 
  %dlen(A-B,Len),
  ScoreMid is ScoreIn * BScore * Len,
  append([BStruc],StrucIn,StrucMid),
  B1 is B+1,
  (B1<Z ->
   pathfinder_l(B1-Z,MI-MO,ScoreMid-ScoreOut,StrucMid-StrucOut); 
   pathfinder_l(B1-Z,[B1|MI]-MO,ScoreMid-ScoreOut,StrucMid-StrucOut)).
pathfinder_r(A-B-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :-  % or indirect path to end, long-1 first
  BB is B-1,
  A<BB,
  pathfinder_r(A-BB-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut).
