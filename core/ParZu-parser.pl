:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- style_check(-discontiguous).
:- use_module(library(lists)).

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

:- dynamic chart/11, min_len/1, inccount/1, inccount/2, lastpos/1, tops_chart/4, statschart/8, level/1, tried/2, perlevel/1, graphical/1, sentno/1, output/7, outputformat/1,sentdelim/1,returnsentdelim/1,nbestmode/1, morphology/1, lemmatisation/1.

:- index(chart(1,1,1,0,0,0,0,0,0,1,0));true. %% only has an effect in SWI
:- index(head(1,1,1,0,0,0,0,0,0,0));true.
:- index(head2(1,1,1,0,0,0,0,0,0,0));true.

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
    asserta(chart(ID,Pos,Pos,[[F,Ftag,Chunk]],[Pos,1,Chunk,1],F,Ftag,w,FChunk,0,MORPH)), % last arg: F/FChunk?
    shift(Stack,[[F,Ftag,Chunk,MORPH]|SRest],[Pos|PosList],LastPos,FStack,2).  %% shift calls sparse again -> recursion
    


%% sparse/21 this is the core CYK parsing algorithm
%% main parsing step:
%% REDUCE LEFT 
sparse(FID,FPos,_Ffrom,Fto,FFh,FChunk,FScore,Ftag,FuncF,[WFormF|MORPHF],
       GID,GPos,Gfrom,_Gto,FGh,GChunk,GScore,Gtag,FuncG,[_|MORPHG], Level) :-  %% Ffrom \= Gfrom,
  %F(G) = dep to left, reduce-Stack reversed!
  (tried(FID,GID) -> !, fail; assert(tried(FID,GID))), % already tried
  constant(commit,COMMIT),
  constant(discard,DISCARD),
  constant(alter,ALTER),
  constant(alterlocal,ALTERL),
  constant(aggressive_start,MAXCHART),
  constant(aggressive_thresh,THRESH),
  FuncF=..[SF|OF],           % get F pred (FF)
  FuncG=..[SG|OG],           % get G pred (FG)
  Dist is FPos - GPos,
  head2(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG,FID,GID],FPos-GPos,MORPHF,MORPHG,MORPH),
  (statschart(SF,MORPHF,SG,MORPHG,Type,Prob,Percent,Dist) -> true ; (once(stats2(Type,Ftag,FFh,SF,MORPHF,Gtag,FGh,SG,MORPHG,Prob,Percent,Dist,FChunk-OF)), asserta(statschart(SF,MORPHF,SG,MORPHG,Type,Prob,Percent,Dist)))),
  Prob >= DISCARD,
  %% assert stats
  atom_concat('<-',Type,ND1),atom_concat(ND1,'<-',DType),
  %% Build Func-Struc:
  appl_chunk_l(FuncF,FuncG,DType,FuncFTRes),
  (chart(_,Gfrom,Fto,_,_,_,Transtag,_,FuncFTRes,_,_) -> (fail);true), % alternative path joins in again
  OPScore is FScore * GScore * Percent, Len is Fto - Gfrom,
  inc(ID),
  (ID>MAXCHART -> (((OPScore / ((Len+(Len**sqrt(2)))+(ID/2))) < THRESH) -> (write(' TOO LOW!'),nl,fail);true); true),
  %do not assert if alternative is not among n best.
  (ALTERL>=0 ->
    (findall(PruneScore, (chart(_,Gfrom,Fto,_,[_,PruneScore,_,_],_,_,_,_,_,_),\+var(OPScore),PruneScore>OPScore),PruneList),
    len(PruneList,PruneLen),
    PruneLen < ALTER + ALTERL);true),
  %% OGfrom \= OFto,
  asserta(chart(ID,Gfrom,Fto,[[SF,Ftag,FChunk],[SG,Gtag,GChunk]],[FPos,OPScore,FChunk,Len],FFh,Transtag,Type,FuncFTRes,Level,[WFormF|MORPH])),
  (debug(1) -> (write(ID),write(' LEFT'),nl,write_tree(FuncFTRes),nl);true),
  retract(perlevel(X)),
  X1 is X+1, assert(perlevel(X1)),
  ((Prob > COMMIT) -> !; true), %% early commitment
  fail.

%% main parsing step:
%% REDUCE RIGHT
sparse(FID,FPos,_Ffrom,Fto,FFh,FChunk,FScore,Ftag,FuncF,[_|MORPHF],
       GID,GPos,Gfrom,_Gto,FGh,GChunk,GScore,Gtag,FuncG,[WFormG|MORPHG],Level) :- %% Ffrom \= Gfrom,
  %G(F) = dep to right, reduce-Stack reversed!
  (tried(GID,FID) -> !, fail; assert(tried(GID,FID))), % already tried
  constant(commit,COMMIT),
  constant(discard,DISCARD),
  constant(alter,ALTER),
  constant(alterlocal,ALTERL),
  constant(aggressive_start,MAXCHART),
  constant(aggressive_thresh,THRESH),
  FuncF=..[SF|OF],
  FuncG=..[SG|OG],
  !,
  Dist is FPos - GPos,
  head2(Gtag,Ftag,r,Type,Transtag,[GChunk,FChunk,FGh,FFh,OG,OF,GID,FID],GPos-FPos, MORPHG,MORPHF,MORPH),
  (statschart(SG,MORPHG,SF,MORPHF,Type,Prob,Percent,Dist) -> true; (once(stats2(Type,Gtag,FGh,SG,MORPHG,Ftag,FFh,SF,MORPHF,Prob,Percent,Dist,GChunk-OG)), asserta(statschart(SG,MORPHG,SF,MORPHF,Type,Prob,Percent,Dist)))),
  Prob >= DISCARD,
  %% assert stats
  atom_concat('->',Type,ND1),atom_concat(ND1,'->',DType),  % ->func->
  %% Build Func-Struc:
  appl_chunk_r(FuncG,FuncF,DType,FuncGTRes),
 (chart(_,Gfrom,Fto,_,_,_,Transtag,_,FuncGTRes,_,_) -> (fail); true), % alternative paths join again
  OPScore is FScore * GScore * Percent, Len is Fto - Gfrom,
  inc(ID),
  (ID>MAXCHART -> (((OPScore / ((Len+(Len**sqrt(2)))+(ID/2))) < THRESH) -> (write(' TOO LOW!'),nl,!,fail);true); true),
  %do not assert if alternative is not among n best.
  (ALTERL>=0 ->
    (findall(PruneScore, (chart(_,Gfrom,Fto,_,[_,PruneScore,_,_],_,_,_,_,_,_),\+var(OPScore),PruneScore>OPScore),PruneList),
    len(PruneList,PruneLen),
    PruneLen < ALTER + ALTERL);true),
  %% OGfrom \= OFto,
  asserta(chart(ID,Gfrom,Fto,[[SF,Ftag,FChunk],[SG,Gtag,GChunk]],[GPos,OPScore,GChunk,Len],FGh,Transtag,Type,FuncGTRes,Level,[WFormG|MORPH])),
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
    constant(alter,ALTER),
    chart( FID,Ffrom,Fto,_,[FPos,FScore,FChunk,_FLen],F,Ftag,_FType,FuncF,LA,MORPHF), % foreach chart entry

    (FID = 1 -> (!,true); (
     Gto is Ffrom-1,
     (LA is L-1 -> true; LB is L-1),
     chart( GID,Gfrom,Gto,_,[GPos,GScore,GChunk,_GLen],G,Gtag,_GType,FuncG,LB,MORPHG),
     sparse(FID,FPos,Ffrom,Fto,F,FChunk,FScore,Ftag,FuncF, MORPHF,
                    GID,GPos,Gfrom,Gto,G,GChunk,GScore,Gtag,FuncG, MORPHG,
		    L)
    )),
    write('End of Level '), write(L), write(' reduced items X: '), perlevel(X), lastpos(LastPos), !, XFact is (X/LastPos), write(X), write('XChunks :'), write(XFact), nl,
    %% has anything managed to reduce at this level? IF NOT, END
    X>0,
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
prune(L,XFact,ALTER) :-
    XFact < 100,
    write('fixed pruning keep '),
    %% constant(alter,ALTER),
    %% Div is (XFact/10), Div > 0, 
    write(ALTER), nl,
    %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    chart(_,Ffrom,Fto,_,[_,_Score,_,_],_,_,_,_,L,_),
    findall((Score,ID), chart(ID,Ffrom,Fto,_,[_,Score,_,_],_,_,_,_,_,_),List),
    len(List,Len),
    (Len < ALTER -> fail ;
      (sort(List,SList),
       Till is Len-(ALTER+1),  %% fixed beam length
       %% Till is Len-(Len/Div),  write(Till), write('/'),write(Len), nl, %% variable beam length
       prunechart(0,Till,SList),
       fail)).

%% aggressive pruning
prune(L,XFact,ALTER) :-
    XFact >= 100,
    write('variable pruning at '),
    %% constant(alter,ALTER),
    Div is (XFact/8), Div > 0, write(Div), nl,
   %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    chart(_,Ffrom,Fto,_,[_,_Score,_,_],_,_,_,_,L,_),
    findall((Score,ID), chart(ID,Ffrom,Fto,_,[_,Score,_,_],_,_,_,_,_,_),List),
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
    retract(chart(ID,_,_,_,_,_,_,_,_,_,_)),
    C1 is C+1,
    prunechart(C1,Till,RList).

prunechart(_,_,_). %eorec


shift(Stack,[[F,Ftag,Chunk,C]|SRest],[Pos|PosList],LastPos,FStack,Shift):-
    NewPos is LastPos + 1,
    sparse([[F,Ftag,Chunk,C,1]|Stack],SRest,[NewPos,Pos|PosList],NewPos,[F|FStack],Shift).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end of parsing




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
    (trim_stacks->true;true),
    retractall(statschart(_,_,_,_,_,_,_,_)),
    retractall(inccount(_,_)),
    collect_sent([],Sent,EOF,1),
    retractall(min_len(_)),
    retractall(chart(_,_,_,_,_,_,_,_,_,_,_)),
    retractall(tops_chart(_,_,_,_)),
    retractall(tried(_,_)),
    retractall(lastpos(_)),
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
    flush_output,
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
  (chart(_,I,J,_,[_,CScore,_,MaxLen],_,_,_,CStruc,_,_), \+ I=J, I>=A, J=<Z, append(SIn,[CStruc],SOut), ScOut is ScIn*CScore, MOut=MIn)
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
  chart(_ID,A,Z,_,[_,ZScore,_,Len],_,_,_,ZStruc,_,_),
  %dlen(A-Z,Len),
  ScoreMid is ScoreIn * ZScore * Len,
  append([ZStruc],StrucIn,StrucMid),
  pathfinder(Z-Z,MI-MO,ScoreMid-ScoreOut,StrucMid-StrucOut).
pathfinder(A-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :- % indirect path, decrease from right
  B is Z-1,
  A<B,
  pathfinder_r(A-B-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut).

pathfinder_r(A-B-Z,MI-MO,ScoreIn-ScoreOut,StrucIn-StrucOut) :-  % indirect path to end, long first
  chart(_ID,A,B,_,[_,BScore,_,Len],_,_,_,BStruc,_,_), 
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
