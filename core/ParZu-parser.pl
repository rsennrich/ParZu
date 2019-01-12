:- set_prolog_flag(encoding,utf8).
:- set_stream(user_input, encoding(utf8));true.
:- set_stream(user_output, encoding(utf8));true.
:- system:prompt(_, '').
:- style_check(-discontiguous).
:- use_module(library(lists)).

% Use this to parse stdin
start :- consult('ParZu_parameters.pl'), load_grammar_german, go_textual.

% Use this for to load grammar and then go into interactive mode (i.e. to parse file and/or debug)
start_german :- consult('ParZu_parameters.pl'), load_grammar_german,
write('A sample call is:'),nl,  write('go_textual(\'preprocessed_input.pl\'), told.'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic chart/10, scores/5, inccount/1, lastpos/1, tops_chart/4, statschart/8, perlevel/1, sentno/1, output/7, outputformat/1, sentdelim/1, returnsentdelim/1, nbestmode/1, morphology/1, lemmatisation/1, case_nom/2, case_acc/2, case_dat/2, case_gen/2, gender_neut/2.

:- ensure_loaded('tree_textual.pl').
:- assert(outputformat(raw)).
:- assert(sentdelim('$.')).
:- assert(returnsentdelim(yes)).
:- assert(nbestmode(0)).
:- assert(morphology(gertwol)).
:- assert(lemmatisation(gertwol)).
:- assert(extrainfo(no)).

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
    ensure_loaded('../statistics/adv_pred_data'),
    ensure_loaded('../statistics/gmod_ne_data'),
    ensure_loaded('../statistics/freq_data')); true),
    (lemmatisation(off) -> (ensure_loaded('../statistics/ppstat_data_nolemma'),
    ensure_loaded('../statistics/advstat_data_nolemma'),
    ensure_loaded('../statistics/vstat_data_nolemma'),
    ensure_loaded('../statistics/adv_pred_data_nolemma'),
    ensure_loaded('../statistics/freq_data_nolemma')) ; true).

%% END of PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% END OF HEADER
%%\section{parsing}

% shift is allowed in firstparse, but not in backtrack %% in CYK, make chart entries
% sparse/6 reads in a sentence by shift/6ing, and nothing else
sparse(Stack,[[F,Ftag,Chunk,MORPH]|SRest],[Pos|PosList],LastPos,FStack,2) :-               % sparse/6 shifts and dose nothing else
    !,
    inc(ID),
    name('#', Hash),
    name(F,NF), name(ID,NID),
    append(NF,Hash,NFhash),
    append(NFhash,NID,NFID),
    name(FID,NFID),
    appl(FID,Chunk,FChunk),
    asserta(chart(ID,Pos,Pos,[Pos,1,Chunk,1],F,Ftag,w,FChunk,0,MORPH)), % last arg: F/FChunk?
    shift(Stack,[[F,Ftag,Chunk,MORPH]|SRest],[Pos|PosList],LastPos,FStack,2).  %% shift calls sparse again -> recursion
    


%% sparse/21 this is the core CYK parsing algorithm
%% main parsing step:
sparse(FID,FPos,_Ffrom,Fto,FFh,FChunk,FScore,Ftag,FuncF,[WFormF|MORPHF],
       GID,GPos,Gfrom,_Gto,FGh,GChunk,GScore,Gtag,FuncG,[WFormG|MORPHG], Level) :-
  constant(commit,COMMIT),
  constant(discard,DISCARD),
  constant(aggressive_start,MAXCHART),
  constant(aggressive_thresh,THRESH),
  FuncF=..[SF|OF],           % get F pred (FF)
  FuncG=..[SG|OG],           % get G pred (FG)
  !,
  (
  %right word is head
    (head(Ftag,Gtag,l,Type,Transtag,[FChunk,GChunk,FFh,FGh,OF,OG,FID,GID],FPos-GPos,MORPHF,MORPHG,MORPH),
     Dist is FPos - GPos,
     (statschart(SF,SG,Type,Ftag,MORPHF,Gtag,MORPHG,Prob) -> true ; (once(stats2(Type,Ftag,FFh,SF,WFormF,MORPHF,Gtag,FGh,SG,WFormG,MORPHG,Prob,Dist,FChunk-OF,GChunk-OG)), asserta(statschart(SF,SG,Type,Ftag,MORPHF,Gtag,MORPHG,Prob)))),
     Prob >= DISCARD,
     Dir = l,
     atom_concat('<-',Type,ND1),atom_concat(ND1,'<-',DType),  % <-func<-
     appl_chunk_l(FuncF,FuncG,DType,FuncTRes)
    );
  %left word is head
    (head(Gtag,Ftag,r,Type,Transtag,[GChunk,FChunk,FGh,FFh,OG,OF,GID,FID],GPos-FPos, MORPHG,MORPHF,MORPH),
     Dist is FPos - GPos,
     (statschart(SG,SF,Type,Gtag,MORPHG,Ftag,MORPHF,Prob) -> true; (once(stats2(Type,Gtag,FGh,SG,WFormG,MORPHG,Ftag,FFh,SF,WFormF,MORPHF,Prob,Dist,GChunk-OG,FChunk-OF)), asserta(statschart(SG,SF,Type,Gtag,MORPHG,Ftag,MORPHF,Prob)))),
     Prob >= DISCARD,
     Dir = r,
     atom_concat('->',Type,ND1),atom_concat(ND1,'->',DType),  % ->func->
     appl_chunk_r(FuncG,FuncF,DType,FuncTRes)
    )
  ),
  ((Prob > COMMIT) -> !; true), %% early commitment
  (chart(_,Gfrom,Fto,_,_,Transtag,_,FuncTRes,_,_) -> (fail);true), % alternative path joins in again
  OPScore is FScore * GScore * Prob, Len is Fto - Gfrom,
  inc(ID),
  (ID>MAXCHART -> (((OPScore / ((Len+(Len**sqrt(2)))+(ID/2))) < THRESH) -> (write(' TOO LOW!'),nl,!,fail);true); true),
  %do not assert if alternative is not among ALTER best.
  constant(alter,ALTER),
  findall(AltScore, (scores(Gfrom,Fto,_,_,AltScore),\+var(OPScore),AltScore>OPScore),AltList),
  length(AltList,AltLen),
  AltLen < ALTER,
  asserta(scores(Gfrom,Fto,Level,ID,OPScore)),
  (Dir = l -> 
    asserta(chart(ID,Gfrom,Fto,[FPos,OPScore,FChunk,Len],FFh,Transtag,Type,FuncTRes,Level,[WFormF|MORPH]));
    asserta(chart(ID,Gfrom,Fto,[GPos,OPScore,GChunk,Len],FGh,Transtag,Type,FuncTRes,Level,[WFormG|MORPH]))
  ),
  retract(perlevel(X)),
  X1 is X+1, assert(perlevel(X1)),
  fail.


%% initiate the parsing process
%% sparse/6 -> nextlevel/2 -> sparse/21: sparse/21 reduces all it can on one CYK level
sparse(_,[],_,N,_,_) :-     % NEW FUNCTION IN CYK ::: launch the parsing process (hitherto it was just shifting)
    retractall(sentrel(_)),
    retractall(lastpos(_)),
    LastPos is N-1,
    assert(lastpos(LastPos)),
    nl,  
    retractall(perlevel(_)),
    assert(perlevel(0)),
    %% NEXT LEVEL: SELECT CANDIDATES FROM CHART: foreach chart entry ...
    constant(levels,MAX),
    nextlevel(1,MAX).


% nextlevel -> sparse/21: reduces all it can on the given CYK level     
nextlevel(L,_) :-
    LA is L-1,
    chart( FID,Ffrom,Fto,[FPos,FScore,FChunk,_FLen],F,Ftag,_FType,FuncF,LA,MORPHF), % foreach new chart entry
       %try to parse with any left neighbours
    ( ( Gto is Ffrom-1,
        chart( GID,Gfrom,Gto,[GPos,GScore,GChunk,_],G,Gtag,_,FuncG,_,MORPHG),
        sparse(FID,FPos,Ffrom,Fto,F,FChunk,FScore,Ftag,FuncF,MORPHF,
               GID,GPos,Gfrom,Gto,G,GChunk,GScore,Gtag,FuncG,MORPHG,
               L)
      );
       %try to parse with any right neighbours
      ( Gfrom is Fto+1,
        chart( GID,Gfrom,Gto,[GPos,GScore,GChunk,_],G,Gtag,_,FuncG,LB,MORPHG),
        LB \= LA, %if both FID and GID are on same level, this has already been done.
        sparse(GID,GPos,Gfrom,Gto,G,GChunk,GScore,Gtag,FuncG,MORPHG,
               FID,FPos,Ffrom,Fto,F,FChunk,FScore,Ftag,FuncF,MORPHF,
               L)
      )
    ).


nextlevel(L,MAX) :-
    lastpos(LastPos), 
    LastPos > 0,
    constant(alter,ALTER),
    write('End of Level '), write(L), write(' reduced items X: '), perlevel(X), !, XFact is (X/LastPos), write(X), write('XChunks :'), write(XFact), nl,
    %% has anything managed to reduce at this level? IF NOT, END
    X>0,
    prune(L,XFact,ALTER),
    L1 is L+1,
    retractall(perlevel(_)),
    assert(perlevel(0)),
    !, % CUT, this level is done
    L1 < MAX,
    nextlevel(L1,MAX).


%% \subsection{pruning}
%% use this if you want no pruning ever, for developping
%prune(_,XFact,ALTER):- !. %% no pruning at all


%% mild pruning
prune(L,XFact,ALTER) :-
    XFact < 100,
    write('fixed pruning keep '),
    write(ALTER), nl,
    %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    scores(Gfrom,Fto,L,_,_Score),
    findall((Score,ID), scores(Gfrom,Fto,_,ID,Score),List),
    length(List,Len),
    (Len =< ALTER -> fail ;
      (sort(List,SList),
       Till is Len-(ALTER),  %% fixed beam length
       prunechart(0,Till,SList),
       fail)).

%% aggressive pruning
prune(L,XFact,ALTER) :-
    XFact >= 100,
    write('variable pruning at '),
    Div is (XFact/8), Div > 0, write(Div), nl,
   %% foreach stretch A-Z : discard low prob-half, if there are at least 3 possibilities
    scores(Gfrom,Fto,L,_,_Score),
    findall((Score,ID), scores(Gfrom,Fto,_,ID,Score),List),
    length(List,Len),
    (Len =< ALTER -> fail ;
      (sort(List,SList),
       Till is Len-(Len/Div), %% variable beam length
       Till > 0,
       write(Till), write('/'),write(Len), nl, %% variable beam length
       prunechart(0,Till,SList),
       fail)).

prune(_,_,_) :- !.

%% prunechart: removes as many chart items as told. stops at Till.
prunechart(_,_,[]). %eol

prunechart(C,Till,[(_Score,ID)|RList]) :-
    C < Till, !,
    retract(scores(_,_,_,ID,_)),
    retract(chart(ID,_,_,_,_,_,_,_,_,_)),
    C1 is C+1,
    prunechart(C1,Till,RList).

prunechart(_,_,_) :- !.


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

    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Driver Predicates:

%read from file
go_textual(File) :-
    abolish(perlevel/1),
    retractall(sentno(_)), assert(sentno(0)),
    open(File, read, Stream,[encoding(utf8)]), 
    set_input(Stream),
    collect_sents(no-end),
    seen.

go_textual(File, OutFile) :-
    abolish(perlevel/1),
    retractall(sentno(_)), assert(sentno(0)),
    open(File, read, Stream,[encoding(utf8)]), 
    set_input(Stream),
    open(OutFile, write, OutStream,[encoding(utf8)]), 
    set_output(OutStream),
    collect_sents(no-end),
    !,
    seen,
    close(OutStream).

%read from stdin
go_textual :-
    abolish(perlevel/1),
    retractall(sentno(_)), assert(sentno(0)),
    collect_sents(no-end),
    seen.


%% \begin{Input/Output}

%% read in sentence word by word
collect_sents(end-of-file).
collect_sents(no-end) :-
    garbage_collect,
    (trim_stacks->true;true),
    retractall(statschart(_,_,_,_,_,_,_,_)),
    collect_sent([],Sent,EOF,1),
    retractall(scores(_,_,_,_,_)),
    retractall(chart(_,_,_,_,_,_,_,_,_,_)),
    retractall(tops_chart(_,_,_,_)),
    retractall(lastpos(_)),
    retractall(inccount(_)),
    assert(inccount(0)),
    %write('NEW SENTENCE :: =============================='), nl,
    writeq(sent(Sent)), write('.'), nl,
    assert(lastpos(1)),
    findall(Sent,sparse([],Sent,[1],1,[],2),_),  %% this prints all solutions
    % listing(chart), %%%%%%%%%%%%%%%%%%%%%%% debugging
    !, %% no backtracking into parser
    pathfinder,
    write('%%END_OF_SENTENCE'),nl,nl,
    flush_output,
    !, collect_sents(EOF). % cut, this sentence is done

collect_sents(fail) :- 
    write('NO COMPLETE STRUCTURE'), nl,
    collect_sents(_).


collect_sent(Acc,Sent,EOF,Position) :-
    read_term(T,[syntax_errors(dec10)]),
    process_sent(T,Acc,Sent,EOF,Position).

%%%%%%%%%%%%%%%%%%%%%%
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


%% read in an individual chunk, recursively
process_sent(w(Head,HeadTag,Chunk,C),Acc,Sent,no-end,Position) :-
    % T == w(Head,HeadTag,Chunk,C),
    append(Acc,[[Head,HeadTag,Chunk,C]],NewAcc),
     Position1 is Position+1,
    collect_sent(NewAcc,Sent,no-end,Position1).


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
    inc(Count),
    assertz(tops_chart(FinalScore,MC,1-Z,Strucs)),
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
    ((Struc =.. [Head,_,Rel,Dep], name(Rel,[45,62|NRel])) -> Dir='(->)';         %% dep found TO THE RIGHT -> =[45,62]
     (Struc =.. [Head,Dep,Rel,_], name(Rel,[60,45|NRel]),    Dir='(<-)')),       %% dep found TO THE LEFT  <- =[60,45]
    reverse(NRel,RNRel), append([_,_],RRel,RNRel),
    reverse(RRel,RRRel), name(MyRel,RRRel),
    createMorphOutput(Struc,Dep,MyRel),
    process_struc(Dep,HeadOfDep,HeadofDepDep),         %% Rel(Head,HeadOfDep).
    createRelOutput(Head,HeadOfDep,MyRel),
    % Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir], awriteq(Res), write('.'), nl, assert(Res).
    (outputformat(raw) -> (
      statschart(Head,HeadOfDep,MyRel,_,_,_,_,Prob),
      Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir, Prob], awriteq(Res), write('.'), nl) ; true).


process_struc(Struc,Head,HeadOfDep) :-
    ((Struc =.. [Head,Chunk,Rel,Dep|Rest], name(Rel,[45,62|NRel])) -> Dir='(->)';    %% dep found TO THE RIGHT, but more deps to follow
     (Struc =.. [Head,Dep,Rel,Chunk|Rest], name(Rel,[60,45|NRel]),    Dir='(<-)')),  %% dep found TO THE LEFT , but more deps to follow
    reverse(NRel,RNRel), append([_,_],RRel,RNRel),
    reverse(RRel,RRRel), name(MyRel,RRRel),
    createMorphOutput(Struc,Dep,MyRel),
    process_struc(Dep,HeadOfDep,HeadofDepDep),        %% Rel(Head,HeadOfDep)
    createRelOutput(Head,HeadOfDep,MyRel),
    (outputformat(raw) -> (
      statschart(Head,HeadOfDep,MyRel,_,_,_,_,Prob),
      Res =.. [MyRel,Head,HeadOfDep,HeadofDepDep,Dir,Prob], awriteq(Res), write('.'), nl) ; true),
    MoreDeps =.. ([Head,Chunk|Rest]),
    process_struc(MoreDeps,_,_),
    !.


%check grammar for (language-specific) rules.
% createMorphOutput(_,_,_) :- !.
% createRelOutput(_,_,_) :- !.

awriteq(Res) :- writeq(Res), Res =.. ResList, assert(sentrel(ResList)).


%%%%longpathfinder(From-To,Missing,Score,Struc).
longpathfinder(Z-Z,M-M,Sc-Sc,S-S) :- !. 
longpathfinder(A-Z,MIn-MOut,ScIn-ScOut,SIn-SOut) :-
  MaxLen is Z-A,
  findlongest(MaxLen,A-Z,A-I-J-Z,MIn-MMid1,ScIn-ScMid1,SIn-SMid1), % find longest chart entry
  (I=A -> (I1 is A, MMid2=MMid1) ; (I1 is I-1, (I1=A -> MMid2=[A|MMid1];MMid2=MMid1) )),
  longpathfinder(A-I1,MMid2-MMid3,ScMid1-ScMid2,SMid1-SMid2),      % left  of found
  (J=Z -> (J1 is Z, MMid4=MMid3) ; (J1 is J+1, (J1=Z -> MMid4=[Z|MMid3];MMid4=MMid3) )),
  longpathfinder(J1-Z,MMid4-MOut,ScMid2-ScOut,SMid2-SOut).         % right of found

%findlongest(Len,From-To,From-ViaFrom-ViaTo-To,Missing,Score).
findlongest(0,A-Z,A-A-Z-Z,_MIn-[A-Z],S-S,Sc-Sc) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%SWI VERSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findlongest(MaxLen,A-Z,A-I-J-Z,MIn-MOut,ScIn-ScOut,SIn-SOut) :-
  (chart(_,I,J,[_,CScore,_,MaxLen],_,_,_,CStruc,_,_), \+ I=J, I>=A, J=<Z, append(SIn,[CStruc],SOut), ScOut is ScIn*CScore, MOut=MIn)
  *-> true;
     (NewMaxLen is MaxLen-1,findlongest(NewMaxLen,A-Z,A-I-J-Z,MIn-MOut,ScIn-ScOut,SIn-SOut)).
