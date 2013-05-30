%morphisto-style APPRART
splitappr(am,an,_) :- !.
splitappr(ans,an,_) :- !.
splitappr(aufs,auf,_) :- !.
splitappr(beim,bei,_) :- !.
splitappr(durchs,durch,_) :- !.
splitappr(im,in,_) :- !.
splitappr(ins,in,_) :- !.
splitappr(übers,über,_) :- !.
splitappr(vom,von,_) :- !.
splitappr(zum,zu,_) :- !.
splitappr(zur,zu,_) :- !.


%gertwol-style APPRART
%'in-das' -> 'in' + 'das'
splitappr(WordI,Word,I) :-
    atomic(WordI),
    sub_atom(WordI,Before,1,After,'-'),
    sub_atom(WordI,0,Before,_,Word), 
    Before1 is Before+1,
    sub_atom(WordI,Before1,After,_,Iaaa),
    name(Iaaa,Iaa), name(I,Iaa), !.

splitappr(WordI,WordI,_) :- !.



%some adverbial pronouns are placeholders for prepositional phrases (darin, dafür)
%get corresponding preposition here to do statistics with it.
derive_prep_from_pav(PAV,Prep) :-
        sub_atom(PAV,0,3,NewLen,'dar'),
        sub_atom(PAV,3,NewLen,0,Prep), !.


derive_prep_from_pav(PAV,Prep) :-
        sub_atom(PAV,0,2,NewLen,'da'),
        sub_atom(PAV,2,NewLen,0,Prep), !.

derive_prep_from_pav(PAV,Prep) :-
        sub_atom(PAV,0,3,NewLen,'wor'),
        NewLen > 0,
        sub_atom(PAV,3,NewLen,0,Prep), !.

derive_prep_from_pav(PAV,Prep) :-
        sub_atom(PAV,0,2,NewLen,'wo'),
        NewLen > 0,
        sub_atom(PAV,2,NewLen,0,Prep), !.


verbtag('v').
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

nountag('nn').
nametag('ne').
nametag('fm').