% simple predicates that define groups of tags/words and are shared by several programs

%full verbs that sometimes act like modal verbs
%some may govern infinitive, some past participle, some both
%"er sieht ihn kommen"; "er will ihn bestraft sehen"
modallike('bekommen') :- !.
modallike('bleiben') :- !.
modallike('brauchen') :- !.
modallike('erhalten') :- !.
modallike('gehen') :- !.
modallike('helfen') :- !.
modallike('hören') :- !.
modallike('kriegen') :- !.
modallike('lassen') :- !.
modallike('lehren') :- !.
modallike('lernen') :- !.
modallike('machen') :- !.
modallike('sehen') :- !.
modallike('tun') :- !.



%list of nouns that can have adverbial (temporal) meaning: "Er singt jeden Tag"
zeitcand('Montag').
zeitcand('Dienstag').
zeitcand('Mittwoch').
zeitcand('Donnerstag').
zeitcand('Freitag').
zeitcand('Samstag').
zeitcand('Sonntag').

zeitcand('Januar').
zeitcand('Februar').
zeitcand('März').
zeitcand('April').
zeitcand('Mai').
zeitcand('Juni').
zeitcand('Juli').
zeitcand('August').
zeitcand('September').
zeitcand('Oktober').
zeitcand('November').
zeitcand('Dezember').

zeitcand('Sekunde').
zeitcand('Minute').
zeitcand('Stunde').
zeitcand('Tag').
zeitcand('Woche').
zeitcand('Monat').
zeitcand('Jahr').
zeitcand('Jahrzehnt').
zeitcand('Jahrhundert').
zeitcand('Jahrtausend').

zeitcand('Morgen').
zeitcand('Mittag').
zeitcand('Nachmittag').
zeitcand('Vormittag').
zeitcand('Abend').
zeitcand('Nacht').

zeitcand('Frühling').
zeitcand('Sommer').
zeitcand('Herbst').
zeitcand('Winter').

zeitcand('Quartal').
zeitcand('Trimester').
zeitcand('Semester').
zeitcand('Saison').
zeitcand('Zeit').

zeitcand('Mal').