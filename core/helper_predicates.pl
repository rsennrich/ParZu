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

% control verbs; if possible, secondary edge is created
% lists are probably incomplete

%subject is subject of infinitive/past participle clause (er bleibt liegen)
control(subj,'ablehnen') :- !.
control(subj,'anbieten') :- !.
control(subj,'anfangen') :- !.
control(subj,'ankündigen') :- !.
control(subj,'aufhören') :- !.
control(subj,'bleiben') :- !.
control(subj,'beabsichtigen') :- !.
control(subj,'begehren') :- !.
control(subj,'beginnen') :- !.
control(subj,'beschließen') :- !.
control(subj,'beschliessen') :- !.
control(subj,'brauchen') :- !.
control(subj,'denken') :- !.
control(subj,'drohen') :- !.
control(subj,'entscheiden') :- !.
control(subj,'entschließen') :- !.
control(subj,'entschliessen') :- !.
control(subj,'erklären') :- !.
control(subj,'fürchten') :- !.
control(subj,'gedenken') :- !.
control(subj,'gehen') :- !.
control(subj,'glauben') :- !.
control(subj,'haben') :- !.
control(subj,'hoffen') :- !.
control(subj,'lernen') :- !.
control(subj,'planen') :- !.
control(subj,'pflegen') :- !.
control(subj,'schaffen') :- !.
control(subj,'scheinen') :- !.
control(subj,'tun') :- !.
control(subj,'verlangen') :- !.
control(subj,'vermeiden') :- !.
control(subj,'vermögen') :- !.
control(subj,'versäumen') :- !.
control(subj,'verstehen') :- !.
control(subj,'versprechen') :- !.
control(subj,'versuchen') :- !.
control(subj,'vorhaben') :- !.
control(subj,'vorschlagen') :- !. % I guess this is ambiguous subj/objd
control(subj,'wagen') :- !.
control(subj,'weigern') :- !.
control(subj,'wissen') :- !.

%accusative object is subject of infinitive/past participle clause (er hört mich kommen)
control(obja,'auffordern') :- !.
control(obja,'beauftragen') :- !.
control(obja,'beschuldigen') :- !.
control(obja,'bekommen') :- !.
control(obja,'bitten') :- !.
control(obja,'erhalten') :- !.
control(obja,'ermuntern') :- !.
control(obja,'fordern') :- !.
control(obja,'heißen') :- !.
control(obja,'heissen') :- !.
control(obja,'hören') :- !.
control(obja,'kriegen') :- !.
control(obja,'lassen') :- !.
control(obja,'lehren') :- !.
control(obja,'machen') :- !.
control(obja,'reizen') :- !.
control(obja,'sehen') :- !.
control(obja,'überreden') :- !.
control(obja,'verpflichten') :- !.
control(obja,'zwingen') :- !.

%dative object is subject of infinitive/past participle clause (er hilft mir auf(zu)stehen)
control(objd,'empfehlen') :- !.
control(objd,'erlauben') :- !.
control(objd,'ermöglichen') :- !.
control(objd,'geben') :- !.
control(objd,'helfen') :- !.
control(objd,'gewähren') :- !.
control(objd,'gebieten') :- !.
control(objd,'gefallen') :- !.
control(objd,'gelingen') :- !.
control(objd,'gönnen') :- !.
control(objd,'raten') :- !.
control(objd,'wünschen') :- !.
control(objd,'verbieten') :- !.
control(objd,'vorwerfen') :- !.


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