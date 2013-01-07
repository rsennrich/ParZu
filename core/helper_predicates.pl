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
modallike('spüren') :- !.
modallike('tun') :- !.

% control verbs; if possible, secondary edge is created
% lists are probably incomplete

%subject is subject of infinitive/past participle clause (er bleibt liegen)
control(subj,'ablehnen') :- !.
control(subj,'abmühen') :- !.
control(subj,'anbieten') :- !.
control(subj,'androhen') :- !.
control(subj,'anfangen') :- !.
control(subj,'angewöhnen') :- !.
control(subj,'ankündigen') :- !.
control(subj,'anschicken') :- !.
control(subj,'ansehen') :- !.
control(subj,'aufhören') :- !.
control(subj,'aufraffen') :- !.
control(subj,'ausnutzen') :- !.
control(subj,'beabsichtigen') :- !.
control(subj,'beanspruchen') :- !.
control(subj,'beeilen') :- !.
control(subj,'befinden') :- !.
control(subj,'befürchten') :- !.
control(subj,'begehren') :- !.
control(subj,'beginnen') :- !.
control(subj,'behaupten') :- !.
control(subj,'benutzen') :- !.
control(subj,'bereiterklären') :- !.
control(subj,'beschliessen') :- !.
control(subj,'beschließen') :- !.
control(subj,'beteuern') :- !.
control(subj,'betonen') :- !.
control(subj,'bewerkstelligen') :- !.
control(subj,'bleiben') :- !.
control(subj,'brauchen') :- !.
control(subj,'denken') :- !.
control(subj,'drohen') :- !.
control(subj,'einigen') :- !.
control(subj,'einwilligen') :- !.
control(subj,'entscheiden') :- !.
control(subj,'entschliessen') :- !.
control(subj,'entschließen') :- !.
control(subj,'erklären') :- !.
control(subj,'erwägen') :- !.
control(subj,'fahren') :- !.
control(subj,'fortfahren') :- !.
control(subj,'fühlen') :- !.
control(subj,'fürchten') :- !.
control(subj,'gedenken') :- !.
control(subj,'gehen') :- !.
control(subj,'getrauen') :- !.
control(subj,'glauben') :- !.
control(subj,'haben') :- !.
control(subj,'halten') :- !.
control(subj,'hoffen') :- !.
control(subj,'kämpfen') :- !.
control(subj,'kommen') :- !.
control(subj,'leisten') :- !.
control(subj,'lernen') :- !.
control(subj,'lieben') :- !.
control(subj,'meinen') :- !.
control(subj,'neigen') :- !.
control(subj,'pflegen') :- !.
control(subj,'planen') :- !.
control(subj,'riskieren') :- !.
control(subj,'schaffen') :- !.
control(subj,'scheinen') :- !.
control(subj,'scheuen') :- !.
control(subj,'schwören') :- !.
control(subj,'sträuben') :- !.
control(subj,'streben') :- !.
control(subj,'suchen') :- !.
control(subj,'trauen') :- !.
control(subj,'tun') :- !.
control(subj,'übereinkommen') :- !.
control(subj,'überlegen') :- !.
control(subj,'umhinkommen') :- !.
control(subj,'vereinbaren') :- !.
control(subj,'vergessen') :- !.
control(subj,'verlangen') :- !.
control(subj,'vermeiden') :- !.
control(subj,'vermögen') :- !.
control(subj,'versäumen') :- !.
control(subj,'versichern') :- !.
control(subj,'versprechen') :- !.
control(subj,'verstehen') :- !.
control(subj,'versuchen') :- !.
control(subj,'vorbehalten') :- !.
control(subj,'vorgeben') :- !.
control(subj,'vorhaben') :- !.
control(subj,'vornehmen') :- !.
control(subj,'vorschlagen') :- !. % I guess this is ambiguous subj/objd
control(subj,'vorsehen') :- !.
control(subj,'vorstellen') :- !.
control(subj,'wagen') :- !.
control(subj,'weigern') :- !.
control(subj,'wissen') :- !.
control(objd,'wünschen') :- !.
control(subj,'zugeben') :- !.
control(subj,'zusagen') :- !.
control(subj,'zusichern') :- !.


%accusative object is subject of infinitive/past participle clause (er hört mich kommen)
control(obja,'anhalten') :- !.
control(obja,'anmahnen') :- !.
control(obja,'anregen') :- !.
control(obja,'auffordern') :- !.
control(obja,'aufrufen') :- !.
control(obja,'beauftragen') :- !.
control(obja,'bekommen') :- !.
control(obja,'bemühen') :- !.
control(obja,'beschäftigen') :- !.
control(obja,'beschuldigen') :- !.
control(obja,'beschwören') :- !.
control(obja,'bewegen') :- !.
control(obja,'bitten') :- !.
control(obja,'drängen') :- !.
control(obja,'erhalten') :- !.
control(obja,'ermächtigen') :- !.
control(obja,'ermahnen') :- !.
control(obja,'ermuntern') :- !.
control(obja,'ermutigen') :- !.
control(obja,'ersuchen') :- !.
control(obja,'fordern') :- !.
control(obja,'heissen') :- !.
control(obja,'heißen') :- !.
control(obja,'hindern') :- !.
control(obja,'hören') :- !.
control(obja,'instruieren') :- !.
control(obja,'kriegen') :- !.
control(obja,'lassen') :- !.
control(obja,'legitimieren') :- !.
control(obja,'lehren') :- !.
control(obja,'machen') :- !.
control(obja,'mahnen') :- !.
control(obja,'motivieren') :- !.
control(obja,'reizen') :- !.
control(obja,'sehen') :- !.
control(obja,'spüren') :- !.
control(obja,'überreden') :- !.
control(obja,'verdonnern') :- !.
control(obja,'verpflichten') :- !.
control(obja,'verurteilen') :- !.
control(obja,'warnen') :- !.
control(obja,'zwingen') :- !.


%dative object is subject of infinitive/past participle clause (er hilft mir auf(zu)stehen)
control(objd,'anordnen') :- !.
control(objd,'aufgeben') :- !.
control(objd,'auftragen') :- !.
control(objd,'befehlen') :- !.
control(objd,'beibringen') :- !.
control(objd,'empfehlen') :- !.
control(objd,'erlauben') :- !.
control(objd,'ermöglichen') :- !.
control(objd,'erschweren') :- !.
control(objd,'ersparen') :- !.
control(objd,'geben') :- !.
control(objd,'gebieten') :- !.
control(objd,'gefallen') :- !.
control(objd,'gelingen') :- !.
control(objd,'gestatten') :- !.
control(objd,'gewähren') :- !.
control(objd,'gönnen') :- !.
control(objd,'helfen') :- !.
control(objd,'raten') :- !.
control(objd,'untersagen') :- !.
control(objd,'verbieten') :- !.
control(objd,'vorschreiben') :- !.
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