% simple predicates that define groups of tags/words and are shared by several programs

:- dynamic measure/1.

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
modallike('scheinen') :- !.
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
control(subj,'wünschen') :- !.
control(subj,'zugeben') :- !.
control(subj,'zusagen') :- !.
control(subj,'zusichern') :- !.


%accusative object is subject of infinitive/past participle clause (er hört mich kommen)
control(obja,'anhalten') :- !.
control(obja,'anmahnen') :- !.
control(obja,'anregen') :- !.
control(obja,'anweisen') :- !.
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
zeitcand('Nacht').
zeitcand('Woche').
zeitcand('Monat').
zeitcand('Jahr').
zeitcand('Jahrzehnt').
zeitcand('Jahrhundert').
zeitcand('Jahrtausend').

zeitcand('Zwanziger').
zeitcand('Dreissiger').
zeitcand('Vierziger').
zeitcand('Fünfziger').
zeitcand('Siebziger').
zeitcand('Achtziger').
zeitcand('Neunziger').

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
zeitcand('Weile').
zeitcand('Augenblick').
zeitcand('Moment').


%nouns that can have adverbial (temporal) meaning if they are the head of another noun phrase with temporal meaning:
%Anfang 1995
%Ende des Jahres
zeit_like_anfang('Anfang').
zeit_like_anfang('Mitte').
zeit_like_anfang('Ende').

%fixed expressions that have adverbial meaning in genitive case: "Eines Tages", "des Öfteren" usw.
zeitgen('Tag', 'eine_ART', '<-det<-').
zeitgen('Tag', 'ein_ART', '<-det<-').
zeitgen('Abend', 'eine_ART', '<-det<-').
zeitgen('Abend', 'ein_ART', '<-det<-').
zeitgen('Morgen', 'eine_ART', '<-det<-').
zeitgen('Morgen', 'ein_ART', '<-det<-').
zeitgen('weit', 'die_ART', '<-det<-').
zeitgen('weit', 'der_ART', '<-det<-').
zeitgen('weit', 'das_ART', '<-det<-').
zeitgen('oft', 'die_ART', '<-det<-').
zeitgen('oft', 'der_ART', '<-det<-').
zeitgen('oft', 'das_ART', '<-det<-').
zeitgen('Öfteren', 'die_ART', '<-det<-').
zeitgen('Öfteren', 'der_ART', '<-det<-').
zeitgen('Öfteren', 'das_ART', '<-det<-').
zeitgen('Erachten', 'mein_PPOSAT', '<-det<-').
zeitgen('Erachten', 'meine_PPOSAT', '<-det<-').
zeitgen('erachten', 'mein_PPOSAT', '<-det<-').
zeitgen('erachten', 'meine_PPOSAT', '<-det<-').
zeitgen('Gewissen', 'gut_ADJA', '<-attr<-').
zeitgen('Ende', 'letzte_ADJA', '<-attr<-').
zeitgen('Ende', 'letzt_ADJA', '<-attr<-').

month('Januar').
month('Februar').
month('März').
month('April').
month('Mai').
month('Juni').
month('Juli').
month('August').
month('September').
month('Oktober').
month('November').
month('Dezember').


%override morphological analysis for some units of measurement: they may remain uninflected ("10 Grad" instead of "10 Grade").

%SI-Units (except units of time, which are inflected):
measure_stem('Meter').
measure_stem('Gramm').
measure_stem('Ampere').
measure_stem('Kelvin').
measure_stem('Mol').
measure_stem('Candela').

measure_stem('Radiant').
measure_stem('Steradiant').
measure_stem('Hertz').
measure_stem('Newton').
measure_stem('Pascal').
measure_stem('Joule').
measure_stem('Watt').
measure_stem('Coulomb').
measure_stem('Volt').
measure_stem('Farad').
measure_stem('Weber').
measure_stem('Tesla').
measure_stem('Henry').
measure_stem('Ohm').
measure_stem('Siemens').
measure_stem('Grad').
measure_stem('Lumen').
measure_stem('Lux').
measure_stem('Becquerel').
measure_stem('Gray').
measure_stem('Dezibel').

measure_stem('Liter').

measure_stem('Byte').
measure_stem('Hertz').

%with Meter/Byte/Hertz/Pascal/Liter
measure_prefix('').
measure_prefix('Deka').
measure_prefix('Hekto').
measure_prefix('Kilo').
measure_prefix('Mega').
measure_prefix('Giga').
measure_prefix('Tera').
measure_prefix('Peta').
measure_prefix('Exa').
measure_prefix('Zetta').
measure_prefix('Yotta').

measure_prefix('Dezi').
measure_prefix('Zenti').
measure_prefix('Centi').
measure_prefix('Milli').
measure_prefix('Mikro').
measure_prefix('Nano').
measure_prefix('Piko').
measure_prefix('Femto').
measure_prefix('Atto').
measure_prefix('Zepto').
measure_prefix('Yokto').

:- measure_prefix(Prefix), measure_stem(Stem), (Prefix \= ''->downcase_atom(Stem,LowerStem);LowerStem=Stem), atom_concat(Prefix,LowerStem,Measure), assert(measure(Measure)),fail;true.
:- measure_prefix(Prefix), downcase_atom(Prefix,LowerPrefix), atom_concat('Quadrat',LowerPrefix,FullPrefix), atom_concat(FullPrefix,'meter',Measure), assert(measure(Measure)),fail;true.
:- measure_prefix(Prefix), downcase_atom(Prefix,LowerPrefix), atom_concat('Kubik',LowerPrefix,FullPrefix), atom_concat(FullPrefix,'meter',Measure), assert(measure(Measure)),fail;true.

measure('Fuss').
measure('Fuß').
measure('Zoll').
measure('Hektar').
measure('Schritt').

measure('Fass').
measure('Glas').
measure('Mann').

measure('Prozent').
measure('Promille').

measure('Mal').
measure('Stück').
measure('Paar').
measure('Dutzend').

% collective nouns can refer to a group of individuals, and sometimes occur with plural verb (even though prescriptive linguists might consider this ungrammatical)
% unlike nouns of measure, we allow plural verb even if whole noun phrase (including article etc.) is singular
% "Eine Mehrheit der Demonstranten fordern mehr Lohn"
collective('Mehrheit').
collective('Minderheit').
collective('Anzahl').
collective('Anteil').
collective('Teil').
collective('Reihe').
collective('Dutzend').
collective('Paar').
collective('Prozent').
collective('Promille').
collective('Gruppe').
collective('Rudel').
collective('Herde').
collective('Menge').


%currencies (only uninflected ones: Franken, Rupien etc. do not need exception)
measure('Baht').
measure('Cent').
measure('Dollar').
measure('Ecu').
measure('Euro').
measure('Eurocent').
measure('Florin').
measure('Franc').
measure('Kip').
measure('Kwanza').
measure('Lek').
measure('Leu').
measure('Lira').
measure('Pfund').
measure('Mark').
measure('Pfennig').
measure('Peso').
measure('Rand').
measure('Real').
measure('Reichsmark').
measure('Rial').
measure('Riyal').
measure('Rubel').
measure('Schekel').
measure('Schilling').
measure('Won').
measure('Yen').
measure('Zloty').


% "von A nach B": treat this as a coordinated pp construction.
valid_pp_coord_start(von).
valid_pp_coord_start(aus).
valid_pp_coord_start('über').

valid_pp_coord_end(auf).
valid_pp_coord_end(bis).
valid_pp_coord_end(nach).
valid_pp_coord_end(zu).
valid_pp_coord_end('über').

%list of lemmas that may have multi-token number as apposition: phone numbers, bank accounts etc.
number_head('BLZ').
number_head('Bankleitzahl').
number_head('Kto.').
number_head('Konto').

number_head('Fax').
number_head('Tel').
number_head('Tel.').
number_head('Telefon').
number_head('Nummer').
number_head('Telefonnummer').



%prepositions that are often modified by a measurement (GRAD) ("zwei stunden nach dem Rennen")
grad_head('nach').
grad_head('vor').
grad_head('hinter').
grad_head('neben').

grad_head('über').
grad_head('überhalb').
grad_head('unter').
grad_head('unterhalb').

grad_head('nördlich').
grad_head('westlich').
grad_head('östlich').
grad_head('südlich').

%attributive pronouns
grad_head('mehr').
grad_head('weniger').


% adverbs that may indicate conjunction (even though conjunction is missing):
% er ist mal hier, mal dort.
conjunctive_adverb('aber_ADV').
conjunctive_adverb('also_ADV').
conjunctive_adverb('auch_ADV').
conjunctive_adverb('besser_ADJD').
conjunctive_adverb('bloß_ADV').
conjunctive_adverb('dann_ADV').
conjunctive_adverb('direkt_ADJD').
conjunctive_adverb('eher_ADV').
conjunctive_adverb('einfach_ADV').
conjunctive_adverb('ermäßigt_ADJD').
conjunctive_adverb('etwa_ADV').
conjunctive_adverb('etwas_ADV').
conjunctive_adverb('extrem_ADJD').
conjunctive_adverb('fast_ADV').
conjunctive_adverb('ganz_ADV').
conjunctive_adverb('gar_ADV').
conjunctive_adverb('geradezu_ADV').
conjunctive_adverb('gänzlich_ADJD').
conjunctive_adverb('halb_ADJD').
conjunctive_adverb('hart_ADJD').
conjunctive_adverb('heute_ADV').
conjunctive_adverb('hin_ADV').
conjunctive_adverb('immer_ADV').
conjunctive_adverb('ja_ADV').
conjunctive_adverb('jedoch_ADV').
conjunctive_adverb('kaum_ADV').
conjunctive_adverb('kurz_ADJD').
conjunctive_adverb('lediglich_ADV').
conjunctive_adverb('leicht_ADJD').
conjunctive_adverb('mal_ADV').
conjunctive_adverb('manchmal_ADV').
conjunctive_adverb('miteinander_ADV').
conjunctive_adverb('nicht_PTKNEG').
conjunctive_adverb('noch_ADV').
conjunctive_adverb('nur_ADV').
conjunctive_adverb('nämlich_ADV').
conjunctive_adverb('oft_ADV').
conjunctive_adverb('ohnehin_ADV').
conjunctive_adverb('perfekt_ADJD').
conjunctive_adverb('politisch_ADJD').
conjunctive_adverb('schon_ADV').
conjunctive_adverb('schwer_ADJD').
conjunctive_adverb('sehr_ADV').
conjunctive_adverb('selbst_ADV').
conjunctive_adverb('so_ADV').
conjunctive_adverb('sogar_ADV').
conjunctive_adverb('sozial_ADJD').
conjunctive_adverb('später_ADJD').
conjunctive_adverb('spätestens_ADV').
conjunctive_adverb('teilweise_ADV').
conjunctive_adverb('viel_ADV').
conjunctive_adverb('weniger_ADV').
conjunctive_adverb('zu_PTKA').
conjunctive_adverb('zuletzt_ADV').
conjunctive_adverb('äußerst_ADV').
