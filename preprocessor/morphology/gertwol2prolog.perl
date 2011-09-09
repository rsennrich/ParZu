#!/usr/bin/perl -w

# $Header: /home/ludwig7/siclemat/cvs/gertwol2prolog/gertwol2prolog.perl,v 1.14 2006/04/23 10:30:34 siclemat Exp $


{
#for each Wortanalyse WA do
#  NewToken := true;
#  for each Analysezeile Line do
#    if NewToken
#    then
#      Info := info(Line);
#      NewToken := false;
#    else
#      if OldLemma != lemma(Line)
#      then
#        print OldInfo;
#        OldInfo := info(Line);
#        OldLemma := lemma(Line);
#      else 
#        OldInfo := OldInfo ^ info(Line);
#  done
#done
my $NOUNKNOWN = 0;
my $MORPHLIST = 0;
my $NOMORPHSTRUCTURE = 0;
my %gertwollisthash;
my $ListLemma;
my $USAGE="usage: $0 [options] [file]
extract sentences from stdin (or file) to stdout
   -nounknown   # do not generate clauses for unknown word forms
   -nomorph     # remove GERTWOL word structure
";# Optionen der Befehlszeile abarbeiten
use Getopt::Long; # Standardbibliothek fuer Optionenverarbeitung laden

GetOptions
	(
	 "nounknown" => \$NOUNKNOWN,
	 "h|help" => \$HELP,
	 "nomorph" => \$NOMORPHSTRUCTURE,
	 "morphlist" => \$MORPHLIST
	 );
if ($HELP) {
  warn $USAGE;
  exit(0);
}

use locale;

local $/ = "\n\n";
# Akkumulator fuer Stilangaben
my $Pragmainfo;
# Grossschreibungsinformation
my $Capinfo;
# Akkumulator fuer morphosyntaktische Information
my $Morphinfo;
# Zustandsvariable: true, wenn neues Token eingelesen wurde
my $NewToken;

while (<>) {
  # Zustandsvariable setzen
  $NewToken = 1;
# fuehrende Leerzeilen loeschen
  s/^\n+//;

  # Token verarbeiten
  # Token matchen
  next unless ( s/^"<([^>]+)>"\n//);
  # Grossschreibung wiederherstellen
  $token = $1;
  $token =~ s/\*ö/Ö/g;	$token =~ s/\*ä/Ä/g;	$token =~ s/\*ü/Ü/g;
  $token =~ s/\*([^\s])/uc($1)/eg;
  # Einfache Hochkommata schuetzen
  $token =~ s/\'/\'\'/g;
  # Backslashes schuetzen (Fugentrenner in Gertwol???)
  $token =~ s/\\/\\\\/g;
  $token = "'$token'";
  # Unbekannte Woerter
#  warn "Gertwoloutput <$_>\n";
  if (!/\t/) {
	print "raw_gertwol($token, _, '<unknown>').\n" unless ($NOUNKNOWN || $MORPHLIST);
	next;
  }
  # Merkmale verarbeiten  
  while (/\t\"(.*?)\"  (.*)/gm) {
  	# Ruecksetzen von Variablen	
	$Lemma = $Features = $Pragmainfo = $Capinfo = "";
	
	$Features = $2;
	# Stern-Kleinbuchstaben in Grossbuchstaben verwandeln
	$Lemma = $1;
	$Lemma =~ s/\*ö/Ö/g;	$Lemma =~ s/\*ä/Ä/g;	$Lemma =~ s/\*ü/Ü/g;
	$Lemma =~ s/\*([^\s])/uc($1)/eg;
	if ( $NOMORPHSTRUCTURE ) {$Lemma =~ y/\\#~|//d;}
	
#	$Lemma =~ s/\\/\\\\/g;
	# Einfache Hochkommata fuer Prolog schuetzen
#	$Lemma =~ s/\'/\'\'/g;

	if($MORPHLIST){
	  $ListLemma = $Lemma;
	  $ListLemma =~ s/\'/\'\'/g;
	  $ListLemma =~ s/([#\\~|-])/','$1','/g;
	  $ListLemma =~ s/\\/\\\\/g;
	  $ListLemma = "['$ListLemma']";
	}	
	  $Lemma =~ s/\\/\\\\/g;
	# Einfache Hochkommata fuer Prolog schuetzen
	  $Lemma =~ s/\'/\'\'/g;
	  $Lemma = "'$Lemma'"; 
	
	if($MORPHLIST){
	  $gertwollisthash{$Lemma}=$ListLemma;
	  next;
	}
	# Ignorieren von Einzelzeichen, deren als Lemma "" angegeben ist
#	if ($Lemma eq ""){
#	  $OldLemma = "";
#	  next;
#	};

  # Grosschreibung von Nicht-Substantiven vermerken
	$Capinfo = ($Features =~ s/^(\* )// ? q('*') : q('NOCAP'));
	if ($Features =~ s/^SELTEN \* //) {
	  $Capinfo = q('*');
	  $Pragmainfo = 'SELTEN';
	}
	# Gertwolbug mit * A * bei Innern
	$Features =~ s/ \*//;
	#	$Capinfo = ($1 eq " *" ?  q('NOCAP') : q('*'));
	# Pragmatische Infos extrahieren
	$Pragmainfo .= $1 if ($Features =~ s/ (\d+)$//);
	$Pragmainfo .= ' SELTEN' if ($Features =~  s/ ?SELTEN( $)?//);
	$Pragmainfo .= ' GESPROCHEN' if ($Features =~  s/ ?GESPROCHEN( $)?//);
	$Pragmainfo .= ' VERALTET' if ($Features =~  s/ ?VERALTET( $)?//);
	$Pragmainfo .= ' FALSCH' if ($Features =~  s/ ?FALSCH( $)?//);
	$Pragmainfo .= ' HÖFLICH' if ($Features =~  s/ ?HÖFLICH( $)?//);
	$Pragmainfo =~ s/^ //;
	$Pragmainfo =~ s/ /','/g;
	$Pragmainfo = ($Pragmainfo ? "['$Pragmainfo']" :'[]');
	$Pragmainfo =~ s/'(\d+)'/$1/;
	#	warn "Pragmainfo: <$Pragmainfo>\n";
	
	# Morphologische Merkmale
	$Features =~ s/^ //;
	$Features =~ s# #','#g;
	  if ($NewToken) {
#		print "gertwol('$token','$OldLemma', [$AllFeatures]).\n" if ($AllFeatures); # Akkumuliertes ausdrucken
		$AllFeatures = "[['$Features'],$Pragmainfo,$Capinfo]";
		$OldLemma = $Lemma;
		$NewToken = 0;
	  } else {
		if ($OldLemma ne $Lemma) {
		  print "raw_gertwol($token,$OldLemma, [$AllFeatures]).\n" if ($AllFeatures); # Akkumuliertes ausdrucken
		  $OldLemma = $Lemma;
		  $AllFeatures = "[['$Features'],$Pragmainfo,$Capinfo]";
		} else {
		  $AllFeatures =
			($AllFeatures ?
			 $AllFeatures . ",[['$Features'],$Pragmainfo,$Capinfo]" :  
			 "[['$Features'],$Pragmainfo,$Capinfo]" );
		}
	  }
  } # End Lineanalyse
  print "raw_gertwol($token,$OldLemma, [$AllFeatures]).\n" if ($AllFeatures); # Akkumuliertes ausdrucken
} # End Wortanalyse

if($MORPHLIST){
  while ( (my $k, my $v) = each %gertwollisthash) {
	print "gertwol_ll($k,$v).\n";
  }
}
}
