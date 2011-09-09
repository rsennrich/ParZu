$on=0;
while(<>) {
	chomp;
        s/\x0d//; #MS_DOS
	if (s/^(sent\()//) {$count++; $sent="\n$1 $count, $_\n"};
	if (s/^(analyses\()//) {$ana="\n$1 $count, $_\n"};
	if (s/<\/PROLOGPREDS>//){$on=0};
	if (s/<PROLOGPREDS \d>//){$on=1 ; print "\n%% Sentence $count :\n$sent"."$ana \n"};
	s/\)\.//g; #all dots
# 	tr/[A-Z]/[a-z]/; # a few words are not decap by morpha
# 	s/\,(\d+)\,/\,\'$1\'\,/g;
	s/_//g;
	($pred,$args)= split(/\(/);
	@args= split(/\,/,$args);
        s/ +/ /g;
	if (($on==1) && ($_ ne "")) {print "$_, $count\).\n"};
}
