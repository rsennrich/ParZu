$on=0;
while(<>) {
	chomp;
        s/\x0d//; #MS_DOS
	if (s/<\/PROLOGPREDS>//){$on=0; print "\n"};
	if (s/<PROLOGPREDS \d>//){$on=1};
	if (($on==1) && ($_ ne "")) {print "$_ \n"};
}
