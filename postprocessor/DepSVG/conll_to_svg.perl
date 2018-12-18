# Converts CoNLL-formatted data into SVG for visualization.
# Author: Kaarel Kaljurand

# Usage:
#
# perl -I ../src/ conll_to_svg.perl --dir img < treebank.tsv
#
# As a result, a directory 'img' is created into the current directory and
# the SVG image files (1.svg, 2.svg, ...) are created into this new directory.

use strict;
use warnings;
use Getopt::Long;
use DepSVG;
use List::Util qw[min max];

my $dir = ".";
my $verbosity = 0;
my $help = "";
my $stdout = "";
my $version = "";

my $getopt_result = GetOptions(
	"dir=s"        => \$dir,
	"verbosity=i"  => \$verbosity,
	"help"         => \$help,
        "stdout"       => \$stdout,
	"version"      => \$version
);

if($version) { &show_version(); exit; }
if(!$getopt_result || $help) { &show_help(); exit; }

# Datastructures holding all the required information gathered from the input.
my $properties = {};
my $relations = {};

my $sentstart = 1;
my $linecount = 0;
my $debug = 0;
my $sid = 1;


print "Parsing input data... " if $verbosity > 0;

while(<STDIN>) {

	chomp;

	$linecount++;

	# Sentences are separated by an empty line.
	if(/^$/) {
		$sentstart = 1;
		&sentences_to_svg($properties, $relations, $sid);
		$sid++;
		$properties = {};
		$relations = {};
		next;
	}

	# If we are in the sentence...
	if($sentstart) {

		my ($loc, $token, $lemma, $tag, $etag, $morph, $head, $type, $nhead, $ntype) = split "\t+";

		# BUG: do more serious error checking
		if(!defined($head)) {
			warn "Syntax error in corpus on line: $linecount\n";
			next;
		}

		$properties->{$loc}->{"LOC"} = $loc;
		$properties->{$loc}->{"token"} = $token;
		$properties->{$loc}->{"lemma"} = $lemma;
		$properties->{$loc}->{"tag"} = $tag;
		$properties->{$loc}->{"etag"} = $etag;

		### BUG: comment back in if you want to see the morph
		$properties->{$loc}->{"morph"} = $morph;

		$relations->{$head}->{$loc}->{$type} = 1;

		$ntype =~ s/\s+$//;
		if($ntype ne "_" && $nhead ne "_" && ($type ne $ntype || $head != $nhead)) {
			$relations->{$nhead}->{$loc}->{$ntype} = 2;
		}
	}
}
exit;


###
# This is where we access the DepSVG library. &get_svg() takes the following arguments:
# 1. A complex hash containing the properties of the nodes.
# 2. A complex hash containing the dependency relations between the nodes.
# 3. An array of property types which determines the order in which the properties are printed.
# 4. (Set to 0 for the time being.)
# 5. Height of the SVG image.
# 6. Width of the SVG image.
# 7. A string to be output along with an error message if the graph contains a loop.
###
sub sentences_to_svg
{
	my $properties = shift;
	my $relations = shift;
	my $sid = shift;

	my $svg = &get_svg($properties, $relations,
		["token", "tag", "etag", "lemma", "morph", "LOC"],
		0, 1000, 1000, $sid);	


	&output_sentence($dir, $sid, $svg);

}


###
#
###
sub output_sentence
{
	my $dir = shift;
	my $sid = shift;
	my $svg = shift;
	my $on = 0;
	my $max_y = 0;
        my $filename = "";

        if (!$stdout) {
	    mkdir $dir;

	    $filename = $dir . "/" . $sid . "." . "svg";

	    open(STDOUT, "> $filename") or die "conll_to_svg.perl: fatal error: open $filename: $!\n";
        }

	my @lines = split /\n/, $svg;
		foreach my $line (@lines) {
			#Filter out some unwanted lines
			$on=1;
			if ($line =~ m/^<path d=\'M0 0/) {$on=0};
			if ($line =~ m/root<\/text>/) {$on=0};
			if ($line =~ m/root <\/text>/) {$on=0};
			if ($line =~ m/y='(\d+)'/) {$max_y = max($1,$max_y)};
                        # link to previous/next svg
			if ($line =~ m/<\/svg>/ && !$stdout) {
				$max_y = 20;
				$line = "<a xlink:href='" . ($sid-1) . ".svg'><text stroke-width='1px' stroke-linecap='butt' font-family='Arial, Helvetica, sans-serif' font-size='12px' word-spacing='0px' letter-spacing='0px' x=\"0\" y=\"$max_y\">Previous</text></a>\n<a xlink:href='" . ($sid+1) . ".svg'><text stroke-width='1px' stroke-linecap='butt' font-family='Arial, Helvetica, sans-serif' font-size='12px' word-spacing='0px' letter-spacing='0px' x=\"60\" y=\"$max_y\">Next</text></a>\n</svg>"
				}
			# Print those lines that aren't disabled
			if ($on==1) {print STDOUT "$line \n"};
				}

        if (!$stdout) {
            close STDOUT or die "conll_to_svg.perl: fatal error: close $filename: $!\n";
        }
}


###
# Output the version number
###
sub show_version
{
print <<EOF;
conll_to_svg.perl, ver 0.03 (2008-05-18)
Kaarel Kaljurand (kaljurand\@gmail.com)
EOF
}


###
# Output the help message
###
sub show_help
{
print <<EOF;
usage: conll_to_svg.perl OPTION...
OPTIONS:
	--dir=<directory name>  (where the SVG is saved) (defaults to .)
        --stdout: write to stdout (instead of file)
	--verbosity=<integer>	(defaults to 0)
	--version: print version information
	--help: this help message
EOF
}
