# This module is part of DepSVG.
# Copyright 2008 Kaarel Kaljurand, University of Zurich
#
# Author: Kaarel Kaljurand
# Version: 2008-05-18
#
# A library to handle a representation of a dependency structure
# (which is basically a graph).

package DepUtils;

use strict;
use warnings;

BEGIN {
        use Exporter ();
        our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

        $VERSION = 0.8;

        @ISA = qw(Exporter);
        @EXPORT = qw(&strip_space &get_props &get_dep2heads &get_head2deps &get_depths &get_deepest_depth);
        %EXPORT_TAGS = ();
        @EXPORT_OK = ();
}

END {}

###
#
###
sub print_rels
{
	my $r = shift;

	foreach my $i (keys %{$r}) {
		foreach my $j (keys %{$r->{$i}}) {
			foreach my $k (keys %{$r->{$i}->{$j}}) {
				print $i, "\t", $j, "\t", $k, "\n";
			}
		}
	}
}

###
#
###
sub print_set_of_sets
{
	my $set = shift;

	foreach my $setel (keys %{$set}) {
		my $str = join " ", keys %{$set->{$setel}};
		print $setel, "\t", $str, "\n";
	}
}

###
#
###
sub print_depths
{
	my $d = shift;

	foreach my $i (keys %{$d}) {
		print $i, "\t", $d->{$i}, "\n";
	}
}

###
#
###
sub get_dep2heads
{
	my $r = shift;
	my $h = {};

	foreach my $head (keys %{$r}) {
		foreach my $dep (keys %{$r->{$head}}) {
			$h->{$dep}->{$head} = 1;
		}
	}
	return $h;
}

###
#
###
sub get_head2deps
{
	my $r = shift;
	my $h = {};

	foreach my $head (keys %{$r}) {
		foreach my $dep (keys %{$r->{$head}}) {
			$h->{$head}->{$dep} = 1;
		}
	}
	return $h;
}


###
#
###
sub get_deepest_depth 
{
	my $d = shift;
	my $max = 0;

	foreach my $i (keys %{$d}) {
		$max = $d->{$i} if $max < $d->{$i};
	}

	return $max;
}

###
#
###
sub get_props
{
	my $w = shift;

	my $props = {};

	foreach my $i (keys %{$w}) {
		foreach my $j (keys %{$w->{$i}}) {
			$props->{$j} = 1;
		}
	}

	return $props;
}

####
# 
####
sub get_depths
{
	my $h = shift;
	my $tag = shift;
	my $d = {};

	foreach my $i (keys %{$h}) {
		$d->{$i} = &get_maxdepth($h, $i, { $i => 1}, $tag);
	}

	return $d;
}

####
#
####
sub get_maxdepth 
{
	my $h = shift;
	my $i = shift;
	my $seen = shift;
	my $tag = shift;

	my $max = 0;

	foreach my $j (keys %{$h->{$i}}) {

		# Check for repeated node to avoid infinite loops
		if(defined $seen->{$j}) {
			warn "depparse: warning: cycle in graph: node $j ($tag)\n";
		}
		else {
			my $md = &get_maxdepth($h, $j, { %{$seen}, $j => 1 }, $tag) + 1;
			$max = $md if $max < $md;
		}

	}

	return $max;
}

###
#
###
sub strip_space
{
	$_ = shift;

	s/^\s+//;
	s/\s+$//;

	return $_;
}

1;
