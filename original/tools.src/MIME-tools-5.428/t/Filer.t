#!/usr/bin/perl
use strict;
use warnings;
use Test::More tests => 13;

BEGIN {
	use_ok('MIME::Parser::Filer');
}

# Tests for CPAN ticket 6789, and others
{
	my $filer = MIME::Parser::Filer->new();

	# All of these filenames should be considered evil
	my %evil = (
		' '               => '.dat' ,
		' leading_space'  => 'leading_space.dat',
		'trailing_space ' => 'trailing_space.dat',
		'.'               => '..dat',
		'..'              => '...dat',
		'index[1].html'   => '.html',
	);

	foreach my $name (keys %evil) {
		ok( $filer->evil_filename( $name ), "$name is evil");
	}

	while( my ($evil, $clean) = each %evil ) {
		is( $filer->exorcise_filename( $evil), $clean, "$evil was exorcised");
	}

}
