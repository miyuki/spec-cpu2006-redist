#!/usr/bin/perl -w
use strict;
use warnings;
use Test::More tests => 18;

use MIME::QuotedPrint qw(decode_qp);
use MIME::WordDecoder;

my $mwd = (new MIME::WordDecoder::ISO_8859 1);
{
    local($/) = '';
    open WORDS, "<testin/words.txt" or die "open: $!";
    while (<WORDS>) {
	s{\A\s+|\s+\Z}{}g;    # trim

	my ($isgood, $expect, $enc) = split /\n/, $_, 3;

	# Create the expected data
	$expect = eval $expect;

	my $dec = $mwd->decode($enc);
	if( $isgood eq 'GOOD' ) {
		ok( ! $@, 'No exceptions');
		is( $dec, $expect, "$enc produced correct output");
	} else {
		ok( $@, 'Got an exception as expected');
	}

    }
    close WORDS;
}    
