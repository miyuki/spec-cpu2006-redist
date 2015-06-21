#
# benchset.pm
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchset.pm 6710 2011-08-05 21:53:46Z CloyceS $
#

package Spec::Benchset;

use strict;
use UNIVERSAL qw(isa);
use vars '@ISA';

@ISA = (qw(Spec::Config));

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'benchset.pm'} = $version;

require 'benchset_common.pl';

sub results_list {
    my ($me) = @_;
    my $benchhash = $me->{'results'};
    return () if ref($benchhash) ne 'HASH';
    my @result;
    for my $tune ('base', 'peak') {
	for my $bench (sort keys %$benchhash) {
	    next if ref($benchhash->{$bench}) ne 'HASH';
	    next if !exists $benchhash->{$bench}{$tune};
            if (!exists($benchhash->{$bench}{$tune}{'data'})) {
                Log(0, "WARNING: No data for $bench:$tune\n");
                next;
            }
	    push (@result, @{$benchhash->{$bench}{$tune}{'data'}});
	}
    }
    return @result;
}

sub benchmark_results_list {
    my ($me, $bench, $tune) = @_;
    my $benchhash = $me->{'results'};
    return () unless isa($benchhash, 'HASH');
    return () unless isa($benchhash->{$bench}, 'HASH');
    return () unless isa($benchhash->{$bench}{$tune}, 'HASH');

    if (!exists($benchhash->{$bench}{$tune}{'data'})) {
	Log(0, "WARNING: No data for $bench:$tune\n");
	return ();
    }
    return @{$benchhash->{$bench}{$tune}{'data'}};
}

1;
