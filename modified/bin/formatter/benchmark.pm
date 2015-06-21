#
# benchmark.pm
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchmark.pm 6710 2011-08-05 21:53:46Z CloyceS $
#

package Spec::Benchmark;

use strict;
use vars '@ISA';

@ISA = (qw(Spec::Config));

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'benchmark.pm'} = $version;

sub new {
    no strict 'refs';
    my ($class, $topdir, $config, $num, $name) = @_;
    my $me       = bless {}, $class;

    $me->{'name'}        = ${"${class}::benchname"};
    $me->{'num'}         = ${"${class}::benchnum"};

    if (!defined(${"${class}::benchlang"}) || ${"${class}::benchlang"} eq '') {
        %{$me->{'BENCHLANG'}} = %{"${class}::benchlang"};
        @{$me->{'allBENCHLANG'}}= ();
        # Fix up the benchlang lists (so that they're lists), and make the
        # full list of all benchlangs
        foreach my $exe (keys %{$me->{'BENCHLANG'}}) {
            if (ref($me->{'BENCHLANG'}->{$exe}) eq 'ARRAY') {
                push @{$me->{'allBENCHLANG'}}, @{$me->{'BENCHLANG'}->{$exe}};
            } else {
                my @langs = split(/[\s,]+/, $me->{'BENCHLANG'}->{$exe});
                $me->{'BENCHLANG'}->{$exe} = [ @langs ];
                push @{$me->{'allBENCHLANG'}}, @langs;
            }
        }
    } else {
        @{$me->{'BENCHLANG'}}= split(/[\s,]+/, ${"${class}::benchlang"});
        @{$me->{'allBENCHLANG'}}= @{$me->{'BENCHLANG'}};
    }
    if ($::lcsuite =~ /cpu(?:2006|v6)/ &&
        grep { $_ eq 'F77' } @{$me->{'allBENCHLANG'}}) {
      # SPEC CPU uses F variables for F77 codes
      push @{$me->{'allBENCHLANG'}}, 'F';
    }

    $me->{'benchmark'}   = $me->{'num'}.'.'.$me->{'name'};
    $me->{'path'}        = $topdir;
    $me->{'config'}      = $config;
    $me->{'refs'}        = [ $me, $config ];
    $me->{'result_list'} = [ ];

    return $me;
}

1;
