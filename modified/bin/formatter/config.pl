#
# config.pl
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: config.pl 6710 2011-08-05 21:53:46Z CloyceS $
#
package Spec::Config;

use strict;
use IO::File;
use Safe;
use File::Basename;
use UNIVERSAL qw(isa);

require 'config_common.pl';

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'config.pl'} = $version;

sub copies {
  my $me = shift;
  return 1 unless istrue($me->rate);
  my @check = qw(clcopies);
  push @check, 'copies' unless $me->tune eq 'base';
  foreach my $check (@check) {
    my $tmp = $me->accessor_nowarn($check);
    next unless defined($tmp) && $tmp ne '';
    return main::expand_ranges(split(/,+|\s+/, $tmp));
  }
  return main::expand_ranges(@{$me->copylist});
}

sub ranks {
  my $me = shift;
  my @check = qw(ranks clranks);
  foreach my $check (@check) {
    my $tmp = $me->accessor_nowarn($check);
    next unless defined($tmp) && $tmp ne '';
    return $tmp;
  }
  return $::global_config->{'ranks'};
}

1;
