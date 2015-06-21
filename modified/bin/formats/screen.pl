#
#  screen.pl - produces ASCII table on stdout
#  Copyright 2004-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Cloyce D. Spradling
#
# $Id: screen.pl 6400 2011-03-31 18:21:54Z cloyce $

use strict;

use vars qw($name $extension $synonyms);

$name      = 'Screen';
$extension = undef;
$synonyms  = { map { lc($_) => 1 } ($name, qw(scr display disp terminal term)) };

$Spec::Format::screen::non_default = 1;       # You must ask for it by name
my $screen_version = '$LastChangedRevision: 6400 $ '; # Make emacs happier
$screen_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'screen.pl'} = $screen_version;

sub format {
    my($me, $r, $fn) = @_;

    my @nc = ::allof($r->{'nc'});
    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));

    ::Log(0, "\n\n".join("\n", Spec::Format::asc::screen_format($me, $r, $fn, 0, $invalid, \@nc))."\n\n");

    return ([], []);
}
