#
#  pdf.pl - produces PDF output
#  Copyright 1999-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author:  Christopher Chan-Nui
#
# $Id: pdf.pl 6400 2011-03-31 18:21:54Z cloyce $

use strict;
use PSPDF;

use vars qw($name $extension $synonyms $binary);

$name      = 'PDF';
$extension = 'pdf';
$synonyms  = { map { lc($_) => 1 } ($name, qw(adobe)) };
$binary    = 1;

my $version = '$LastChangedRevision: 6400 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::pdf::non_default = 1;  # You must ask for it by name
$Spec::Format::pdf::part_of_all = 1;  # Part of '-o all'
$::tools_versions{'pdf.pl'} = $version;

sub format () {
    my($me, $r, $path) = @_;
    return undef unless exists $::tools_versions{'ps.pl'};

    eval 'use PDF::API2;
          use PDF::API2::Page;
          use PDF::API2::Content;
          use PDF::API2::Annotation;
          use PDF::API2::NamedDestination;';
    if ($@) {
        main::Log(0, "ERROR: Cannot load PDF::API2 modules for PDF:\n  $@\n");
        $::pdf_ok = 0;
        return undef;
    }

    my @output = split ("\n", Spec::Format::ps::SPEC_report($r, 'PDF', $path));
    return (\@output, []);
}
