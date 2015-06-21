#
#  cfgfile.pl - produces config files
#  Copyright 1999-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author: Cloyce D. Spradling
#
# $Id: cfgfile.pl 6710 2011-08-05 21:53:46Z CloyceS $

use IO::File;
use File::Basename;
use File::Spec;
use strict;

use vars qw($name $extension $synonyms);

$name      = 'config';
$extension = 'cfg';
$synonyms  = { map { lc($_) => 1 } ($name, $extension, qw(conf conffile configfile cfgfile)) };

$Spec::Format::cfgfile::non_default = 1; # You must ask for it by name
$Spec::Format::cfgfile::part_of_all = 1; # You must ask for it by name
my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'cfgfile.pl'} = $version;

sub format {
    my($me, $r, $fn) = @_;
    my $enconfig = '';
    my @output = ();
    my $written = [];

    # Assemble the rawtxtconfig lines
    if (exists($r->{'rawconfig'}) && # it should
        ref($r->{'rawconfig'}) eq 'ARRAY') { # it should
      $enconfig = join('', @{$r->{'rawconfig'}});
    } else {
      return (undef, []);
    }
    my (undef, $decodedconfig, $txtconfig) = ::decode_decompress($enconfig);

    # Decide which of the three possibilities to use.
    # Basically, choose in descending order of preference:
    # txtconfig (decoded, decompressed)
    # decodedconfig (just decoded)
    # enconfig (the original input)
    my $compconfig = defined($txtconfig) ? $txtconfig : defined($decodedconfig) ? $decodedconfig : $enconfig;
    push @output, split(/(?:\r\n|\n)/, $compconfig, -1);

    # The first line of the stored config file should be a comment labelling
    # the invocation command line, so check for that:
    if ($output[0] !~ /^\# Invocation/) {
        ::Log(0, "ERROR: Contents of rawconfig array are not a configuration file!\n");
        return(undef, []);
    }

    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    # Dump the original config, too (if necessary)
    if (exists($r->{'origconfig'})) {
        my $origtext = ::decode_decompress(join("\n", @{$r->{'origconfig'}}));
        my $end_nl = ($origtext =~ /\n$/) || 0;
	my @orig = split(/(?:\r\n|\n)/, $origtext, -1);
	foreach my $line (@orig) {
	    $line =~ tr/\015\012//d; # More reliable than the double chomp
	}
        $origtext = join("\n", @orig);
        $origtext .= "\n" unless $end_nl;

        my ($barename, $outputpath) = fileparse($fn, ".$extension");
        if (!(defined($::website_formatter) && $::website_formatter)) {
          # Only convert to relative path if the main output file is relative
          if ($outputpath =~ /^\./) {
              $fn = File::Spec->abs2rel(::unrel_path($fn));
          }
          $outputpath = dirname($fn).'/';
          $outputpath = '' if ($outputpath =~ /^\.[\/\\]$/o);
        }
	my $outfn = "${outputpath}${barename}.orig.$extension";
	my $ofh = new IO::File '>'.$outfn;
	if (defined($ofh)) {
	    $ofh->print($origtext);
	    $ofh->close();
            # Present a nicer path to the user
            my $dn = File::Basename::dirname($fn);
            if ($dn eq '.') {
                $dn = '';
            } else {
                $dn .= '/';
            }
	    push @{$written}, $dn.File::Basename::basename($outfn);
	} else {
	    ::Log(0, "ERROR: Could not open orig config for writing: $!\n");
	}
    }
	
    return (\@output, $written);
}

1;
