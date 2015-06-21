# This perl script adds dependencies for Fortran 95 files whose .foo files have
# get_from directives.  This script is specific to Tonto.
#
#*******************************************************************************
# Usage :
#
#  perl -w make_get_dependencies.perl -o outfile -src infile
#
#  Where :
#
#     "-o outfile"           : Output dependency information to file "outfile"
#     "-src infile"          : Process source file "infile"

#*******************************************************************************
# $Id: make_foo_dependencies.perl,v 1.2.2.2 2003/05/15 02:51:21 reaper Exp $ #
# Copyright 2003 Daniel Grimwood, <reaper@theochem.uwa.edu.au>
#*******************************************************************************

use File::Basename;

#*******************************************************************************
# Argument parsing.

$argerr=0;
$argerr=1 if ($#ARGV+1==0);

while (@ARGV) {
    $arg=shift;
    SWITCH: for ($arg) {
        /^-src$/       && do { $filename=shift; last; };
        /^-o$/         && do { $depsfile=shift; last; };
        warn "Error : unexpected argument $arg\n";
        $argerr=1;
    }
}

#*******************************************************************************
# Error checks.

defined $filename || do {$argerr=1; warn "Error : -src flag not supplied\n"};
defined $depsfile || do {$argerr=1; warn "Error : -o flag not supplied\n"};

# Print out standard help message if there's an error.
if ($argerr==1) {
    die(
        "\nUsage:\n",
        "\t perl -w make_dependencies.perl -o outfile -src infile\n\n",
        "Where :\n",
        "\t\"-o outfile\"\t\t: Output dependency information to file\n",
        "\t\t\t\t  \"outfile\"\n",
        "\t\"-src infile\"\t\t: Process source file \"infile\"\n");
}

#*******************************************************************************
# now to construct all the variables and lists.
#*******************************************************************************

undef @gets;

# get the base part of the file.  (minus extension and directory).
$base = basename($filename);
$base =~ s/\.[^\.]+$//;

&get_deps($filename);

if (defined @gets) {
  foreach $get (&uniq(@gets)) {
    ($name = $get) =~ s/$/.foo/;
    push (@getlist,'$(foodir)/' . $name);
  }
}

#*******************************************************************************
# open the dependency output file for writing.
#*******************************************************************************

open(OUTFILE,">" . $depsfile) or die "Cannot open $depsfile for writing.\n";

print OUTFILE "#get_from dependencies for $filename:\n";
if (defined @getlist) {
  $word = '$(f95dir)/' . "$base" . '.$(FSUFFIX) :';
  &PrintWords($word, @getlist);

  print OUTFILE "\n";
  $word = '$(htmldir)/' . "$base" . '.html :';
  &PrintWords($word, @getlist);
}

print OUTFILE "\n\n";             # Add newline to .dep file
close(OUTFILE);



#*******************************************************************************
# Now the subroutine definitions....
#*******************************************************************************

#*******************************************************************************
# &PrintWords(list);
# print the list to span multiple lines if necessary.
# Based on the one from makemake.perl.
#
sub PrintWords {
    my ($columns, $wordlength);
    $columns = 79;

    foreach $word (@_) {
        $wordlength = length($word);
        if ($columns == 79) {
            print OUTFILE "$word";
            $columns -= $wordlength + 1;
        } elsif ($wordlength + 1 < $columns) {
            print OUTFILE " $word";
            $columns -= $wordlength + 1;
        } else {
           print OUTFILE " \\\n\t$word";
           $columns = 71 - $wordlength;
        }
    }
}

#*******************************************************************************
# &uniq(list)
# return the list in original order minus duplicates.
sub uniq {
    my @words;
    foreach $x (@_) {
      $_ = join(" ",@words);
      if (! /\b$x\b/) { push(@words,$x); }
    }
    return @words;
}

#*******************************************************************************
# &get_deps(filename)
sub get_deps {
  my ($dir,$filehead,$inc,$inchead,$use,$usehead);

  open(FILE, $_[0]) or die "Cannot find file $_[0]\n";

  while (<FILE>) {
    /(?:.+):::(?:.+)get_from\((\w+)\)/i && do { $x=$1; push(@gets, lc($x))};
  }
  close(FILE);
}

