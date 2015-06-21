#*******************************************************************************
# This script is to print out the byte numbers of differences between two files.
# It prints them in both the forward and reverse directions.
#
# (The locations in the forward direction should be the same as given by the cmp
# utility).
#
# Copyright (c) Daniel Grimwood, University of Western Australia, 2003.
#
# $Id: print_cmp.perl,v 1.2 2003/02/19 07:49:44 reaper Exp $
#
#*******************************************************************************


#*******************************************************************************
# Argument checking.
#
$argerr=0;
$n_arg=$#ARGV+1;
$n_arg >= 2 || do {print STDERR "Error : need at least two arguments\n"; $argerr=1; };
@ARGS = @ARGV[0 .. $n_arg-3];
$file1 = $ARGV[$n_arg-2];
$file2 = $ARGV[$n_arg-1];

while (@ARGS) {
  $arg=shift @ARGS;
  for ($arg) {
    /^-compiler/ && do {
      $fc=shift @ARGS;
      defined $fc || do {print STDERR "Error : no compiler specified\n"; $argerr=1};
      last;
    };
    print STDERR ("Error : unexpected argument $arg\n");
    $argerr=1;
  }
}

if ($argerr==1) {
  warn(
    "\nUsage :\n",
    "\t perl -w compare_module_file.perl \\\n",
    "\t\t filename1 filename2 \n",
    "\n",
    "Where :\n",
    "\tfilename1 and filename2 are the two files to compare.\n");
   exit 1;
}

#*******************************************************************************
# check whether files exist.
#
(-f $file1) or die "File $file1 does not exist\n";
(-f $file2) or die "File $file2 does not exist\n";

#*******************************************************************************
# quick filesize comparison.
#
$size1 = (stat($file1))[7];
$size2 = (stat($file2))[7];
($size1 == $size2) or exit 1;

open(FILE1,$file1) or die "Cannot open $file1\n";
open(FILE2,$file2) or die "Cannot open $file2\n";

@offsets = ();  # initialise the array to nothing. (not really necessary).

&do_compare;
&printout;

#*******************************************************************************
# Store all the byte numbers in the array @offsets.
sub do_compare {
  $i=0;
  while (! eof FILE1) {
    $i++;
    $ch1 = getc(FILE1);
    $ch2 = getc(FILE2);
    if (! ($ch1 eq $ch2)) { push @offsets, $i; }
  }
  $size1 = $i;
}

#*******************************************************************************
# Hmmm, I wonder what this routine does....
sub printout {
  print STDERR "\nNumber of different bytes is ",$#offsets+1,".\n";

  if ($#offsets > -1) {
    print STDERR "\nBytes offsets from the start of file:\n";
    $i=0;
    while ($i <= $#offsets) {
      print STDERR $offsets[$i],"\n";
      $i++;
    }

    print STDERR "\nBytes offsets starting from the end of file:\n";
    $i=0;
    while ($i <= $#offsets) {
      print STDERR $size1-$offsets[$i]+1,"\n";
      $i++;
    }
  }
}

