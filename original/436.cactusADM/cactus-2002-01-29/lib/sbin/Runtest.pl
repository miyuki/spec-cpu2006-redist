#!/bin/perl -s
#
# Test Suite tool
# Version: $Header: /cactus/Cactus/lib/sbin/Attic/Runtest.pl,v 1.54 2001/10/14 18:18:05 goodale Exp $

$sep = "/";

$prompt = shift;
$config = shift;;

$prompt =~ tr/A-Z/a-z/;

$tolerance = 13;

$ansinormal="";
$ansibold="";

if ($prompt ne "no") {
  $ansinormal  = "\033[0m";
  $ansibold    =  "\033[1m";
}

# Work out where the config directory is
if($ENV{"CONFIGS_DIR"})
{
  $configs_dir = $ENV{"CONFIGS_DIR"};
}
else
{
  $configs_dir = "configs";
}

$current_directory = `pwd`;
chop($current_directory);
# Stuff necessary for cygwin
$current_directory =~ s,^/cygdrive/(.)/,$1:/,;
$current_directory =~ s,^//(.)/,$1:/,;

&print_header;

# Look to see if MPI is defined
$extra = "$current_directory${sep}configs${sep}$config${sep}config-data${sep}cctk_Extradefs.h";

$mpi = 0;
if (-e "$extra")
{
  open(EXTRA,"<$extra");
  while(<EXTRA>)
  {
    if (/\#define CCTK_MPI/)
    {
      $mpi = 1
    }
  }
  close(EXTRA);
}


# Check the name and directory of executable
$defns = "$current_directory${sep}configs${sep}$config${sep}config-data${sep}make.config.defn";

$defexename = "cactus_$config";

if (-e "$defns")
{
  open(DEFNS,"<$defns");
  while(<DEFNS>)
  {
    if (/EXE\s*=\s*([\w-]+)/)
    {
      $defexename = $1;
    }
    if (/EXEDIR\s*=\s*([\w-]+)/)
    {
      $defexedirname = $1;
    }
  }
  close(DEFNS);

}


$executable = &defprompt("Enter executable name (relative to Cactus home dir)","exe$sep$defexename");

if ($mpi)
{
  $numprocs = &defprompt("Enter number of processors","2");
  $command = &defprompt("Enter command to run executable","mpirun -np $numprocs ");
}
else
{
  $command = &defprompt("Enter command to run executable"," ");
}
$tests = &defprompt("Run (A)ll tests or go to (M)enu","All");



# Get the active thorns list and test files with thorns
$scratchdir = "$configs_dir$sep$config";

if (!open (AT, "< $scratchdir${sep}ThornList")) {
  print "Can't open $scratchdir/ThornList - no thorn tests";
}
else
{
  chdir "arrangements";

  while (<AT>)
  {
    next if (m:^\#:);
    $name = $_;
    $name =~ s/\#(.*)$//g;
    $name =~ /^\s*([^\s]*)\s*/;
    $T = $1;
    push(@allthorns, $T);

    $T =~ m:^.*/([^\s]*)\s*:;
    $database{"\U$T THORN\E"} = $1;
    $T =~ m:^\s*([^\s]*)/:;
    $database{"\U$T ARRANGEMENT"} = $1;

    $number = 0;
    if (-d "$T${sep}test")
    {
      $thisdir = `pwd`;
      chop($thisdir);
      chdir "$T${sep}test" || die "Unable to chdir to $T${sep}test";
      foreach $file (sort <*.par>)
      {
        $database{"\U$T TESTFILE\E"} = $file;
        push(@testfiles, $file);
        push(@testthorns, $T);
        $number++;
      }
      chdir "../../.." || die "Unable to chdir to $thisdir";

    }
    $ntests{"$T"} = $number;
    $database{"\U$T NTESTS\E"} = $number;
  }

  chdir "..";
}

# Parse the parameter files for directives
$ntests = 0;
foreach $t (@testfiles)
{
  $file =  "arrangements/$testthorns[$ntests]/test/$t";
  open (IN, "<$file") || die "Can not open $file";

  $processing_active = 0;

  # Give a default test name in case none are specified in the parameter file.
  $testnames{$ntests} = "$testthorns[$ntests]/test/$t";

  while (<IN>)
  {
    $line = $_;

    if($processing_active == 1)
    {
      if($line =~ m/(.*)\"/)
      {
        $activethorns[$ntests] .= $1;
        $processing_active = 0;
      }
      else
      {
        $activethorns[$ntests] .= $line;
      }
    }
    elsif ($line =~ m/^\s*\!\s*DESC(RIPTION)?\s*\"(.*)\"\s*$/i)
    {
      $testnames[$ntests] = $2;
    }
    elsif ($line =~ m/^\s*ActiveThorns\s*=\s*\"(.*)\"/i)
    {
      $activethorns[$ntests] = $1;
    }
    elsif($line =~ m/^\s*ActiveThorns\s*=\s*\"(.*)/i)
    {
      $activethorns[$ntests] = $1;
      $processing_active = 1;
    }
  }
  close IN;
  $ntests++;
}

$ntests=0;
$number_missing=0;

foreach $t (@testfiles)
{
  $haveallthorns = 1;
  $active = $activethorns[$ntests];
  @at = split(' ',$active);
  foreach $th (@at)
  {
    $th = "\U$th";
    $foundit = 0;

    foreach $tthorn  (@allthorns)
    {
      $tthorn =~ m:.*/(.*)$:;
      $thornpart = "\U$1";
      if ($thornpart eq $th)
      {
        $foundit = 1;
      }
    }
    if (!$foundit)
    {
      $haveallthorns = 0;
    }
  }

  if ($haveallthorns)
  {
    $havethorns{"$t"} = 1;
  }
  else
  {
    $havethorns{"$t"} = 0;
    $number_missing++;
  }
  $ntests++;
}

if ($tests =~ /All/)
{

  # Run all parameter files
  $number_failed=0;
  $number_extra=0;
  $number_zerofiles=0;
  $number_passed1=0;
  $number_passed2=0;
  $ntested = 0;

  foreach $t (@testfiles)
  {
    $thorn = $testthorns[$ntested];

    if ($havethorns{"$t"})
    {
      push(@actually_tested, $testnames[$ntested]);
      &runtest($t,$thorn,$ntested);
    }
    else
    {
      push(@not_tested, $testnames[$ntested]);
      push(@not_tested_parfile, $t);
      push(@not_tested_thorns, $thorn);
      print "Ignoring test '$testnames[$ntested]' from thorn '$thorn' - missing thorns.\n";
    }

    $ntested++;
  }

  # Show the statistics

  print "==================================================\n";
  print "All tests run for configuration $config\n\n";
  print "Tested: \n";
  foreach $thorn (keys %ntests)
  {

    if ($ntests{"$thorn"} > 0)
    {
      print "  $thorn [$ntests{\"$thorn\"}]\n";
    }
  }

  print "\n";
  print "  Total Tests   -> $ntests\n";
  if ($number_missing > 0)
  {
    print "  Number which couldn't be run -> $number_missing\n";
  }
  print "  Number passed -> $number_passed1\n";
  if ($number_passed2 > 0)
  {
    print " (Number passed to only $tolerance digits -> $number_passed2)\n";
  }
  print "  Number failed -> $number_failed\n";

  if ($number_failed>0)
  {
    print "\n  Tests failed:\n";
    for ($i=0; $i<$number_failed;$i++)
    {
      print "    ". $which_failed[$i]." (from ". $thorn_failed[$i].")\n";
    }
  }

  if ($number_zerofiles > 0)
  {
    print "  Number with no output files -> $number_zerofiles\n";
  }

  print "=======================================================\n\n";

  printf("  $ansibold Warnings:  $ansinormal \n\n");

  if ($number_missing>0)
  {
    print "  Tests Missed for lack of thorns:\n";
    for ($i=0; $i<$number_missing;$i++)
    {
      print "    ". $not_tested[$i]."\n    (".$not_tested_parfile[$i]." from ". $not_tested_thorns[$i].")\n";
    }
    print "\n";
  }

  if ($number_extra>0)
  {
    print "  Tests with different numbers of output files:\n";
    for ($i=0; $i<$number_extra;$i++)
    {
      print "    ". $thorn_extra_desc[$i]."\n    (".$thorn_extra_parfile[$i]." from ". $thorn_extra[$i].")\n";
    }
    print "\n";
  }

  print "=======================================================\n\n";

}

else

{

  # Show the parameter file menu

  $choice = test01;
  $ntests = 0;
  foreach $t (@testfiles)
  {
    $t =~ m:([^${sep}]+)\.par$:;
  $num = $1;
  $inp{$num} = $t;
  $testnum[$ntests] = $num;
  $ntests++;
}
while (!($choice =~ /^q/i) )
{
  print "\n--- Menu ---\n";
  $sp = "     ";
  for ($i=0;$i<$ntests;$i++)
  {
    if($havethorns{$inp{$testnum[$i]}})
    {
      $number = $i+1;
    }
    else
    {
      $number = "x";
    }
    print "[$number] $testthorns[$i] $testnum[$i]: \n      \"$testnames[$i]\"\n";
  }
  print "\n  Enter number of test to run (quit to end) : ";
  $choice = <STDIN> if ($prompt eq "yes");
  $choice =~ s/\n//;
  $choice =~ s/\s//;
  print "\n";
  $ip = $inp{$testnum[$choice-1]};
  $thorn = $testthorns[$choice-1];
  if (!($choice =~ m/^q/i || $choice =~ m/^\s*$/))
  {
    if($choice > 0 && $choice <= $ntests)
    {
      if($havethorns{$ip})
      {
        &runtest($ip,$thorn,$choice-1);
      }
      else
      {
        print "This test cannot be run - missing thorns\n";
      }
    }
  }
  if (!($choice =~ m/^q/i))
  {
    print "  Hit return to continue ";
    $continue = <STDIN> if ($prompt eq "yes");
  }
}

print "\n";

}

sub runtest
{
  my ($inpf,$inthorn,$num) = @_;

  # File name from thorn
  $inpf = "arrangements/$inthorn/test/$inpf";

  # Directory for output
  $tsttop = ".${sep}TEST${sep}$config";
  mkdir (TEST,0755);
  mkdir ($tsttop,0755);

  $tp = $inpf;
  $tp =~ s:^.*$sep::;
  $tp =~ s/\.par$//;

  $test_base_dir = $inpf;
  $test_base_dir =~ s:[^${sep}]*$::;

  print "Running $tp: $testnames[$num]\n";

  # check that the executable is there
  if (! (-e "$current_directory$sep$executable"))
  {
    if (-e "$current_directory$sep${executable}.exe")
    {
      $executable .= ".exe";
    }
    else
    {
      die "Cannot locate $executable";
    }
  }

  # clear out test directory
  chdir ("$tsttop");
  mkdir ($tp,0755);
  chdir ("$tp");
  opendir (DIR, ".");
  unlink (grep (/.+\..+/, readdir (DIR)));
  closedir (DIR);
  chdir ('..');

  $cmd = "($command $current_directory$sep$executable $current_directory$sep$inpf)";

  printf "Issuing $cmd\n";
  $retcode = 0;
  open (CMD, "$cmd |");
  open (LOG, "> $tp.log");

  while (<CMD>)
  {
    print LOG;

    if( /Cactus exiting with return code (.*)/)
    {
      $retcode = $1 + 0;
    }
  }
  close LOG;
  close CMD;
  $retcode = $? >> 8 if($retcode==0);

  chdir ("../..");


  if($retcode != 0)
  {
    print "${ansibold}Cactus exited with error code $retcode $ansinormal \n";
    print "Please check the logfile $tsttop$sep$tp.log\n\n";
    $number_failed++;
    @which_failed = (@which_failed,$tp);
    @thorn_failed = (@thorn_failed,$inthorn);
    return;
  }

  $indir = $inpf;
  $indir =~ s:\.par$:${sep}:g;
  opendir (DIR, $indir);
  @oldout = grep (/\..+l$/, readdir (DIR));
#print STDERR "\nindir is $indir\n"; #readdir is :",readdir(DIR),":\n";
#print STDERR "oldout[0] == @oldout[0]\n";

  closedir (DIR);
  $blewit = 0;
  $reallyblewit = 0;
  $nfiles = 0;

  # Count number of files in test directory
  opendir (DIR, "$tsttop$sep$tp");
  @newout = grep (/\..+l$/, readdir (DIR));
  closedir (DIR);

  foreach $file (@oldout)
  {

    $file="$indir$file";

    $nfiles ++;
    $newfile = $file;
    $newfile =~ s:^.*${sep}([^${sep}]+)$:$1:;
    $newfile = "$tsttop$sep$tp$sep$newfile";
    #print STDERR "Comparing :${file}: with :${newfile}:\n";

    if ( -e $newfile)
    {

        #print STDERR "*************** -e :${newfile}:!\n";
        #if (-e $file) {print "************** file exists\n";}

      open (INORIG, "<$file");
      open (INNEW,  "<$newfile");
      $nblow = 0;
      $nrealblow = 0;
      while ($oline = <INORIG>)
      {
    #print STDERR "***************** got oline = :${oline}:\n";

        $nline = <INNEW>;
        # Now lets see if they differ.
        if (!($nline eq $oline))
        {

          # Check against nans
          #print STDERR "*************** Looking for nans...........\n\a\a\a";
          if ($nline =~ /nan/i)
          {
            print "****CAUGHT NAN in $newfile****\n";
            $nblow ++;
            $nrealblow ++;
          }
          # Check against inf
          elsif ($nline =~ /inf/i)
          {
            print "****CAUGHT INF in $newfile****\n";
            $nblow ++;
            $nrealblow ++;
          }
          else
          {
            # This is the new comparison (subtract last two numbers)
            ($t1,$v1) = split(' ', $nline);
            ($t2,$v2) = split(' ', $oline);
            # Make sure that floating point numbers have 'e' if exponential.
            $v1 =~ s/[dD]/e/;
            $v2 =~ s/[dD]/e/;

            $vdiff = abs($v1 - $v2);
            if ($vdiff > 0)
            {

              # They diff. But do they differ strongly?
              $nblow ++;

              $exp = sprintf("%e",$vdiff);
              $exp =~ s/^.*e-(\d+)/$1/;
              unless ($exp >= $tolerance)
              {
                $nrealblow++;
              }
            }
          }
        } # if
      } #while
      if ($nblow != 0)
      {
        $blewit ++;
        $stripfile = $newfile;
        $stripfile =~ s:^.*${sep}(.*)$:$1:;
        if ($nrealblow == 0)
        {
          print "     $stripfile differs at machine precision (which is OK!)\n";
        }
        else
        {
          $reallyblewit ++;
          print "Substantial differences detected in $stripfile\n";
          print "     $newfile $file\n";
          print "     Differ on $nblow lines!\n";
        }
      }
    }
    else
    {
      print "     $newfile not there for comparison\n";
      $reallyblewit++;
      $blewit++;
    }
  }

  # Give a warning if there were different numbers of files
  if (scalar(@oldout) != scalar(@newout))
  {
    printf("\n  $ansibold WARNING: Comparing different numbers of output files ! $ansinormal \n");
    print "            ","Counted ",scalar(@oldout)," in thorn and ";
    print scalar(@newout)," from test\n";
    foreach $file (@newout)
    {
      $oldfile = $file;
      $oldfile =~ s:^.*${sep}([^${sep}]+)$:$1:;
      $oldfile = "$indir${sep}$oldfile";
      if (!-e $oldfile)
      {
        print "            $oldfile not in thorn archive\n";
      }
    }
    foreach $file (@oldout)
    {
      $newfile = $file;
      $newfile =~ s:^.*${sep}([^${sep}]+)$:$1:;
      $newfile = "$tsttop$sep$tp$sep$newfile";
      if (!-e $newfile)
      {
        print "            $newfile not created in test\n";
      }
    }
    $number_extra++;
    @thorn_extra_desc = (@thorn_extra_desc,$testnames[$num]);
    @thorn_extra_parfile = (@thorn_extra_parfile,$tp);
    @thorn_extra = (@thorn_extra,$inthorn);
  }

  if ($nfiles == 0)
  {
    printf("\n  $ansibold WARNING: ZERO files compared ! $ansinormal \n");
    $number_zerofiles++;
  }
  elsif ($blewit == 0)
  {
    printf("\n  $ansibold Test succeeded!$ansinormal $nfiles files identical\n");
    $number_passed1++;
  }
  else
  {
    if ($reallyblewit == 0)
    {
      printf "\n  $ansibold Test passed to $tolerance figures:$ansinormal ".
        "$nfiles compared, $blewit files differ in the last digits\n";
      $number_passed1++;
      $number_passed2++;
    }
    else
    {
      printf "\n  $ansibold TEST FAILED!!:$ansinormal ".
         "$nfiles compared, $blewit files differ, $reallyblewit differ significantly\n";
      $number_failed++;
      @which_failed = (@which_failed,$tp);
      @thorn_failed = (@thorn_failed,$inthorn);
    }
  }
  printf ("\n\n");
}

sub defprompt
{
  my ($pr, $de) = @_;
  my ($res);

  print "$pr [$de] \n";
  print "   --> ";

  $res = <STDIN> if ($prompt eq "yes");
  if ($res =~ m/^$/)
  {
    $res = $de;
  }
  elsif ($res =~ m/^ $/)
  {
    $res = "";
  }
  $res =~ s/\n//;
  print "\n";
  return $res;
}

sub fpabs
{
  my ($val) = $_[0];
  $val > 0 ? $val:-$val;
}


sub print_header
{
  print <<EOT;

-------------------------------
 Cactus Code Test Suite Tool
-------------------------------

EOT
}
