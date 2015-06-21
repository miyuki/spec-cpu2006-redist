#!/bin/perl -s
#
# Script to copy parameter files
#
# Only copies those parameter files that used the compiled thorns
#
# Version: $Id: CopyParFiles.pl,v 1.7 2001/08/25 17:36:18 allen Exp $

$home = `pwd`;
chop($home);

$config = $ARGV[0];

open(THORNLIST,"<configs/$config/ThornList") || die "ThornList not available";

# Create examples directory if needed
if (! -d "examples")
{
    mkdir("examples", 0755) || die "Unable to create examples directory";
}

# Create configuration directory if needed
if (! -d "examples/$config")
{
  mkdir("examples/$config", 0755) || die "Unable to create examples/$config directory";
}

# Create an array of compiled thorns 
$nthorns = 0;
while (<THORNLIST>)
{
  /^(\S*)/;
  $line = $1;
  next if (m:^\#:);
  $thorns[$nthorns] = $line;
  $nthorns++;
}
close(THORNLIST);

for ($i=0;$i<$nthorns;$i++)
{
 $thorn=$thorns[$i];
 $thorn = "arrangements/$thorn/par";
 if (-d $thorn)
 {    
   chdir $thorn;
   while ($parfile = <*.par>)
   { 
     $gotall = 1;
     @ActiveThorns = &GetActiveThorns($parfile);
     $donothave = "";
     for ($j=0;$j<scalar(@ActiveThorns);$j++)
     {
       $gotit = 0;
       for ($k=0;$k<$nthorns;$k++)
       {
	 $thorns[$k] =~ m:.*/(\w*):;
	 if ($ActiveThorns[$j] =~ /^$1$/i)
	 {
	   $gotit = 1;
	 }
       }
       if ($gotit == 0)
       {
	 $donothave .= "$ActiveThorns[$j] ";
	 $gotall = 0;
       }
     }

     if ($gotall == 1)
     {
       if (-e "$home/examples/$config/$parfile")
       {
	 print "  $parfile:  Exists, no overwrite\n";
       }
       else
       {
	 print "  $parfile: Copying from $thorns[$i]\n";
	 system("cp $parfile $home/examples/$config/$parfile");
       }
     }
     else
     {
       print "  $parfile: Missing thorns ($donothave)\n";
     }
   }
   
   chdir "$home${sep}";

 }
}

# Parse the active thorns from a parameter file
sub GetActiveThorns
{
  my($parfile) = @_;
  my $i;
  my @ActiveThorns;

  open(PAR,"<$parfile");
  while (<PAR>)
  {
    if ($_ =~ /ActiveThorns\s*=\s*\"(.*)\"/)
    {
      @ActiveThorns = split(' ',$1);
    }
  }
  close(PAR);
  
  # Get rid of spaces and change to lower case
  for ($i=0;$i<scalar(@ActiveThorns);$i++)
  {
    $ActiveThorns[$i] =~ s/\s*//g;
    $ActiveThorns[$i] = lc $ActiveThorns[$i];
  } 
 
  return sort(@ActiveThorns);

}
