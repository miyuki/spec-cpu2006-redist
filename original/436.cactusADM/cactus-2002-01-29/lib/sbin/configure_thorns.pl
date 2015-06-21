#! /usr/bin/perl
#/*@@
#  @file      configure_thorns.pl
#  @date      Fri Jan  8 18:19:00 1999
#  @author    Tom Goodale
#  @desc 
#  Configures the thorn directories for a build of the CCTK
#  @enddesc 
#@@*/

$top = shift(@ARGV);
$config = shift(@ARGV);

%activethorns = &GetActiveThorns($top, $config);

chdir $top;

chdir $config;

chdir "arrangements";

foreach $thorn (keys %activethorns)
{
  if (! -d $activethorns{$thorn} && ! -l $activethorns{$thorn})
  {
    mkdir($activethorns{$thorn}, 0755);
  }

  chdir $activethorns{$thorn};

  if (! -d $thorn && ! -l $thorn)
  {
    mkdir($thorn, 0755);
  }

  
