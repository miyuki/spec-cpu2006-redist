#! /usr/bin/perl
#/*@@
#  @file      BuildActiveThorns.pl
#  @date      Tue Jan 19 14:02:07 1999
#  @author    Tom Goodale
#  @desc 
#  Creates an ActiveThornsList
#  @enddesc 
#  @version $Id: BuildActiveThorns.pl,v 1.10 2002/01/03 22:17:04 tradke Exp $
#@@*/

if ($ENV{'CCTK_HOME'})
{
  require "$ENV{'CCTK_HOME'}/lib/sbin/MakeUtils.pl";
}
else
{
  require "lib/sbin/MakeUtils.pl";
}

$package_dir = shift(@ARGV);

%info = &buildthorns($package_dir,"thorns");

printf("# arrangement/thorn %-14s # implements (inherits) [friend] {shares}\n#\n");

foreach $thorn (sort keys %info)
{
  printf("%-34s # %s\n",$thorn,$info{$thorn});
}
