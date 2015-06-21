#/*@@
#  @file      ProcessConfiguration.pl
#  @date      Mon May  8 15:52:08 2000
#  @author    Tom Goodale
#  @desc 
#  
#  @enddesc 
#@@*/

#/*@@
#  @routine    SplitThorns
#  @date       Mon May  8 16:04:59 2000
#  @author     Tom Goodale
#  @desc 
#  Splits the thorns hash intto those with source and those without source
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub SplitThorns
{
  my ($configuration_database, $thorns, $source_thorns, $nosource_thorns) = @_;

  foreach $thorn (keys %$thorns)
  {
    if($configuration_database->{"\U$thorn OPTIONS\E"} =~ m/NO_SOURCE/i)
    {
      $nosource_thorns->{"$thorn"} = $thorns->{"$thorn"};
    }
    else
    {
      $source_thorns->{"$thorn"} = $thorns->{"$thorn"};
    }
  }
}



1;
