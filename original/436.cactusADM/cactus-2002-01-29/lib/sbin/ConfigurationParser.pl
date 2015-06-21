#! /usr/bin/perl -w

#/*@@
#  @file      ConfigurationParser.pl
#  @date      Tue Feb  8 17:36:48 2000
#  @author    Tom Goodale
#  @desc 
#  Parser for configuration.ccl files
#  @enddesc
#  @version $Header: /cactus/Cactus/lib/sbin/ConfigurationParser.pl,v 1.2 2001/10/14 18:18:04 goodale Exp $ 
#@@*/

#/*@@
#  @routine    CreateConfigurationDatabase
#  @date       Tue Feb  8 17:47:26 2000
#  @author     Tom Goodale
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub CreateConfigurationDatabase
{
  my(%thorns) = @_;
  my($thorn, @indata);
  my(%configuration_data) = ();
  my($filename);

  #  Loop through each  thorn's interface file.
  foreach $thorn (keys %thorns)
  {
    #       Get the arrangement name for the thorn
    $thorns{$thorn} =~ m:.*/arrangements/([^/]*)/[^/]*:;
    $arrangement = $1;

    $filename = "$thorns{$thorn}/configuration.ccl";

    if(-r $filename)
    { 
      print "   $thorn\n";

      #       Read the data
      @indata = &read_file($filename);

      #       Get the configuration data from it
      &ParseConfigurationCCL($arrangement,$thorn, \%configuration_data, \@indata);
      
      &PrintConfigurationStatistics($thorn, \%configuration_data);
    }
  }
  
  return \%configuration_data;
}


#/*@@
#  @routine    ParseConfigurationCCL
#  @date       Tue Feb  8 19:23:18 2000
#  @author     Tom Goodale
#  @desc 
#  Parses a configuration.ccl file and generates a database of the values
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseConfigurationCCL
{
  my($arrangement, $thorn, $rh_configuration_db, $ra_data) = @_;
  my($line_number, $line);
  my($provides, $script, $lang);
  my($optional, $define);

  # Initialise some stuff to prevent perl -w from complaining.

  $rh_configuration_db->{"\U$thorn PROVIDES\E"} = "";
  $rh_configuration_db->{"\U$thorn REQUIRES\E"} = "";
  $rh_configuration_db->{"\U$thorn OPTIONAL\E"} = "";
  $rh_configuration_db->{"\U$thorn OPTIONS\E"}  = "";
  

  for($line_number = 0; $line_number < @$ra_data; $line_number++)
  {
    $line = $ra_data->[$line_number];
    
    #       Parse the line

    if($line =~ m/^\s*PROVIDES\s*/i)
    {
      ($provides, $script, $lang, $line_number) = &ParseProvidesBlock($line_number, $ra_data);
      $rh_configuration_db->{"\U$thorn PROVIDES\E"} .= "$provides";
      $rh_configuration_db->{"\U$thorn PROVIDES $provides\E SCRIPT"} = "$script";
      $rh_configuration_db->{"\U$thorn PROVIDES $provides\E LANG"} = "$lang";
      next;
    }
    elsif($line =~ m/^\s*REQUIRES\s*(.*)/i)
    {
      $rh_configuration_db->{"\U$thorn REQUIRES\E"} .= "$1";
    }
    elsif($line =~ m/^\s*OPTIONAL\s*/i)
    {
      ($optional, $define, $line_number) = &ParseOptionalBlock($line_number, $ra_data);
      $rh_configuration_db->{"\U$thorn OPTIONAL\E"} .= "$provides";
      $rh_configuration_db->{"\U$thorn OPTIONAL $optional\E DEFINE"} = "$define";
    }
    elsif($line =~ m/^\s*NO_SOURCE\s*/i)
    {
      $rh_configuration_db->{"\U$thorn OPTIONS\E"} .= "NO_SOURCE";
    }
    else
    {
      print STDERR "Error: Unrecognised line in configure.ccl '$line' \n";
      $CST_errors++;
    }

  }

  return;
}

#/*@@
#  @routine    ParseProvidesBlock
#  @date       Mon May  8 15:52:40 2000
#  @author     Tom Goodale
#  @desc 
#  Parses the PROVIDES block in a configuration.ccl file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseProvidesBlock
{
  my ($line_number, $ra_data) = @_;
  my ($provides, $script, $lang);

  $provides = "";
  $script   = "";
  $lang     = "";

  $ra_data->[$line_number] =~ m/^\s*PROVIDES\s*(.*)/i;
  
  $provides = $1;

  $line_number++;
  if($ra_data->[$line_number] !~ m/^\s*\{\s*$/)
  {
    print STDERR "Error parsing provides block line '$ra_data->[$line_number]'\n";
    print STDERR "Missing { at start of block\n";
    $CST_errors++;
    $line_number++ while($ra_data[$line_number] !~ m:\s*\}\s*:);
  }
  else
  {
    while($ra_data->[$line_number] !~ m:\s*\}\s*:)
    {
      $line_number++;
      if($ra_data->[$line_number] =~ m/^\s*SCRIPT\s*(.*)$/i)
      {
        $script = $1;
        next;
      }
      elsif($ra_data->[$line_number] =~ m/^\s*LANG[^\s]*\s*(.*)$/i)
      {
        $lang = $1;
        next;
      }
      elsif($ra_data->[$line_number] =~ m:\s*\}\s*:)
      {
        # do nothing.
      }
      else
      {
        print STDERR "Error parsing provides block line '$ra_data->[$line_number]'\n";
        print STDERR "Unrecognised statement\n";
        $CST_errors++;
      } 
    }
  }


  return ($provides, $script, $lang, $line_number);
}

#/*@@
#  @routine    ParseProvidesBlock
#  @date       Mon May  8 15:52:40 2000
#  @author     Tom Goodale
#  @desc 
#  Parses the OPTIONAL block in a configuration.ccl file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseOptionalBlock
{
  my ($line_number, $ra_data) = @_;
  my ($optional, $define);

  $ra_data->[$line_number] =~ m/^\s*OPTIONAL\s*(.*)/i;
  
  $optional = $1;

  $define = "";

  $line_number++;

  if($ra_data->[$line_number] !~ m/^\s*\{\s*$/)
  {
    print STDERR "Error parsing optional block line '$ra_data->[$line_number]'\n";
    print STDERR "Missing { at start of block\n";
    $CST_errors++;
    $line_number++ while($ra_data->[$line_number] !~ m:\s*\}\s*:);
  }
  else
  {
    while($ra_data->[$line_number] !~ m:\s*\}\s*:)
    {
      $line_number++;
      if($ra_data->[$line_number] =~ m/^\s*DEFINE\s*(.*)$/i)
      {
        if($define eq "")
        {
          $define = $1;
          next;
        }
        else
        {
          print STDERR "Error parsing optional block line '$ra_data->[$line_number]'\n";
          print STDERR "Only one define allowed\n";
          $CST_errors++;
        }
      }
      elsif($ra_data->[$line_number] =~ m:\s*\}\s*:)
      {
        # do nothing.
      }
      else
      {
        print STDERR "Error parsing provides block line '$ra_data->[$line_number]'\n";
        print STDERR "Unrecognised statement\n";
        $CST_errors++;
      } 
    }
  }

  return ($optional, $define, $line_number);
}

#/*@@
#  @routine    PrintConfigurationDatabase
#  @date       Mon May  8 15:53:23 2000
#  @author     Tom Goodale
#  @desc 
#  Prints out the configuration database.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintConfigurationDatabase
{
  my($configuration_data) = @_;
  my($field);
  
  foreach $field ( sort keys %$configuration_data)
  {
    print "$field has value $configuration_data->{$field}\n";
  }
}

#/*@@
#  @routine    PrintConfigurationStatistics
#  @date       Mon May  8 15:53:23 2000
#  @author     Tom Goodale
#  @desc 
#  Prints out the configuration statistics.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintConfigurationStatistics
{
  my($thorn, $configuration_data) = @_;

  print "          " . "Provides: " . $configuration_data->{"\U$thorn\E PROVIDES"} . ".\n";

  return;
}

1;
