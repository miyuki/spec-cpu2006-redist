#! /usr/bin/perl
#/*@@
#  @file      create_fortran_stuff.pl
#  @date      Tue Jan 12 09:52:35 1999
#  @author    Tom Goodale
#  @desc 
#   
#  @enddesc 
#@@*/

sub CreateFortranThornParameterBindings
{
  my($thorn, $rhparameter_db, $rhinterface_db) = @_;
  my($line);
  my(%these_parameters);
  my($implementation);
  my(@data);
  my(@file);
  my(%alias_names);
  my(%num_aliases);

  push(@file, "#define DECLARE_CCTK_PARAMETERS \\");

  # Generate all global parameters
  %these_parameters = &get_global_parameters($rhparameter_db);

  if((keys %these_parameters) > 0)
  {
    @data = &CreateFortranCommonDeclaration("cctk_params_global", \%these_parameters, $rhparameter_db);

    foreach $line (@data)
    {
      push(@file, "$line&&\\");
    }
  }

  # Generate all restricted parameters of this thorn
  %these_parameters = &GetThornParameterList($thorn, "RESTRICTED", $rhparameter_db);

  if((keys %these_parameters > 0))
  {
    $implementation = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};
    
    @data = &CreateFortranCommonDeclaration("$implementation"."rest", \%these_parameters, $rhparameter_db);

    foreach $line (@data)
    {
      push(@file, "$line&&\\");
    }
  }

  # Generate all private parameters of this thorn
  %these_parameters = &GetThornParameterList($thorn, "PRIVATE", $rhparameter_db);

  if((keys %these_parameters > 0))
  {
    @data = &CreateFortranCommonDeclaration("$thorn"."priv", \%these_parameters, $rhparameter_db);

    foreach $line (@data)
    {
      push(@file, "$line&&\\");
    }
  }

  # Parameters from friends

  # This number can be local to each thorn - it doesn't matter if 
  # members of a common block get different names in different
  # thorns, especially if the variable isn't being used !
  $num_aliases = 0;

  foreach $friend (split(" ",$rhparameter_db->{"\U$thorn\E SHARES implementations"}))
  {

    # Determine which thorn provides this friend implementation
    $rhinterface_db->{"IMPLEMENTATION \U$friend\E THORNS"} =~ m:([^ ]*):;
    
    $friend_thorn = $1;
    
    %these_parameters = &GetThornParameterList($friend_thorn, "RESTRICTED", $rhparameter_db);
    
    %alias_names = ();

    foreach $parameter (sort(keys %these_parameters))
    {
      # Alias the parameter unless it is one we want.
      if(($rhparameter_db->{"\U$thorn SHARES $friend\E variables"} =~ m:( )*$parameter( )*:) && (length($1) > 0)||length($2)>0||$1 eq $rhparameter_db->{"\U$thorn SHARES $friend\E variables"})
      {
        $alias_names{$parameter} = "$parameter";
      }
      else
      {
        $alias_names{$parameter} = "CCTKH".$num_aliases;
        $num_aliases++;
      }
    }

    @data = &CreateFortranCommonDeclaration("$friend"."rest", \%these_parameters, $rhparameter_db, \%alias_names);
      
    foreach $line (@data)
    {
      push(@file, "$line&&\\");
    }

    
  }

  push(@file, ("",""));
  
  return (@file);
}

sub CreateFortranCommonDeclaration
{
  my($common_block, $rhparameters, $rhparameter_db, $rhaliases) = @_;
  my($line,@data);
  my(%parameters);
  my($type, $type_string);
  my($definition);
  my($aliases);

  if(defined $rhaliases)
  {
    $aliases = scalar(keys %$rhaliases);
  }
  else
  {
    $aliases = 0;
  }

  # Create the data

  $definition = "COMMON /$common_block/";

  $sepchar = "";

  foreach $parameter (&order_params($rhparameters,$rhparameter_db))
  {
    $type = $rhparameter_db->{"\U$rhparameters->{$parameter} $parameter\E type"};
      
    $type_string = &get_fortran_type_string($type);

    if($aliases == 0)
    {
      $line = "$type_string $parameter";
    }
    else
    {
      $line = "$type_string $rhaliases->{$parameter}";
    }

    push(@data, $line);

    if($aliases == 0)
    {
      $definition .= "$sepchar$parameter";
    }
    else
    {
      $definition .= "$sepchar$rhaliases->{$parameter}";
    }


    $sepchar = ",";
  }

  push(@data, $definition);

  return @data;
}
  

sub get_fortran_type_string
{
  my($type) = @_;
  my($type_string);


  if($type eq "KEYWORD" ||
     $type eq "STRING"  ||
     $type eq "SENTENCE")
  {
    $type_string = "CCTK_STRING ";
  } 
  elsif($type eq "BOOLEAN" ||
        $type eq "INT")
  {
    $type_string = "CCTK_INT";
  }
  elsif($type eq "REAL")
  {
    $type_string = "CCTK_REAL ";
  }
  else
  {
    $message = "Unknown parameter type '$type'";
    &CST_error(0,$message,"",__LINE__,__FILE__);
  }

  return $type_string;

}

1;
