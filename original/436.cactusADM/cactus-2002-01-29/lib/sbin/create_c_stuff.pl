#! /usr/bin/perl
#/*@@
#  @file      create_c_stuff.pl
#  @date      Mon Jan 11 10:53:22 1999
#  @author    Tom Goodale
#  @desc 
#  
#  @enddesc
#  @version $Id: create_c_stuff.pl,v 1.43 2001/10/14 18:18:07 goodale Exp $ 
#@@*/


#/*@@
#  @routine    CreateParameterBindingFile
#  @date       Wed Jan 20 15:20:23 1999
#  @author     Tom Goodale
#  @desc 
#  Creates the bindings used to link the thorn parameters with the flesh.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub CreateParameterBindingFile
{
  my($prefix, $structure, $rhparameters, $rhparameter_db) = @_;
  my($line,@data);
  my(%parameters);
  my($type, $type_string);

  # Header Data
  $line = "\#include <stdio.h>";
  push(@data, $line);
  $line = "\#include <stdlib.h>";
  push(@data, $line);
  $line = "\#include <string.h>";
  push(@data, $line);
  $line = "\#include <stdarg.h>";
  push(@data, $line);
  $line = "\#include \"cctk_Config.h\"";
  push(@data, $line);
  $line = "\#include \"CParameterStructNames.h\"";
  push(@data, $line);
  $line = "\#include \"cctk_Misc.h\"";
  push(@data, $line);   
  $line = "\#include \"ParameterBindings.h\"";
  push(@data, $line);
  push(@data, "");

  # Create the structure

  push(@data,( "struct ", "{"));

  foreach $parameter (&order_params($rhparameters,$rhparameter_db))
  {
    $type = $rhparameter_db->{"\U$rhparameters->{$parameter} $parameter\E type"};
      
    $type_string = &get_c_type_string($type);

    $line = "  " . $type_string ." " .$parameter . ";";

    push(@data, $line);
  }

  # Some compilers don't like an empty structure.
  if((keys %$rhparameters) == 0)
  {
    push(@data, "  int dummy_parameter;");
  }

  push(@data, "} $structure;");

  push(@data, "");

  return @data;
}


#/*@@
#  @routine    get_c_type_string
#  @date       Mon Jan 11 15:33:50 1999
#  @author     Tom Goodale
#  @desc 
#  Returns the correct type string for a parameter
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub get_c_type_string
{
  my($type) = @_;
  my($type_string);


  if($type eq "KEYWORD" ||
     $type eq "STRING"  ||
     $type eq "SENTENCE")
  {
    $type_string = "char *";
  }
  elsif($type eq "BOOLEAN")
  {
    $type_string = "CCTK_INT ";
  } 
  elsif($type eq "INT")
  {
    $type_string = "CCTK_INT ";
  }
  elsif($type eq "INT2")
  {
    $type_string = "CCTK_INT2 ";
  }
  elsif($type eq "INT4")
  {
    $type_string = "CCTK_INT4 ";
  }
  elsif($type eq "INT8")
  {
    $type_string = "CCTK_INT8 ";
  }
  elsif($type eq "REAL")
  {
    $type_string = "CCTK_REAL ";
  }
  elsif($type eq "REAL4")
  {
    $type_string = "CCTK_REAL4 ";
  }
  elsif($type eq "REAL8")
  {
    $type_string = "CCTK_REAL8 ";
  }
  elsif($type eq "REAL16")
  {
    $type_string = "CCTK_REAL16 ";
  }
  else
  {
      $message = "Unknown parameter type '$type'";
      &CST_error(0,$message,"",__LINE__,__FILE__);
  }

  return $type_string;

}

#/*@@
#  @routine    GetThornParameterList
#  @date       Wed Jan 20 15:29:40 1999
#  @author     Tom Goodale
#  @desc 
#  Gets a list of all parameters in a particular block in a thorn.
#  Returns a hash table.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub GetThornParameterList
{
  my($thorn, $block, $rhparameter_db) = @_;
  my(%parameter_list);

  $params = $rhparameter_db->{"\U$thorn $block\E variables"};

  foreach $parameter (split(" ", $params))
  {
    if($parameter =~ m:[^ ]:)
    {
      $parameter_list{$parameter} = $thorn;
    }
  }

  return %parameter_list;
}

sub CreateCStructureParameterHeader
{
  my($prefix, $structure, $rhparameters, $rhparameter_db) = @_;
  my($line,@data);
  my(%parameters);
  my($type, $type_string);
  my(@definition);
  my(@use);

  # Create the structure

  push(@data,("#ifdef __cplusplus", "extern \"C\"", "{", "#endif", ""));
  push(@data,( "extern struct ", "{"));

  foreach $parameter (&order_params($rhparameters, $rhparameter_db))
  {
    $type = $rhparameter_db->{"\U$rhparameters->{$parameter} $parameter\E type"};

    $type_string = &get_c_type_string($type);

    $line = "  ".$type_string ." " .$parameter . ";";

    push(@data, $line);

    $line = "  const $type_string $parameter = $structure.$parameter; \\";

    push(@definition, $line);

    $line = "  cctk_pdummy_pointer = \&$parameter; \\";

    push(@use, $line);
  }

  # Some compilers don't like an empty structure.
  if((keys %$rhparameters) == 0)
  {
    push(@data, "  int dummy_parameter;");
  }

  push(@data, "} $structure;", "");

  push(@data,("#ifdef __cplusplus", "}", "#endif", ""));

  push(@data, "#define DECLARE_$structure"."_PARAMS \\", @definition);
  push(@data, "");
  push(@data, "#define USE_$structure"."_PARAMS \\", @use);

  return @data;
}

sub order_params
{
  my($rhparameters, $rhparameter_db) = @_;
  my(@float_params) = ();;
  my(@int_params)   = ();
  my(@string_params)= ();

  foreach $parameter (sort(keys %$rhparameters))
  {
    $type = $rhparameter_db->{"\U$rhparameters->{$parameter} $parameter\E type"};

    if($type eq "KEYWORD" ||
       $type eq "STRING"  ||
       $type eq "SENTENCE")
    {
      push(@string_params, $parameter);
    }
    elsif($type eq "BOOLEAN" ||
          $type eq "INT")
    {
      push(@int_params, $parameter);
    }
    elsif($type eq "REAL")
    {
      push(@float_params, $parameter);
    }
    else
    {
        $message = "Unknown parameter type '$type'";
        &CST_error(0,$message,__LINE__,__FILE__);
    }
    
  }
  
  return (@float_params, @string_params, @int_params);
}

sub create_parameter_code
{
  my($structure, $implementation,$parameter, $rhparameter_db) = @_;
  my($type, $type_string);
  my($line, @lines);
  my($default);
  my($temp_default);

  $default = $rhparameter_db->{"\U$implementation $parameter\E default"};
  $type = $rhparameter_db->{"\U$implementation $parameter\E type"};

  $type_string = &get_c_type_string($type);

  if($type_string eq "char *")
  {
    $line = "  $structure" .".$parameter = malloc(" 
      . (length($default)-1). "\*sizeof(char));";
    push(@lines, $line);

    $line = "  if($structure.$parameter)";
    push(@lines, $line);

    $line = "    strcpy($structure.$parameter, $default);";
    push(@lines, $line);
  }
  elsif($type eq "BOOLEAN")
  {
    # Logicals need to be done specially.

    # Strip out any quote marks, and spaces at start and end.
    $temp_default = $default;
    $temp_default =~ s:\"::g;
    $temp_default =~ s:\s*$:: ;
    $temp_default =~ s:^\s*:: ;

    $line = "  CCTK_SetLogical(\&($structure.$parameter),\"$temp_default\");";
    push(@lines, $line);
  }
  else
  {
    $line = "  $structure.$parameter = $default;";
    push(@lines, $line);
  }

      $line = "CCTKi_ParameterCreate($parameter, $implementation,
                    \"foobar\",\"" . $rhparameter_db->{"\U$implementation $parameter\E type"}."\"
                    const char *scope,
                    int        steerable,
                    const char *description,
                    const char *defval,
                    void       *data)";


  return @lines;
}
  
1;

