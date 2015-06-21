#/*@@
#  @file      CreateParameterBindings.pl
#  @date      Sun Jul 25 00:52:36 1999
#  @author    Tom Goodale
#  @desc
#  Parameter binding stuff
#  @enddesc
#  @version $Header: /cactus/Cactus/lib/sbin/CreateParameterBindings.pl,v 1.35 2001/08/19 12:52:14 allen Exp $
#@@*/

#/*@@
#  @routine    CreateParameterBindings
#  @date       Thu Jan 28 15:27:16 1999
#  @author     Tom Goodale
#  @desc
#  Create the bindings used for the parameters.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateParameterBindings
{
  my($bindings_dir, $rhparameter_db, $rhinterface_db) = @_;
  my($start_dir);
  my($line);
  my(%these_parameters);
  my($implementation, $thorn);
  my($files);
  my(%routines);
  my($structure, %structures);
  my(%header_files);


  if(! -d $bindings_dir)
  {
    mkdir("$bindings_dir", 0755) || die "Unable to create $bindings_dir";
  }
  $start_dir = `pwd`;

  chdir $bindings_dir;

  if(! -d "Parameters")
  {
    mkdir("Parameters", 0755) || die "Unable to create Parameters directory";
  }

  if(! -d "include")
  {
    mkdir("include", 0755) || die "Unable to create include directory";
  }

  chdir "Parameters";


  # Generate all global parameters
  %these_parameters = &get_global_parameters($rhparameter_db);

  @data = &CreateParameterBindingFile("CCTK_BindingsParametersGlobal", "GLOBAL_PARAMETER_STRUCT", \%these_parameters, $rhparameter_db);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= "$line\n";
  }
  &WriteFile("Global.c",\$dataout);

  $files = "Global.c";
  $structures{"GLOBAL_PARAMETER_STRUCT"} = "cctk_params_global";

  # Generate the global data header file

  chdir "..";
  chdir "include";

  @data = &CreateCStructureParameterHeader("CCTK_BindingsParametersGlobal", "GLOBAL_PARAMETER_STRUCT", \%these_parameters, $rhparameter_db);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= "$line\n";
  }
  $dataout .= "\n\n";

  &WriteFile("ParameterCGlobal.h",\$dataout);

  $header_files{"GLOBAL"} = "ParameterCGlobal.h";

  chdir "..";
  chdir "Parameters";

  # Generate all restricted parameters
  foreach $implementation (split(" ",$rhinterface_db->{"IMPLEMENTATIONS"}))
  {
    $rhinterface_db->{"IMPLEMENTATION \U$implementation\E THORNS"} =~ m:([^ ]+):;

    $thorn = $1;

    %these_parameters = &GetThornParameterList($thorn, "RESTRICTED", $rhparameter_db);

    if((keys %these_parameters > 0))
    {
      @data = &CreateParameterBindingFile("CCTK_BindingsParameters$implementation"."_restricted", "RESTRICTED_\U$implementation\E_STRUCT", \%these_parameters, $rhparameter_db);

      $dataout = "";
      foreach $line (@data)
      {
        $dataout .= "$line\n";
      }
      &WriteFile("\U$implementation\E". "_restricted.c",\$dataout);


      $files .= " \U$implementation\E". "_restricted.c";
      $routines{"CCTK_BindingsParameters$implementation"."_restricted"} = "$implementation";
      $structures{"RESTRICTED_\U$implementation\E_STRUCT"} = "$implementation"."rest";

      # Generate the data header file

      chdir "..";
      chdir "include";

      @data = &CreateCStructureParameterHeader("CCTK_BindingsParameters$implementation"."_restricted", "RESTRICTED_\U$implementation\E_STRUCT", \%these_parameters, $rhparameter_db);


      $dataout = "";
      foreach $line (@data)
      {
        $dataout .= "$line\n";
      }
      $dataout .= "\n\n";
      &WriteFile("ParameterCRestricted\U$implementation\E".".h",\$dataout);

      $header_files{"\U$implementation\E RESTRICTED"} = "ParameterCRestricted\U$implementation\E".".h";

      chdir "..";
      chdir "Parameters";

    }
  }

  # Generate all private parameters
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    %these_parameters = &GetThornParameterList($thorn, "PRIVATE", $rhparameter_db);

    if((keys %these_parameters > 0))
    {
      @data = &CreateParameterBindingFile("CCTK_BindingsParameters$thorn"."_private", "PRIVATE_\U$thorn\E_STRUCT", \%these_parameters, $rhparameter_db);


      $dataout = "";
      foreach $line (@data)
      {
        $dataout .= "$line\n";
      }
      $dataout .= "\n\n";
      &WriteFile("\U$thorn\E"."_private.c",\$dataout);


      $files .= " \U$thorn\E". "_private.c";
      $routines{"CCTK_BindingsParameters$thorn"."_private"} = "$thorn";

      # Generate the data header file

      chdir "..";
      chdir "include";

      @data = &CreateCStructureParameterHeader("CCTK_BindingsParameters$thorn"."_private", "PRIVATE_\U$thorn\E_STRUCT", \%these_parameters, $rhparameter_db);

      $structures{"PRIVATE_\U$thorn\E_STRUCT"} = "$thorn"."priv";


      $dataout = "";
      foreach $line (@data)
      {
        $dataout .= "$line\n";
      }
      $dataout .= "\n\n";
      &WriteFile("ParameterCPrivate\U$thorn\E".".h",\$dataout);


      $header_files{"\U$thorn\E PRIVATE"} = "ParameterCPrivate\U$thorn\E".".h";

      chdir "..";
      chdir "Parameters";

    }
  }


  $dataout = "";
  $dataout .= "\#include <stdio.h>\n";
  $dataout .= "\#include <stdlib.h>\n";
  $dataout .= "\#include <string.h>\n";
  $dataout .= "\#include \"cctk_Config.h\"\n";
  $dataout .= "\#include \"cctk_Misc.h\"\n";
  $dataout .= "\#include \"cctk_WarnLevel.h\"\n";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "extern int CCTKi_BindingsCreate$thorn"."Parameters(void);\n\n";
  }

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "extern int CCTKi_Bindings$thorn"."ParameterExtensions(void);\n\n";
  }

  $dataout .= "int CCTKi_BindingsParametersInitialise(void);\n\n";
  $dataout .= "int CCTKi_BindingsParametersInitialise(void)\n";
  $dataout .= "\{\n\n";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "  CCTKi_BindingsCreate$thorn"."Parameters();\n\n";
  }

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "  CCTKi_Bindings$thorn"."ParameterExtensions();\n\n";
  }

  $dataout .= "return 0;\n";
  $dataout .= "}\n\n";

  &WriteFile("BindingsParameters.c",\$dataout);

  $newfilelist = NewParamStuff($rhparameter_db, $rhinterface_db);

  $dataout = "";
  $dataout .= "SRCS = BindingsParameters.c $files $newfilelist\n";
  &WriteFile("make.code.defn",\$dataout);

  # Create the appropriate thorn parameter headers

  chdir "..";
  chdir "include";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {

    @data = &CreateFortranThornParameterBindings($thorn, $rhparameter_db, $rhinterface_db);

    $dataout = "";
    foreach $line (@data)
    {
      $dataout .= "$line\n";
    }
    &WriteFile("\U$thorn\E"."_FParameters.h",\$dataout);

    $dataout = "";
    $implementation = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};
    $dataout .= "\#ifndef _\U$thorn\E_PARAMETERS_H_\n\n";
    $dataout .= "\#define _\U$thorn\E_PARAMETERS_H_\n\n";

    if($header_files{"GLOBAL"})
    {
        $dataout .= "#include \"". $header_files{"GLOBAL"} ."\"\n";
      }

    if($header_files{"\U$implementation\E RESTRICTED"})
    {
      $dataout .= "#include \"". $header_files{"\U$implementation\E RESTRICTED"}."\"\n";
    }

    if($header_files{"\U$thorn\E PRIVATE"})
    {
      $dataout .= "#include \"".$header_files{"\U$thorn\E PRIVATE"}."\"\n";
    }

    $dataout .= "\n";

    @data = ();
    foreach $friend (split(" ",$rhparameter_db->{"\U$thorn\E SHARES implementations"}))
    {
      $dataout .= "#include \"ParameterCRestricted\U$friend\E.h\"\n";

      $rhinterface_db->{"IMPLEMENTATION \U$friend\E THORNS"} =~ m:([^ ]*):;

      $friend_thorn = $1;

      foreach $parameter (split(" ",$rhparameter_db->{"\U$thorn SHARES $friend\E variables"}))
      {
        $type = $rhparameter_db->{"\U$friend_thorn $parameter\E type"};

        $type_string = &get_c_type_string($type);

        $line = "const $type_string $parameter = RESTRICTED_\U$friend\E_STRUCT.$parameter;";

        push(@data, $line);

      }
    }

    $dataout .= "\n";

    $dataout .= "#define DECLARE_CCTK_PARAMETERS \\\n";

    if($header_files{"GLOBAL"})
    {
      $dataout .= "  DECLARE_GLOBAL_PARAMETER_STRUCT_PARAMS \\\n";
    }

    if($header_files{"\U$implementation\E RESTRICTED"})
    {
      $dataout .= "  DECLARE_RESTRICTED_\U$implementation\E_STRUCT_PARAMS \\\n";
    }

    if($header_files{"\U$thorn\E PRIVATE"})
    {
      $dataout .= "  DECLARE_PRIVATE_\U$thorn\E_STRUCT_PARAMS \\\n";
    }

    foreach $line (@data)
    {
      $dataout .= "  $line \\\n";
    }

    $dataout .= "  const void *cctk_pdummy_pointer;\n\n";

    $dataout .= "#define USE_CCTK_PARAMETERS \\\n";

    if($header_files{"GLOBAL"})
    {
      $dataout .= "  USE_GLOBAL_PARAMETER_STRUCT_PARAMS \\\n";
    }

    if($header_files{"\U$implementation\E RESTRICTED"})
    {
      $dataout .= "  USE_RESTRICTED_\U$implementation\E_STRUCT_PARAMS \\\n";
    }

    if($header_files{"\U$thorn\E PRIVATE"})
    {
      $dataout .= "  USE_PRIVATE_\U$thorn\E_STRUCT_PARAMS \\\n";
    }

    foreach $friend (split(" ",$rhparameter_db->{"\U$thorn\E SHARES implementations"}))
    {
      $rhinterface_db->{"IMPLEMENTATION \U$friend\E THORNS"} =~ m:([^ ]*):;
      foreach $parameter (split(" ",$rhparameter_db->{"\U$thorn SHARES $friend\E variables"}))
      {
        $dataout .= "  cctk_pdummy_pointer = \&$parameter; \\\n";
      }
    }

    $dataout .= "  cctk_pdummy_pointer = cctk_pdummy_pointer;\n\n";

    $dataout .= "#endif\n";
    &WriteFile("\U$thorn\E"."_CParameters.h",\$dataout);

  }

# Write this one to a temporary file and read it back in
# Can probably do this better

  open(OUT, "| perl $cctk_home/lib/sbin/c_file_processor.pl $top/config-data > CParameterStructNames_temp.h") || die "Cannot create CParameterStructNames.h by running c_file_processor.pl";

  foreach $structure (keys %structures)
  {
    print OUT "#define $structure CCTK_FORTRAN_COMMON_NAME($structures{$structure})\n";
  }

  print OUT "\n";

  close OUT;

  open(IN,"<CParameterStructNames_temp.h");
  $dataout = "";
  while (<IN>)
  {
    $dataout .= $_;
  }
  close IN;

  &WriteFile("CParameterStructNames.h",\$dataout);


  $dataout = "";
  $dataout .= "#include \"CParameterStructNames.h\"\n\n";
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "\#ifdef THORN\_IS\_$thorn\n";
    $dataout .= "\#include \"\U$thorn\E"."\_CParameters.h\"\n";
    $dataout .= "\#endif\n\n";
  }
  &WriteFile("CParameters.h",\$dataout);

  $dataout = "";
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "\#ifdef THORN_IS\_$thorn\n";
    $dataout .= "\#include \"\U$thorn\E"."\_FParameters.h\"\n";
    $dataout .= "\#endif\n\n";
  }
  &WriteFile("FParameters.h",\$dataout);

  $dataout  = "/* get the CCTK datatype definitions */\n";
  $dataout .= "#include \"cctk_Types.h\"\n\n";
  $dataout .= "#ifdef CCODE\n";
  $dataout .= "#include \"CParameters.h\"\n";
  $dataout .= "#endif\n\n";
  $dataout .= "#ifdef FCODE\n";
  $dataout .= "#include \"FParameters.h\"\n";
  $dataout .= "#endif\n\n";
  &WriteFile("cctk_Parameters.h",\$dataout);


  chdir $start_dir;


}

sub NewParamStuff
{
  my($rhparameter_db, $rhinterface_db) = @_;
  my($line);
  my(%these_parameters);
  my($implementation, $thorn);
  my($files);
  my(%routines);
  my($structure, %structures);
  my(%header_files);
  my($block);
  my($filelist);
  my(@creationdata);
  my(@extensiondata);
  my(@data);

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $imp = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};

    push(@data, "#include <stdarg.h>");
    push(@data, "");
    push(@data, "#include \"cctk_Config.h\"");
    push(@data, "#include \"cctk_Constants.h\"");
    push(@data, "#include \"ParameterBindings.h\"");

    push(@data, "#include \"CParameterStructNames.h\"");

    foreach $block ("GLOBAL", "RESTRICTED", "PRIVATE")
    {
      %these_parameters = &GetThornParameterList($thorn, $block, $rhparameter_db);

      if((keys %these_parameters > 0))
      {
        if($block eq "GLOBAL")
        {
          push(@data, "#include \"ParameterCGlobal.h\"");
        }
        elsif($block eq "RESTRICTED")
        {
          push(@data, "#include \"ParameterCRestricted\U$imp\E.h\"");
        }
        elsif($block eq "PRIVATE")
        {
          push(@data, "#include \"ParameterCPrivate\U$thorn\E.h\"");
        }
        else
        {
          die "Internal error";
        }

#        print "Generating $block parameters for $thorn, providing $imp\n";
        push(@creationdata,&CreateParameterRegistrationStuff($block, $thorn, $imp, $rhparameter_db, %these_parameters));
      }
    }


    # Now the parameter extensions
#    print $rhparameter_db->{"\U$thorn\E SHARES implementations"} . "\n";

    foreach $block (split(" ",$rhparameter_db->{"\U$thorn\E SHARES implementations"}))
    {

      push(@data, "#include \"ParameterCRestricted\U$block\E.h\"");

#      print "Generating $block extension from $thorn\n";
      push(@extensiondata,&CreateParameterExtensionStuff($block, $thorn, $rhparameter_db));

    }

    push(@data, "");
    push(@data, "int CCTKi_BindingsCreate$thorn"."Parameters(void);\n");
    push(@data, "int CCTKi_BindingsCreate$thorn"."Parameters(void)");
    push(@data, "{");

    push(@data, @creationdata);

    push(@data, "  return 0;");
    push(@data, "}");

    push(@data, "");
    push(@data, "int CCTKi_Bindings$thorn"."ParameterExtensions(void);\n");
    push(@data, "int CCTKi_Bindings$thorn"."ParameterExtensions(void)");
    push(@data, "{");

    push(@data, @extensiondata);

    push(@data, "  return 0;");
    push(@data, "}");

    $dataout = "";
    foreach $line (@data)
    {
      $dataout .= "$line\n";
    }
    &WriteFile("Create$thorn"."Parameters.c",\$dataout);

    @data=();
    @creationdata=();
    @extensiondata=();

    $filelist .= " Create$thorn"."Parameters.c";
  }

  return $filelist;
}

sub CreateParameterRegistrationStuff
{
  my($block, $thorn, $imp, $rhparameter_db, %these_parameters) = @_;
  my(@data);
  my($line);
  my($structure, $type, $n_ranges);

  if($block eq "GLOBAL")
  {
    $structure="GLOBAL_PARAMETER_STRUCT";
  }
  elsif($block eq "RESTRICTED")
  {
    $structure="RESTRICTED_\U$imp\E_STRUCT";
  }
  elsif($block eq "PRIVATE")
  {
    $structure = "PRIVATE_\U$thorn\E_STRUCT";
  }
  else
  {
    die "Internal error";
  }

#  print "Thorn is $thorn\n";
#  print "Structure is $structure\n";

  foreach $parameter (sort keys %these_parameters)
  {

#    print "This param is $parameter\n";

    $type = $rhparameter_db->{"\U$thorn $parameter\E type"};

#    print "Type is $type\n";

    $n_ranges = $rhparameter_db->{"\U$thorn $parameter\E ranges"};

#    print "N_ranges is $n_ranges\n";

    $quoted_default = $rhparameter_db->{"\U$thorn $parameter\E default"};

#    $quoted_default =~ s:\"::g;  The database now strips all unescaped quotes.

    # Set steerable details
    $steerable = $rhparameter_db->{"\U$thorn $parameter\E steerable"};
    if ($steerable =~ /never/i || $steerable =~/^$/)
    {
      $steerable_type = "CCTK_STEERABLE_NEVER";
    }
    elsif ($steerable =~ /always/i)
    {
      $steerable_type = "CCTK_STEERABLE_ALWAYS";
    }
    elsif ($steerable =~ /recover/i)
    {
      $steerable_type = "CCTK_STEERABLE_RECOVER";
    }
    else
    {
      $message = "Illegal steerable type ($steerable) for parameter $parameter in $thorn";
      &CST_error(0,$message,"",__LINE__,__FILE__);
    }

    $line="  CCTKi_ParameterCreate(\"$parameter\", /* The parameter name */\n".
          "                        \"$thorn\",     /* The thorn          */\n".
          "                        \"$type\",       /* The parameter type*/\n".
          "                        \"$block\",     /* The scoping block  */\n".
          "                  $steerable_type,              /* Is it steerable ?  */\n".
          "                  " . $rhparameter_db->{"\U$thorn $parameter\E description"} . ", /* The description */\n" .
          "                  \"" . $quoted_default . "\",  /* The default value */\n" .
          "                  &($structure.$parameter),   /* The actual data pointer */\n".
          "                  $n_ranges       /* How many allowed ranges it has */";

    for($range=1; $range <= $n_ranges; $range++)
    {
      $quoted_range = $rhparameter_db->{"\U$thorn $parameter\E range $range range"};
      $range_description = $rhparameter_db->{"\U$thorn $parameter\E range $range description"};

      if($range_description !~ m:\":)
      {
        $range_description = "\"$range_description\"";
      }

      $range_description =~ s:,$::;

      #$quoted_range =~ s:\":\\\":g;
      $quoted_range =~ s:\"::g;
      $quoted_range =~ s:^\s*::;
      $quoted_range =~ s:\s*$::;

      # escape all backslashes so that they aren't treated
      # as the beginning of an escape sequence in C strings
      $quoted_range =~ s:\\:\\\\:g;

      $line .= ",\n                  \"".$quoted_range."\", $range_description";

    }

    $line .=");\n";

    push(@data, $line);
  }


  return @data;
}

sub CreateParameterExtensionStuff
{
  my($block, $thorn, $rhparameter_db) = @_;
  my(@data);
  my($line);
  my($structure, $type, $n_ranges, $range, $quoted_range, $range_description);

#  print "Extending $block from $thorn\n";

  foreach $parameter (split(" ",$rhparameter_db->{"\U$thorn\E SHARES \U$block\E variables"}))
  {
    $n_ranges = $rhparameter_db->{"\U$thorn $parameter\E ranges"};

    for($range=1; $range <= $n_ranges; $range++)
    {
      $quoted_range = $rhparameter_db->{"\U$thorn $parameter\E range $range range"};
      $range_description = $rhparameter_db->{"\U$thorn $parameter\E range $range description"};

      if($range_description !~ m:\":)
      {
        $range_description = "\"$range_description\"";
      }

      #$quoted_range =~ s:\":\\\":g;
      $quoted_range =~ s:\"::g;
      $quoted_range =~ s:^\s*::;
      $quoted_range =~ s:\s*$::;

      push(@data, "  CCTKi_ParameterAddRange(\"$block\",");
      push(@data, "                          \"$parameter\",");
      push(@data, "                          \"$thorn\",");
      push(@data, "                          \"$quoted_range\",");
      push(@data, "                          $range_description);");
      push(@data, "");


#      print "Adding \"$quoted_range\" to $parameter\n";


    }

  }

  return @data;
}

1;
