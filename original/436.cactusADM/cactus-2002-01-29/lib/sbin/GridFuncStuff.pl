#! /usr/bin/perl
#/*@@
#  @file      GridFuncStuff.pl
#  @date      Tue Jan 12 11:07:45 1999
#  @author    Tom Goodale
#  @desc
#
#  @enddesc
#  @version $Id: GridFuncStuff.pl,v 1.73 2001/12/13 13:29:51 tradke Exp $
#@@*/


#/*@@
#  @routine    CreateVariableBindings
#  @date       Thu Jan 28 15:14:20 1999
#  @author     Tom Goodale
#  @desc
#  Creates all the binding files for the variables.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateVariableBindings
{
  my($bindings_dir, $rhinterface_db, $rhparameter_db) = @_;
  my($thorn, @data);
  my($line, $block, $filelist);

  if(! -d $bindings_dir)
  {
    mkdir("$bindings_dir", 0755) || die "Unable to create $bindings_dir";
  }
  $start_dir = `pwd`;
  chdir $bindings_dir;

  # Create the header files
  if(! -d "include")
  {
    mkdir("include", 0755) || die "Unable to create include directory";
  }
  chdir "include";


  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {

    @data = &CreateThornArgumentHeaderFile($thorn, $rhinterface_db);

    $dataout = "";

    foreach $line (@data)
    {
      $dataout .= "$line\n";
    }

    &WriteFile("$thorn\_arguments.h",\$dataout);
  }

  $dataout  = "/* get the CCTK datatype definitions */\n";
  $dataout .= "#include \"cctk_Types.h\"\n\n";
  $dataout .= "#ifdef CCODE\n";
  $dataout .= "#define CCTK_ARGUMENTS CCTK_CARGUMENTS\n";
  $dataout .= "#define _CCTK_ARGUMENTS _CCTK_CARGUMENTS\n";
  $dataout .= "#define DECLARE_CCTK_ARGUMENTS DECLARE_CCTK_CARGUMENTS\n";
  $dataout .= "#endif\n\n";
  $dataout .= "#ifdef FCODE\n";
  $dataout .= "#define CCTK_ARGUMENTS CCTK_FARGUMENTS\n";
  $dataout .= "#define _CCTK_ARGUMENTS _CCTK_FARGUMENTS\n";
  $dataout .= "#define DECLARE_CCTK_ARGUMENTS DECLARE_CCTK_FARGUMENTS\n";
  $dataout .= "#endif\n\n";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "#ifdef THORN_IS_$thorn\n";
    $dataout .= "#include \"$thorn"."_arguments.h\"\n";
    $dataout .= "#define CCTK_FARGUMENTS \U$thorn"."_FARGUMENTS\n";
    $dataout .= "#define DECLARE_CCTK_FARGUMENTS DECLARE_\U$thorn"."_FARGUMENTS\n";
    $dataout .= "#define CCTK_CARGUMENTS \U$thorn"."_CARGUMENTS\n";
    $dataout .= "#define DECLARE_CCTK_CARGUMENTS DECLARE_\U$thorn"."_CARGUMENTS\n";
    $dataout .= "#define USE_CCTK_CARGUMENTS USE_\U$thorn"."_CARGUMENTS\n";
    $dataout .= "#endif\n\n";
  }

  &WriteFile("cctk_Arguments.h",\$dataout);

  chdir "..";

  if(! -d "Variables")
  {
    mkdir("Variables", 0755) || die "Unable to create Variables directory";
  }
  chdir "Variables";

  $filelist = "BindingsVariables.c";

  $dataout = "";

  $dataout .= "#include \"cctk_ActiveThorns.h\"\n";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "int CactusBindingsVariables_$thorn"."_Initialise(void);\n";
  }

  $dataout .= "\n";

  $dataout .= "int CCTKi_BindingsVariablesInitialise(void);\n\n";

  $dataout .= "int CCTKi_BindingsVariablesInitialise(void)\n{\n";

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout .= "  if(CCTK_IsThornActive(\"$thorn\"))\n  {\n";
    $dataout .= "    CactusBindingsVariables_$thorn"."_Initialise();\n";
    $dataout .= "  }\n";
  }

  $dataout .= "  return 0;\n}\n\n";

  &WriteFile("BindingsVariables.c",\$dataout);

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout = "";
    $dataout .= "\#include \"cctk_Types.h\"\n";
    $dataout .= "\#include \"cctk_WarnLevel.h\"\n";
    $dataout .= "\#include \"cctk_Parameter.h\"\n";
    $dataout .= "\#include \"cctki_Groups.h\"\n";
    $dataout .= "\#include \"cctki_FortranWrappers.h\"\n";
    $dataout .= "int CCTKi_BindingsFortranWrapper$thorn(void *GH, void *fpointer);\n";

    $dataout .= "int CactusBindingsVariables_$thorn"."_Initialise(void);\n";
    $dataout .= "int CactusBindingsVariables_$thorn"."_Initialise(void)\n{\n";
    foreach $block ("PUBLIC", "PROTECTED", "PRIVATE")
    {
      @data = &CreateThornGroupInitialisers($thorn, $block, $rhinterface_db, $rhparameter_db);

      foreach $line (@data)
      {
        $dataout  .= "$line\n";
      }
    }
    $dataout .= "  CCTKi_RegisterFortranWrapper(\"$thorn\", CCTKi_BindingsFortranWrapper$thorn);\n\n";

    $dataout .= "  return 0;\n}\n";

    &WriteFile("$thorn.c",\$dataout);

    $filelist .= " $thorn.c";
  }

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $dataout = "";

    @data = &CreateThornFortranWrapper($thorn);

    foreach $line (@data)
    {
      $dataout .= "$line\n";
    }

    &WriteFile("$thorn\_FortranWrapper.c",\$dataout);
    $filelist .= " $thorn\_FortranWrapper.c";
  }

  $dataout = "SRCS = $filelist\n";
  &WriteFile("make.code.defn",\$dataout);

  chdir $start_dir;
}


#/*@@
#  @routine    GetThornArguments
#  @date       Thu Jan 28 14:31:38 1999
#  @author     Tom Goodale
#  @desc
#  Gets a list of all the variables available for a thorn in a
#  particular block.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/
sub GetThornArguments
{
  my($this_thorn, $block, $rhinterface_db) = @_;
  my(%arguments);
  my(@other_imps);
  my($my_imp);
  my($imp);
  my($thorn, $group, $variable, $vtype, $gtype, $type);

  $my_imp = $rhinterface_db->{"\U$this_thorn IMPLEMENTS"};

  if($block eq "PUBLIC")
  {
    @other_imps = split(" ",$rhinterface_db->{"IMPLEMENTATION \U$my_imp\E ANCESTORS"});
  }
  elsif($block eq "PROTECTED")
  {
    @other_imps = split(" ", $rhinterface_db->{"IMPLEMENTATION \U$my_imp\E FRIENDS"});
  }
  elsif($block eq "PRIVATE")
  {
    @other_imps = ();

  }
  else
  {
    die "Unknown block type $block!!!\n";
  }

#  print "Thorn is $this_thorn, implementation $my_imp, block is $block\n";
#  print "Other imps are @other_imps\n";

  foreach $imp (@other_imps,$my_imp)
  {

    next if (! defined $imp);

    if ($block eq "PRIVATE")
    {
      $thorn = $this_thorn;
    }
    else
    {
      $rhinterface_db->{"IMPLEMENTATION \U$imp\E THORNS"} =~ m:([^ ]*):;

      $thorn = $1;
    }

    foreach $group (split(" ",$rhinterface_db->{"\U$thorn $block GROUPS\E"}))
    {
      $vtype = $rhinterface_db->{"\U$thorn GROUP $group VTYPE\E"};
      $gtype = $rhinterface_db->{"\U$thorn GROUP $group GTYPE\E"};
      $ntimelevels = $rhinterface_db->{"\U$thorn GROUP $group TIMELEVELS\E"};

      $type = "$vtype";

      my $vararraysize = $rhinterface_db->{"\U$thorn GROUP $group\E VARARRAY_SIZE"};
      my $compactgroup = $rhinterface_db->{"\U$thorn GROUP $group\E COMPACT"};

      if($gtype eq "GF" || $gtype eq "ARRAY")
      {
        $type .= " (";

        if(defined($vararraysize) && $compactgroup == 1)
        {
          $type .= "${group}_length";
          $sep = ",";
        }
        else
        {
          $sep = "";
        }

        for($dim =0; $dim < $rhinterface_db->{"\U$thorn GROUP $group DIM\E"}; $dim++)
        {
# FIXME: quick hack to shorten argument names
#          $type .= "${sep}cctkv$group$dim";
          $type .= "${sep}X$group$dim";
          $sep = ",";
          if($block eq "PRIVATE")
          {
# FIXME: quick hack to shorten argument names
#            $arguments{"cctkv$group$dim"} = "(STORAGESIZE($thorn\::$group, $dim))";
            $arguments{"X$group$dim"} = "(STORAGESIZE($thorn\::$group, $dim))";
          }
          else
          {
# FIXME: quick hack to shorten argument names
#            $arguments{"cctkv$group$dim"} = "(STORAGESIZE($imp\::$group, $dim))";
            $arguments{"X$group$dim"} = "(STORAGESIZE($imp\::$group, $dim))";
          }
        }
        if(defined($vararraysize) && $compactgroup == 0)
        {
          $type .= "$sep${group}_length";
        }
        $type .= ")";

        if(defined($vararraysize))
        {
          if($block eq "PRIVATE")
          {
            $arguments{"${group}_length"} = "(GROUPLENGTH($thorn\::$group)";
          }
          else
          {
            $arguments{"${group}_length"} = "(GROUPLENGTH($imp\::$group)";
          }
        }
      }

      if($block eq "PRIVATE")
      {
        $type .= "!$thorn\::$group";
      }
      else
      {
        $type .= "!$imp\::$group";
      }

      $type .="!$ntimelevels";

      if(defined($vararraysize))
      {
        $type .= "![0]";
      }
      else
      {
        $type .= "!";
      }

#      print "Group is $group, resulting type is $type\n";

      foreach $variable (split(" ", $rhinterface_db->{"\U$thorn GROUP $group\E"}))
      {
        $arguments{$variable} = $type;
      }
    }
  }

  return %arguments;
}


#/*@@
#  @routine    CreateFortranArgumentDeclarations
#  @date       Thu Jan 28 14:32:57 1999
#  @author     Tom Goodale
#  @desc
#  Creates the requisite argument list declarations for Fortran.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateFortranArgumentDeclarations
{
  my(%arguments) = @_;
  my($argument);
  my(@declarations) = ();
  my($suffix);

  # Put all storage arguments first.
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} =~ m:STORAGESIZE:)
    {
      push(@declarations, "INTEGER $argument");
    }
    if($arguments{$argument} =~ m:GROUPLENGTH:)
    {
      push(@declarations, "INTEGER $argument");
    }
  }

  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {
    $suffix = "";
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*)!([^!]*):;

      $type        = $1;
      $dimensions  = $2;
      $ntimelevels = $4;

#     print "var $argument - type \"$arguments{$argument}\" - tl $ntimelevels \n";

      for($level = 1; $level <= $ntimelevels; $level++)
      {
        # Modify the name for the time level
        if($level == 1)
        {
          $suffix = "";
        }
        else
        {
          $suffix .= "_p";
        }

        if($type =~ /^(CHAR|BYTE|INT|INT2|INT4|INT8|REAL|REAL4|REAL8|REAL16|COMPLEX|COMPLEX8|COMPLEX16|COMPLEX32)$/)
        {
          # DEPRECATED IN BETA 10
          if($type eq 'CHAR')
          {
            &CST_error(1,"CCTK_CHAR is replaced by CCTK_BYTE, please change your code","",__LINE__,__FILE__);
          }

          push(@declarations, "CCTK_$type $argument$suffix$dimensions");
        }
        else
        {
          &CST_error(0,"Unknown argument type \"$type\"","",__LINE__,__FILE__);
        }
      }
    }
  }

  return @declarations;

}


#/*@@
#  @routine    CreateCArgumentDeclarations
#  @date       Jun 29 1999
#  @author     Tom Goodale, Gabrielle Allen
#  @desc
#  Creates the requisite argument list declarations for C.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentDeclarations
{
  my(%arguments) = @_;
  my($argument);
  my(@declarations) = ();
  my($suffix);
  my($imp);
  my($type, $thorn, $ntimelevels);


  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {
    $suffix = "";
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m\^([^! ]+) ?([^!]*)?!([^!]*)::([^!]*)!([^!]*)!([^!]*)\;

      $type        = $1;
      $thorn       = $3;
      $ntimelevels = $5;
      $varsuffix   = $6;

      for($level = 1; $level <= $ntimelevels; $level++)
      {
        # Modify the name for the time level
        if($level == 1)
        {
          $suffix = "";
        }
        else
        {
          $suffix .= "_p";
        }

        $levelmone=$level-1;
        if($type =~ /^(CHAR|BYTE|INT|INT2|INT4|INT8|REAL|REAL4|REAL8|REAL16|COMPLEX|COMPLEX8|COMPLEX16|COMPLEX32)$/)
        {
          # DEPRECATED IN BETA 10 */
          if($type eq 'CHAR')
          {
            &CST_error(1,"CCTK_CHAR is replaced by CCTK_BYTE, please change your code","",__LINE__,__FILE__);
          }

          push(@declarations, "CCTK_$type *$argument$suffix=(CCTK_$type *)(cctkGH->data[CCTK_VarIndex(\"$thorn\::$argument$varsuffix\")][$levelmone]);");
        }
        else
        {
          &CST_error(0,"Unknown argument type $type","",__LINE__,__FILE__);
        }
      }
    }
  }

  return @declarations;

}

#/*@@
#  @routine    CreateCArgumentUses
#  @date       Nov 5 1999
#  @author     Gabrielle Allen
#  @desc
#  Creates the requisite argument list declarations for C.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentUses
{
  my(%arguments) = @_;
  my($argument);
  my(@declarations) = ();
  my($suffix);
  my($imp);


  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {
    $suffix = "";
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m\^([^! ]+) ?([^!]*)?!([^!]*)::([^!]*)!([^!]*)\;

      $ntimelevels = $5;

      for($level = 1; $level <= $ntimelevels; $level++)
      {
        # Modify the name for the time level
        if($level == 1)
        {
          $suffix = "";
        }
        else
        {
          $suffix .= "_p";
        }

        push(@declarations, "cctk_dummy_pointer = \&$argument$suffix;");

      }
    }
  }

  return @declarations;

}



#/*@@
#  @routine    CreateFortranArgumentList
#  @date       Thu Jan 28 14:33:50 1999
#  @author     Tom Goodale
#  @desc
#  Creates the argument list a Fortran subroutine sees.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateFortranArgumentList
{
  my(%arguments) = @_;
  my($argument);
  my($argumentlist) = "";
  my($sep);

  $sep = "";
  # Put all storage arguments first.
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} =~ m:STORAGESIZE:)
    {
      $argumentlist .= "$sep$argument";
      $sep = ",";
    }
    if($arguments{$argument} =~ m:GROUPLENGTH:)
    {
      $argumentlist .= "$sep$argument";
      $sep = ",";
    }
  }

  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $suffix = "";
      if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
      {
        $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*)!([^!]*):;

        $ntimelevels = $4;

        for($level = 1; $level <= $ntimelevels; $level++)
        {
          # Modify the name for the time level
          if($level == 1)
          {
            $suffix = "";
          }
          else
          {
            $suffix .= "_p";
          }

          $argumentlist .= "$sep$argument$suffix";
          $sep = ",";
        }
      }
    }
  }
  return $argumentlist;
}

#/*@@
#  @routine    CreateCArgumentStatics
#  @date       Thu Jan 28 14:33:50 1999
#  @author     Tom Goodale
#  @desc
#  Creates the declarations of static variables used to speed up
#  construction of arguments to pass to Fortran.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentStatics
{
  my(%arguments) = @_;
  my($argument);
  my(@declarations) = ();
  my($allgroups) = "";
  my($group);

  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      push(@declarations, "static int CCTKARGNUM_$argument = -1");
      $arguments{$argument} =~ /::([^!]+)![0-9]+/;
      $group = $1;

#      print "ARG is $arguments{$argument}, group is $group\n";

      if ($allgroups !~ / $group /)
      {
        $allgroups .= " $group ";
        push(@declarations, "static int CCTKGROUPNUM_$group = -1");
      }
    }
  }

  return @declarations;
}


#/*@@
#  @routine    CreateCArgumentInitialisers
#  @date       Thu Jan 28 14:33:50 1999
#  @author     Tom Goodale
#  @desc
#  Creates the code to initialise the statics.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentInitialisers
{
  my(%arguments) = @_;
  my($argument);
  my(@initialisers) = ();
  my($allgroups) = "";
  my($group);
  my($qualifier);
  my($suffix);

  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m,^([^! ]+) ?([^!]*)?!([^!]*)\::([^!]*)!([^!]*)!([^!]*),;
      $qualifier = $3;
      $varsuffix = $6;

      push(@initialisers, "if(CCTKARGNUM_$argument == -1) CCTKARGNUM_$argument = CCTK_VarIndex(\"$qualifier\::$argument$varsuffix\")");

      $arguments{$argument} =~ /\::([^!]+)/;
      $group = $1;
      if ($allgroups !~ / $group /)
      {
        $allgroups .= " $group ";
        push(@initialisers, "if(CCTKGROUPNUM_$group == -1) CCTKGROUPNUM_$group = CCTK_GroupIndex(\"$qualifier\::$group\")");
      }
    }
  }

  return @initialisers;
}

#/*@@
#  @routine    CreateCArgumentPrototype
#  @date       Thu Jan 28 14:36:25 1999
#  @author     Tom Goodale
#  @desc
#  Creates the prototype needed to call a Fortran function from C.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentPrototype
{
  my(%arguments) = @_;
  my($argument);
  my($prototype) = "";
  my($sep);
  my($type, $ntimelevels);

  $sep = "";

  # Put all storage arguments first.
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} =~ m:STORAGESIZE:)
    {
      $prototype .= "$sep"."const int *";
      $sep = ",";
    }
    if($arguments{$argument} =~ m:GROUPLENGTH:)
    {
      $prototype .= "$sep"."const int *";
      $sep = ",";
    }
  }

  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {

    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*):;

      $suffix = "";
      if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
      {
        $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*)!([^!]*):;

        $type        = $1;
        $ntimelevels = $4;

        for($level = 1; $level <= $ntimelevels; $level++)
        {
          if($type =~ /^(CHAR|BYTE|INT|INT2|INT4|INT8|REAL|REAL4|REAL8|REAL16|COMPLEX|COMPLEX8|COMPLEX16|COMPLEX32)$/)
          {
            $prototype .="$sep". "CCTK_$type *";
            $sep = ',';
          }
          else
          {
            &CST_error(0,"Unknown argument type $type","",__LINE__,__FILE__);
          }
        }
      }
    }
  }
  return $prototype;
}



#/*@@
#  @routine    CreateCArgumentList
#  @date       Thu Jan 28 14:37:07 1999
#  @author     Tom Goodale
#  @desc
#  Creates the argument list used to call a Fortran function from C.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateCArgumentList
{
  my(%arguments) = @_;
  my($argument);
  my($arglist) = "";
  my($sep);
  my($type, $ntimelevels);

  $sep = "";

  # Put all storage arguments first.
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} =~ m/STORAGESIZE\(([^,]*)::([^,]*),\s*(\d+)/)
    {
      $arglist .= "$sep"."(const int *)(CCTKGROUPNUM_$2<0 ? &(_cctk_one) : (CCTK_STORAGESIZE(xGH, $3, \"$1::$2\")))";
      $sep = ",";
    }
    if($arguments{$argument} =~ m/GROUPLENGTH\(([^:]*)::([^)]*)\)/)
    {
      $arglist .= "$sep"."(const int *)(CCTKGROUPNUM_$2<0 ? &(_cctk_one) : (CCTK_GROUPLENGTH(xGH, \"$1::$2\")))";
      $sep = ",";
    }
  }

  # Now deal with the rest of the arguments
  foreach $argument (sort keys %arguments)
  {
    if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
    {
      $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*):;

      $suffix = "";
      if($arguments{$argument} !~ m:STORAGESIZE|GROUPLENGTH:)
      {
        $arguments{$argument} =~ m:^([^! ]+) ?([^!]*)?!([^!]*)!([^!]*):;

        $type        = $1;
        $ntimelevels = $4;

        for($level = 1; $level <= $ntimelevels; $level++)
        {
          if($type =~ /^(CHAR|BYTE|INT|INT2|INT4|INT8|REAL|REAL4|REAL8|REAL16|COMPLEX|COMPLEX8|COMPLEX16|COMPLEX32)$/)
          {
            # DEPRECATED IN BETA 10
            if($type eq 'CHAR')
            {
              &CST_error(1,"CCTK_CHAR is replaced by CCTK_BYTE, please change your code","",__LINE__,__FILE__);
            }

            $arglist .= "$sep"."(CCTK_$type *)(CCTKARGNUM_$argument<0 ? NULL : (xGH)->data[CCTKARGNUM_$argument][$level-1])";
            $sep = ',';
          }
          else
          {
            &CST_error(0,"Unknown argument type $type","",__LINE__,__FILE__);
          }
        }
      }
    }
  }
  return $arglist;
}

#/*@@
#  @routine    CreateThornArgumentHeaderFile
#  @date       Thu Jan 28 14:37:58 1999
#  @author     Tom Goodale
#  @desc
#  Creates all the argument list stuff necessary to call Fortran from C
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateThornArgumentHeaderFile
{
  my($thorn, $rhinterface_db) = @_;
  my($line);
  my(@returndata) = ();
  my(%hasvars);

  # Create the basic thorn block definitions

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {

    %data = &GetThornArguments($thorn, $block, $rhinterface_db);

#    $print_data = 1;
    if ($print_data)
    {
      foreach $arg (keys %data)
      {
        print "$thorn data: $arg : $data{\"$arg\"}\n";
      }
    }
    # Remember if there actually are any arguments here.
    $hasvars{$block} = 1 if(keys %data > 0) ;

    # Do the fortran definitions
    push(@returndata, "#ifdef FCODE");

    # Create the fortran argument declarations

    @data = &CreateFortranArgumentDeclarations(%data);

    push(@returndata, "#define \UDECLARE_$thorn"."_$block"."_FARGUMENTS \\");

    foreach $line (@data)
    {
      push(@returndata, "$line&&\\");
    }

    push(@returndata, ("",""));

    # Create the fortran argument list

    push(@returndata, "#define \U$thorn"."_$block"."_FARGUMENTS \\");

    push(@returndata, &CreateFortranArgumentList(%data));

    push(@returndata, ("",""));

    push(@returndata, "#endif /*FCODE*/");

    push(@returndata, ("",""));


    ##########################################################

    # Do the C definitions
    push(@returndata, "#ifdef CCODE");

    # Create the C argument declarations

    @data = &CreateCArgumentDeclarations(%data);

    push(@returndata, "#define \UDECLARE_$thorn"."_$block"."_CARGUMENTS \\");

    foreach $line (@data)
    {
      push(@returndata, "$line \\");
    }

    push(@returndata, ("",""));

    # Create code to use each C argument variable

    @data = &CreateCArgumentUses(%data);

    push(@returndata, "#define \UUSE_$thorn"."_$block"."_CARGUMENTS \\");

    foreach $line (@data)
    {
      push(@returndata, "$line \\");
    }

    push(@returndata, ("",""));

    # Create the C argument variable number statics

    push(@returndata, "#define \UDECLARE_$thorn"."_$block"."_C2F \\");

    @data = &CreateCArgumentStatics(%data);
    foreach $line (@data)
    {
      push(@returndata, "$line; \\");
    }
    push(@returndata, ("",""));


    # Create the C argument variable number statics initialisers
    push(@returndata, "#define \UINITIALISE_$thorn"."_$block"."_C2F \\");
    @data = &CreateCArgumentInitialisers(%data);
    foreach $line (@data)
    {
      push(@returndata,"$line; \\");
    }

    push(@returndata, ("",""));

    # Create the C argument prototypes
    push(@returndata, "#define \U$thorn"."_$block"."_C2F_PROTO \\");

    push(@returndata, &CreateCArgumentPrototype(%data));

    push(@returndata, ("",""));

    # Create the C argument list
    push(@returndata, "#define \UPASS_$thorn"."_$block"."_C2F(xGH) \\");

    push(@returndata, &CreateCArgumentList(%data));

    push(@returndata, ("",""));

    push(@returndata, "#endif /*CCODE*/");

    push(@returndata, ("",""));

  }

  ################################################################


  # Create the final thorn argument macros

  # Do the Fortran argument lists
  push(@returndata, "#ifdef FCODE");

  $sep = ",";

  push(@returndata, "#define \U$thorn"."_FARGUMENTS _CCTK_FARGUMENTS\\");
  $sep = ",";

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "$sep"."\U$thorn"."_$block"."_FARGUMENTS\\");
      $sep = ",";
    }
  }

  push(@returndata, ("",""));

  # Do the fortran declarations
  push(@returndata, "#define \UDECLARE_$thorn"."_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \\");

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "DECLARE_\U$thorn"."_$block"."_FARGUMENTS \\");
    }
  }

  push(@returndata, ("",""));

  push(@returndata, "#endif /*FCODE*/");

  push(@returndata, ("",""));



  push(@returndata, "#ifdef CCODE");

  # Don't need C arguments

  # Do the C declarations
  push(@returndata, "#define \UDECLARE_$thorn"."_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \\");

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "DECLARE_\U$thorn"."_$block"."_CARGUMENTS \\");
    }
  }

  push(@returndata, ("",""));

  # Do the C declarations
  push(@returndata, "#define \UUSE_$thorn"."_CARGUMENTS _USE_CCTK_CARGUMENTS \\");

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "USE_\U$thorn"."_$block"."_CARGUMENTS \\");
    }
  }

  push(@returndata, ("",""));

  push(@returndata, "#endif /*CCODE*/");

  push(@returndata, ("",""));


  ################################################

  # Do the C definitions
  push(@returndata, "#ifdef CCODE");

  $sep = "";

  # Argument prototypes
  push(@returndata, "#define \U$thorn"."_C2F_PROTO _CCTK_C2F_PROTO\\");
  $sep = ",";

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "$sep"."\U$thorn"."_$block"."_C2F_PROTO\\");
    }
  }

  push(@returndata, ("",""));

  # Argument lists
  $sep = "";

  push(@returndata, "#define PASS_\U$thorn"."_C2F(xGH) _PASS_CCTK_C2F(xGH)\\");
  $sep = ",";

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "$sep"."PASS_\U$thorn"."_$block"."_C2F(xGH)\\");
    }
  }

  push(@returndata, ("",""));

  # Declare statics

  push(@returndata, "#define DECLARE_\U$thorn"."_C2F _DECLARE_CCTK_C2F \\");

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "DECLARE_\U$thorn"."_$block"."_C2F \\");
    }
  }

  push(@returndata, ("",""));

  # Initialise statics

  push(@returndata, "#define INITIALISE_\U$thorn"."_C2F _INITIALISE_CCTK_C2F \\");

  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
  {
    if($hasvars{$block})
    {
      push(@returndata, "INITIALISE_\U$thorn"."_$block"."_C2F \\");
    }
  }

  push(@returndata, ("",""));

  # Dummy C declarations

  push(@returndata, "#define \U$thorn"."_CARGUMENTS cGH *cctkGH ");

#  push(@returndata, "#define \UDECLARE_$thorn"."_CARGUMENTS \\");

#  foreach $block ("PRIVATE", "PROTECTED", "PUBLIC")
#  {
#    if($hasvars{$block})
#    {
#        push(@returndata, "\UDECLARE_$thorn_$block"."_CARGUMENTS \\");
#    }
#  }

  push(@returndata, ("",""));

  push(@returndata, "#endif /*CCODE*/");

  push(@returndata, ("",""));

  return @returndata;
}



#/*@@
#  @routine    CreateThornGroupInitialisers
#  @date       Thu Jan 28 14:38:56 1999
#  @author     Tom Goodale
#  @desc
#  Creates the calls used to setup groups for a particular thorn block.
#  @enddesc
#  @calls
#  @calledby
#  @history
#
#  @endhistory
#
#@@*/

sub CreateThornGroupInitialisers
{
  my($thorn, $block, $rhinterface_db, $rhparameter_db) = @_;
  my($imp);
  my($group, @variables);
  my($line);
  my(@definitions);
  my ($dim,$string,$numsize,$message,$type);

  $imp = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};

  foreach $group (split(" ", $rhinterface_db->{"\U$thorn $block GROUPS"}))
  {

    $type = $rhinterface_db->{"\U$thorn GROUP $group\E GTYPE"};

    # Check consistency for arrays
    if ($type eq "ARRAY")
    {
      $dim = $rhinterface_db->{"\U$thorn GROUP $group\E DIM"};
      $string = $rhinterface_db->{"\U$thorn GROUP $group\E SIZE"};
      &CheckArraySizes($string,$thorn,$rhparameter_db,$rhinterface_db);
      $numsize = ($string =~ s/,//g)+1;
      if ($dim != $numsize)
      {
        $message = "Array dimension $dim doesn't match the $numsize array sizes ";
        $message .= "\n     ($rhinterface_db->{\"\U$thorn GROUP $group\E SIZE\"}) for $group in $thorn";
        $message .= "\n     (Array sizes must be comma separated list of parameters)";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
    }

    @variables = split(" ", $rhinterface_db->{"\U$thorn GROUP $group\E"});

    $line  = "  if (CCTKi_CreateGroup(\"\U$group\",\"$thorn\",\"$imp\",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E GTYPE"} . "\",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E VTYPE"} . "\",\n"
           . "                    \"" . $block . "\",\n"
           . "                    " . $rhinterface_db->{"\U$thorn GROUP $group\E DIM"} . ",\n"
           . "                    " . $rhinterface_db->{"\U$thorn GROUP $group\E TIMELEVELS"} . ",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E STYPE"} . "\",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E DISTRIB"} . "\",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E SIZE"} . "\",\n"
           . "                    \"" . $rhinterface_db->{"\U$thorn GROUP $group\E GHOSTSIZE"} . "\",\n";

    # Is it a vector group ?
    if(defined($rhinterface_db->{"\U$thorn GROUP $group\E VARARRAY_SIZE"}))
    {
      # Check that the size is allowed.
      &CheckArraySizes($rhinterface_db->{"\U$thorn GROUP $group\E VARARRAY_SIZE"},$thorn,$rhparameter_db,$rhinterface_db);
      # Flag Cactus that it is a vector group.
      $line .= "                    -1";
    }
    else
    {
      $line .= "                    " . scalar(@variables);
    }

    foreach $variable (@variables)
    {
      $line .= ",\n                   \"$variable\"";
    }

    # Pass in the size of the GV array, which may be a valid parameter expression
    if(defined($rhinterface_db->{"\U$thorn GROUP $group\E VARARRAY_SIZE"}))
    {
      $line .= ",\n                   \"" . $rhinterface_db->{"\U$thorn GROUP $group\E VARARRAY_SIZE"} . "\"";
    }

    $line  .= ")==1)\n";

    $line  .= "{\n";
    $line  .= "  int param_type;\n";
    $line  .= "  const CCTK_INT *allow_mixeddim_gfs;\n";
    $line  .= "  allow_mixeddim_gfs = (const CCTK_INT *) CCTK_ParameterGet(\"allow_mixeddim_gfs\",\"Cactus\",\&param_type);\n";
    $line  .= "  if (*allow_mixeddim_gfs)\n";
    $line  .= "  {\n";
    $line  .= "    CCTK_VWarn(2,__LINE__,__FILE__,\"Cactus\"\n,";
    $line  .= "    \"CCTKi_CreateGroup: Working dimension already set,\"\n";
    $line  .= "    \" creating GF group $group with different dimension $rhinterface_db->{\"\U$thorn GROUP $group\E DIM\"}\");\n";
    $line  .= "  }\n";
    $line  .= "  else\n";
    $line  .= "  {\n";
    $line  .= "    CCTK_VWarn(0,__LINE__,__FILE__,\"Cactus\"\n,";
    $line  .= "    \"CCTKi_CreateGroup: Working dimension already set,\"\n";
    $line  .= "    \" cannot create GF group $group with dimension $rhinterface_db->{\"\U$thorn GROUP $group\E DIM\"}\");\n";
    $line  .= "  }\n";
    $line  .= "}\n\n";


    push(@definitions, $line);
  }

  return @definitions;

}

sub CreateThornFortranWrapper
{
  my($thorn) = @_;
  my(@data);

  push(@data, "#define THORN_IS_$thorn");
  push(@data, "#include \"cctk.h\"");
  push(@data, "#include \"cctk_Flesh.h\"");
  push(@data, "#include \"cctk_Groups.h\"");
  push(@data, "#include \"cctk_Comm.h\"");
  push(@data, "#include \"cctk_Arguments.h\"");
  push(@data, "");

  push(@data, "int CCTKi_BindingsFortranWrapper$thorn(cGH *GH, void *fpointer);\n");
  push(@data, "int CCTKi_BindingsFortranWrapper$thorn(cGH *GH, void *fpointer)");
  push(@data, "{");
  push(@data, "  void (*function)(\U$thorn\E_C2F_PROTO);");
  push(@data, "");
  push(@data, "  DECLARE_\U$thorn\E_C2F");
  push(@data, "  INITIALISE_\U$thorn\E_C2F");
  push(@data, "");

  push(@data, "  function = (void (*)(\U$thorn\E_C2F_PROTO))fpointer;");
  push(@data, "");
  push(@data, "  function(PASS_\U$thorn\E_C2F(GH));");
  push(@data, "");
  push(@data, "  return 0;");
  push(@data, "");

  push(@data, "}");

  return (@data);
}



#/*@@
#  @routine    CheckArraySizes
#  @date       Thu May 10 2001
#  @author     Gabrielle Allen
#  @desc
#              Arrays sizes must be given as a comma-separated list of
#                - integer contants (no sign character)
#                - parameter names (either fullname or just the basename)
#                  optionally with a "+/-<integer constant>" postfix
#  @enddesc
#@@*/

sub CheckArraySizes
{
  my($size,$thornname,$rhparameter_db,$rhinterface_db) = @_;
  my($par,$thorn,$base);

  foreach $par (split(",",$size))
  {

#     # check for size to be a constant
#     next if $par =~ /^\d+$/;

#     # check for size to be a parameter
#     if ($par =~ /^([A-Za-z]\w*)(::([A-Za-z]\w*))?([+-]\d+)?$/)
#     {
#       if (defined $2)
#       {
#         $thorn = $1;
#         $base = $3;
#       }
#       else
#       {
#         $thorn = $thornname;
#         $base = $1;
#       }

#       # check if the parameter really exists
#       if ($rhparameter_db->{"\U$thorn Private\E variables"} !~ m:$base:i &&
#           $rhparameter_db->{"\U$thorn Global\E variables"} !~ m:$base:i &&
#           $rhparameter_db->{"\U$thorn Restricted\E variables"} !~ m:$base:i)
#       {
#         &CST_error(0,"Array size \'$par\' in $thornname is not a parameter",
#                    "",__LINE__,__FILE__);
#       }
#     }
#     else
#     {
#       &CST_error(0,"Array size \'$par\' in $thornname has invalid syntax",
#                  "",__LINE__,__FILE__);
#     }

    &VerifyParameterExpression($par,$thornname,$rhparameter_db);
  }
    
    
}

#/*@@
#  @routine    VerifyParameterExpression
#  @date       Sat Oct 13 16:40:07 2001
#  @author     Tom Goodale
#  @desc 
#  Does some sanity checking on an arithmetic expression 
#  involving parameter values.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub VerifyParameterExpression
{
  my($expression,$thornname,$rhparameter_db) = @_;
  my($i,$count,@fields);

  # First do some global checks
  if($expression !~ m%^[-+*/a-zA-Z0-9_()]+$%)
  {
    &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "      '$expression' contains invalid characters");
  }

  $count = 0;

  for $i (split(//,$expression))
  {
    $count++ if($i eq "(");
    $count-- if($i eq ")");
    
    if($count < 0)
    {
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
                 .  "        '$expression' has too many closing parentheses",
                "",__LINE__,__FILE__);
    }
  }
  
    if($count > 0)
    {
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
                 .  "        '$expression' has unmatched parentheses",
                 "",__LINE__,__FILE__);
    }
  

  if($expression =~ m:[-+*/]$:)
  {
    &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' ends with an operator",
               "",__LINE__,__FILE__);

  }

  # Now split the string on operators and parentheses
  @fields = split(/([-+*\/()]+)/, $expression);

  for $i (@fields)
  {
    # Get rid of any empty tokens
    next if($i =~ m:^\s*$:);

    # Deal with the easy valid cases

    next if($i =~ m:^[-+/*]\(*$:);
    next if($i =~ m:^\)*[-+/*]$:);
    next if($i =~ m:^\(+$:);
    next if($i =~ m:^\)+$:);
    next if($i =~ m:^\d+$:);

    # Now check if it is a valid parameter name
    if($i =~ m:^([a-zA-Z][a-zA-Z0-9_]*)(\:\:([a-zA-Z][a-zA-Z0-9_]*))?:)
    {
      if (defined $2)
      {
        $thorn = $1;
        $base = $3;
      }
      else
      {
        $thorn = $thornname;
        $base = $1;
      }

      # check if the parameter really exists
      if ($rhparameter_db->{"\U$thorn Private\E variables"} !~ m:$base:i &&
          $rhparameter_db->{"\U$thorn Global\E variables"} !~ m:$base:i &&
          $rhparameter_db->{"\U$thorn Restricted\E variables"} !~ m:$base:i)
      {
        &CST_error(0,"Array size \'$expression\' in $thornname contains a constant which isn\'t a parameter",
                   "",__LINE__,__FILE__);
      }
    }
    elsif($i =~ m:^\(\)$:)
    {
      # Empty parenthesis - bad
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' contains empty parentheses",
               "",__LINE__,__FILE__);
    }
    elsif($i =~ m:[-+/*]{2,}:)
    {
      # Two operators in a row - bad
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' contains two operators in a row",
               "",__LINE__,__FILE__);
    }
    elsif($i =~ m:[-+/*]\):)
    {
      # Operator followed by closing parenthesis - bad
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' has a missing operand",
               "",__LINE__,__FILE__);
    }
    elsif($i =~ m:\([-+/*]:)
    {
      # Opening parenthesis followed by operator - bad
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' has a missing operand",
               "",__LINE__,__FILE__);
    }
    else
    {
      # I've run out of imagination
      &CST_error(0, "Array size in $thornname is an invalid arithmatic expression \n"
               .  "          '$expression' contains unrecognised token '$i'",
               "",__LINE__,__FILE__);
    }
  }

}

1;
