#/*@@
#  @file      CreateScheduleBindings.pl
#  @date      Thu Sep 16 23:30:21 1999
#  @author    Tom Goodale
#  @desc 
#  New schedule stuff.  Should be renamed !!!
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/sbin/CreateScheduleBindings.pl,v 1.22 2001/10/14 18:18:05 goodale Exp $
#@@*/

#/*@@
#  @routine    CreateScheduleBindings
#  @date       Fri Sep 17 14:16:23 1999
#  @author     Tom Goodale
#  @desc 
#  Creates the schedule bindings.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub CreateScheduleBindings
{
  my($bindings_dir, $rhinterface_db, $rhschedule_db) = @_;
  my($start_dir);
  my($thorn);
  my($file_list);
  my($rsbuffer);

  if(! -d $bindings_dir)
  {
    mkdir("$bindings_dir", 0755) || die "Unable to create $bindings_dir";
  }
  $start_dir = `pwd`;

  chdir $bindings_dir;

  if(! -d "Schedule")
  {
    mkdir("Schedule", 0755) || die "Unable to create Schedule directory";
  }

  if(! -d "include")
  {
    mkdir("include", 0755) || die "Unable to create include directory";
  }

  chdir "Schedule";

  $file_list = "";

  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    $rsbuffer = &ScheduleCreateFile($thorn, $rhinterface_db, $rhschedule_db);

    &WriteFile("Schedule$thorn.c",$rsbuffer);

    $file_list .= " Schedule$thorn.c";

    $rsbuffer = &ParameterRecoveryCreateFile($thorn, $rhinterface_db, $rhschedule_db);

    &WriteFile("ParameterRecovery$thorn.c",$rsbuffer);

    $file_list .= " ParameterRecovery$thorn.c";
  }

  $rsbuffer = &ScheduleCreateBindings($rhinterface_db, $rhschedule_db);

  &WriteFile("BindingsSchedule.c",$rsbuffer);

  $file_list .= " BindingsSchedule.c";

  $rsbuffer = &ParameterRecoveryCreateBindings($rhinterface_db, $rhschedule_db);

  &WriteFile("BindingsParameterRecovery.c",$rsbuffer);

  $file_list .= " BindingsParameterRecovery.c";

  $line = "SRCS = $file_list\n";

  &WriteFile("make.code.defn",\$line);
  
  chdir "$start_dir";
}  

#/*@@
#  @routine    ScheduleCreateFile
#  @date       Fri Sep 17 17:34:26 1999
#  @author     Tom Goodale
#  @desc 
#  Creates a string containing all the data which should go into a schedule file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleCreateFile
{
  my($thorn, $rhinterface_db, $rhschedule_db) = @_;

  my($implementation);
  my($buffer, $prototypes);
  my($block, $block_buffer, $block_prototype);
  my($statement, $statement_buffer, $statement_prototype);
  my($indent, $language, $function);
  my(@mem_groups);
  my(@comm_groups);
  my(@trigger_groups);
  my(@before_list);
  my(@after_list);
  my(@while_list);
  my($outfile);
  my($outbuf);

  $implementation = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};
  
  $buffer = $rhschedule_db->{"\U$thorn\E FILE"};
  
  # Process each schedule block
  for($block = 0 ; $block < $rhschedule_db->{"\U$thorn\E N_BLOCKS"}; $block++)
  {
    if ($rhschedule_db->{"\U$thorn\E BLOCK_$block WHERE"} !~ /RECOVER_PARAMETERS/)
    {
      ($block_buffer, $block_prototype) = &ScheduleBlock($thorn, $implementation, $block,
                                                         $rhinterface_db, $rhschedule_db);
      $buffer =~ s:\@BLOCK\@$block:$block_buffer:;
      $prototypes .= "$block_prototype";
    }
    else
    {
      $buffer =~ s:\@BLOCK\@$block::;
    }
  }

  # Process each schedule statement
  for($statement = 0 ; $statement < $rhschedule_db->{"\U$thorn\E N_STATEMENTS"}; $statement++)
  {
    ($statement_buffer, $statement_prototype) = &ScheduleStatement($thorn, $implementation, $statement, 
                                                                   $rhinterface_db, $rhschedule_db);
    $buffer =~ s:\@STATEMENT\@$statement:$statement_buffer:;
    $prototypes .= "$statement_prototype";
  }
    
  # Actually create the string

  # Header stuff
  $outbuf = "";
  $outbuf .=  "\#define THORN_IS_$thorn\n";
  $outbuf .=  "\n";
  $outbuf .=  "\#include <stdarg.h>\n";
  $outbuf .=  "\n";
  $outbuf .=  "\#include \"cctk.h\"\n";
  $outbuf .=  "\#include \"cctk_Parameters.h\"\n";
  $outbuf .=  "\#include \"cctki_ScheduleBindings.h\"\n";
  $outbuf .=  "\n";
  $outbuf .=  "/* Prototypes for functions to be registered. */\n";
  $outbuf .=  "$prototypes\n";
  $outbuf .=  "\n";
  $outbuf .=  "/*\@\@\n";
  $outbuf .=  "  \@routine    CCTKi_BindingsSchedule_$thorn\n";
  $outbuf .=  "  \@date       \n";
  $outbuf .=  "  \@author     \n";
  $outbuf .=  "  \@desc \n";
  $outbuf .=  "  Creates the schedule bindings for thorn $thorn\n";
  $outbuf .=  "  \@enddesc \n";
  $outbuf .=  "  \@calls     \n";
  $outbuf .=  "  \@calledby   \n";
  $outbuf .=  "  \@history \n";
  $outbuf .=  "\n";
  $outbuf .=  "  \@endhistory\n"; 
  $outbuf .=  "\n";
  $outbuf .=  "\@\@*/\n";
  $outbuf .=  "void CCTKi_BindingsSchedule_$thorn(void);\n";
  $outbuf .=  "void CCTKi_BindingsSchedule_$thorn(void)\n";
  $outbuf .=  "{\n";
  $outbuf .=  "  DECLARE_CCTK_PARAMETERS\n";
  $outbuf .=  "$buffer\n";
  $outbuf .=  "return;\n";
  $outbuf .=  "}\n";
  $outbuf .=  "\n";

  return \$outbuf;

}


#/*@@
#  @routine    ParameterRecoveryCreateFile
#  @date       Tue Apr 18 17:34:26 2000
#  @author     Gabrielle Allen
#  @desc 
#  Creates a string containing all the data which should go into a parameter recovery file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParameterRecoveryCreateFile
{
  my($thorn, $rhinterface_db, $rhschedule_db) = @_;

  my($implementation);
  my($buffer, $prototypes);
  my($block, $block_buffer, $block_prototype);
  my($statement, $statement_buffer, $statement_prototype);
  my($indent, $language, $function);
  my(@mem_groups);
  my(@comm_groups);
  my(@trigger_groups);
  my(@before_list);
  my(@after_list);
  my(@while_list);
  my($outfile);
  my($outbuf);

  $implementation = $rhinterface_db->{"\U$thorn\E IMPLEMENTS"};
  
  $buffer = $rhschedule_db->{"\U$thorn\E FILE"};
  
  # Process each schedule block
  for($block = 0 ; $block < $rhschedule_db->{"\U$thorn\E N_BLOCKS"}; $block++)
  {
    if ($rhschedule_db->{"\U$thorn\E BLOCK_$block WHERE"} =~ /RECOVER_PARAMETERS/)
      {
        if($rhschedule_db->{"\U$thorn\E BLOCK_$block LANG"} =~ m:^\s*C\s*$:i )
        {
          $block_buffer = $rhschedule_db->{"\U$thorn\E BLOCK_$block NAME"};
        }
        elsif($rhschedule_db->{"\U$thorn\E BLOCK_$block LANG"} =~ m:^\s*(F|F77|FORTRAN|F90)\s*$:i )
        {
          $block_buffer = "CCTK_FNAME(".$rhschedule_db->{"\U$thorn\E BLOCK_$block NAME"} .")";
        }

      $buffer =~ s:\@BLOCK\@$block:result =  $block_buffer();:;
      $prototypes .= "extern int $block_buffer(void);\n";
    }
    else
    {
      $buffer =~ s:\@BLOCK\@$block::;
    }      
  }

  # Process each schedule statement
  for($statement = 0 ; $statement < $rhschedule_db->{"\U$thorn\E N_STATEMENTS"}; $statement++)
  {
    $buffer =~ s:\@STATEMENT\@$statement::;
  }
    
  # Actually create the string

  # Header stuff
  $outbuf = "";
  $outbuf .=  "\#define THORN_IS_$thorn\n";
  $outbuf .=  "\n";
  $outbuf .=  "\#include <stdarg.h>\n";
  $outbuf .=  "\n";
  $outbuf .=  "\#include \"cctk.h\"\n";
  $outbuf .=  "\#include \"cctk_Parameters.h\"\n";
  $outbuf .=  "\#include \"cctki_ScheduleBindings.h\"\n";
  $outbuf .=  "\n";
  $outbuf .=  "/* Prototypes for functions to be registered. */\n";
  $outbuf .=  "$prototypes\n";
  $outbuf .=  "\n";
  $outbuf .=  "/*\@\@\n";
  $outbuf .=  "  \@routine    CCTKi_BindingsParameterRecovery_$thorn\n";
  $outbuf .=  "  \@date       \n";
  $outbuf .=  "  \@author     \n";
  $outbuf .=  "  \@desc \n";
  $outbuf .=  "  Creates the parameter recovery bindings for thorn $thorn\n";
  $outbuf .=  "  \@enddesc \n";
  $outbuf .=  "  \@calls     \n";
  $outbuf .=  "  \@calledby   \n";
  $outbuf .=  "  \@history \n";
  $outbuf .=  "\n";
  $outbuf .=  "  \@endhistory\n"; 
  $outbuf .=  "\n";
  $outbuf .=  "\@\@*/\n";
  $outbuf .=  "int CCTKi_BindingsParameterRecovery_$thorn(void);\n";
  $outbuf .=  "int CCTKi_BindingsParameterRecovery_$thorn(void)\n";
  $outbuf .=  "{\n";
  $outbuf .=  "  DECLARE_CCTK_PARAMETERS\n";
  $outbuf .=  "  int result = 0;\n\n";
  $outbuf .=  "$buffer\n";
  $outbuf .=  "  return (result);\n";
  $outbuf .=  "}\n";
  $outbuf .=  "\n";

  return \$outbuf;

}



#/*@@
#  @routine    ScheduleCreateBindings
#  @date       Fri Sep 17 18:17:13 1999
#  @author     Tom Goodale
#  @desc 
#  Creates a string containing all the data which should go into the master 
#  schedule bindings file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleCreateBindings
{
  my($rhinterface_db, $rhschedule_db) = @_;

  my($outbuf);

  $outbuf = "";
  $outbuf .=  "\#include \"SKBinTree.h\"\n";
  $outbuf .=  "\#include \"cctk_ActiveThorns.h\"\n";
  $outbuf .=   "\n";
  $outbuf .=  "/* Prototypes for functions to be registered. */\n";

  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    $outbuf .= "void CCTKi_BindingsSchedule_$thorn(void);\n";
  }

  $outbuf .=  "/*\@\@\n";
  $outbuf .=  "  \@routine    CCTKi_BindingsScheduleInitialise\n";
  $outbuf .=  "  \@date       \n";
  $outbuf .=  "  \@author     \n";
  $outbuf .=  "  \@desc \n";
  $outbuf .=  "  Calls all the thorn schedule bindings file if the thorns are active.\n";
  $outbuf .=  "  \@enddesc \n";
  $outbuf .=  "  \@calls     \n";
  $outbuf .=  "  \@calledby   \n";
  $outbuf .=  "  \@history \n";
  $outbuf .=  "\n";
  $outbuf .=  "  \@endhistory\n"; 
  $outbuf .=  "\n";
  $outbuf .=  "\@\@*/\n";
  $outbuf .=  "int CCTKi_BindingsScheduleInitialise(void);\n";
  $outbuf .=  "int CCTKi_BindingsScheduleInitialise(void)\n";
  $outbuf .=  "{\n";
  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    $outbuf .= "  if(CCTK_IsThornActive(\"$thorn\"))\n";
    $outbuf .= "  {\n";
    $outbuf .= "    CCTKi_BindingsSchedule_$thorn();\n";
    $outbuf .= "  }\n";
  }
  $outbuf .=  "  return 0;\n";
  $outbuf .=  "}\n";
  $outbuf .=  "\n";

  return \$outbuf;
}


#/*@@
#  @routine    ParameterRecoveryCreateBindings
#  @date       Tue Apr 18 13:17:13 2000
#  @author     Gabrielle Allen
#  @desc 
#  Creates a string containing all the data which should go into the master 
#  parameter recovery bindings file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParameterRecoveryCreateBindings
{
  my($rhinterface_db, $rhschedule_db) = @_;

  my($outbuf);

  $outbuf = "";
  $outbuf .=  "\#include \"SKBinTree.h\"\n";
  $outbuf .=  "\#include \"cctk_ActiveThorns.h\"\n";
  $outbuf .=   "\n";
  $outbuf .=  "/* Prototypes for functions to be registered. */\n";

  $outbuf .=  "int CCTKi_BindingsParameterRecoveryInitialise(void);\n";
  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    $outbuf .= "int CCTKi_BindingsParameterRecovery_$thorn(void);\n";
  }

  $outbuf .=  "/*\@\@\n";
  $outbuf .=  "  \@routine    CCTKi_BindingsParameterRecoveryInitialise\n";
  $outbuf .=  "  \@date       \n";
  $outbuf .=  "  \@author     \n";
  $outbuf .=  "  \@desc \n";
  $outbuf .=  "  Calls all the thorn parameter recovery bindings file if the thorns are active.\n";
  $outbuf .=  "  \@enddesc \n";
  $outbuf .=  "  \@calls     \n";
  $outbuf .=  "  \@calledby   \n";
  $outbuf .=  "  \@history \n";
  $outbuf .=  "\n";
  $outbuf .=  "  \@endhistory\n"; 
  $outbuf .=  "\n";
  $outbuf .=  "\@\@*/\n";
  $outbuf .=  "int CCTKi_BindingsParameterRecoveryInitialise(void)\n";
  $outbuf .=  "{\n";

  $outbuf .= "  int result;\n";
  $outbuf .= "  int retval = 0;\n\n";
  $outbuf .= "  do\n";
  $outbuf .= "  {\n";
  foreach $thorn (sort split(" ", $rhinterface_db->{"THORNS"}))
  {
    $outbuf .= "    if(CCTK_IsThornActive(\"$thorn\"))\n";
    $outbuf .= "    {\n";
    $outbuf .= "      result = CCTKi_BindingsParameterRecovery_$thorn();\n";
    $outbuf .= "      if (result != 0)\n";
    $outbuf .= "        retval = result;\n";
    $outbuf .= "      if (retval > 0)\n";
    $outbuf .= "        break;\n";
    $outbuf .= "    }\n";
  }
  $outbuf .=  "  } while (0);\n";
  $outbuf .=  "  return retval;\n";
  $outbuf .=  "}\n";
  $outbuf .=  "\n";

  return \$outbuf;
}



#/*@@
#  @routine    ScheduleBlock
#  @date       Fri Sep 17 17:37:59 1999
#  @author     Tom Goodale
#  @desc 
#  Creates the code for a given schedule block
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleBlock
{
  my($thorn, $implementation, $block, $rhinterface_db, $rhschedule_db) = @_;

  my($buffer, $prototype);
  my($indent, $language, $function);
  my(@mem_groups);
  my(@comm_groups);
  my(@trigger_groups);
  my(@sync_groups);
  my(@options);
  my(@before_list);
  my(@after_list);
  my(@while_list);

  # Extract group and routine information from the databases
  @mem_groups = &ScheduleSelectGroups($thorn, $implementation, 
                                      $rhschedule_db->{"\U$thorn\E BLOCK_$block STOR"},
                                      $rhinterface_db);


  @unused_comm_groups = &ScheduleSelectGroups($thorn, $implementation, 
                                              $rhschedule_db->{"\U$thorn\E BLOCK_$block COMM"},
                                              $rhinterface_db);
  if (@unused_comm_groups)
  {
    print "No need to switch on Communication in $thorn\n";
    print "Communication automatically assigned for variables with storage\n";
  }

  @comm_groups = @mem_groups; # Switch on storage for groups with comm

  @trigger_groups = &ScheduleSelectGroups($thorn, $implementation, 
                                          $rhschedule_db->{"\U$thorn\E BLOCK_$block TRIG"},
                                          $rhinterface_db);
                
  @sync_groups = &ScheduleSelectGroups($thorn, $implementation, 
                                          $rhschedule_db->{"\U$thorn\E BLOCK_$block SYNC"},
                                          $rhinterface_db);

  @options = split(/,/, $rhschedule_db->{"\U$thorn\E BLOCK_$block OPTIONS"});

  @before_list = &ScheduleSelectRoutines($thorn, $implementation, 
                                         $rhschedule_db->{"\U$thorn\E BLOCK_$block BEFORE"},
                                         $rhschedule_db);

  @after_list = &ScheduleSelectRoutines($thorn, $implementation, 
                                        $rhschedule_db->{"\U$thorn\E BLOCK_$block AFTER"},
                                        $rhschedule_db);

  @while_list = &ScheduleSelectVars($thorn, $implementation, 
                                    $rhschedule_db->{"\U$thorn\E BLOCK_$block WHILE"},
                                    $rhinterface_db);


  # Start writing out the data 
  if($rhschedule_db->{"\U$thorn\E BLOCK_$block TYPE"} eq "GROUP")
  {
    $prototype = "";
    $buffer = "  CCTKi_ScheduleGroup(";
    $indent = "                     ";
    $language = "";
  }
  elsif($rhschedule_db->{"\U$thorn\E BLOCK_$block TYPE"} eq "FUNCTION")
  {
    if($rhschedule_db->{"\U$thorn\E BLOCK_$block LANG"} =~ m:^\s*C\s*$:i )
    {
      $language = "C";
      $function = $rhschedule_db->{"\U$thorn\E BLOCK_$block NAME"};
    }
    elsif($rhschedule_db->{"\U$thorn\E BLOCK_$block LANG"} =~ m:^\s*(F|F77|FORTRAN|F90)\s*$:i )
    {
      $language = "Fortran";
      $function = "CCTK_FNAME(".$rhschedule_db->{"\U$thorn\E BLOCK_$block NAME"} .")";
    }
    else
    {
      $mess = "Unknown language " .$rhschedule_db->{"\U$thorn\E BLOCK_$block LANG"};
      &CST_error(0,$mess,"",__LINE__,__FILE__);
      return ("", "");
    }
    $prototype = "extern int $function(void); /* Note that this is a cheat, we just need a function pointer. */\n";
    $buffer = "  CCTKi_ScheduleFunction((void *)$function,\n";
    $indent = "                        ";
    $buffer .= "$indent";
  }
  else
  {
    $mess = "Internal error: Unknown schedule block type " . $rhschedule_db->{"\U$thorn\E BLOCK_$block TYPE"};
    &CST_error(0,$mess,"",__LINE__,__FILE__);
    return ("", "");
  }
  
  $buffer .= "\"" . $rhschedule_db->{"\U$thorn\E BLOCK_$block AS"} . "\"" . ",\n";
  $buffer .= $indent . "\"" . $thorn . "\"" . ",\n";
  $buffer .= $indent . "\"" . $implementation . "\"" . ",\n";
  $buffer .= $indent . "\"" . $rhschedule_db->{"\U$thorn\E BLOCK_$block DESCRIPTION"} . "\"" . ",\n";
  $buffer .= $indent . "\"" . $rhschedule_db->{"\U$thorn\E BLOCK_$block WHERE"} . "\"" . ",\n";
  if($language ne "")
  {
    $buffer .= $indent . "\"" . $language . "\"" . ",\n";
  }

  $buffer .= $indent . scalar(@mem_groups) . ",                       /* Number of STORAGE  groups   */\n";
  $buffer .= $indent . scalar(@comm_groups) . ",                      /* Number of COMM     groups   */\n";
  $buffer .= $indent . scalar(@trigger_groups) . ",                   /* Number of TRIGGERS groups   */\n";
  $buffer .= $indent . scalar(@sync_groups) . ",                      /* Number of SYNC     groups   */\n";
  $buffer .= $indent . scalar(@options) . ",                          /* Number of Options           */\n";
  $buffer .= $indent . scalar(@before_list) . ",                      /* Number of BEFORE  routines  */\n";
  $buffer .= $indent . scalar(@after_list) . ",                       /* Number of AFTER   routines  */\n";
  $buffer .= $indent . scalar(@while_list) . "                        /* Number of WHILE   variables */";
  
  foreach $item (@mem_groups, @comm_groups, @trigger_groups, @sync_groups, 
                 @options, @before_list, @after_list, @while_list)
  {
    $buffer .= ",\n" . $indent . "\"" . $item . "\"" ;
  }

  $buffer .= ");\n\n";

  return ($buffer, $prototype);
}

#/*@@
#  @routine    ScheduleStatement
#  @date       Fri Sep 17 17:38:30 1999
#  @author     Tom Goodale
#  @desc 
#  Creates the code for a given schedule statement
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleStatement
{
  my($thorn, $implementation, $statement, $rhinterface_db, $rhschedule_db) = @_;

  my($buffer, $prototype);
  my(@groups);
  my($group);

  # Extract the groups.
  @groups = &ScheduleSelectGroups($thorn, $implementation, 
                                  $rhschedule_db->{"\U$thorn\E STATEMENT_$statement GROUPS"},
                                  $rhinterface_db);

  if($rhschedule_db->{"\U$thorn\E STATEMENT_$statement TYPE"} eq "STOR")
  {
    $function = "CCTKi_ScheduleGroupStorage(";
    $prototype = "";

    foreach $group (@groups)
    {
      $buffer .= "  $function " . "\"" . $group . "\"" . ");\n"
    }

    $function = "CCTKi_ScheduleGroupComm(";
    $prototype = "";

    foreach $group (@groups)
    {
      $buffer .= "  $function " . "\"" . $group . "\"" . ");\n"
    }
  }
  elsif($rhschedule_db->{"\U$thorn\E STATEMENT_$statement TYPE"} eq "COMM")
  {
    print "No need to switch on Communication in $thorn\n";
    print "Communication automatically assigned for variables with storage\n";
  }
  else
  {

    $mess = "Unknown statement type '" .$rhschedule_db{"\U$thorn\E STATEMENT_$statement TYPE"};
    &CST_error(0,$mess,"",__LINE__,__FILE__);
    return ("", "");
  }

  return ($buffer, $prototype);
}


#/*@@
#  @routine    ScheduleSelectGroups
#  @date       Fri Sep 17 17:38:53 1999
#  @author     Tom Goodale
#  @desc 
#  Parses a list of variable groups and selects valid ones.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleSelectGroups
{
  my($thorn, $implementation, $group_list, $rhinterface_db) = @_;
  my(@groups);
  my(@temp_list);
  my($group);
  my($other_imp, $other_thorn, $foundit, $block);

  @temp_list = split(/[,\s\n]+/, $group_list);

  foreach $group (@temp_list)
  {
    next if($group =~ m:^\s*$:);

    $other_imp = "";

    if($group =~ m/(.+)::(.+)/)
    {
      $other_imp=$1;
      $group = $2;

      if(($1 !~ m:^\s*$thorn\s*$:i) && ($1 !~ m:^\s*$implementation\s*$:i))
      {
        # The name has been given completely specified but it isn't this thorn.
      
        if($rhinterface_db->{"IMPLEMENTATION \U$implementation\E ANCESTORS"} =~ m:\b$other_imp\b:i)
        {
          $block = "PUBLIC";
        }
        elsif($rhinterface_db->{"IMPLEMENTATION \U$implementation\E FRIENDS"} =~ m:\b$other_imp\b:i)
        {
          $block = "PROTECTED";
        }
        else
        {
          $mess = "Schedule error: Thorn $thorn - group $other_imp\:\:$group doesn't exist.";
          $help = "Check thorn $thorn inherits from implementation $other_imp";
          &CST_error(0,$mess,$help,__LINE__,__FILE__);
          next;
        }

        $rhinterface_db->{"IMPLEMENTATION \U$other_imp\E THORNS"} =~ m:(\w+):;
        $other_thorn = $1;
      
        if($rhinterface_db->{"\U$other_thorn\E $block GROUPS"} =~ m:\b$group\b:i)
        {
          push(@groups, "$other_imp\::$group");
          next;
        }
        else
        {
          $mess = "Schedule error: Thorn $thorn - group $other_imp\:\:$group doesn't exist.\n";
          &CST_error(0,$mess,"",__LINE__,__FILE__);
          next;
        }       
      }
    }

    if($rhinterface_db->{"\U$thorn\E PRIVATE GROUPS"} =~ m:\b$group\b:i)
    {
      push(@groups, "$thorn\::$group");
    }
    elsif($rhinterface_db->{"\U$thorn\E PROTECTED GROUPS"} =~ m:\b$group\b:i)
    {
      push(@groups, "$implementation\::$group");
    }
    elsif($rhinterface_db->{"\U$thorn\E PUBLIC GROUPS"} =~ m:\b$group\b:i)
    {
      push(@groups, "$implementation\::$group");
    }
    elsif($other_imp eq "")
    {
      $foundit = 0;
      # Check ancestors and friends
      foreach $other_imp (split(" ", $rhinterface_db->{"IMPLEMENTATION \U$implementation\E ANCESTORS"}))
      {
        $rhinterface_db->{"IMPLEMENTATION \U$other_imp\E THORNS"} =~ m:(\w+):;
        $other_thorn = $1;

        if($rhinterface_db->{"\U$other_thorn\E PUBLIC GROUPS"} =~ m:\b$group\b:i)
        {
          push(@groups, "$other_imp\::$group");
          $foundit = 1;
          last;
        }
      }
      if(! $foundit)
      {
        foreach $other_imp (split(" ", $rhinterface_db->{"IMPLEMENTATION \U$implementation\E FRIENDS"}))
        {
          $rhinterface_db->{"IMPLEMENTATION \U$other_imp\E THORNS"} =~ m:(\w+):;
          $other_thorn = $1;

          if($rhinterface_db->{"\U$other_thorn\E PROTECTED GROUPS"} =~ m:\b$group\b:i)
          {
            push(@groups, "$other_imp\::$group");
            $foundit = 1;
            last;
          }
        }
      }
      if(! $foundit)
      {
        $mess = "Schedule error: Thorn $thorn - group $group doesn't exist.";
        $help = "Check $group really is in thorn $thorn. Groups from other thorns ";
        $help .= "need to be specified using \$implementation\:\:\$group and ";
        $help .= "$implementation must be inheritied by your thorn.";
        &CST_error(0,$mess,$help,__LINE__,__FILE__);
        
      }
    }
    else
    {
      $mess = "Schedule error: Thorn $thorn - group $group doesn't exist.";
      &CST_error(0,$mess,"",__LINE__,__FILE__);

    }
  }

  return @groups;
}

#/*@@
#  @routine    ScheduleSelectRoutines
#  @date       Fri Sep 17 17:39:29 1999
#  @author     Tom Goodale
#  @desc 
#  Parses a list of schedule routines/groups.
#  FIXME - should validate
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleSelectRoutines
{
  my($thorn, $implementation, $routine_list, $rhschedule_db) = @_;
  my(@routines);
  my(@temp_list);
  my($routine);

  @temp_list = split(/[,\s\n]+/, $routine_list);

  foreach $routine (@temp_list)
  {
    next if($routine =~ m:^\s*$:);

    push(@routines, $routine);

  }

  return @routines;
}

#/*@@
#  @routine    ScheduleSelectVars
#  @date       Fri Sep 17 17:39:58 1999
#  @author     Tom Goodale
#  @desc 
#  Parses a list of variables
#  FIXME - should validate
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ScheduleSelectVars
{
  my($thorn, $implementation, $var_list, $rhinterface_db) = @_;
  my(@vars);
  my(@temp_list);
  my($var);

  @temp_list = split(/[,\s\n]+/, $var_list);

  foreach $var (@temp_list)
  {
    next if($var =~ m:^\s*$:);

    push(@vars, $var);
  }

  return @vars;
}

1;
