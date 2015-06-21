#! /usr/bin/perl
#/*@@
#  @file      CreateFunctionBindings.pl
#  @date      Sat Feb 10 2001
#  @author    Gabrielle Allen
#  @desc 
#  
#  @enddesc
#  @version $Id: CreateFunctionBindings.pl,v 1.6 2001/10/14 18:18:04 goodale Exp $ 
#@@*/


#/*@@
#  @routine    CreateFunctionBindings
#  @date       Sat Feb 10 2001
#  @author     Gabrielle Allen
#  @desc 
#  Creates bindings for thorn provided functions
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub CreateFunctionBindings
{
  my($bindings_dir, $rhinterface_db) = @_;
  my($dataout);
  my($function_db);

# Create Function Database
  $function_db = &FunctionDatabase($rhinterface_db);

# Create directories
  if(! -d $bindings_dir)
  {
    mkdir("$bindings_dir", 0755) || die "Unable to create $bindings_dir";
  }
  $start_dir = `pwd`;

  chdir $bindings_dir;

  if(! -d "Functions")
  {
    mkdir("Functions", 0755) || die "Unable to create Functions directory";
  }

  if(! -d "include")
  {
    mkdir("include", 0755) || die "Unable to create include directory";
  }


# Create ThornOverloadables.h
  chdir "include";
  $dataout = &ThornOverloadables($function_db);
  &WriteFile("ThornOverloadables.h",\$dataout);
  chdir $bindings_dir;

# Create DummyThornFunctions.c
  chdir "Functions";
  $dataout = &DummyThornFunctions($function_db);
  &WriteFile("DummyThornFunctions.c",\$dataout);
  chdir $bindings_dir;

# Create OverloadThorns.c
  chdir "Functions";
  $dataout = &OverloadThorns();
  &WriteFile("OverloadThorns.c",\$dataout);
  chdir $bindings_dir;

# Create FortranThornFunctions.c
  chdir "Functions";
  $dataout = &FortranThornFunctions($function_db);
  &WriteFile("FortranThornFunctions.c",\$dataout);
  chdir $bindings_dir;

# Create Thorn Include Prototypes
  chdir "include";
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $filename = $thorn."_Prototypes.h";
    $dataout = &ThornIncludes($thorn,$function_db,$rhinterface_db);
    &WriteFile($filename,\$dataout);
  }
  chdir $bindings_dir;

# Create Master Include Prototypes
  chdir "include";
  $filename = "cctk_FunctionAliases.h";
  $dataout = &ThornMasterIncludes($rhinterface_db);
  &WriteFile($filename,\$dataout);
  chdir $bindings_dir;

# Create THORN_Register.c
  chdir "Functions";
  $registerfiles = "";
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $filename = $thorn."_Register.c";
    $dataout = &RegisterThornFunctions($thorn,$function_db,$rhinterface_db);
    &WriteFile($filename,\$dataout);
    $registerfiles .= " $filename";
  }
  chdir $bindings_dir;

# Create Master registration file RegisterThornFunctions.c
  chdir "Functions";
  $dataout = &RegisterAllFunctions($rhinterface_db);
  &WriteFile("RegisterThornFunctions.c",\$dataout);
  chdir $bindings_dir;

# Create IsOverloaded functions
  chdir "Functions";
  $dataout = &IsOverloadedBindings($function_db);
  &WriteFile("IsOverloaded.c",\$dataout);
  chdir $bindings_dir;

# Create make.code.defn
  chdir "Functions";
  $dataout = "\nSRCS = IsOverloaded.c OverloadThorns.c FortranThornFunctions.c DummyThornFunctions.c RegisterThornFunctions.c $registerfiles\n\n"; 
  &WriteFile("make.code.defn",\$dataout);
  chdir $start_dir;

  return;
}


#/*@@
#  @routine    IsOverloadedBindings
#  @date       Tue Feb 20 2001
#  @author     Gabrielle Allen
#  @desc 
#  Code for returning number of times a function has been overloaded.
#  This should be done in a better way, and include flesh overloaded 
#  functions
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub IsOverloadedBindings
{
  my($function_db) = @_;
  my($dataout,$line,@data);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    IsOverloaded.c\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Query how many times a function is overloaded\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);

  $line = "\#include <stdlib.h>\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_Flesh.h\"\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_FortranString.h\"\n\n";
  push(@data, $line);

  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
    {
      $line = "int CCTKBindings_Overload$function(void *);\n";
      push(@data, $line);
    }
  }

  $line = "int CCTK_IsOverloaded(const char *function);\n";
  push(@data, $line);

  $line = "int CCTK_IsOverloaded(const char *function)\n";
  push(@data, $line);
  $line = "{\n";
  push(@data, $line);
  $line = "  int retval=0;\n\n";
  push(@data, $line);
  $line = "  const char *cctk_dummy_string; /* avoid warnings */\n\n";  
  push(@data, $line);
  $line = "  cctk_dummy_string=function; /* avoid warnings */\n\n";  
  push(@data, $line);

  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
    {
      $line = "  if (strcmp(function,\"$function\")==0)\n";
      push(@data, $line);
      $line = "  {\n";
      push(@data, $line);
      $line = "    retval = CCTKBindings_Overload$function(NULL);\n";
      push(@data, $line);
      $line = "  }\n\n";
      push(@data, $line);
    }
  }

  $line = "  return retval;\n";
  push(@data, $line);
  $line = "}\n\n\n";
  push(@data, $line);

  # Put fortran binding here for the moment
  $line = "void CCTK_FCALL CCTK_FNAME(CCTK_IsOverloaded)\n";
  push(@data, $line);
  $line = "  (int *ret, ONE_FORTSTRING_ARG);\n";
  push(@data, $line);
  $line = "void CCTK_FCALL CCTK_FNAME(CCTK_IsOverloaded)\n";
  push(@data, $line);
  $line = "  (int *ret, ONE_FORTSTRING_ARG)\n";
  push(@data, $line);
  $line = "{\n";
  push(@data, $line);
  $line = "  ONE_FORTSTRING_CREATE(name);\n";
  push(@data, $line);
  $line = "  *ret = CCTK_IsOverloaded(name);\n";
  push(@data, $line);
  $line = "  free(name);\n";
  push(@data, $line);
  $line = "}\n";
  push(@data, $line);
    

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
}


#/*@@
#  @routine    ThornOverloadables
#  @date       Sat Feb 10
#  @author     Gabrielle Allen
#  @desc 
#  Create include file for thorn function overloads
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ThornOverloadables
{
  my($function_db) = @_;
  my($dataout,$line,@data);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    ThornOverloadables.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  The overloadable functions from thorns\n";
  push(@data, $line);
  $line = "  See OverloadMacros.h to see how to use these.\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#ifdef OVERLOADABLE_CALL\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_CALL\n";
  push(@data, $line);
  $line = "\#endif\n\n";
  push(@data, $line);
  $line = "\#ifdef OVERLOABLE_PREFIX\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_PREFIX\n";
  push(@data, $line);
  $line = "\#endif\n\n";
  push(@data, $line);
  $line = "\#ifdef OVERLOABLE_DUMMY_PREFIX\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_DUMMY_PREFIX\n";
  push(@data, $line);
  $line = "\#endif\n\n";
  push(@data, $line);
  $line = "\#define OVERLOADABLE_CALL CCTKBindings_\n";
  push(@data, $line);
  $line = "\#define OVERLOADABLE_PREFIX\n";
  push(@data, $line);
  $line = "\#define OVERLOADABLE_DUMMY_PREFIX CCTKBindings_Dummy\n\n";
  push(@data, $line);
  $line = "#ifdef ARGUMENTS\n";
  push(@data, $line);
  $line = "#undef ARGUMENTS\n";
  push(@data, $line);
  $line = "#endif\n\n";
  push(@data, $line);
  $line = "#ifdef RETURN_TYPE\n";
  push(@data, $line);
  $line = "#undef RETURN_TYPE\n";
  push(@data, $line);
  $line = "#endif\n\n";
  push(@data, $line);

  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
    {
      $line = "\#define ARGUMENTS $function_db->{\"$function CARGS\"}\n";
      push(@data, $line);
      $line = "\#define RETURN_TYPE $function_db->{\"$function RET\"}\n\n";
      push(@data,$line);
      $line = "OVERLOADABLE($function)\n\n";
      push(@data,$line);
      $line = "\#undef ARGUMENTS\n";
      push(@data,$line);
      $line = "\#undef RETURN_TYPE\n\n";
      push(@data,$line);
    }
  }
    
  $line = "#ifdef ARGUMENTS\n";
  push(@data, $line);
  $line = "#undef ARGUMENTS\n";
  push(@data, $line);
  $line = "#endif\n\n";
  push(@data, $line);
  $line = "#ifdef RETURN_TYPE\n";
  push(@data, $line);
  $line = "#undef RETURN_TYPE\n";
  push(@data, $line);
  $line = "#endif\n\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_CALL\n";
  push(@data,$line);
  $line = "\#undef OVERLOADABLE_PREFIX\n";
  push(@data,$line);
  $line = "\#undef OVERLOADABLE_DUMMY_PREFIX\n";
  push(@data,$line);
  
  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
}



#/*@@
#  @routine    ThornMasterIncludes
#  @date       Thu Feb 15 2001
#  @author     Gabrielle Allen
#  @desc 
#  Master file of function prototypes for each thorn
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ThornMasterIncludes
{
  my($rhinterface_db) = @_;
  my($line,@data,$dataout,$thorn);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    cctk_FunctionAliases.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Prototypes for overloaded functions used by all thorn\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#ifndef _CCTK_FUNCTIONALIASES_H_\n";
  push(@data, $line);
  $line = "\#define _CCTK_FUNCTIONALIASES_H_\n\n";
  push(@data, $line);

  $line = "\#ifdef CCODE\n";
  push(@data, $line);
  $line = "int CCTK_IsOverloaded(const char *function);\n\n";
  push(@data, $line);
  $line = "\#endif\n\n";
  push(@data, $line);

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $line = "\#ifdef THORN_IS_$thorn\n";
    push(@data, $line);
    $line = "\#include \"$thorn"."_Prototypes.h\"\n";
    push(@data, $line);
    $line = "\#endif\n\n";
    push(@data, $line);
  }

  $line = "\#endif\n\n";
  push(@data, $line);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
}



#/*@@
#  @routine    OverloadThorns
#  @date       Tue Feb 20 2001
#  @author     Gabrielle Allen
#  @desc 
#  Main file for overloading thorns. Note that the text doesn't change
#  but the contents does depending on the thorn set used. For this reason
#  it is in the bindings and not in the Flesh.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub OverloadThorns
{
  my($line,@data,$dataout);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "\@file      OverloadThornFunctions.c\n";
  push(@data, $line);
  $line = "\@desc \n";
  push(@data, $line);
  $line = "Contains routines to overload thorn functions\n";
  push(@data, $line);
  $line = "Uses the overload macros to make sure of consistency and\n";
  push(@data, $line);
  $line = "to save typing !\n";
  push(@data, $line);
  $line = "\@enddesc\n";
  push(@data, $line);
  $line = "\@\@*/\n\n";
  push(@data, $line);

  $line = "\#include <stdio.h>\n";
  push(@data, $line);
  $line = "\#include <stdlib.h>\n";
  push(@data, $line);
  $line = "\#include <string.h>\n\n";
  push(@data, $line);

  $line = "\#include \"cctk_Flesh.h\"\n";
  push(@data, $line);
  $line = "\#include \"cctk_WarnLevel.h\"\n";
  push(@data, $line);
  $line = "\#include \"OverloadMacros.h\"\n\n";
  push(@data, $line);

  $line = "/* Define the prototypes for the dummy functions. */\n";
  push(@data, $line);
  $line = "\#define OVERLOADABLE(name) OVERLOADABLE_DUMMYPROTOTYPE(name)\n\n";
  push(@data, $line);

  $line = "\#include \"ThornOverloadables.h\"\n\n";
  push(@data, $line);

  $line = "\#undef OVERLOADABLE\n\n";
  push(@data, $line);

  $line = "\#define OVERLOADABLE(name) OVERLOADABLE_FUNCTION(name)\n\n";
  push(@data, $line);

  $line = "\#include \"ThornOverloadables.h\"\n\n";
  push(@data, $line);

  $line = "\#undef OVERLOADABLE\n\n";
  push(@data, $line);

  $line = "\#undef OVERLOADABLE_CALL\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_PREFIX\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE_DUMMY_PREFIX\n\n";
  push(@data, $line);

  $line = "/* Initialising Stuff */\n\n";
  push(@data, $line);

  $line = "void CCTKBindings_SetupThornFunctions(void);\n";
  push(@data, $line);
  $line = "void CCTKBindings_SetupThornFunctions(void)\n";
  push(@data, $line);
  $line = "{\n";
  push(@data, $line);
  $line = "\#undef OVERLOADABLE\n";
  push(@data, $line);
  $line = "\#define OVERLOADABLE(name) OVERLOADABLE_INITIALISE(name)\n";
  push(@data, $line);
  $line = "\#include \"ThornOverloadables.h\"\n";
  push(@data, $line);
  $line = "#undef OVERLOADABLE\n";
  push(@data, $line);
  $line = "}\n\n";
  push(@data, $line);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
}



#/*@@
#  @routine    ThornIncludes.h
#  @date       Thu Feb 15 2001
#  @author     Gabrielle Allen
#  @desc 
#  Create function prototypes for each thorn
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ThornIncludes
{

  my($thorn,$function_db,$rhinterface_db) = @_;
  my($line,@data,$dataout,$function);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    $thorn"."_Prototypes.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Prototypes for overloaded functions used by this thorn\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#ifndef _\U$thorn\E_PROTOTYPES_H_\n";
  push(@data, $line);
  $line = "\#define _\U$thorn\E_PROTOTYPES_H_\n\n";
  push(@data, $line);


  $line = "\#ifdef CCODE\n";
  push(@data, $line);

  foreach $function (split(" ",($rhinterface_db->{"\U$thorn USES FUNCTION\E"})))
  {
    if ($function !~ m:^\s*$:)
    {
        $line = "extern $function_db->{\"$function RET\"} (*$function)($function_db->{\"$function CARGS\"});\n";
        push(@data, $line);
    }
  }

  $line = "\#endif /*CCODE*/\n\n";
  push(@data, $line);

  $line = "\#endif\n\n";
  push(@data, $line);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
}


#/*@@
#  @routine    RegisterAllFunctions
#  @date       Sun Feb 11 2001
#  @author     Gabrielle Allen
#  @desc 
#  Create file to call all thorn function registration
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub RegisterAllFunctions
{
  my($rhinterface_db) = @_;
  my($dataout,$line,@data);

  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    RegisterAllFunctions.c\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Register aliased functions from active thorns\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@returntype int\n";
  push(@data, $line);
  $line = "  \@returndesc \n";
  push(@data, $line);
  $line = "  Minus number of failed overloads\n";
  push(@data, $line);
  $line = "  \@endreturndesc\n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_Flesh.h\"\n";
  push(@data, $line);
  $line = "\#include \"cctk_ActiveThorns.h\"\n\n";
  push(@data, $line);

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $line = "int CCTKBindings_".$thorn."Aliases(void);\n";
    push(@data, $line);
  }
  $line = "int CCTKBindings_RegisterThornFunctions(void);\n\n";
  push(@data, $line);
  $line = "int CCTKBindings_RegisterThornFunctions(void)\n";
  push(@data, $line);
  $line = "{\n";
  push(@data, $line);
  $line = "  int retval = 0;\n";
  push(@data, $line);

  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    $line = "  if (CCTK_IsThornActive(\"$thorn\"))\n";
    push(@data, $line);
    $line = "  {\n";
    push(@data, $line);
    $line = "    retval =+ CCTKBindings_".$thorn."Aliases();\n";
    push(@data, $line);
    $line = "  }\n";
    push(@data, $line);
  }
  $line = "  return retval;\n";
  push(@data, $line);
  $line = "}\n";
  push(@data, $line);

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;

}

#/*@@
#  @routine    DummyThornFunctions
#  @date       Sat Feb 10
#  @author     Gabrielle Allen
#  @desc 
#  Check contents for ThornOverloadables_h
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub DummyThornFunctions
{
  my($function_db) = @_;
  my($dataout,$line,@data);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    DummyThornFunctions.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Dummy functions for overloaded thorn functions\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#include <stdlib.h>\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_Flesh.h\"\n";
  push(@data, $line);
  $line = "\#include \"cctk_WarnLevel.h\"\n\n";
  push(@data, $line);

  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
      {
        $ret = $function_db->{"$function RET"};

        $line = "$ret CCTKBindings_Dummy$function($function_db->{\"$function CARGS\"});\n";
        push(@data, $line);
        $line = "$ret CCTKBindings_Dummy$function($function_db->{\"$function CARGS\"})\n";
        push(@data, $line);
        $line = "{\n";
        push(@data, $line);
        
        # Make sure we use all arguments to avoid warnings
        $line = "  const void *cctk_dummy_pointer;\n";
        push(@data, $line);
        $line = "  cctk_dummy_pointer = cctk_dummy_pointer;\n";
        push(@data, $line);
        foreach $arg (split(",",$function_db->{"$function CARGS"}))
        {
          $arg =~ m:(.*\s+\**)([^\s*\*]+)\s*:;
#         $type=$1;
          $name=$2;
          $line = "  cctk_dummy_pointer = \&$name;\n";
          push(@data, $line);
        }
        $line = "  CCTK_Warn(1,__LINE__,__FILE__,\"Bindings\",\n";
        push(@data, $line);
        $line = "            \"CCTKBindings_Dummy$function: Calling thorn function $function which has not been overloaded\");\n";
        push(@data, $line);
        if ($ret =~ m:INT:i)
        {
          $line = "return -1;";
          push(@data, $line);
        }
        elsif ($ret =~ m:REAL:i)
        {
          $line = "return 0;";
          push(@data, $line);
        }         
        $line = "}\n\n";
        push(@data, $line);
    }      
  }

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return $dataout;
  
}


#/*@@
#  @routine    RegisterThornFunctions
#  @date       Sun Feb 11 2001
#  @author     Gabrielle Allen
#  @desc 
#  Create contents for files to register aliased functions
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub RegisterThornFunctions
{
  my($thorn,$function_db,$rhinterface_db) = @_;
  my($dataout,$line,@data,$function);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    $thorn"."_Register.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Register aliased functions for $thorn\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_Flesh.h\"\n";
  push(@data, $line);

  foreach $function (split(" ",$rhinterface_db->{"\U$thorn PROVIDES FUNCTION\E"}))
  {
    if ($function !~ m:^\s*$:)
    {
      $provided_with = $rhinterface_db->{"\U$thorn PROVIDES FUNCTION\E $function WITH"};
      $ret = $function_db->{"$function RET"};
      $args = $function_db->{"$function CARGS"};

      $line = "$ret $provided_with($args);\n";
      push(@data, $line);
      $line = "int CCTKBindings_Overload$function($ret (* $function)($args));\n";
      push(@data, $line);
    }
  }
  $line = "\n";
  push(@data, $line);
  $line = "int CCTKBindings_".$thorn."Aliases(void);\n";
  push(@data, $line);
  $line = "\n";
  push(@data, $line);
  $line = "int CCTKBindings_".$thorn."Aliases(void)\n";
  push(@data, $line);
  $line = "{\n";
  push(@data, $line);
  $line = "  int retval=0; /* returns minus number of failed overloads */\n";
  push(@data, $line);
  $line = "  int ierr=0;\n\n";
  push(@data, $line);
  $line = "  retval = ierr; /* use ierr to prevent warnings */\n\n";
  push(@data, $line);


  foreach $function (split(" ",$rhinterface_db->{"\U$thorn PROVIDES FUNCTION\E"}))
  {
    if ($function !~ m:^\s*$:)
    {
        $line = "  ierr = CCTKBindings_Overload$function($rhinterface_db->{\"\U$thorn PROVIDES FUNCTION\E $function WITH\"});\n";
        push(@data, $line);
        $line = "  retval = (ierr == 0) ? retval-- : retval;\n";
        push(@data, $line);
    }
  }
  
  $line = "  return retval;\n";
  push(@data, $line);
  $line = "}\n";
  push(@data, $line);
  
  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }

  return ($dataout);
}


#/*@@
#  @routine    FortranThornFunctions
#  @date       Sat Feb 10 2001
#  @author     Gabrielle Allen
#  @desc 
#  Create fortran wrappers for thorn functions
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub FortranThornFunctions
{
  my($function_db) = @_;
  my($dataout,$line,@data,$function);

  # Header Data
  $line = "/*\@\@\n";
  push(@data, $line);
  $line = "  \@header    FortranThornFunctions.h\n";
  push(@data, $line);
  $line = "  \@desc\n";
  push(@data, $line);
  $line = "  Fortran wrappers for overloaded thorn functions\n";
  push(@data, $line);
  $line = "  \@enddesc \n";
  push(@data, $line);
  $line = "  \@\@*/\n\n";
  push(@data, $line);
  $line = "\#include <stdlib.h>\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_Flesh.h\"\n";
  push(@data, $line);
  $line = "\#include \"cctk_WarnLevel.h\"\n\n";
  push(@data, $line);
  $line = "\#include \"cctk_FortranString.h\"\n\n";
  push(@data, $line);

  # Do aliased function prototypes
  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
    {
        $line = "extern $function_db->{\"$function RET\"} (*$function)($function_db->{\"$function CARGS\"});\n";
        push(@data, $line);
    }
  }
  $line = "\n\n";
  push(@data, $line);


  foreach $function (split(" ",$function_db->{"FUNCTIONS"}))
  {
    if ($function !~ m:^\s*$:)
    {
      $line = "$function_db->{\"$function RET\"} CCTK_FCALL CCTK_FNAME($function)\n";
      $line .= "($function_db->{\"$function WARGS\"}";
      if ($function_db->{"$function STRINGS"} == 1)
      {
        $line .= ", ONE_FORTSTRING_ARG";
      }
      elsif ($function_db->{"$function STRINGS"} == 2)
      {
        $line .= ", TWO_FORTSTRINGS_ARGS";
      }
      elsif ($function_db->{"$function STRINGS"} == 3)
      {
        $line .= ", THREE_FORTSTRINGS_ARGS";
      }

      $line .= ")";

      # prototype
      push(@data, "$line;\n");
      # call
      push(@data, "$line\n");
      $line = "{\n";
      push(@data, $line);

      if ($function_db->{"$function TYPE"} =~ "FUNC")
      {
        $line = "  $function_db->{\"$function RET\"} cctki_retval;\n";
        push(@data, $line);
      }

      if ($function_db->{"$function STRINGS"} == 1)
      {
        $line = "ONE_FORTSTRING_CREATE(cctki_string1)\n";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 2)
      {
        $line = "TWO_FORTSTRINGS_CREATE(cctki_string1,cctki_string2)\n";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 3)
      {
        $line = "THREE_FORTSTRINGS_CREATE(cctki_string1,cctki_string2,cctki_string3)\n";
        push(@data, $line);
      }

      if ($function_db->{"$function TYPE"} =~ "FUNC")
      {
        $line = "  cctki_retval = \n";
        push(@data, $line);
      }

      $line = "    $function($function_db->{\"$function WCALL\"}";
      push(@data, $line);

      if ($function_db->{"$function STRINGS"} == 1)
      {
        $line = ", cctki_string1";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 2)
      {
        $line = ", cctki_string1, cctki_string2";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 3)
      {
        $line = ", cctki_string1, cctki_string2, cctki_string3";
        push(@data, $line);
      }

      $line = ");\n";
      push(@data, $line);

      if ($function_db->{"$function STRINGS"} == 1)
      {
        $line = "  free(cctki_string1);\n";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 2)
      {
        $line = "  free(cctki_string1);\n  free(cctki_string2);\n";
        push(@data, $line);
      }
      elsif ($function_db->{"$function STRINGS"} == 3)
      {
        $line = "  free(cctki_string1);\n  free(cctki_string2);\n  free(cctki_string3);\n";
        push(@data, $line);
      }

      if ($function_db->{"$function TYPE"} =~ "FUNC")
      {
        $line = "  return cctki_retval; \n";
        push(@data, $line);
      }

      $line = "}\n";
      push(@data, $line);
    }
  }

  $dataout = "";
  foreach $line (@data)
  {
    $dataout .= $line;
  }
  
  return $dataout;
}

#/*@@
#  @routine    FunctionDatabase
#  @date       Wed Dec 06 11.37
#  @author     Gabrielle Allen
#  @desc 
#  Check consistency for Thorn Functions and create database
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub FunctionDatabase
{
  my($rhinterface_db) = @_;
  my($thorn,$inret,$inargs,$message,$function);

  $function_db->{"FUNCTIONS"}= " ";
  $function_db->{"PROVIDED FUNCTIONS"}= " ";

  # Add used functions to database
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    foreach $function (split(" ",($rhinterface_db->{"\U$thorn USES FUNCTION\E"})))
    {
      $inargs = $rhinterface_db->{"\U$thorn FUNCTION\E $function ARGS"};
      $inret  = $rhinterface_db->{"\U$thorn FUNCTION\E $function RET"};

      ($nstrings,$types,$c,$fortran,$wrappercall,$wrapperargs,$cargs) = &ParseArguments($inret,$inargs);

      if ($function_db->{"FUNCTIONS"} =~ / $function / && $function !~ /^\s*$/)
      {
        if ($types ne $function_db->{"$function TYPES"})
        {
          $message = "Argument types for aliased $function do not match";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
        if ($inret ne $function_db->{"$function RET"})
        {
          $message = "Return types for aliased $function do not match";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
      }
      else
      {
        if ($inret =~ m:^\s*void\s*$:)
        {
          $function_db->{"$function CARGS"} = "SUB";
        }
        else
        {
          $function_db->{"$function CARGS"} = "FUNC";
        }

        if ($c)
        {
          if ($fortran)
          {
            $function_db->{"$function LANG"} = "FC";
          }
          else
          {
            $function_db->{"$function LANG"} = "C";
            $message = "Fortran wrapper not created for alias $function";
            &CST_error(1,$message,"",__LINE__,__FILE__);
          }
        }
        else
        {
          $function_db->{"$function LANG"} = "";
          $message = "Can't create alias for $function";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }         

        if ($rhinterface_db->{"\U$thorn FUNCTION\E $function RET"} eq "void")
        {
          $function_db->{"$function TYPE"} = "SUB";
        }
        else
        {
          $function_db->{"$function TYPE"} = "FUNC";
        }

        $function_db->{"FUNCTIONS"} .= "$function ";
        $function_db->{"$function STRINGS"} = $nstrings;
        $function_db->{"$function CARGS"} = $cargs;
        $function_db->{"$function TYPES"} = $types;
        $function_db->{"$function WARGS"} = $wrapperargs;
        $function_db->{"$function WCALL"} = $wrappercall;
        $function_db->{"$function RET"} = $rhinterface_db->{"\U$thorn FUNCTION\E $function RET"};
      }
    }
  }

  # Check consistency of providing functions
  foreach $thorn (split(" ",$rhinterface_db->{"THORNS"}))
  {
    foreach $function (split(" ",($rhinterface_db->{"\U$thorn PROVIDES FUNCTION\E"})))
    {
      $inargs = $rhinterface_db->{"\U$thorn FUNCTION\E $function ARGS"};
      $inret  = $rhinterface_db->{"\U$thorn FUNCTION\E $function RET"};

      ($nstrings,$types,$c,$fortran,$wrappercall,$wrapperargs,$cargs) = &ParseArguments($inret,$inargs);

      if ($function_db->{"FUNCTIONS"} =~ / $function / && $function !~ /^\s*$/)
      {
        if ($types ne $function_db->{"$function TYPES"})
        {
          $message = "Argument types for aliased $function do not match";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
        if ($inret ne $function_db->{"$function RET"})
        {
          $message = "Return types for aliased $function do not match";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
      }
      $function_db->{"PROVIDED FUNCTIONS"} .= "$function ";
    }
  }

  # Check to see if any functions are potentially used and not provided
  foreach $function (split(" ",($function_db->{"FUNCTIONS"})))
  {
    if ($function_db->{"PROVIDED FUNCTIONS"} !~ / $function /)
    {
      $message = "Aliased function $function is not provided by any thorn";
      &CST_error(1,$message,"",__LINE__,__FILE__);
    }
  }

  return $function_db;
}

#/*@@
#  @routine    ParseArguments
#  @date       Sun Feb 11 2001
#  @author     Gabrielle Allen
#  @desc 
#  Parse the argument list and create versions for C and Fortran
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ParseArguments
{
  my($ret,$args) = @_;
  my($number_args);

#  print "\nParsing Arguments\n";
#  print "=================\n";
#  print "All args: $args\n";

  $fwrapperargs     = "";
  $fwrappercallargs = "";
  $ccallargs        = "";
  $types            = "";
  $number_args  = split(",",$args);

  # Need to count strings for fortran wrappers
  $number_strings = 0;

  # Will be set to zero if can't generate a fortran wrapper
  $fortran = 1;
  # Will be set to zero if can't add the aliased C function
  $c = 1;

  foreach $arg (split(",",$args))
  {

#    print "  Arg is $arg\n";

    # last part is the argument name
    $arg =~ m:(.*)\s+([^\s]+)\s*:;
    $name = $2;
    $type = $1;
    $name =~ s:^\s*::;
    $name =~ s:\s*$::;
    $type =~ s:^\s*::;
    $type =~ s:\s*$::;

    $types = "$types $type";

#    print "    Name is $name\n";
#    print "    Type is $type\n";

    # treat string differently
    
    if ($type =~ m/CCTK_STRING/)
    {
      $number_strings++;
      $ccallargs .= "$type $name, ";
    }
    elsif ($type =~ m/CCTK_INT/ || $type =~ m/CCTK_REAL/)
    {
      if ($number_strings)
      {
        $fortran = 0;
      }
      # look for an array
      if ($type =~ m/^\s*(CCTK_INT??|CCTK_REAL??):ARRAY\s*$/)
      {
        $ccallargs .= "$1 *$name, ";
        $fwrapperargs .= "$1 *$name, ";
        $fwrappercallargs .= "$name, ";
      }
      else
      {
        $ccallargs .= "$type $name, ";
        $fwrapperargs .= "$type *$name, ";
        $fwrappercallargs .= "*$name, ";
      }
    }
    else
    {
      $fortran = 0;
      $c = 0;
      $message = "Error parsing aliased function argument $arg";
      &CST_error(1,$message,"",__LINE__,__FILE__);
    }
  }

  # Remove trailing comma
  $ccallargs =~ s/,\s$//;
  $fwrapperargs =~ s/,\s$//;
  $fwrappercallargs =~ s/,\s$//;

  # Can't do more than three strings for fortran
  if ($number_strings > 3)
  {
    $fortran = 0;
  }

  return ($number_strings,$types,$c,$fortran,$fwrappercallargs,$fwrapperargs,$ccallargs);
}

1;
