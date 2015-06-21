#! /usr/bin/perl -s
#/*@@
#  @file      configure.pl
#  @date      Fri Jan  8 15:06:22 1999
#  @author    Tom Goodale
#  @desc 
#  Perl configuration script for the CCTK.
#  Does extra work it would be awkward to do from within the normal
#  autoconf stuff (or at least that I'm too lazy to do 8-)
#  @enddesc 
#@@*/

$tmphome = shift(@ARGV);

print "Determining number of fortran underscores...\n";

($retcode,$data) = test_fortran_name();
push(@routines, $data); 
# Some compilers do something really strange with common blocks.
($retcode,$data) = test_fortran_common_name();
push(@routines, $data); 
push(@routines, "1;");

# Create the perl module to map the fortran names.
open(OUT, ">$tmphome/fortran_name.pl") || die "Cannot create fortran_name.pl\n";

foreach $line (@routines)
{
  print OUT "$line\n";
}

close(OUT);

if ($retcode > 0)
{
  print "Fortran compilation failed ...\n";
  print "COMPILATION WILL FAIL WITH ANY FORTRAN SOURCE CODE\n";
}

sub test_fortran_name
{
  local($data);
  local($use_f77,$use_f90);
  local($retcode, $line, $name, $case, $n_underscores);
  local($underscore_suffix, $normal_suffix, $case_prefix);

  $use_f77 = 0;
  $use_f90 = 0;

  if($compiler_f77 && $compiler_f77 ne "" && $compiler_f77 !~ /none/)
  {
    ($retcode,$case, $n_underscores) = &compile_fortran_name($compiler_f77,$opts_f77);
    if ($retcode <= 0) 
    {
      $use_f77 = 1
    };
  }
  elsif ($compiler_f90 && $compiler_f90 ne "" && $compiler_f90 !~ /none/)
  {
    ($retcode,$case, $n_underscores) = &compile_fortran_name($compiler_f90,$opts_f90);
    if ($retcode <= 0) 
    {
      $use_f90 = 1
    };
  }
  
  if($use_f90 || $use_f77)
  {
    # Determine the case and number of underscores
    ($underscore_suffix, $normal_suffix, $case_prefix) = &determine_transformation($n_underscores, $case);

    $data =  "
sub fortran_name
{
  local(\$old_name) = \@_;
  local(\$new_name);

  \$new_name = \"$case_prefix\$old_name\\E\";
  
  if(\$new_name =~ m:_: ) 
  {
    \$new_name = \$new_name.\"$underscore_suffix\";
  }
  else
  {
    \$new_name = \$new_name.\"$normal_suffix\";
  }

  return \$new_name;
}

";
  }
  else
  {
    if ($retcode <= 0)
    {
      print "No Fortran compiler - creating null fortran name conversion routine.\n";
    }
    else
    {
      print "Creating null fortran name conversion routine\n";
    }
$data = "

sub fortran_name
{
  local(\$old_name) = \@_;

  return \"\\L\$old_name\";

}
";
  }

  return ($retcode,$data);
}


sub test_fortran_common_name
{
  local($data);
  local($retcode, $line, $name, $case, $n_underscores);
  local($underscore_suffix, $normal_suffix, $case_prefix);

  $use_f77 = 0;
  $use_f90 = 0;

  if($compiler_f77 && $compiler_f77 ne "" && $compiler_f77 !~ /none/)
  {
    ($retcode,$case, $n_underscores) = &compile_fortran_common_name($compiler_f77,$opts_f77);
    if ($retcode <=0) {$use_f77 = 1};
  }
  elsif ($compiler_f90 && $compiler_f90 ne "" && $compiler_f90 !~ /none/)
  {
    ($retcode,$case, $n_underscores) = &compile_fortran_common_name($compiler_f90,$opts_f90);
    if ($retcode<=0) {$use_f90 = 1};
  }
  
  if($use_f90 || $use_f77)
  {
    # Determine the case and number of underscores
    ($underscore_suffix, $normal_suffix, $case_prefix) = &determine_transformation($n_underscores, $case);

    $data =  "
sub fortran_common_name
{
    local(\$old_name) = \@_;
    local(\$new_name);

    \$new_name = \"$case_prefix\$old_name\\E\";

    if(\$new_name =~ m:_: ) 
    {
	\$new_name = \$new_name.\"$underscore_suffix\";
    }
    else
    {
	\$new_name = \$new_name.\"$normal_suffix\";
    }

    return \"$prefix\".\$new_name;
}

";

  }
  else
  {
    if ($retcode <= 0)
    {
      print "No Fortran compiler - creating null fortran common name conversion routine.\n";
    }
    else
    {
      print "Creating null fortran common name conversion routine.\n";
    }
    $data = "

sub fortran_common_name
{
  local(\$old_name) = \@_;

  return \"\\L\$old_name\";

}
";
}

  return ($retcode,$data);
}

sub determine_transformation
{
  local ($n_underscores, $case) = @_;

  if($n_underscores == 0)
  {
    $normal_suffix = "";
    $underscore_suffix = "";
  }

  if($n_underscores == 1)
  {
    $normal_suffix = "_";
    $underscore_suffix = "_";
  }

  if($n_underscores == 2)
  {
    $normal_suffix = "_";
    $underscore_suffix = "__";
  }

  if($case == 0)
  {
    $case_prefix = "\\L";
  }
  if($case == 1)
  {
    $case_prefix = "\\U";
  }

  return ($underscore_suffix, $normal_suffix, $case_prefix);
}


sub compile_fortran_common_name
{
  local($compiler,$opts) = @_;
  local($data);
  local($retcode, $line, $name, $case, $n_underscores);
  local($underscore_suffix, $normal_suffix, $case_prefix);

  # Create a test file
  open(OUT, ">fname_test.f") || die "Cannot open fname_test.f\n";

  print OUT <<EOT;
      subroutine test_name
      real b
      common /test_common/b
      b = 2.0
      return
      end

EOT

  close OUT;

  # Compile the test file
  print "Compiling test file with $compiler $opts ...\n";
  system("$compiler $opts -c fname_test.f");
  
  $retcode = $? >> 8;
  
  if($retcode > 0)
  {
    print "Failed to compile fname_test.f\n";
  }
  else
  {
  
    # Search the object file for the appropriate symbols
    open(IN, "<fname_test.o") || open(IN, "<fname_test.obj") || die "Cannot open fname_test.o\n";
    
    while(<IN>)
    {
      $line = $_;
      if($line =~ m:(_[\w_]*)?(TEST_COMMON)(_*):i)
      {
	$prefix = $1;
	$name = $2;
	$underscores = $3;
      
	# This is a pain.  If all symbols have underscores, need to remove
	# the first one here.
      
	if($symbols_preceeded_by_underscores)
	{
	  if($prefix =~ m:^_(.*):)
	  {
	    $prefix = $1;
	  }
	}

	if($name =~ m:TEST_COMMON:)
	{
	  print "Uppercase - ";
	  $case = 1;
	}
	if($name =~ m:test_common:)
	{
	  print "Lowercase - ";
	  $case = 0;
	}
	if($underscores eq "")
	{
	  print " No trailing underscore\n";
	  $n_underscores = 0;
	}
	if($underscores eq "_")
	{
	  print "One trailing underscore\n";
	  $n_underscores = 1;
	}
	if($underscores eq "__")
	{
	  print "Two trailing underscores\n";
	  $n_underscores = 2;
	}

	last;
      }
    }
    
    close IN;
  }    
  # Delete the temporary files
  unlink <fname_test.*>;

  return ($retcode,$case,$n_underscores);
}


sub compile_fortran_name
{
  local($compiler,$opts) = @_;
  local($data);
  local($retcode, $line, $name, $case, $n_underscores);
  local($underscore_suffix, $normal_suffix, $case_prefix);

  # Create a test file
  open(OUT, ">fname_test.f") || die "Cannot open fname_test.f\n";

  print OUT <<EOT;
      subroutine test(a)
      integer a
      a = 1
      call test_name(a)
      return
      end

EOT
  
  close OUT;
  
  # Compile the test file
  print "Compiling test file with $compiler_f77 $opts_f77 ...\n";
  system("$compiler_f77 $opts_f77 -c fname_test.f");
  
  $retcode = $? >> 8;
  
  if($retcode > 0)
  {
    print "Failed to compile fname_test.f\n";
  }
  else
  {  
  
    # Search the object file for the appropriate symbols
    open(IN, "<fname_test.o") || open(IN, "<fname_test.obj") || die "Cannot open fname_test.o\n";
    
    while(<IN>)
    {
      $line = $_;
      if($line =~ m:(TEST_NAME)(_*):i)
      {
	$name = $1;
	$underscores = $2;

	# Extremely quick hack to sort out problems later on with common block
	# names.
	
	if($_ =~ m:_TEST_NAME:i)
	{
	  $symbols_preceeded_by_underscores=1;
	}
	else
	{
	  $symbols_preceeded_by_underscores=0;
	}

	# Find out suffices.
	if($name =~ m:TEST_NAME:)
	{
	  print "Uppercase - ";
	  $case = 1;
	}
	if($name =~ m:test_name:)
	{
	  print "Lowercase - ";
	  $case = 0;
	}
	if($underscores eq "")
	{
	  print " No trailing underscore\n";
	  $n_underscores = 0;
	}
	if($underscores eq "_")
	{
	  print "One trailing underscore\n";
	  $n_underscores = 1;
	}
	if($underscores eq "__")
	{
	  print "Two trailing underscores\n";
	  $n_underscores = 2;
	}

	last;
      }
    }
    
    close IN;

  }

  # Delete the temporary files
  unlink <fname_test.*>;

  return ($retcode,$case,$n_underscores);
}
