#/*@@
#  @routine   CST_error
#  @date      4 July 1999
#  @author    Gabrielle Allen
#  @desc 
#  Print an error or warning message
#  @enddesc 
#  @version $Id: CSTUtils.pl,v 1.14 2001/10/14 18:18:04 goodale Exp $
#@@*/

sub CST_error
{
    my($level,$mess,$help,$line,$file) = @_;
    my($error);

    if ($help !~ /^\s*$/)
    {
      $help = "     HINT: $help\n";
    }

    if ($full_warnings)
    {
        if ($level == 0)
        {
            $CST_errors++;
            $error = "\nCST error in $file (at $line)\n  -> $mess\n";
            print STDERR "$error\n";
            $error_string .= "$error$help\n";
        }
        else
        {
            $error = "\nCST warning in $file (at $line)\n  -> $mess\n";
            print STDERR "$error\n";
            $error_string .= "$error$help\n";
        }
    }
    else
    {
        if ($level == 0)
        {
            $CST_errors++;
            $error = "\nCST error $CST_errors:\n  -> $mess\n";
            print STDERR "$error\n";
            $error_string .= "$error$help\n";
        }
        else
        {
            $error = "\nCST warning:\n  -> $mess\n";
            print STDERR "$error\n";
            $error_string .= "$error$help\n";
        }           
    }

    return;
}


#/*@@
#  @routine   CST_PrintErrors
#  @date      5 December 1999
#  @author    Gabrielle Allen
#  @desc 
#  Print all the errors and warnings from the CST
#  @enddesc 
#  @version $Id: CSTUtils.pl,v 1.14 2001/10/14 18:18:04 goodale Exp $
#@@*/

sub CST_PrintErrors
{
  print "$error_string";
}


#/*@@
#  @routine    read_file
#  @date       Wed Sep 16 11:54:38 1998
#  @author     Tom Goodale
#  @desc 
#  Reads a file deleting comments and blank lines. 
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#  @hdate Fri Sep 10 10:25:47 1999 @hauthor Tom Goodale
#  @hdesc Allows a \ to escape the end of a line. 
#  @endhistory 
#@@*/

sub read_file
{
  my($file) = @_;
  my(@indata);
  my($line);

  open(IN, "<$file") || die("Can't open $file\n");
  
  $line = "";

  while(<IN>)
  {
    $_ =~ s/\#.*//;
    
    next if(m/^\s+$/);
 
    &chompme($_);

    # Add to the currently processed line.
    $line .= $_;

    # Check the line for line-continuation
    if(m:[^\\]\\\s*$:)
    {
      $line =~ s:\\\s*$::;
    }
    else
    {
      push(@indata, $line);
      $line = "";
    }
  }
  
  # Make sure to dump out the last line, even if it ends in a \
  if($line ne "")
  {
    push(@indata, $line);
  }
  close IN;
  
  return @indata;
}


#/*@@
#  @routine    chompme
#  @date       Mon 26th April 1999
#  @author     Gabrielle Allen
#  @desc 
#  Implements a version of the perl5 chomp function,
#  returning the string passed in with the last character
#  removed unless it is a newline
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
# 
#  @endhistory 
#@@*/

sub chompme
{
    my($in) = @_;

    $lastchar = chop($in);
    if ($lastchar eq "\n")
    {
        return $_;
    }
    else
    {
        return $in;
    }
}

#/*@@
#  @routine    WriteFile
#  @date       Tue Oct 19 21:09:12 CEST 1999 
#  @author     Gabrielle Allen
#  @desc 
#  Writes a file only if the contents haven't changed
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
# 
#  @endhistory 
#@@*/

sub WriteFile
{
  my ($filename,$rdata) = @_;
  my ($data_in);

# Read in file
  $data_in = "";
  if (-e $filename) 
  {
    open(IN, "<$filename");
    while (<IN>)
    {
      $data_in .= $_;
    }
  }

  if ($$rdata ne $data_in)   
  {
#    print "Creating new file $filename\n";
    open(OUT, ">$filename") || die("Can't open $filename\n");
    print OUT $$rdata;
    close OUT;
  }

}

#/*@@
#  @routine    TestName
#  @date       Sat Dec 16 1.48
#  @author     Gabrielle Allen
#  @desc 
#  Check thorn/arrangement name is valid
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub TestName
{
  local($thorn,$name) = @_;
  local($valid);

  $valid = 1;

  if (!$name)
  {
    $valid = 0;
  }
  elsif ($name !~ /^[a-zA-Z]/)
  {
    print STDERR "Name must begin with a letter!\n\n";
    $valid = 0;
  }
  elsif ($name !~ /^[a-zA-Z0-9_]*$/)
  {
    print STDERR "Name can only contain letters, numbers or underscores!\n\n";
    $valid = 0;
  }

  if ($thorn && $name eq "doc")
  {
    print STDERR "Thorn name doc is not allowed!\n\n";
    $valid = 0;
  }

  return $valid;
}

1;
