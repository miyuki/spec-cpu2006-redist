#/*@@
#  @file      cpp.pl
#  @date      Wed Sep 15 14:21:53 1999
#  @author    Tom Goodale
#  @desc 
#  Replacement C pre-processor
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/sbin/cpp.pl,v 1.4 2001/11/20 02:50:57 goodale Exp $
#@@*/

#$debug=1;

###############################################################################
###############################################################################
# Setup some global variables.

# Symbol table
%defines = ();

#Initial symbols

&Define("__FILE__", "\"replace-me\"", "<main>",__LINE__);
&Define("__LINE__", "\"replace-me\"", "<main>",__LINE__);

# Current working directory for opening files.
$current_wd = ".";

# Include path
@include_path = ();

# Filename and linenumber stacks for error traces.
@filelist = ();
@linelist = ();

# Complete list of included files for generating dependencies
%complete_file_list = ();

# Are we in the middle of a comment ?
$incomment = 0;

###############################################################################
###############################################################################

# Parse the command line
($source_file, $output_file, $do_deps, @include_path) = &ParseCommandLine(@ARGV);

###############################################################################
###############################################################################

#If no source file given, choose stdin
if(! $source_file)
{
  $source_file = "-"
}

# Setup output stream
if($output_file && $output_file ne "-")
{
  open(OUTSTREAM,">$output_file") || die "Unable to open output file";
}
else
{
  *OUTSTREAM = STDOUT;
}

###############################################################################
###############################################################################

# Parse the input.
&ProcessFile($source_file, "-", -1, 1-$do_deps);

###############################################################################
###############################################################################

# Do Dependency generation if requested
if($do_deps)
{
  my $file;

  my $depend_target;

  $source_file =~ m,^.+/([^/]+)$,;

  if($1)
  {
    $depend_target = "$1.o";
  }
  else
  {
    $depend_target = "$source_file.o";
  }

  foreach $file (sort keys %complete_file_list)
  {
    # Ignore any empry entries
    next if($file =~ m/^\s*$/);    
    # The source file depends upon this file
    print OUTSTREAM "$depend_target : $file\n";
  }

  foreach $file (sort keys %complete_file_list)
  {
    # Ignore any empry entries
    next if($file =~ m/^\s*$/);
    # Generate empty rule for file so can delete header files without problems
    print OUTSTREAM "$file :\n";
  }
}

exit;

###############################################################################
###############################################################################
###############################################################################
###############################################################################

#/*@@
#  @routine    ParseCommandLine
#  @date       Wed Sep 15 14:22:28 1999
#  @author     Tom Goodale
#  @desc 
#  Checks the command line for options  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseCommandLine
{
  my(@args) = @_;
  my($source_file, $output_file, $do_deps, @include_path) = (0,0,0,());

  while($arg = shift(@args))
  {
    if($arg =~ m:^-I(.*):)
    {
      push(@include_path, $1);
    }
    elsif($arg =~ m:^-D([^=]+)(=)?(.*):)
    {
      &Define($1, $3,"command-line",0);
    }
    elsif($arg =~ m:^-M(.*):)
    {
      $do_deps = 1;
    }
    elsif($arg =~ m:^-.+:)
    {
      die("Unknown preprocessor option '$arg'");
    }
    elsif($source_file && $output_file)
    {
      die("Source and output files already set");
    }
    elsif($source_file)
    {
      $output_file = $arg;
    }
    else
    {
      $source_file = $arg;
    }
  }
  
  return ($source_file, $output_file, $do_deps, @include_path);
}

###############################################################################

#/*@@
#  @routine    ProcessFile
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Open a file and parse its contents.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ProcessFile
{
  my ($newfilename, $oldfilename, $oldlinenumber, $printline) = @_;
  local *FILEDESC;
  my $fullpath;
  my $new_current_wd;

  ($new_current_wd,$fullpath) = &FindFile($newfilename,$current_wd,\@include_path);

  # Override this variable on the stack
  local $current_wd = $new_current_wd;

  if($newfilename ne "-" && !$fullpath)
  {
    die "Unable to find $newfilename included at $oldfilename:$oldlinenumber";
  }

  if($newfilename ne "-")
  {
    if($debug)
    {
      print "Opening $newfilename\n";
    }

    open(FILEDESC, "< $fullpath") || die "Unable to open file $fullpath";
    push(@filelist, $oldfilename);
    push(@linelist, $oldlinenumber);
  }
  else
  {
    $newfilename = "<STDIN>";
    $current_wd = ".";
    *FILEDESC = STDIN;
  }

  # If we are not printing lines, must being doing deps, so save file name
  if($newfilename ne "-" && ! $printline )
  {
    $complete_file_list{"$fullpath"} = 1;
  }

  &ParseFile(FILEDESC,$newfilename,0,1,$printline);

  if($newfilename ne "-")
  {
    if($debug)
    {
      print "Closing $newfilename\n";
    }
    close(FILEDESC);
    pop(@filelist);
    pop(@linelist);
  }
}

###############################################################################

#/*@@
#  @routine    FindFile
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Finds a file and works out its full name and the directory its in.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub FindFile
{
  my($newfilename,$old_current_wd,$ra_include_path) = @_;

  my $fullpath;
  my $new_current_wd;

  if($newfilename =~ m,^/,)
  {
    #absolute path
    $fullpath = $newfilename;
  }
  elsif($old_current_wd && -r "$old_current_wd/$newfilename")
  {
    $fullpath = "$old_current_wd/$newfilename";
  }
  else
  {
    for(my $dir=0; $dir < @$ra_include_path; $dir++)
    {
      if(-r "$ra_include_path->[$dir]/$newfilename")
      {
        $fullpath = "$ra_include_path->[$dir]/$newfilename";
        last;
      }
    }
  }

  # Tidy up the path a bit
  $fullpath =~ s,/./,/,g;

  if($fullpath)
  {
    $fullpath =~ m,^(.+)/[^/]+$,;
    $new_current_wd = $1;
  }

  $fullpath =~ s,^./,,;
  return ($new_current_wd, $fullpath);
}
  
###############################################################################

#/*@@
#  @routine    ParseFile
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Parse part or all of a file.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseFile
{
  local *INFILE  = $_[0];
  my $filename   = $_[1];
  my $linenumber = $_[2];
  my $active     = $_[3];
  my $printline  = $_[4];

  my $firstline = $linenumber;

  my $retcode = 0;

  my $line;
  my $currentline;

  if($debug)
  {
    print "Entered ParseFile: $filename:$linenumber, active=$active\n";
  }

  if($printline && $active && $linenumber==0)
  {
    print "# 1 \"$filename\"\n";
  }

  while(1)
  {
    ($line, $linenumber) = &ReadLine(*INFILE,$linenumber);

    $currentline = $line;

    # Exit loop if file is finished
    last if(! defined($line));
    
    # If it isn't a preprocessor command, just process it
    if($line !~ m/^\#/)
    {
      if($active)
      {
        my $retval;
        ($expanded,$retval) = &ParseAndExpand($line, "STDIN", $linenumber);
        print OUTSTREAM "$expanded\n" if $printline;
        $retcode += $retval;
      }
      next;
    }

    if($line =~ m/^#define\s+([^\s]+)(\s+(.*))?/)
    {
      # Define a macro
      &Define($1,$3,$filename, $linenumber) if($active);
      next;
    }
    elsif($line =~ m/^#undef\s+([^\s]+)/)
    {
      # Undefine a macro
      &UnDefine($1,$filename,$linenumber) if($active);
      next;
    }
    elsif($line =~ m/^#if(.+)/)
    {
      #Deal with a #if clause - do it recursively
      my $newactive;

      # Parse the if statement and see if the first clause is active
      if($active)
      {
        $newactive = &ProcessIf($1, $filename, $linumber,$printline);
      }
      else
      {
        #If not active before, still inactive
        $newactive = 0;
      }
      my $beenactive = $newactive;
      my $foundelse = 0;

      # Now process first clause and any #elif or #else clauses
      while(1)
      {
        # Parse the clause
        ($currentline, $linenumber) = &ParseFile(*INFILE,$filename,$linenumber,$newactive && $active,$printline);
        if(! $currentline)
        {
          # Got EOF !
          die "Unexpected EOF when parsing $filename";
        }
        elsif($currentline && $currentline =~ /\#endif\s*/)
        {
          # Finished
          last;
        }
        elsif($currentline =~ m/^#elif\s+(.+)/ && ! $foundelse)
        {
          # Got #elif, is this next clause active ?
          if(! $beenactive)
          {
            if($active)
            {
              $newactive = &ProcessIf($1, $filename, $linumber);
            }
            else
            {
              $newactive = 0;
            }
            $beenactive = $newactive;
          }
          else
          {
            $newactive = 0;
          }
        }
        elsif($currentline =~ m/^#else\s*$/ && ! $foundelse)
        {
          # Got #else, have any of the clauses been active ?
          if($active)
          {
            $newactive = 1 - $beenactive
          }
          else
          {
            $newactive = 0;
          }
          
          $foundelse = 1;
        }
        else
        {
          if($currentline =~ m/^#else/ || $currentline =~ m/^#elsif/)
          {
            print STDERR "Extraneous #else of #elsif found at $filename:$linenumber\n";
            $newactive = 0;
          }
          else
          {
            die "Unexpected line '$currentline' at $filename:$linenumber";
          }
        }
      }
    }
    elsif($line =~ m/^\#elif/ || $line =~ m/^\#else/ || $line =~ m/^\#endif/)
    {
      if($firstline > 0)
      {
      # If we are processing just part of the file, ok
        last;
      }
      else
      {
        # Otherwise there's an extra one here 
        die "Unexpected #elif/#else/#endif at $filename:$linenumber";
      }
    }
    elsif($line =~ m/^#include\s+(.+)?/)
    {
      # Now to include files.
      if(! defined($1))
      {
        print STDERR "Missing argument to #include directive at $filename:$linenumber\n";
      }
      else
      {
        if($active)
        {
          my $argument = $1;
          if($argument =~ m/<[^>]*>\s*/)
          {
            # Ignore system includes
            print OUTSTREAM "$line\n" if $printline;
          }
          else
          {
            # Allow people to use macros to define name of include file
            ($argument,undef) = &ParseAndExpand($argument,$filename,$linenumber);
            
            if($argument !~ m/\s*\"(.+)\"\s*$/)
            {
              print STDERR "Invalid filename $argument in #include directive at $filename:$linenumber\n";
            }
            else
            {
              # Process the new file.  Don't need to pass $active since wouldn't be here if inactive.
              &ProcessFile($1,$filename,$linenumber,$printline);

	      if($printline && $active)
	      {
		print "# $linenumber \"$filename\"\n";
	      }
            }
          }
        }
      }
    }
    else
    {
      print STDERR "Unrecognised # directive at $filename:$linenumber\n"
    }
  }

  if($debug)
  {
    print "Leaving ParseFile : currentline = '$currentline', linenumber = $linenumber\n";
  }

  return ($currentline,$linenumber);
}

###############################################################################

#/*@@
#  @routine    ReadLine
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Read a line from the current file descriptor.
#  Deals with comments and continuation lines
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ReadLine
{
  local *INFILE = $_[0];
  my $linenumber = $_[1];
  my $line;

  ($line,$linenumber) = &GetNextLine(*INFILE,$linenumber);

  # Deal with C-style comments

  # Deal with completely enclosed comments
  $line =~ s,/\*.*\*/, ,g;

  # Are we already processing a comment ?
  if($incomment)
  {
    if($line =~ m,\*/,)
    {
      # Get rid of line up to end of comment
      $line =~ s,^.*\*/, ,;
      # Line finished the comment
      $incomment = 0;
    }
    else
    {
      # Line doesn't finish the comment
      $line = " ";
    }
  }

  if(! $incomment)
  {
    if($line =~ m,/\*,)
    {
      # Get rid of line after beginning of comment
      $line =~ s,/\*.*$, ,;
      # Line starts the comment
      $incomment = 1;
    }
  }

  # Get rid of C++ comments too
  if(! $incomment)
  {
    $line =~ s,//.*$, ,;
  }

  return ($line, $linenumber);
}

###############################################################################

#/*@@
#  @routine    Get next line
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Read a line from the current file descriptor.
#  Dealing with continuation lines.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub GetNextLine
{
  local *INFILE = $_[0];
  my $linenumber = $_[1];

  my $line = <INFILE>;
  $linenumber++;

  # Deal with continuation lines
  while($line =~ m/\\\n$/)
  {
    chop($line);
    chop($line);
    $line .= <INFILE>;
    $linenumber++;
  }
  chop($line);

  return ($line, $linenumber);
}
###############################################################################

#/*@@
#  @routine    ProcessIf
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Parse an #if statement and return true or false.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ProcessIf
{
  my($line,$filename,$linenumber) = @_;

  my $retval = 0;

  if($debug)
  {
    print "if requested on $line\n";
  }

  if($line =~ m/^def\s+([^\s]+)/)
  {
    $retval = defined($defines{$1}) ? 1 : 0;
  }
  elsif($line =~ m/^ndef\s+([^\s]+)/)
  {
    $retval = defined($defines{$1}) ? 0 : 1;
  }
  else
  {
    print STDERR "#if can currently to #ifdef and #ifndef, sorry !\n";
    $retval = 0;
  }

  if($debug)
  {
    print "retval is $retval\n";
  }

  return $retval;
}

###############################################################################

#/*@@
#  @routine    Define
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Define a macro.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub Define
{
  my ($arg1,$arg2,$filename,$linenumber) = @_;

  $arg1 =~ m:^([a-zA-Z_][a-zA-Z0-9_]*)(\(([a-zA-Z0-9_,]+)\))?$:;
  
  my $defname = $1;
  my $defargs = $3;

  my @args = split(/,/, $defargs);

  if($debug)
  {
    print "Defining '$defname'\n";
  }

  if($defines{$defname})
  {
    print STDERR "Redefining $defname at $filename:$linenumber\n";
  }

  # Translate argument names just once at original definition.
  my @transargs = ();
  for(my $arg = 0; $arg < @args; $arg++)
  {
    $transargs[$arg] = "__^CCTK_INTERNAL${arg}__";
  }

  my $newbody = &ArgumentSubstitute($arg2, scalar(@args), @args, @transargs);

  $defines{$defname}{"ARGS"} = \@transargs;

  $defines{$defname}{"BODY"} = $newbody;
}

###############################################################################

#/*@@
#  @routine    Undefine
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Undefine a macro.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub UnDefine
{
  my ($def,$filename,$linenumber) = @_;

  delete $defines{$def};
}

###############################################################################

#/*@@
#  @routine    ExpandMacro
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Expand a macro recursively.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ExpandMacro
{
  my ($macro, $args, $filename, $linenumber) = @_;
  
  my $retcode = 0;
  my @arguments = &SplitArgs($args);

  my $outstring = $defines{$macro}{"BODY"};

  if($macro eq "__FILE__")
  {
    $outstring = "\"$filename\"";
  }
  elsif($macro eq "__LINE__")
  {
    $outstring = "$linenumber";
  }

  if(@arguments != @{$defines{$macro}{"ARGS"}})
  {
    my $expected = @{$defines{$macro}{"ARGS"}};
    my $got      = @arguments;
    print STDERR "Error expanding macro '$macro' at $filename:$linenumber\n";
    print STDERR "      Expected $expected arguments\n";
    print STDERR "      Got      $got arguments\n";
    $outstring = $macro;
    $retcode--;
  }
  else
  {
    my @prescanned_args;

    # Argument prescan
    for(my $arg = 0; $arg < @arguments; $arg++)
    {
      my $retval;
      ($prescanned_args[$arg],$retval) = &ParseAndExpand($arguments[$arg], $filename, $linenumber);
      $retcode += $retval;
    }

    # Argument substitution
    for(my $arg = 0; $arg < @arguments; $arg++)
    {
      if($debug)
      {
        print "Outstring is '$outstring'\n";
        print "Arg $arg: '$defines{$macro}{\"ARGS\"}[$arg]', '$arguments[$arg]', '$prescanned_args[$arg]'\n";
      }

      my $tobesubsted = quotemeta($defines{$macro}{"ARGS"}[$arg]);

      # Concatenation takes non-prescanned argument 
      $outstring =~ s/##\s*$tobesubsted\b/##$arguments[$arg]/g;

      # Stringification takes non-prescanned argument and stringifies it
      $outstring =~ s/#\s*$tobesubsted\b/\"$arguments[$arg]\"/g;

      $outstring =~ s/\b$tobesubsted\b/$prescanned_args[$arg]/g;
    }

    # Now recurse

    ($outstring,$retval) = &ParseAndExpand($outstring, $filename, $linenumber);
    $retcode += $retval;

    # Final Concatenation
    $outstring =~ s/\s*##\s*//g;

    # Now get rid of repeated ""

    $outstring =~ s/\\\"/__CCTK_STRINGPROTECT__/g;
    $outstring =~ s/\"\"//g;
    $outstring =~ s/__CCTK_STRINGPROTECT__/\\\"/g;
  }

  return ($outstring,$retcode);
}

###############################################################################

#/*@@
#  @routine    SplitArgs
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Split the arguments given to a macro into an array.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub SplitArgs
{
  my ($args) = @_;
  my @outargs;

  # Split the input into individual chars
  my @splitargs = split(//, $args);

  my $nestlevel = 0;
  
  my @thistoken = ();

  my $insstring = 0;
  my $indstring = 0;

  for(my $pos = 0; $pos < @splitargs; $pos++)
  {
    if($splitargs[$pos] eq '\'' || $splitargs[$pos] eq '"' || 
       $insstring == 1 || $indstring == 1)
    {
      # Just pass the token through if in a string.
      if($splitargs[$pos] eq '\'')
      {
        if($pos == 0 || ($pos > 0 && $splitargs[$pos-1] ne '\\'))
        {
          $insstring = 1 - $insstring;
        }
      }
      if($splitargs[$pos] eq '"')
      {
        if($pos == 0 || ($pos > 0 && $splitargs[$pos-1] ne '\\'))
        {
          $indstring = 1 - $indstring;
        }
      }
    }
    elsif($splitargs[$pos] eq "(")
    {
      # Increase nesting level
      $nestlevel++;
    }
    elsif($splitargs[$pos] eq ")")
    {
      # Decrease nesting level
      $nestlevel--;
    }
    elsif($splitargs[$pos] eq "," && $nestlevel == 0)
    {
      # At top level, and not in a string, so must be end of this arg.
      push(@outargs, join("",@thistoken));
      @thistoken = ();
      next;
    }
    push(@thistoken, $splitargs[$pos]);
  }

  # Push any remaining token
  if(@thistoken > 0)
  {
    push(@outargs, join("",@thistoken));
  }

  return @outargs;
}

###############################################################################

#/*@@
#  @routine    ParseAndExpand
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Parse a string and expand any macros in it.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseAndExpand
{
  my ($line,$filename, $linenumber) = @_;

  # Split the line into individual characters.
  my @splitline = split(//, $line);

  my @outline = ();
  my $retcode = 0;

  my $insstring = 0;
  my $indstring = 0;

  for(my $pos = 0 ; $pos < @splitline; $pos++)
  {
    # Pass through anything we're not interested in and anything in a string.
    if($splitline[$pos] !~ m/[A-Za-z_]/ || $insstring == 1 || $indstring == 1)
    {
      if($splitline[$pos] eq '\'')
      {
        if($pos == 0 || ($pos > 0 && $splitline[$pos-1] ne '\\'))
        {
          $insstring = 1 - $insstring;
        }
      }
      if($splitline[$pos] eq '"')
      {
        if($pos == 0 || ($pos > 0 && $splitline[$pos-1] ne '\\'))
        {
          $indstring = 1 - $indstring;
        }
      }
      push(@outline, $splitline[$pos]);
      next;
    }
    
    # Ok, should be at the beginning of a token

    my $token = $splitline[$pos];
    
    while($pos+1 < @splitline && $splitline[$pos+1] =~ m:[A-Za-z0-9_]:)
    {
      $pos++;
      $token .= $splitline[$pos];
    }

    # Is this token a macro ?
    if($defines{$token})
    {
      my $arg = "";
      if($pos+1 < @splitline)
      {
	# Eat up whitepace between token and arguments
	for(my $newpos=$pos+1; $newpos < @splitline; $newpos++)
	{
	  next if($splitline[$newpos] =~ m/\s/);
	  if($splitline[$newpos] eq "(")
	  {
	    $pos = $newpos-1;
	    last;
	  }
	  else
	  {
	    last;
	  }
	}
        # Find any arguments
        if($splitline[$pos+1] eq "(")
        {
          $pos++;
          my $depth = 1;
          $pos++;
          while($pos < @splitline && $depth > 0)
          {
            if($splitline[$pos] eq "(")
            {
              $depth++;
            }
            elsif($splitline[$pos] eq ")")
            {
              $depth--;
            }
            if($depth > 0)
            {
              $arg .= $splitline[$pos];
              $pos++;
            }
          }
        }
      }
      if($debug)
      {
        print "Token is '$token', arguments are '$arg'\n";
      }
      # Expand the macro
      my($expanded,$retval) = &ExpandMacro($token,$arg,$filename,$linenumber);
      $retcode += $retval;
      if($debug)
      {
        print "Expanded version is '$expanded'\n";
      }
      #Put the final expanded version into output.
      push(@outline, $expanded);
    }
    else
    {
      push(@outline, $token);
    }
  }

  return (join("",@outline),$retcode);
}

###############################################################################

#/*@@
#  @routine    ArgumentSubstitute
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Substitute all non-string-enclosed arguments with replacement values.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ArgumentSubstitute
{
  my ($body, $nargs, @args) = @_;

  my @splitbody = split(//,$body);
  my @outbody = ();

  my $insstring = 0;
  my $indstring = 0;

  for(my $pos = 0 ; $pos < @splitbody; $pos++)
  {
    
    # Just pass through all non-tokens and all tokens in a string.
    if($splitbody[$pos] !~ m/[A-Za-z_]/ || $insstring == 1 || $indstring == 1)
    {
      if($splitbody[$pos] eq '\'')
      {
        if($pos == 0 || ($pos > 0 && $splitbody[$pos-1] ne '\\'))
        {
          $insstring = 1 - $insstring;
        }
      }
      if($splitbody[$pos] eq '"')
      {
        if($pos == 0 || ($pos > 0 && $splitbody[$pos-1] ne '\\'))
        {
          $indstring = 1 - $indstring;
        }
      }
      push(@outbody, $splitbody[$pos]);
      next;
    }
    
    # Ok, should be at the beginning of a token

    my $token = $splitbody[$pos];
    
    while($pos+1 < @splitbody && $splitbody[$pos+1] =~ m:[A-Za-z0-9_]:)
    {
      $pos++;
      $token .= $splitbody[$pos];
    }
    
    if($debug)
    {
      print "Token is '$token'\n";
    }

    for(my $arg = 0; $arg < $nargs; $arg++)
    {
      if($token =~ m/^$args[$arg]$/)
      {
        $token = $args[$arg+$nargs];
        last;
      }
    }

    push(@outbody, $token);
  }

  return join("", @outbody);
}

###############################################################################

#/*@@
#  @routine    Print Defines
#  @date       Mon Nov 19 23:51:03 2001
#  @author     Tom Goodale
#  @desc 
#  Print all the macros in the symbol table.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintDefines
{
  my $def;

  foreach $def (sort keys %defines)
  {
    print "Macro '$def'\n";
    print @{$defines{$def}{"ARGS"}} . " arguments\n";
    if (@{$defines{$def}{"ARGS"}})
    {
      for (my $arg = 0 ; $arg < @{$defines{$def}{"ARGS"}}; $arg++)
      {
        print "$arg: $defines{$def}{\"ARGS\"}[$arg]\n";
      }
    }
    print "Body '$defines{$def}{\"BODY\"}'\n";

  }
}
