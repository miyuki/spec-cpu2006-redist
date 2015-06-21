#!/usr/bin/perl -s
#/*@@
#  @file      f_file_processor.pl
#  @date      Jan 22 1995
#  @author    Paul Walker
#  @desc
#  Postprocessor for Fortran files.
#   
#  Reads STDIN, writes to STDOUT.
#
#  removes all comments
#  replaces && with newline and tab to col 7
#  replaces &! with newline at col 0
#  Breaks lines greater than 72 cols
#  Does this using multi-line matching!
#  
#  If run with -free_format, chooses free-format 
#  line spleeting.
#  
#  @enddesc 
#  @history 
#  @hdate Wed Apr 21 1997 @hauthor Joan Masso
#  @hdesc get rid of cC comments and handle ! comments properly
#         and fix it so now it is really 72 and we do not get
#         breaks in the middle of fortran strings!
#  @hdate Wed Nov 24 12:17:43 1999 @hauthor Tom Goodale
#  @hdesc Added in Erik Schnetters free-format stuff
#         grdoc-ed
#         reformated as per rest of perl code in Cactus.
#  @endhistory 
#  @version $Header: /cactus/Cactus/lib/sbin/f_file_processor.pl,v 1.8 2000/04/16 08:25:16 allen Exp $
#@@*/

$* = 1;                         # Multi-line is on!

# Pick the correct set of comments to remove.
if ($free_format)
{
  $standard_comments = "\\s*[!]";
}
else
{
  $standard_comments = "[cC!]";
}

# Loop over all lines.
while (<>) 
{
  next if (/^\s*$/);            # Blanks slow down compilation, and cpp makes
                                # lots and lots of them!

  next if (/^\#/);              # Remove any remaining # directives (e.g. line directives).

  # Get rid of final \n
  chomp;

  # Get rid of any tabs
  s/\t/        /g;
  
  # Get rid of standard c C, or even ! comments
  s/^$standard_comments.*$/\n/g;
    
  # Get rid of ! comments : a bit tricky as ! may appear inside strings
  s/(.)![^'"]*$/\1\n/g;

  # OK, now put in the line breaks (&& or &!)
  s/\&\&\s*/\n      /g;
  s/\&\!\s*/\n/g;

  # Get rid of lonesome semicolons
  s/\s*\;\s*$//;

  # And now we can fix the lines.  This is actually a little complicated.
  # since there is a different case if the thing matches a newline
  # than if it doesn't.
  if (/\n/)
  {
    foreach $LINE (split('\n',$_)) 
    {
      &splitline($LINE);
    }
  }
  else
  {
    &splitline($_);
  }
}

#/*@@
#  @routine    splitline
#  @date       Wed Nov 24 12:14:55 1999
#  @author     Tom Goodale
#  @desc 
#  Chooses the correct routine to split lines.  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub splitline
{
  my ($line) = @_;

  if($free_format)
  {
    &free_format_splitline($line);
  }
  else
  {
    &fixed_format_splitline($line);
  }

}

#/*@@
#  @routine    fixed_format_splitline
#  @date       1995
#  @author     Paul Walker
#  @desc 
#  Splits lines for F77 or fixed-format F90
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub fixed_format_splitline 
{
  my ($LINE) = @_;

  # Remove ,, and , \) from blank thorns
  while ($LINE =~ s/,\s*,/,/) {};
  $LINE =~ s/,\s*\)/\)/;

  # Strip out leading spaces in favor of 7 spaces
  # $LINE =~ s/^\s+/       /;
  # Note the new treatement of comments with \S
  if ($LINE =~ /^([^\S].{71,71}).*/) 
  {
    print "$1\n";
    $LINE =~ s/.{72,72}//;
    while ($LINE =~ /^(.{66,66}).*/) 
    {
      print "     &$1\n";
      $LINE =~ s/.{66,66}//;
    }
    print "     &$LINE\n";
  } 
  else 
  {
    print "$LINE\n";
  }

}

#/*@@
#  @routine    free_format_splitline
#  @date       Thu Sep 30 12:05:36 1999
#  @author     Erik Schnetter
#  @desc 
#  Splits lines for freeformat fortran 90.  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub free_format_splitline 
{
  my ($LINE) = @_;
  my $OUT;

  # Remove ,, and , \) from blank thorns
  while ($LINE =~ s/,\s*,/,/) {};
  $LINE =~ s/,\s*\)/\)/;

  # Some preprocessors put extraneous spaces in 8-(
  $LINE =~ s:\. ([a-zA-Z]+) \.:\.$1\.:ig;

  # Strip out leading spaces in favor of 3 spaces
  # $LINE =~ s/^\s+/   /;
  if ($LINE =~ /^(.{78,78})...*/) 
  {
    $OUT = $1;
    print "$OUT";
    # Check if the line already has a continuation mark.
    print "&" if (! ($OUT =~ /\&[\s]*$/));
    print "\n";
    $LINE =~ s/.{78,78}//;

    while ($LINE =~ /^(.{75,75}).*/) 
    {
      $LINE =~ /^(.{74,74}).*/;
      $OUT = $1;
      print "   &" if (! ($OUT =~ /^[\s]*\&/));
      print "$OUT";
      print "&" if (! ($OUT =~ /\&[\s]*$/));
      print "\n";
      $LINE =~ s/.{74,74}//;
    }
    print "   &" if (! ($LINE =~ /^[\s]*\&/));
    print "$LINE\n";
  } 
  else 
  {
    print "$LINE\n";
  }

}







