#/*@@
#  @file      MakeUtils.pl
#  @date      July 1999
#  @author    Tom Goodale
#  @desc 
#  Utility perl routines needed by the Makefile.
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/sbin/MakeUtils.pl,v 1.14 2001/12/14 10:45:15 allen Exp $
#@@*/


#/*@@
#  @routine   buildthorns
#  @date      Tue Jan 19 14:02:07 1999
#  @author    Tom Goodale
#  @desc 
#  Creates an compiled ThornList
#  @enddesc 
#  @version $Id: MakeUtils.pl,v 1.14 2001/12/14 10:45:15 allen Exp $
#@@*/

sub buildthorns
{
  my($arrangement_dir,$choice) = @_;
  my(@arrangements);
  my(%info);

  chdir $arrangement_dir || die "Can't change directory to $arrangement_dir\n";

  open(ARRANGEMENTS, "ls|");
    
  while(<ARRANGEMENTS>)
  {
    chomp;

    # Ignore CVS and backup stuff
    next if (m:^CVS$:);
    next if (m:^\#:);
    next if (m:~$:);
    next if (m:\.bak$:i);
    next if (m:^\.:);
    
    # Just pick directories
    if( -d $_)
    {
      push (@arrangements, $_);
    }
  }
    
  close ARRANGEMENTS;
  
  if ($choice =~ "thorns")
  {
    
    foreach $arrangement (@arrangements)
    {
      chdir $arrangement;
      
      open(THORNLIST, "ls|");
      
      while(<THORNLIST>)
      {
	chomp;
		
	# Ignore CVS and backup stuff
	next if (m:^CVS$:);
	next if (m:^\#:);
	next if (m:~$:);
	next if (m:\.bak$:i);
	next if (m:^\.:);
		
	# Allow each arrangement to have a documentation directory.
	next if (m:^doc$:);
	
	# Just pick directories
	if( -d $_)
	{
	  push(@total_list, "$arrangement/$_");
	}
      }
      chdir "..";
    }
    
  }
  else
  {
    @total_list = @arrangements;
  }
 
  if($choice =~ "thorns")
  {
    foreach $thorn (@total_list)
    {
      if( -r "$thorn/interface.ccl" && -r "$thorn/param.ccl")
      {
	$info{$thorn} = &ThornInfo($thorn);
      }
#      print "$thorn \# $info{$thorn}\n";
    }
  }
  else
  {
    foreach $arrangement (@total_list)
    {
      $info{$arrangement} = 1;
    }
  }

  chdir "..";

  return %info;
}

#/*@@
#  @routine    ThornInfo
#  @date       Sun Oct 17 15:57:44 1999
#  @author     Tom Goodale
#  @desc 
#  Determines some info about a thorn.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ThornInfo
{
  my($thorn) = @_;
  my($implementation) = "";
  my($friends) = "";
  my($inherits) = "";
  my($shares) = "";

  open(INTERFACE, "<$thorn/interface.ccl") || die "Unable to open $thorn/interface.ccl";

  while(<INTERFACE>)
  {
    chomp;
    if (m/^\s*IMPLEMENTS\s*:\s*([a-z]+[a-z_0-9]*)\s*$/i)
    {
      $implementation = $1;
    }
    elsif (m/^\s*INHERITS\s*:((\s*[a-zA-Z]+[a-zA-Z_0-9,]*)*\s*)$/i)
    {
      $inherits = $1;
    }
    elsif (m/^\s*FRIEND\s*:((\s*[a-zA-Z]+[a-zA-Z_0-9,]*)*\s*)$/i)
    {
      $friends = $1;
    }
  }

  close(INTERFACE);

  open(PARAM, "<$thorn/param.ccl") || die "Unable to open $thorn/param.ccl";

  while(<PARAM>)
  {
    chomp;
    if(m/SHARES\s*:(.*)/i)
    {
      $shares .= " $1";
    }
  }

  close(PARAM);

  if($inherits =~ /^[\s\t\n]*$/)
  {
    $inherits = " ";
  }
  else
  {
    $inherits =~ s:^\s*::;
    $inherits =~ s:\s*$::;
    $inherits =~ s:,: :g;
    $inherits =~ s:[\s\t\n]+:,:g;
  }


  if($friends =~ /^[\s\t\n]*$/)
  {
    $friends = " ";
  }
  else
  {
    $friends =~ s:^\s*::;
    $friends =~ s:\s*$::;
    $friends =~ s:,: :g;
    $friends =~ s:[\s\t\n]+:,:g;
  }
  if($shares =~ /^[\s\t\n]*$/)
  {
    $shares = " ";
  }
  else
  {
    $shares =~ s:^\s*::;
    $shares =~ s:\s*$::;
    $shares =~ s:,: :g;
    $shares =~ s:[\s\t\n]+:,:g;
  }

  return "$implementation ($inherits) [$friends] {$shares}";
}


#/*@@
#  @routine    ThornInfo
#  @date       Wed Sep 5 14:04:07 CEST 2001 
#  @author     Ian Kelley 
#  @desc 
#  Reads in a thornlist and returns the arrangements/thorns,
#  strips out all the comments/etc. 
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ReadThornlist
{
   my ($thornlist) = shift;
   my (@temp);
   my (%tl);

   open (TL, "$thornlist") 
      || die "\nCannot open thornlist ($thornlist) for reading: $!";

   while (<TL>) 
   {
      next if m:^!.*:;
      s/(.*?)#.*/\1/;            # read up to the first "#"
      s/\s+//g;                  # replace any spaces with nothing
      if (/\w+/) 
      {
         push @temp, $_;         # add to array if something is left
      }
   }

   foreach (@temp)      # see if docs exist for these thorns
   {
      $tl{$_} = "thorn";
   }

   return %tl;
}
    
1;
