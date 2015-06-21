#/*@@
#  @file      checkout.pl
#  @date      Sat Jul  3 16:38:52 1999
#  @author    Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#@@*/
# /usr/bin/perl -s

# Set debug to 1 for off line checking
$debug = 0;

$sbin_dir = "lib/sbin";

require "$sbin_dir/MakeUtils.pl";
require "$sbin_dir/CheckoutUtils.pl";

# First check that CVS works
die "cvs not found\n" if (!&CVSFound);

# Switch off buffering for STDOUT
select(STDOUT);
$| = 1;

# Print info
&PrintInfo;

# Check can see repository
#die "Repository not found\n" if (!debug && !&RepositoryExists($repository));

# Parse CVS password file .cvspass
@rep = &ParseCVSPasswordFile;
$rep = join(" ",@rep);

# Add all anonymous Cactus repositories
&AddCVSPasswordFile($rep);

while (!$finish)
{   
    
  print "Checkout thornlist, arrangements or individual thorns.\n ";
  print "tl)thornlist, arr)angements, t)horns, q)uit, h)elp : [arr] ";
    
  $which = <STDIN>;

  if ($which =~ /^q/i)
  {
    $finish = 1;
  }
  elsif ($which =~ /^h/i)
  {
    &print_help();
  }
  elsif ($which =~ /^tl/i)
  {
    &get_thornlist();
  }
  elsif ($which =~ /^t/i)
  {
    &get_thorns();
  }
  else
  { 
    &get_arrangements();
  }

}

print "\n All done!\n\n";
exit;


#/*@@
#  @routine    get_arrangements
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub get_arrangements
{   
    my(%info);
    my($arrangement);
    print "\n You already have arrangements: \n\n";

    %info = &buildthorns("arrangements/","arrangements");
    
    foreach $arrangement (sort keys %info)
    {
      print "$arrangement ";
    }

    print "\n\n Arrangements listed in modules file: \n";

    open(MODULES,"cvs co -s | ");
    
    $count = 0;
    while(<MODULES>)
    {
	if (/(\w*)\s*ARRANGEMENT(.*)?/)
	{
	  $count++;
	  $name{$count} = "$1";
	  $devlev{$count} = "$2";
	}
    }
    
    
    for ($i=1; $i<$count+1;$i++)
    {
        $extra = "";
        if ($devlev{$i} == 2) 
	{
	  $extra = "(dev)";
	}
	print "  [$i] $name{$i} $extra\n";
    }
    
    print "\n";
    
    print "Checkout arrangements h)elp, q)uit, c)ustom, range [1-$count] : ";

    # Goto target arrangement directory 
    chdir arrangements || die "Could not find arrangements directory";

    $range = <STDIN>;
    if ($range =~ /^h/i) 
    {
	&print_help();
    }
    elsif ($range =~ /^q/i)
    {
      print "\n\n";
      exit(0);
    }
    elsif ($range =~ /^c/i)
    {
      print "Arrangement required: ";
      $arrname = <STDIN>;
      &CheckOut($arrname);
    }
    elsif ($range =~ /^\s*$/)
    {
	$range = "1-$count";
    }

    while ($range =~/^([0-9]+(?:-[0-9]+)?),?/)
    {
	$range = $';
	$1 =~ /^([0-9]*)(-[0-9]*)?$/;
	$first = $1;
	if (!$2) 
	{$last=$1}
	else
        {$2=~/-([0-9]*)/; $last=$1}

	for ($i=$first; $i<$last+1; $i++)
	{
	  &CheckOut($name{$i});
	}	
    }

    chdir("..") || die "Could not go back to Cactus home directory\n";

}



#/*@@
#  @routine    get_thorns
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub get_thorns
{
    my(%info);
    my($thorn);

    print "\nYou already have thorns: ";
    
    %info = &buildthorns("arrangements/","thorns");
    
    $last_arr = "";
    foreach $thorn (sort keys %info)
    {
      # Parse of the arrangement name
      $thorn =~ m:(.*)/(.*):;
      $this_arr = $1;
      $this_tho = $2;
      if ($last_arr ne $this_arr)
      {
	print "\n\n$this_arr:\n";
	print "  $this_tho ";
      }
      else
      {
	print "$this_tho ";
      }
      $last_arr = $this_arr;
    }

    print "\n\nThorns listed in the modules file: \n";

    open(MODULES,"cvs -q co -s | ");
    
    $count = 0;
    while(<MODULES>)
    {
	if (/(\w*\/?\w*)\s*THORN([^\s])\s/)
	{
	    $count++;
	    $name{$count} = "$1";
	    $devlev{$count} = "$2";
	}
    }


    for ($i=1; $i<$count+1;$i++)
    {
        $extra = "";
        if ($devlev{$i} == "2") 
	{
	  $extra = "(dev)";
	}
	print "  [$i] $name{$i} $extra\n";
    }
    
    print "\n";
    
    print "Checkout thorns h)elp, q)uit, c)ustom, range [1-$count] : ";
    
    # Goto target arrangement directory 
    chdir arrangements || die "Could not find arrangements directory\n";

    $range = <STDIN>;
    print "\n";

    if ($range =~ /^h/i)
    {
	&print_help();
    }
    elsif ($range =~ /^q$/i)
    {
      print "\n\n";
      exit(0);
    }
    elsif ($range =~ /^c/i)
    {
      print "Arrangement/Thorn required: ";
      $thornname = <STDIN>;
      &CheckOut($thornname);
    }
    elsif ($range =~ /^\s*$/)
    {
      $range = "1-$count";
    }

    while ($range =~/^([0-9]+(?:-[0-9]+)?),?/)
    {
      $range = $';
      $1 =~ /^([0-9]*)(-[0-9]*)?$/;
      $first = $1;
      if (!$2) 
	{$last=$1}
      else
	{$2=~/-([0-9]*)/; $last=$1}
      
      for ($i=$first; $i<$last+1; $i++)
	{
	  $arrangement = $name{$i};
	  $arrangement =~ s:/[^/]*$::;
	  &CheckOutREADME("$arrangement/README");
	  &CheckOut($name{$i});
	}
    }

    chdir ("..") || die "Could not return to Cactus home directory\n";
}




#/*@@
#  @routine    get_thornlist
#  @date       Sat Jul  13 16:38:52 2000
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub get_thornlist
{
  if (!-d "thornlists")
  {
    print "\nThorn List directory \"thornlists\" not found\n\n";
    return;
  } 
  else
  {
    $thornlist = 0;
    while (!$thornlist)
    {
      print "\nLists in thornlist directory: \n";
      open(LISTS, "ls thornlists |");
    
      while(<LISTS>)
      {
	print "  $_";
      }
      close LISTS;
      print "\nChoose ThornList : ";
      
      $thornlist = <>;
      print "\n";
      chomp($thornlist);
      if (!-e "thornlists/$thornlist")
      {
	$thornlist = 0;
      }
    }
    &CheckoutThornList("thornlists/$thornlist");
  }
}

#/*@@
#  @routine    print_help
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub print_help
{ 

    print "\nTo select arrangements or thorns for checking out from CVS, give\n";
    print "a comma separated list with the numbers of the thorns/arrangements.\n";
    print "Ranges can also be given, using a hyphen.\n";
    print "For example, to checkout thorns/arrangements 1,2,4,6,7,9 use:\n\n";
    print "   1-2,4,6-7,9\n\n";
}

#/*@@
#  @routine    Checkout
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub CheckOut
{
  my($file) = @_;

  print("Checking out $file\n");

  open(MODULES,"cvs -z9 -q checkout $file |");
#  @dummy = <MODULES>;
  while(<MODULES>)
  {
    print ".";
  }
  print "\n";
  close(MODULES);

}

#/*@@
#  @routine    CheckoutREADME
#  @date       Sat Jul  20 16:38:52 2000
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub CheckOutREADME
{
  my($file) = @_;

  open(MODULES,"cvs -z9 -q checkout $file |");
  @dummy = <MODULES>;
  close(MODULES);

}

#/*@@
#  @routine    PrintInfo
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintInfo
{
  print "\n";
  print "Type \"help\" at any prompt for a description on how\nto use this script\n\n";
  print "________________________________________________________________________\n\n";
}    

#/*@@
#  @routine    CVSFound
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub CVSFound
{

  my($foundit);
  
  $foundif = 0;
  open(MODULES,"cvs -v | ");
  while (<MODULES>)
  {
    if (/Concurrent Versions System/)
    {
      $foundit = 1;
    }
  }
  close(MODULES);

  return $foundit;

}

#/*@@
#  @routine    RepositoryExists
#  @date       Sat Jul  3 16:38:52 1999
#  @author     Gabrielle Allen
#  @desc 
#  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub RepositoryExists
{
  my($repository) = @_;
  my @dummy;

  open(MODULES,"cvs -d $_[0] co -s |");
  @dummy = <MODULES>;
  close(MODULES);

  return !$?;

}



