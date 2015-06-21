#! /usr/bin/perl -s 
#
#/*@@
#   @file      CheckoutUtils.pl
#   @date      Feb 19 2000
#   @author    Gabrielle Allen
#   @desc
#   Installer for Thorns from ThornList
#   @enddesc
#   @version $Header: /cactus/Cactus/lib/sbin/CheckoutUtils.pl,v 1.2 2000/07/20 12:16:14 allen Exp $
# @@*/           

sub CheckoutThornList
{

  local($thornlist) = @_;
  local($mydir);

  # Where am I
  $mydir = `pwd`;
  chomp($mydir);

  # Get repository from Flesh CVS file
  $fleshpath = &GetFleshPath();

  # Get CVS options
  $cvs_options = &CVSOptions;
  
  # Parse the ThornList
  %thorns = &ParseThornList($thornlist,$fleshpath,$tag);

  # Checkout the thorns
  if (chdir "arrangements")
  {
    &GetThorns($fleshpath,$cvs_options,$tag,%thorns);
    chdir $mydir;
  }
  else
  {
    print "\nNo arrangements directory found\nNo checkout of thornlist\n";
  }

}

########################################################################



#/*@@
#  @routine   Repository exists
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Check that the CVS repository is actually there
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
  my $command,@dummy;

  $command = "cvs -d $_[0] co -s |";
  if ($debug != 1)
  {
    open(MODULES,$command);
    @dummy = <MODULES>;
    close(MODULES);
  }
  else
  {
    print "\n DEBUG: Checking repository exists\n";
    print " DEBUG: $command\n\n";
  }

  return !$?;
}

#/*@@
#  @routine   ParseCVSPasswordFile
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Read in .cvspass if it is there
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ParseCVSPasswordFile
{
  my @rep;
  my $numinpass;
  my $home;

  $home = $ENV{"HOME"};
  if ($home =~ /^$/)
  {
    DIE("Set \$HOME environment variable to home directory");
  }
  if (open(CVSPASS,"<$home/.cvspass"))
  {
    $numinpass = 0;
    while (<CVSPASS>)
    {
      /^([^\s]*)\s[^\s]*/;
      $rep[$numinpass] = $1;
      $numinpass++;
    }
  }
  else
  {
    @rep = "";
  }

  return @rep;

}



#/*@@
#  @routine   AddCVSPasswordFile
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  If the checkout is anonymous add any needed passwords to .cvspass
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub AddCVSPasswordFile
{
  my($rep) = @_;
  my $home;

  $home = $ENV{"HOME"};
  if ($home =~ /^$/)
  {
    DIE("Set \$HOME environment variable to home directory");
  }

  $anon_pass = "Ay=0=";
  $rep1 = ":pserver:cvs_anon\@cvs.cactuscode.org:/cactusdevcvs";
  $rep2 = ":pserver:cvs_anon\@cvs.cactuscode.org:/cactus";
  $rep3 = ":pserver:cvs_anon\@cvs.cactuscode.org:/arrangements";
  $rep4 = ":pserver:cvs_anon\@cvs.cactuscode.org:/packages";

  if ( -e "$home/.cvspass")
  {
    open(CVSPASS,">>$home/.cvspass");
  }
  else
  {
    open(CVSPASS,">$home/.cvspass");
  }

  if ($rep !~ /$rep1\b/)
  {
    print CVSPASS "$rep1 $anon_pass\n";
  }
  if ($rep !~ /$rep2\b/)
  {
    print CVSPASS "$rep2 $anon_pass\n";
  }   
  if ($rep !~ /$rep3\b/)
  {
    print CVSPASS "$rep3 $anon_pass\n";
  }
  if ($rep !~ /$rep4\b/)
  {
    print CVSPASS "$rep4 $anon_pass\n";
  }

  close(CVSPASS);

}



#/*@@
#  @routine   StripSpaces
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Remove spaces at start and end of word ... there's probably
#  a much easier way to do this
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub StripSpaces
{
  my($name) = @_;

  $name =~ s/^\s*//;
  $name =~ s/\s*$//;

  return $name;
}


#/*@@
#  @routine   LoginRepository
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Login to a repository with chosen username
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub LoginRepository
{
  my ($repository) = @_;
  my ($command);

  $command = "cvs -d $repository login |";

  if ($debug != 1) 
  {
    open(CVSLOGIN,$command);
    while (<CVSLOGIN>) 
    {
      print $_;
    }
    close(CVSLOGIN);
  }
  else
  {
    print "\n DEBUG: Logging into repository\n";
    print " DEBUG: $command\n\n";
  }

}


#/*@@
#  @routine   GetThorns
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Check out the thorns from CVS
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub GetThorns
{
  my($fleshrep,$options,$tag,%thorns) = @_;

  foreach $th (keys %thorns)
  {
    if ($thorns{"$th"} =~ m|cvs.cactuscode.org:/cactus|)
    {
      if ($thorns{"$th"} ne $fleshrep)
      {
	print " WARNING: Mixing stable and developmental repositories\n";
	print "    Flesh: <$fleshrep>\n";
	print "    $th: <$thorns{\"$th\"}>\n\n";
	print " Continue? (h)elp, y)es, n)o) [yes] : ";
	if (defined $defaults) 
	{
	  print "\n";
	} 
	else 
	{
	  $answer = <STDIN>;
	}
	if ($answer =~ /^n/i)
	{
	  DIE("Leaving GetCactus script");
	}
      }
    }

    print "\n--------------------------------------------------------------------\n\n";
    print " Checking out thorn $th\n";
    print "  (CVS repository: $thorns{\"$th\"})\n";

    # Check we're logged into this repository
    if ($thorns{"$th"} =~ /pserver/)
    {
      @rep = &ParseCVSPasswordFile;
      $rep = join(" ",@rep);
      if ($rep !~ $thorns{"$th"})
      {
	print "\n No login for CVS repository \n  $thorns{\"$th\"}\n\n";
	do 
	{
	  print " Action: (q)uit, l)ogin, h)elp) [login] : ";
	  $answer = <STDIN> if (! defined $defaults);
	  if ($answer =~ /^q/i)
	  {
	    DIE("Leaving GetCactus script");
	  }
	  elsif ($answer =~ /^h/i)
	  {
	    &OtherRepositoriesHelp();
	  }
	  else
	  {
	    &LoginRepository($thorns{"$th"});
	  }
	} while ($answer =~ /^h/i);
	
      }
    }

    # Checkout or update
    if (-e $th) 
    {
      print "\n Thorn already installed ... no overwrite\n";
      print "\n Do you want to update $th [no] : ";
      $answer = <STDIN>;
      if ($answer =~ /y/i)
      {
	print " Updating $th\n";
	$command_up = "cvs $cvs_options -d $thorns{\"$th\"} update -d $tag $th |";
	print "  (CVS repository: $thorns{\"$th\"})\n";
	open(REP,"<$th/CVS/Root") || DIE("No CVS files for $th");
	$installedrep = <REP>;
	close(REP);
	chomp $installedrep;
	if ($thorns{"$th"} !~ m:^$installedrep$:)
	{
	  print " WARNING: Installed $th from different repository\n";
	  print "          ($installedrep)\n";
	}
	else
	{
	  if ($debug != 1)
	  {
	    open(CVSCO,$command_up);
	    while (<CVSCO>)
	    {
	      print $_;  
	    }
	    close CVSCO;
	  }
	  else
	  {
	    print "\n DEBUG: $command_up\n\n";
	  }
	}
      }
    }
    else
    {
      $command_co = "cvs $cvs_options -d $thorns{\"$th\"} co $tag $th |";
      #Get arrangements name
      $arrangement = $th;
      $arrangement =~ s:/[^/]*$::;
      $command_co_arr = "cvs $cvs_options -d $thorns{\"$th\"} co $tag $arrangement/README |";

      if ($debug != 1)
      {
	# Check that the repository exists
	DIE("Repository $thorns{\"$th\"} not found \n Are you connected to the network?\n Is the repository name spelt right in your thornlist file?") if (!&RepositoryExists($thorns{"$th"}));
	
	open(CVSCO,$command_co_arr);
	while (<CVSCO>)
	{
	  print $_;  
	}
	open(CVSCO,$command_co);
	while (<CVSCO>)
	{
	  print $_;  
	}
      }
      else
      {
	print "\n DEBUG: $command_co\n\n";
      }
    }
  }
  chdir "../..";
}

#/*@@
#  @routine   
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Get options to be passed to CVS
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub CVSOptions
{
  my($answer,$cvs_options);

  # Chose information or not
  do
  {
    $verbose = "no" if ((defined $defaults) && (! defined $verbose));

    print "Verbose checkout (y)es, n)o, h)elp) [no] : ";
    if (! defined $verbose) 
    {
       $answer = <STDIN> 
    } 
    else 
    {
       $answer = $verbose;
       print "$answer\n";
    }

    if ($answer =~ /^h/i)
    {
      VerboseCheckoutHelp();
    }
    elsif ($answer =~ /^y/i)
    {
      $cvs_options = " -z9 ";
    }
    else
    {
      $cvs_options = " -Q -z9 ";
    }
  } while ($answer =~ /^h/i);

  return $cvs_options;

}

sub VerboseCheckoutHelp 
{
  print "\n\n";
  print " CactusCode CVS checkout options\n";
  print " -------------------------------\n\n";
  print " By default, all checkouts using this script use the option -z9\n";
  print " which transfers a compressed version of each file across the \n";
  print " network\n\n";
  print " Choosing verbose checkout provides a report of all the files\n";
  print " as they are checked out from the chosen CVS repository, by using \n";
  print " the option -Q\n\n";
  print " cvs -z9 [-Q] -d <repository name> checkout <module name>\n\n";
}



#/*@@
#  @routine   GetCactusDir
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Get directory for Cactus installation
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub GetCactusDir
{
  $mydir = `pwd`;
  chomp($mydir);

  print "\n Directory for Cactus installation [$mydir] : ";

  if (defined $install) 
  {
      print " $install\n";
  }

  $install = "" if ((defined $defaults) && (! defined $install));

  if (! $install) 
  {
     $install = <STDIN>;
  }

  if ($install =~ /^$/) 
  {
    $install = $mydir;
  } 

  chomp($install);

# Needed for Cygwin.
  $install =~ s,^//(.)/,$1:/,;

  DIE("Directory $install not found") if (! -e $install);

  return $install;
}



#/*@@
#  @routine   GetThornList
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Get the full path of the thorn list
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub GetThornList
{

  my($thornlist) = @_;
  my($mydir);

  if ($thornlist)
  {
    if ($thornlist !~ m:^/:)
    {
      $mydir = `pwd`;
      chomp($mydir);
      $thornlist = $mydir."/".$thornlist;
    }
    DIE("ThornList $thornlist not found") if (! -e $thornlist);
  }
  else
  {
    print "\n No ThornList given, checking out Cactus flesh only.\n";
  }

  return $thornlist;

}



#/*@@
#  @routine   ParseThornList
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Read the thorn names and repositories from the thorn list
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub ParseThornList
{
  my($thornlist,$fleshpath,$tag) = @_;
  my($directive,$value);
  my($parfile);
  my($repository_user,$repository_location,$repository_name);
  my($repository_tag,$repository_type);

  $parfile = 0;

  # Get defaults from the flesh server path
  $fleshpath =~ m/^:+(\w*):+([^:.]*)\@([\w.]*):(.*)$/;
  $repository_type     = $1;
  $repository_user     = $2;
  $repository_location = $3;
  $repository_name     = $4;
  $repository_tag      = $tag;

  open(THORNLIST,"<$thornlist");
  while (<THORNLIST>)
  {

    if ($parfile)
    {
      if (m:^\s*\!*\s*$endstring:)
      {
	$parfile = 0;
	&WriteParameterFile($filename,$parfiledata);
	next;
      }
      else
      {
	if (m:^\s*\#(.*)$:)
	{
	  $parfiledata .= "$1\n";
	  next;
	}
      }
    }

    # Skip blank lines and comments
    next if (m:^\s*$:);
    next if (m:^\s*\#.*$:);

    # Parse directives
    if (m:^\s*\!\s*(\w+)\s(.*)$:)
    {
      $directive = &StripSpaces($1);
      $value = &StripSpaces($2);
 
      # Global directives 
      if ($directive =~ "DESC")
      {
        $description = $value;
        next;
      }
      # Thorn directives
      elsif ($directive =~ "REPOSITORY_TYPE")
      {
        $repository_type = $value;
        next;
      }
      elsif ($directive =~ "REPOSITORY_TAG")
      {
        $repository_tag = $value;
        next;
      }
      elsif ($directive =~ "REPOSITORY_LOCATION")
      {
        $repository_location = $value;
        next;
      }
      elsif ($directive =~ "REPOSITORY_USER")
      {
        $repository_user = $value;
        next;
      }
      elsif ($directive =~ "REPOSITORY_NAME")
      {
        $repository_name = $value;
        next;
      }
      elsif ($directive =~ "PARAMETER_FILE")
      {
	$value =~ m:([\w._]*)\s*<<\s*(\w*)\s*$:;
        $parfile     =  1;
        $filename    = $1;
        $endstring   = $2;
        $parfiledata = "";
#        print "Parameter file $filename\n";
#        print "End string $endstring\n";
        next;
      }
    }

    # Thorn name
    m:(.*)\#*:;
    $name = &StripSpaces($1);

    # Thorn repository
    $rep = ":$repository_type:$repository_user\@$repository_location:$repository_name";

#    print "Thorn is $rep $name\n";

    if ($name && $rep)
    {
      $thorns{$name} = $rep;
    }
    else
    {
      # Ignore: blank line
    }
  }
  close(THORNLIST);

  return %thorns;

}



#/*@@
#  @routine   DIE
#  @date      Sat Mar 11 15:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Tidy up and die
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub DIE
{
  my($message) = @_;

  chdir $mydir;
  die "\n $message\n\n";
}

#/*@@
#  @routine   WriteParameterFile
#  @date      Sun Apr 2 21:31:55 CET 2000   
#  @author    Gabrielle Allen
#  @desc 
#  Write parameter file
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub WriteParameterFile
{

  my($filename,$data) = @_;
  my($answer);

  print "\n Extracting embedded parameter file $filename\n";

  if (-e $filename)
  {
    print "\n Parameter file $filename exists, overwrite [n] : ";
    $answer = <STDIN> if (! defined $defaults);
    if ($answer !~ /^y/i)
    {
      return;
    }
  }

  open(FILE,">$filename") || DIE("Could not open $filename");

  print FILE $data;

  close(FILE);

}

sub GetFleshPath
{
  my($path);

  if (-e "CVS/Root")
  {
    open (ROOT,"<CVS/Root") || die "Could not open CVS/Root file";
    $path = <ROOT>;
    chomp($path);
  }

  return $path;
  
}


1;
