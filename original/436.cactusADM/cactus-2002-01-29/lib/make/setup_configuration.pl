#! /usr/bin/perl -s
#/*@@
#  @file      setup
#  @date      Fri Jan  8 13:48:48 1999
#  @author    Tom Goodale
#  @desc 
#  Setup file for a new configuration in the CCTK
#  Invocation is 
#  setup_configuration [-reconfig] [-config_file=<options>] config_name
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/make/setup_configuration.pl,v 1.19 2002/01/06 12:19:32 allen Exp $
#@@*/

$top = `pwd`;

chop $top;

$configure = "sh $top/lib/make/configure";

$uname = `uname`;

$uname =~ s/(sn\d\d\d\d|jsimpson)/UNICOS\/mk/;

chop $uname;

if($#ARGV > -1)
{
  $config = shift(@ARGV);
}
else
{
  $config = $uname;
}

# Work out if there is a user default file

if($ENV{"CACTUSRC_DIR"})
{
  if (-e $ENV{"CACTUSRC_DIR"}."/.cactus/config")
  {
    $default_file = $ENV{"CACTUSRC_DIR"}."/.cactus/config";
  }
}
elsif (-e $ENV{"HOME"}."/.cactus/config")
{
  if (-e $ENV{"HOME"}."/.cactus/config")
  {
    $default_file = $ENV{"HOME"}."/.cactus/config";
  }
}

# Work out where the config directory is

if($ENV{"CONFIGS_DIR"})
{
  $configs_dir = $ENV{"CONFIGS_DIR"};
}
else
{
  $configs_dir = "configs";
}

# Replace slashes with underscores.
$config =~ s:[/\\]:_:g;

# The configs directory doesn't exist.
if (! -d "$configs_dir" && ! -l "$configs_dir")
{
  print "Completely new cactus build.  Creating config database\n";

  mkdir("$configs_dir", 0755)
  
}

chdir "$configs_dir" || die "Internal error - could't enter $configs_dir";

# The specified configuration doesn't exist
if (! -d "$config" && ! -l "$config")
{
  print "Creating new configuration $config.\n";

  for $dir ("$config", "$config/build", "$config/lib", "$config/config-data")
  {
    mkdir("$dir",0755) || die "Internal error - could't create $dir";
  }

}
else
{
  print "Reconfiguring $config.\n";
  $reconfiguring = 1;
}


chdir "$config" || die "Internal error - could't enter $configs_dir/$config";

chdir "config-data" || die "Internal error - could't enter $configs_dir/$config/config-data";

%CONFIGURED = &SetConfigureEnv();

$configure_command = &DetermineConfigureCommand($configure, %ENV);

# remove cached configure options
unlink 'config.cache' if (-f 'config.cache');

system("$configure_command");

$retcode = $? >> 8;

chdir "..";

open(INFO, ">config-info") || die "Internal error - couldn't create $configs_dir/$config/config-info";

print INFO "CONFIG        : $config\n";
print INFO "CONFIG-FLAGS  : ";
foreach $setting (keys %CONFIGURED)
{
  print INFO "$setting=$CONFIGURED{\"$setting\"} ";
}
print INFO "\n";
print INFO "CONFIG-DATE   : " . gmtime(time()) . "\n"; 

$host = `hostname`;

chop $host;

print INFO "CONFIG-HOST   : " . $host . "\n";

print INFO "CONFIG-STATUS : $retcode\n\n";

close(INFO);

exit $retcode;


#/*@@
#  @routine    
#  @date       Fri Feb 19 19:53:48 1999
#  @author     Tom Goodale
#  @desc 
#  Sets the environment for running the configure script.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub SetConfigureEnv
{
  local($line_number);
  # Set a default name for the configuration
  $ENV{"EXE"} = "cactus_$config";

  # Set variables from user default file first
  if ($default_file)
  {
    print "Using configuration options from user defaults...\n";
    
    open(INFILE, "<$default_file") || die "Cannot open configuration file $config_file";
    
    $line_number;

    while(<INFILE>)
    {
      $line_number++;

      #Ignore comments.
      s/\#(.*)$//g;
      
      #Remove spaces at end of lines
      s/\s*$//;

      s/\n//g;		# Different from chop...

      #Ignore blank lines
      next if (m:^\s*$:);

      # Match lines of the form 
      #     keyword value
      # or  keyword = value
      m/\s*([^\s=]*)([\s]*=?\s*)(.*)\s*/;
      
      if($1 && $2)
      {
	print "Setting $1 to '$3'\n";
	$ENV{"$1"} = $3;
	# Remember it for writing to config-info
	$option = AddQuotes($3);
	$CONFIGURED{"$1"} = $option;
      }
      else
      {
	print "Could not parse configuration line $line_number...\n'$_'\n";
      }
    }
    print "End of options from user defaults.\n";

    
    close(INFILE);
  }


  if($config_file)
  {
    # The user has specified a configuration file

    print "Using configuration options from $config_file...\n";
    if($config_file !~ m:^/:)
    {
      $config_file = "$top/$config_file";
    }
    open(INFILE, "<$config_file") || die "Cannot open configuration file $config_file";
    
    $line_number = 0;

    while(<INFILE>)
    {
      $line_number++;

      #Ignore comments.
      s/\#(.*)$//g;
      
      #Remove spaces at end of lines
      s/\s*$//;

      s/\n//g;		# Different from chop...

      #Ignore blank lines
      next if (m:^\s*$:);

      # Match lines of the form 
      #     keyword value
      # or  keyword = value
      m/\s*([^\s=]*)([\s]*=?\s*)(.*)\s*/;
      
      if($1 && $2)
      {
	print "Setting $1 to '$3'\n";
	$ENV{"$1"} = $3;
	# Remember it for writing to config-info
	$option = AddQuotes($3);
	$CONFIGURED{"$1"} = $option;
      }
      else
      {
	print "Could not parse configuration line $line_number...\n'$_'\n";
      }

    }
    print "End of options from $config_file.\n";
    
    close(INFILE);
  }

  $commandline = $ENV{"MAKEFLAGS"};
  $used_commandline = 0;
  while ($commandline =~ /^(.*)\s+(\w*)\s*=\s*([_+\-\.\w\\\/\s]*)\s*$/)
  {
    if ($2 ne "options")
    {
      if (!$used_commandline)
      {
	print "Using configuration options from configure line\n";
      }
      $used_commandline = 1;
      $ENV{"$2"} = $3;
      # Remember it for writing to config-info
      $option = AddQuotes($3);
      $CONFIGURED{"$2"} = $option;

      print "Setting $2 to $option\n";
    }
    $commandline=$1;
    #  print "New commandline = <$commandline>\n";
  }
  if ($used_commandline)
  {
    print "End of options from configure line\n";
  }

  return %CONFIGURED;

}

sub DetermineConfigureCommand
{
  my($configure, %env) = @_;
  my($configure_command);

  $configure_command = "$configure";

  if($ENV{"BUILD_MACHINE"})
  {
    $configure_command .= " --build=". $ENV{"BUILD_MACHINE"};
  }

  if($ENV{"TARGET_MACHINE"})
  {
    $configure_command .= " --target=". $ENV{"TARGET_MACHINE"};
  }
     
  if($ENV{"HOST_MACHINE"})
  {
    $configure_command .= " --host=". $ENV{"HOST_MACHINE"};
  }

  return $configure_command;
}

sub AddQuotes
{
  local($arg) = @_;

  if ($arg =~ /\\/)
  {
    $arg =~ s:\\::g;
    $arg = "\"$arg\"";
  }

  return $arg;
}
