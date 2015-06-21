
# this scrips gathers some basic info on the system it runs on.
# This is intended to be included in bugreports etc.

$config = $ARGV[0];
$home = `pwd`;
chop($home);

$sep = "/";

# Work out where the config directory is
if($ENV{"CONFIGS_DIR"})
{
  $configs_dir = $ENV{"CONFIGS_DIR"};
}
else
{
  $configs_dir = "configs";
}

$current_directory = `pwd`;
chop($current_directory);
$current_directory =~ s,^//([^/]+)/,$1:/,;


# Look to see if MPI is defined
$extra = "$current_directory${sep}configs${sep}$config${sep}config-data${sep}cctk_Extradefs.h";

$mpi = 0;
if (-e "$extra")
{
  open(EXTRA,"<$extra");
  while(<EXTRA>)
  {
    if (/\#define CCTK_MPI/)
    {
      $mpi = "MPI"
    }
    else 
    {
      $mpi = "NO MPI";
    }
  }
}

#Get the version number of the makefile
open (MF,"<Makefile");
while (<MF>)
  {
    if (/CCTK_VERSION_MAJOR\s+=\s+(\S+)/) 
      {
	$version.="$1.";
      }
    if (/CCTK_VERSION_MINOR\s*=\s*(\S+)/) 
      {
	$version.="$1.";
      }
    if (/CCTK_VERSION_OTHER\s*=\s*(\S+)/) 
      {
	$version.="$1";
      }
  }

close(MF);

#Get the CVS repository
if (-e "CVS/Root") 
{
   open(CVS,"CVS/Root");
   $cvsrep = <CVS>;
   chop($cvsrep);
} 
else
{
    printf "NO CVS";
}


#get the uname output
$uname = `uname -a`;
chop($uname);

#get hinv
if ($uname=~m/IRIX/i) 
{
  $hinv = `hinv`;
}


# Create sysinfo directory if needed
if (! -d "sysinfo")
{
  mkdir("sysinfo", 0755) || die "Unable to create sysinfo directory";
}

printf "Generating info file: sysinfo${sep}$config.sysinfo ...\n\n";

open (INFO,">sysinfo${sep}$config.sysinfo");

print INFO "VERSION: $version\n";
print INFO "UNAME  : $uname \n";
print INFO "MPI    : $mpi \n";
print INFO "CVSROOT: $cvsrep\n";
if ($hinv=~m/.*/) {
  print INFO "HINV   : $hinv \n";
}
close (INFO);
  


