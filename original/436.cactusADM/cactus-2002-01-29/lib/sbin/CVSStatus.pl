#/*@@
#  @file      CVSStatus.pl
#  @date      Mon Mar  6 01:20:01 CET 2000
#  @author    Gabrielle Allen
#  @desc 
#     Processes output from cvs status and gives better messages
#     Original version by Paul Walker
#     $Header: /cactus/Cactus/lib/sbin/CVSStatus.pl,v 1.2 2000/03/09 09:25:06 allen Exp $
#  @enddesc 
#@@*/

&writeheader();

$full = 0;

open (CS, "cvs status 2>&1 |");
$gotone = 0;

while (<CS>) {

    if (m/============/ || m/Examining/) {
	if ($gotone) {
	  $module =~ m:^\s*(/\w*/\w*):;
	  $module = $1;
          if ($module ne $module_old) {
            print "\nRepository: $module\n";$module_old=$module;
          }        
	  # Get differences between versions
          if ($case =~ /diff/i)
	  {
	    print "\n\n*******************************************************************\n\ncvs diff -r $rversion $dir/$file\n\n";
	    write;
	    print "\n\n";
	    open (DIFF, "cvs diff -r $rversion $dir/$file |");
	    while (<DIFF>) {print;}
	  }
	  else
	  {
	    write;
	  }
	}
	$gotone = 0;
	$module="";
	$file = ""; 
	$status = ""; 
	$version=""; 
	$rversion="";
    }
    if (m/File: (\S+)/) {$file = $1;}
    if (m/Status: (.+)\s/) {
	$status = $1;
	if (!($status =~ m/Up-to-date/)) {
	    $gotone = 1;
	}
    }
    if (m/Examining\s*(\S+)\s*$/) {$dir = $1;}
    if (m/Working revision:\s*(\S+)\s/) {$version = $1;}
    if (m/Repository revision:\s*(\S+)\s+(\S+)/) {
	$rversion = $1; $module = $2;
    }

}
exit;

format STDOUT =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<<<<< @<<<<<<</@<<<<<<<
$file, $status, $version, $rversion
.


sub writeheader
{
my ($case) = @_;

print "$case\n";
if ($case =~ /diff/)
{
print <<EOF;

CVS Diff report 

File                                         Status          Local/Remote ver
-------------------------------------------------------------------------------
EOF
}
else
{
print <<EOF;

CVS Status report 

File                                         Status          Local/Remote ver
-------------------------------------------------------------------------------
EOF
}
}
