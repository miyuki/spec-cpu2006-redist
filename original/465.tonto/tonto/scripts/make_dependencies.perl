# This is a perl script to generate dependency files from Fortran 95 source
# code.  The script looks for "program", "module", "use", "include" and
# "#include" keywords.
#
# The object file that is made from the source file has the same name as the
# source file but with ".o" extension.  If the source file is for a program,
# then the extension is ".exe" or what is overriden with the -prog_ext argument.
#
# The default module extension is ".mod", but this can be overridden with the
# -mod_ext command line argument.
#
# The dependency on the source file is the full filename given to the script.
# Object files are prefixed with the $(objdir) directory.
# Module files are prefixed with the $(moddir) directory.
# Program files are prefixed with the $(bindir) directory.
# The $(objdir), $(moddir) and $(bindir) are to be set in the makefile. eg,
#       $(moddir) := /path/to/modules
#
# By default, module names from "use" statements are output in lowercase, but
# this can be overridden for those compilers that do uppercase instead.
#
# The script can handle multiple modules in the source file, in which case they
# are all targets in the single "make" dependency rule.  The list of these
# modules is saved as a variable, called $(x.mods), where x is the
# directory/suffix stripped part of the source file.  The beauty of this is that
# in GNU make, $($(*F).mods) gives you all the targets, and $^ gives you all the
# prerequisites!
#
# The script also has the ability to not output for certain modules appearing in
# "use" statements.  This is probably a good idea if you use modules supplied by
# your compiler vendor and you don't want to tell "make" to go searching system
# directories for them.  By default, "service_routines" is not output in
# dependency lists (this comes with Lahey's LF95).

#*******************************************************************************
# Usage :
#
#  perl -w make_dependencies.perl [-uc] [-skip_use mod_name] \\
#      ... [-skip_use mod_name] [-prog_ext name] [-mod_ext name] \\
#      [-obj_ext name] [-I include_dir] -o outfile -src infile
#
#  Where :
#
#     "-uc"                  : means translate module names to uppercase.
#                              (This is for Cray Unicos and SGI).
#     "-skip_use mod_name"   : means USE statements which reference
#                              module "mod_name" should be ignored.  No rules
#                              are produced for these.  To ignore multiple
#                              modules, include multiple "skip_use" arguments
#     "-prog_ext name"       : specify extension "name" for programs.
#                              Default is exe.
#     "-mod_ext name"        : specify extension "name" for modules.
#                              Default is mod.
#     "-obj_ext name"        : specify extension "name" for objects.
#                              Default is o.
#     "-I include_dir        : Use "include_dir" to search for included files
#     "-o outfile"           : Output dependency information to file "outfile"
#     "-src infile"          : Process source file "infile"

#*******************************************************************************
# $Id: make_dependencies.perl,v 1.2.2.5 2003/11/11 02:42:53 dylan Exp $ #
# Copyright 2002 Daniel Grimwood, <reaper@theochem.uwa.edu.au>
#
# Thanks to :
#   Ted Stern <stern@cray.com> for comments and a script to hack.
#   Michael Wester <wester@math.unm.edu> for ideas from makemake.perl, via Ted.
#*******************************************************************************

use File::Basename;

#*******************************************************************************
# Default settings.

# default to lowercase module file basename.
$ModCase=LCase;

# default program extension.
$prog_ext="exe";

# default module extension.
$mod_ext="mod";

# default object extension.
$obj_ext="o";

# define modules that are totally ignored.
push @skip_USE, "service_routines";
push @skip_USE, "f90_unix_io";
push @skip_USE, "mpi";

#*******************************************************************************
# Argument parsing.

$argerr=0;
$argerr=1 if ($#ARGV+1==0);

while (@ARGV) {
    $arg=shift;
    SWITCH: for ($arg) {
        /^-uc$/        && do { $ModCase=UCase; last; };
        /^-src$/       && do { $filename=shift; last; };
        /^-prog_ext$/  && do { $prog_ext=shift; last; };
        /^-mod_ext$/   && do { $mod_ext=shift; last; };
        /^-obj_ext$/   && do { $obj_ext=shift; last; };
        /^-o$/         && do { $depsfile=shift; last; };
        /^-skip_use$/  && do { push @skip_USE, shift; last; };
        /^-I$/         && do { push @include_dirs, shift; last; };
        /^-I([^ ]+)$/  && do { push @include_dirs, $1; last; };
        warn "Error : unexpected argument $arg\n";
        $argerr=1;
    }
}

#*******************************************************************************
# Error checks.

$prog_ext ne '' || do {$argerr=1; warn "Error : -prog_ext flag is null string\n"};
$obj_ext ne '' || do {$argerr=1; warn "Error : -obj_ext flag is null string\n"};
$mod_ext ne '' || do {$argerr=1; warn "Error : -mod_ext flag is null string\n"};
defined $filename || do {$argerr=1; warn "Error : -src flag not supplied\n"};
defined $depsfile || do {$argerr=1; warn "Error : -o flag not supplied\n"};

# Print out standard help message if there's an error.
if ($argerr==1) {
    die(
        "\nUsage:\n",
        "\t perl -w make_dependencies.perl [-uc] [-skip_use mod_name] \\\n",
        "\t\t... [-skip_use mod_name] [-prog_ext name] \\\n",
        "\t\t[-obj_ext name] [-I include_dir] -o outfile -src infile\n\n",
        "Where :\n",
        "\t\"-uc\"\t\t\t: means translate module names to uppercase.\n",
        "\t\t\t\t  (This is for Cray Unicos and SGI).\n",
        "\t\"-skip_use mod_name\"\t: means USE statements which reference\n",
        "\t\t\t\t  module \"mod_name\" should be ignored.  No\n",
        "\t\t\t\t  rules are produced for these.  To ignore\n",
        "\t\t\t\t  multiple modules, include multiple\n",
        "\t\t\t\t  \"skip_use\" arguments\n",
        "\t\"-prog_ext name\"\t: specify extension \"name\" for programs.\n",
        "\t\t\t\t  Default is exe\n",
        "\t\"-mod_ext name\"\t\t: specify extension \"name\" for modules.\n",
        "\t\t\t\t  Default is mod\n",
        "\t\"-obj_ext name\"\t\t: specify extension \"name\" for objects.\n",
        "\t\t\t\t  Default is o\n",
        "\t\"-I include_dir\"\t: Look for included files in \"include_dir\"\n",
        "\t\"-o outfile\"\t\t: Output dependency information to file\n",
        "\t\t\t\t  \"outfile\"\n",
        "\t\"-src infile\"\t\t: Process source file \"infile\"\n");
}

#*******************************************************************************
# now to construct all the variables and lists.
#*******************************************************************************

undef @incs;
undef @modules;
undef @dependencies;
undef @moddefs;
undef $prog;
undef @modlist;

# get the base part of the file.  (minus extension and directory).
$base = basename($filename);
$base =~ s/\.[^\.]+$//;

# Search for includes and USE calls:
&get_includes($filename);

# Recursively search include files.
if (defined @incs) {
  $n=0;
  while ($n < scalar(@incs)) {
    &get_includes($incs[$n]);
  } continue { $n++; }
}

# if not a "program" statement, then assume we are producing an object file.
if (! defined $prog) {
  $prog_ext = $obj_ext;
}

# If modules are defined, create macros for modules and a combined object+module
# target rule:
if (defined @moddefs) {
    # @modlist stores module names with .$mod_ext on the end
    foreach $module (&uniq(@moddefs)) {
        ($name = $module) =~ s/$/.$mod_ext/;
        push(@modlist, $name);
    }

    # Redefine @moddefs to the new @modlist
    @moddefs = @modlist;

    # Redfine @modlist with each element of @moddefs surrounded by
    # MAKE parens:
    undef @modlist;
    foreach $module (@moddefs) {
      ($name = $module) =~ s/.*/\$\(moddir\)\/$&/;
      push(@modlist, $name);
    }
}

# define the main target file to be compiled.
($fullfile = $base) =~ s/$/.$prog_ext/;
if (defined $prog) {
 # output the program with directory $(bindir).
  $fullfile =~ s/^/\$(bindir)\//;
} else {
 # output the object with directory $(objdir).
  $fullfile =~ s/^/\$(objdir)\//;
}

#*******************************************************************************
# open the dependency output file for writing.
#*******************************************************************************

open(OUTFILE,">" . $depsfile) or die "Cannot open $depsfile for writing.\n";

if (defined @modlist) {
    # This defines the macro $(base.prog_ext.mods) as the list of modules
    # produced.  Also, macros like $(modname.$mod_ext.mods) are also produced
    # which contain the same information.
    print OUTFILE "#Variables containing complete list of generated modules:\n";
    $word = "$base.$prog_ext.mods :=";
    &PrintWords($word, @modlist);
      print OUTFILE "\n";
    foreach $module (@moddefs) {
      $name = $module . ".mods";
      $word = "$name :=";
      &PrintWords($word, @modlist);
      print OUTFILE "\n";
    }
    print OUTFILE "\n";

    # This is done in case there is no simple mapping between module filename
    # and object filename.
    print OUTFILE "#Associate an object file with the generated modules:\n";
    foreach $module (@moddefs) {
        $name = $module . ".$prog_ext";
        print OUTFILE "$name := \$(objdir)\/$base.$prog_ext\n";
    }
    print OUTFILE "\n";
}

# Print out extra dependencies for the object file beyond the default:
if (defined @incs || defined @modules) {
    $skipUSE = &$ModCase( join(" ",@skip_USE) );
    foreach $module (&uniq(@modules)) {
        $a = $skipUSE;
        next if ($a =~ /$module/ );
        ($name = $module) =~ s/.*/\$\(moddir)\/$&.$mod_ext/;
        push(@dependencies, $name);
    }
}

print OUTFILE "#Dependencies of all files produced:\n";
if (defined @modlist) {
  $word = "$fullfile \$($base.$prog_ext.mods) :";
  &PrintWords($word, $filename, @dependencies, &uniq(@incs));
} else {
  $word = "$fullfile :";
  &PrintWords($word, $filename, @dependencies, &uniq(@incs));
}

print OUTFILE "\n\n";             # Add newline to .dep file
close(OUTFILE);



#*******************************************************************************
# Now the subroutine definitions....
#*******************************************************************************

#*******************************************************************************
# &PrintWords(list);
# print the list to span multiple lines if necessary.
# Based on the one from makemake.perl.
#
sub PrintWords {
    my ($columns, $wordlength);
    $columns = 79;

    foreach $word (@_) {
        $wordlength = length($word);
        if ($columns == 79) {
            print OUTFILE "$word";
            $columns -= $wordlength + 1;
        } elsif ($wordlength + 1 < $columns) {
            print OUTFILE " $word";
            $columns -= $wordlength + 1;
        } else {
           print OUTFILE " \\\n\t$word";
           $columns = 71 - $wordlength;
        }
    }
}

#*******************************************************************************
# &uniq(list)
# return the list in original order minus duplicates.
sub uniq {
    my @words;
    foreach $x (@_) {
      $_ = join(" ",@words);
      if (! /\b$x\b/) { push(@words,$x); }
    }
    return @words;
}

#*******************************************************************************
# &UCase(string)
# convert string to upper case
sub UCase {
    return uc($_[0]);
}

#*******************************************************************************
# &LCase(string)
# convert string to lower case
sub LCase {
    return lc($_[0]);
}

#*******************************************************************************
# &get_includes(filename)
# Search for include files in the given file, and add them to the @incs array.
# Also retrieve from this file other dependency information.
sub get_includes {
  my ($dir,$filehead,$inc,$inchead,$use,$usehead);

  $dir = "";
  $filehead = "";
  if (! open(FILE, $_[0])) {             # include file doesnt exist
    foreach $dir (@include_dirs) {       # look in the include paths
       if (!open(FILE, "$dir/$_[0]")) { close(FILE); next }
       else                           { $filehead = "$dir/"; last }
    }
    if ($filehead eq "") { die "Cannot find file $_[0]\n"; }
  }
  my $line;
  while ($line=<FILE>) {
    if ($line =~ /^\s*(\#|\?\?)*\s*include\s+[\"\']([^\"\']+)[\"\']/i) {
       $inc = $2;
       $inchead="";
       if (! open(INC, $inc)) {          # include file doesnt exist
          foreach $dir (@include_dirs) { # look in the include paths
             if (!open(INC, "$dir/$inc")) { close(INC); next }
             else                         { $inchead = "$dir/"; last }
          }
          if ($inchead eq "") { die "Cannot find include file $inc\n"; }
       }
       close(INC);
       push(@incs, $inchead . $inc);
    }
    $line =~ /^\s*use\s+(\w+)/i &&
            do {push(@modules, &$ModCase("$1"))};
    $line =~ /^\s*module\s+(\w+)/i && 
            do {push(@moddefs, &$ModCase("$1")) if not (lc($1) eq "procedure")};
    $line =~ /^\s*program\s+(\w+)/i && 
            do {$prog = &$ModCase($1)};
  }
  close(FILE);
  # Remove redundant entries.
  @incs = &uniq(@incs);
  @modules = &uniq(@modules);
}

