#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# foo.perl
#
# Synopsis :
#
#   This script is used to convert foo files into other formats.  It can output
#   to standard compliant Fortran 95 code, and/or produce HTML documentation.
#
# Usage :
#
#   perl -w ./foo.perl [ -fortran file.f95
#
#                      [-fortranint file.int] [-fortranuse file.use] ]
#
#                      [-htmlshort short.html -htmllong long.html]
#
#                      [-types types.foo] [dir/]file.foo
# Where :
#
#   [dir/]file.foo        is the foo file to be preprocessed. The .foo extension
#                         is required.  Also, "file" must be the lower case name
#                         of any module or program defined in "file.foo". If
#                         "dir/" is present, then the directory "dir" is used to
#                         find any required inherited foo modules.
#
#   -fortran file.f95     is the generated fortran90+ file.
#
#   -fortranint file.int  is the generated fortran90+ generic interface file; if
#                         not supplied and -fortran supplied, defaults to
#                         "file.int" where "file" is the head part of argument
#                         "file.foo".  -fortran must be supplied.
#
#   -fortranuse file.use  is the generated fortran90+ USE file; if not supplied
#                         and -fortran is supplied, defaults to "file.int" where
#                         "file" is the head part of argument "file.foo"
#                         -fortran must be supplied.
#
#   -types types.foo      specifies the foo types file, containing all type
#                         definitions; if not supplied, defaults to
#                         "dir/types.foo" where "dir" is the directory head for
#                         the foo file.
#
#   -htmllong.html        specifies the name of the long version of the HTML
#                         documentation to be produced.
#
#   -htmlshort.html       specifies the name of the short version of the HTML
#                         documentation to be produced.  Note that both
#                         -htmllong and -htmlshort must be given in order to
#                         produce HTML documentation.
#
# (c) Dylan Jayatilaka, University of Western Australia, 2002.
# (c) Daniel Grimwood, University of Western Australia, 2002.
#
# ! $Id: foo.perl,v 1.14.2.15 2003/10/06 14:12:46 dylan Exp $
#-------------------------------------------------------------------------------

use English;            # Get rid of horrible Perl short forms
use File::Spec ('splitpath','catpath');
#use strict;             # Make sure the scope of all variables is declared.

$, = ' ';		# set output field separator
$\ = "\n";		# set output record separator
####$/ = ";\n";             # input record separator

################################################################################
#                           File names and handles
#
my $typfile;
my $f90file;
my $intfile;
my $usefile;
my $foofile;
my $short_intfile;
my $short_usefile;
my $HTMLSHORTHANDLE;
my $HTMLLONGHANDLE;
my $foohandle;
my $oldhandle;
my @filestack = ();
my @filenamestack = ();
my %filelinenum = ();

################################################################################
#                                Scoping units.
#
my $scopeunit = '';       # the current scoping unit.
my $oldscope;             # the previous scoping unit.
my $newscopeunitfound;    # whether a new scoping unit is on this line.
my $lastfoundscope;       # the last found scoping unit. (may be an end-scope)
my @scope;                # list of the scoping units currently nested.

################################################################################
my $html_done_use_list = 0;   # Whether have outputted the use list in HTML.
my @types;
my %used = ();
my %function_res_type = ();   # Types of function results.
my %tonto_type = ();
my %global_type = ();         # Types of all globally available variables.
my %local_type = ();          # Types of all locally available variables.
my $inp;

my $module_name;
my $long_module_name;
my $lc_long_module_name;
my $short_routine_name;
my $mod;
my %subst;
my %substs;
my $inherit;
my $linenum;
my %element_type;
my $name;
my $head_name;
my $html_lastline;           # for the HTML
my $first_interface;    # for the HTML
my %linenum;   # line nummber for each filehandle.
my $arg;
my $args;
my $not_blank;
my %case;
my $is_function;
my $is_program;
my $is_private;
my $is_public;
my $being_inherited;
my $last_was_ensure;
my $htmlshortfile;
my $htmllongfile;
my $n_case_opt;
my $current_rout_name;
my $previous_rout_name;
my @rout_name_stack;
my $first_active_line = 0;      # Set to 0 initially, and at start of routine
my $this_is_first_active_line;
my $type_name;
my $self;
my %routine_cnt;
my %arglist;

my $do_unknown = 1;             # Set TRUE if UNKNOWN construct is made
my $do_generic = 1;             # Set TRUE if generic interfaces are to be used

## -----------------------------------------------------------------------------
## Set up the element_types of known list types
## -----------------------------------------------------------------------------

%element_type = (     # Components of the list types
    'REALVEC'             => 'REAL',
    'REALVEC_'            => 'REALVEC',
    'INTVEC'              => 'INT',
    'INTVEC_'             => 'INTVEC',
    'CPXVEC'              => 'CPX',
    'BINVEC'              => 'BIN',
    'STR'                 => 'STR',
    'BSTR'                => 'STR',
    'STRVEC'              => 'STR',
    'STRMAT'              => 'STR',
    'REALVECVEC'          => 'REALVEC_',
    'INTVECVEC'           => 'INTVEC_',
    'REALMATVEC'          => 'REALMAT_',
    'REALMAT3VEC'         => 'REALMAT3_',
    'REALMAT4VEC'         => 'REALMAT4_',
    'INTVECMAT3'          => 'INTVEC_',
    'SHELLVEC'            => 'SHELL',
    'SHELLPAIRVEC'        => 'SHELLPAIR',
    'BASISVEC'            => 'BASIS',
    'COPPENSBASISVEC'     => 'COPPENSBASIS',
    'COPPENSORBITALVEC'   => 'COPPENSORBITAL',
    'SLATERBASISVEC'      => 'SLATERBASIS',
    'SLATERSHELLVEC'      => 'SLATERSHELL',
    'ATOMVEC'             => 'ATOM',
    'REFLECTIONVEC'       => 'REFLECTION',
    'IRREPVEC'            => 'IRREP',
    'MARCHINGCUBEVEC'     => 'MARCHINGCUBE',
    'MOLVEC'              => 'MOL',
    'REALMAT'             => 'REAL',
    'REALMAT_'            => 'REALMAT',
    'INTMAT'              => 'INT',
    'BINMAT'              => 'BIN',
    'CPXMAT'              => 'CPX',
    'REALMAT3'            => 'REAL',
    'REALMAT3_'           => 'REALMAT3',
    'INTMAT3'             => 'INT',
    'BINMAT3'             => 'BIN',
    'CPXMAT3'             => 'CPX',
    'REALMAT4'            => 'REAL',
    'REALMAT4_'           => 'REALMAT4',
    'INTMAT4'             => 'INT',
    'CPXMAT4'             => 'CPX',
    'REALMAT5'            => 'REAL',
    'INTMAT5'             => 'INT',
    'CPXMAT5'             => 'CPX',
);

## -----------------------------------------------------------------------------
## Set up some substitutions which always apply
## -----------------------------------------------------------------------------

%subst = (          # Substitutions .................
    'BSTR'     => 'STR(BSTR_SIZE)',
    'STR'      => 'STR(STR_SIZE)',
    'VEC'      => 'VEC(:)',
    'MAT'      => 'MAT(:,:)',
    'MAT3'     => 'MAT3(:,:,:)',
    'MAT4'     => 'MAT4(:,:,:,:)',
    'MAT5'     => 'MAT5(:,:,:,:,:)',
);

%substs = (                      # STRVEC specific substitutions ...........
                                 # should not need to change these !
    'STRVEC'      => 'STRVEC(STR_SIZE,:)',
    'BSTRVEC'     => 'STRVEC(BSTR_SIZE,:)',
);

## -----------------------------------------------------------------------------
## Process argument list for .fortran, .int, .use, types.foo and file.foo files
## And check that everyuthing is OK, as far as possible.
## -----------------------------------------------------------------------------

$typfile = ""; # This is the types.foo file
$fortranfile = ""; # This is the .fortran file produced
$fortranintfile = ""; # This is the .int file produced
$fortranusefile = ""; # This is the .use file produced
$foofile = ""; # This is the required .foo file to work on
$htmlshortfile = ""; # This is the short .html file produced
$htmllongfile  = ""; # This is the long .html file produced

$argerr=0;
$argerr=1 if ($#ARGV+1==0);

while (@ARGV) {
    $arg = shift;
    $_ = $arg;
    if (/^-/) {
        /^-types\b/      && do {$typfile=shift; next; };
        /^-fortran\b/    && do {$fortranfile=shift; next; };
        /^-fortranint\b/ && do {$fortranintfile=shift; next; };
        /^-fortranuse\b/ && do {$fortranusefile=shift; next; };
        /^-htmlshort\b/  && do {$htmlshortfile=shift; next; };
        /^-htmllong\b/   && do {$htmllongfile=shift; next; };
        /^-nogeneric\b/  && do {$do_generic=0; next; };
        warn "\n Error : unexpected argument $arg\n";
        $argerr=1;
    }
    $foofile = $arg;
    if (@ARGV > 0) {
        warn "\n Error : more than one foo file specified, @ARGV\n";
        $argerr=1;
        last;
    }
}

$do_html=1;
if ($htmllongfile eq "" || $htmlshortfile eq "") {$do_html=0};
$do_fortran=1;
if ($fortranfile eq "") {$do_fortran=0};

$foofile   =~ /(\w+)(\.foo)?$/;
$head_name = $1;
$tail_name = $2;
if ($head_name eq "") {
        warn "\n Error : no head part for file.foo\n";
        $argerr=1;
}
if ($tail_name eq "") {
        warn "\n Error : tail of file.foo does not end in .foo\n";
        $argerr=1;
}


($volume,$foodir,$file) = File::Spec->splitpath($foofile);

if ($do_fortran) {
  if ($fortranintfile eq "") { $fortranintfile = $head_name . ".int"; }
  if ($fortranusefile eq "") { $fortranusefile = $head_name . ".use"; }
} else {
  if ($fortranintfile ne '' || $fortranusefile ne '') {
    warn "\n Error : must specify -fortran option";
    $argerr=1;
  }
}

if ($typfile eq "") { $typfile = File::Spec->catpath($volume,$foodir,"types.foo"); }

if (! open(FOOFILE, $foofile)) {
        warn "\n Error : foofile $foofile does not exist";
        $argerr=1;
}
close(FOOFILE);
if (! open(TYPEFILE,$typfile)) {
        warn "\n Error : type file $typfile does not exist";
        $argerr=1;
}
close(TYPEFILE);

##                       #################### input error ###################

if ($argerr==1) {
print << 'EOF';

 Usage :

   perl -w ./foo.perl [ -fortran file.fortran

                      [-fortranint file.int] [-fortranuse file.use] ]

                      [-htmlshort short.html -htmllong long.html]

                      [-types types.foo] [dir/]file.foo
 Where :

   [dir/]file.foo        is the foo file to be preprocessed. The .foo extension
                         is required.  Also, "file" must be the lower case name
                         of any module or program defined in "file.foo". If
                         "dir/" is present, then the directory "dir" is used to
                         find any required inherited foo modules.

   -fortran file.f95     is the generated fortran90+ file.

   -fortranint file.int  is the generated fortran90+ generic interface file; if
                         not supplied and -fortran supplied, defaults to
                         "file.int" where "file" is the head part of argument
                         "file.foo".  -fortran must be supplied.

   -fortranuse file.use  is the generated fortran90+ USE file; if not supplied
                         and -fortran is supplied, defaults to "file.int" where
                         "file" is the head part of argument "file.foo"
                         -fortran must be supplied.

   -types types.foo      specifies the foo types file, containing all type
                         definitions; if not supplied, defaults to
                         "dir/types.foo" where "dir" is the directory head for
                         the foo file.

   -htmllong.html        specifies the name of the long version of the HTML
                         documentation to be produced.

   -htmlshort.html       specifies the name of the short version of the HTML
                         documentation to be produced.  Note that both
                         -htmllong and -htmlshort must be given in order to
                         produce HTML documentation.
EOF
  die "Stopped";
}

## -----------------------------------------------------------------------------
## Define the type table arrays
## -----------------------------------------------------------------------------

$local_type{'variable name X'} = 'type of the variable X';

$global_type{'self'}   = 'The current module name' ;
$global_type{'tonto'}  = 'SYSTEM';
$global_type{'tonto_parallel'}  = 'PARALLEL';
$global_type{'stdin'}  = 'TEXTFILE';
$global_type{'stdout'} = 'TEXTFILE';
$global_type{'stdout'} = 'TEXTFILE';
$global_type{'std_time'} = 'TIME';

%local_type = %global_type;

$tonto_type{'type X'}{'field Y of X'} = 'type of the field Y of X';

## -----------------------------------------------------------------------------
## Argument lists of subroutines
## -----------------------------------------------------------------------------

$arglist{"argument name"} = "argument index";

$being_inherited = 0;

################################################################################
## -----------------------------------------------------------------------------
## Analyse the types.foo file
## -----------------------------------------------------------------------------

open(TYPEFILE,$typfile);
&start_foofile(*TYPEFILE,$typfile);

# Search for a type line

while (chop($X = <TYPEFILE>)) {

    last if ($X =~ '^end' );                        # End of types.foo
    next if ($X !~ 'type +([a-zA-Z_]\w*)_type\b');  # Look for next type definition
    ($type_name = $X) =~ s/.*type +//s;             # Get the type name
    $type_name =~ s/_type\b.*//s;
    $type_name = uc($type_name);
    $tonto_type{$type_name}{"--exists--"} = 1;      # Create the hash table for this type

    while (chop($X = <TYPEFILE>)) {                 # Analyse the type declaration line
        last if ($X =~ '^ +end(?:\s+|$|!)' );       # End of types def
        next if ($X =~ '^ *!' );                    # Comment
        &analyse_types($X,$tonto_type{$type_name})
    }

    delete $tonto_type{$type_name}{"--exists--"};   # Delete this token

}

close(TYPEFILE);
&end_foofile;

################################################################################
# Construct a list of all the types we know exist.
  @types = (values %element_type,keys %tonto_type);
  foreach(@types) { $arr{$_}++ }
  @types = sort keys %arr;

################################################################################
## -----------------------------------------------------------------------------
## Loop over the .foo file lines and get the procedure interfaces.
## -----------------------------------------------------------------------------

$linenum = 0;
$inp = '';  # current processed input line

open(FOOFILE, $foofile);
&start_foofile(*FOOFILE,$foofile);
LINE: while (<$foohandle>) {
    $filelinenum{$foohandle}++;
    &process_foo_line;
}
close(FOOFILE);
&end_foofile;

$scopeunit = "";
undef %routine_cnt;
undef @scope;
undef %arglist;
$arglist{"argument name"} = "argument index";

################################################################################
## -----------------------------------------------------------------------------
## Loop over the .foo file lines and generate the fortran/HTML file lines
## -----------------------------------------------------------------------------

if ($do_html) { &html_start; }
if ($do_fortran) { &fortran_start; }

open(FOOFILE, $foofile);
&start_foofile(*FOOFILE,$foofile);
$linenum = 0;
LINE: while (<$foohandle>) {
    &process_foo_line;

    if ($do_html) { &html_process_foo_line; }
    if ($do_fortran) { &fortran_process_foo_line; }

}
&end_foofile;
close(FOOFILE);

if ($do_fortran) { &fortran_end; }
if ($do_html) { &html_end; }

################################################################################
########################################## Routines ############################
################################################################################

################################################################################
sub start_foofile {
  if (defined $foohandle) {$oldhandle = $foohandle;}
  else {$oldhandle = '';}
  $foohandle = $_[0];
  push @filestack,$foohandle;
  push @filenamestack,$_[1];
  $filelinenum{$foohandle} = 0;
}

################################################################################
sub end_foofile {
  $filelinenum{$foohandle} = 0;
  $oldhandle = $foohandle;
  pop @filestack;
  pop @filenamestack;
  $foohandle = $filestack[$#filestack];
}

################################################################################
# Die gracefully, outputting the error message and the line number of the file
# that caused the error.
sub crash_file {
 my $errmsg = $_[0];
 my $filename = $filenamestack[$#filenamestack-1];
 my $fileline = $filelinenum{$foohandle};
 print STDERR "Error at line $fileline of $filename : $errmsg";
 exit 1;
}

################################################################################
# Process a line of foo code, to get all required information.
sub process_foo_line {
  my($parent_scope);
    chop;	  # strip record separator

    $inp  = $_;   # Get the line
    $inpi = $inp; # Keep for inheritance

    if ($foohandle eq $oldhandle) { $linenum++; } # update line number for original foo file.

    $finished_this_line = 0;
    $newscopeunitfound = 0;
    $this_is_first_active_line = 0; # first_active_line NOT set to zero

    $not_blank = ($inp =~ '^ *\S' && $inp !~ '^ *[!]' );
    ($inp,$comment) = &split_by_comment($inp);

    if ($not_blank) {               # if not blank ...

      ####### Look for a new scoping unit ####################################
      &find_new_scoping_unit($inp);

      ####### Look for first active line #####################################
      if (($oldscope eq 'subroutine' || $oldscope eq 'function') && $#scope <= 3) {
         &check_for_first_active_line($inp); 
      }

      ########################################### Found a NEW scope unit #####
      if ($newscopeunitfound) {
        if ($scopeunit eq 'module') { &do_new_module_scope($inp); }
        elsif ($scopeunit eq 'virtual module') { &do_new_virtual_module_scope($inp); }
        elsif ($scopeunit eq 'program') { &do_new_program_scope($inp); }
        elsif ($scopeunit eq 'interface' && $parentscope =~ 'module') { &do_new_interface_scope($inp); }
        elsif ($scopeunit eq 'type' && $parentscope =~ 'module') { &do_new_type_scope($inp); }
        elsif ($scopeunit eq 'empty-end' ) { &do_empty_end($inp); } # Inheritance in here
        elsif ($scopeunit eq 'named-end' ) { &do_named_end($inp); }
        elsif ($scopeunit eq 'select' ) { $n_case_opt = 0 ; }
        elsif ($scopeunit eq 'subroutine' || $scopeunit eq 'function') { &do_new_routine_scope($inp); }
      }

      elsif ($scopeunit eq 'program' || $scopeunit =~ 'module') { &process_module($inp); }
      elsif ($scopeunit eq 'interface' && $parentscope =~ 'module') { &process_module_interface($inp); }
      elsif ($scopeunit eq 'subroutine' || $scopeunit eq 'function') { &process_routine($inp); }
      elsif ($scopeunit eq 'interface') { &process_interface($inp); }
      elsif ($scopeunit eq 'type') { &process_type($inp); }
    }

    if (defined $comment) {$inp .= $comment;} # add comment back onto line.
    if ($scopeunit eq 'subroutine' || $scopeunit eq 'function') {
       if (defined $routine_attributes{$current_rout_name}{'inherited'}) { 
          $inherit .= $inpi . "\n";
       }
    }

}

################################################################################
sub analyse_rout_name {

    my($X) = @_;
    my($name,$function,$indent,$args);

    #           space routine   arguments            function result                attributes
    $X =~ m/^ (\s*)  (\w+) (?:\(([\s\w,]+)\))? \s* (?:result\s*\(([\s\w]+)\))? (?:\s*:::\s*(.*?))?\s*$/x;
    if (defined $1) {$indent = $1} else { $indent = ''; }
    if (defined $2) {$name = $2} else {&crash_file("Routine name not found where expected."); }
    if (defined $3) {$args = $3} else {$args = ''; }
    if (defined $4) {$result = $4} else {$result = ''; }
    if (defined $5) {$attr = $5} else {$attr = ''; }

    # 
  # if ($do_generic==0) { $name = $module_name . '_' . $name }

    # Add to the routine name stack
    $current_rout_name = $name;
    push @rout_name_stack, $name;
    undef $routine_attributes{$name};     # Clear the attributes of the function/subroutine.

    # indent
    $routine_attributes{$name}{'indent'}=$indent;

    # Is it a function?
    if ($result ne '') {
      $routine_attributes{$name}{'function'} = 1;
      $routine_attributes{$name}{'function_result'} = $result;
    }

    # Analyse routine attributes (after ::: specifier)
    if ($attr ne '') {
      @tmp = split(/ *,? +/,$attr);
      foreach (@tmp) {
        /^leaky/            && do { $routine_attributes{$name}{'leaky'}=1;            next}; # has a memeory leak
        /^private/          && do { $routine_attributes{$name}{'private'}=1;          next}; # generic interface made private
        /^public/           && do { $routine_attributes{$name}{'public'}=1;           next}; # specific interface made public
        /^selfless/         && do { $routine_attributes{$name}{'selfless'}=1;         next}; # self is not first argument
        /^functional/       && do { $routine_attributes{$name}{'functional'}=1;       next}; # first arg is function, return REAL
        /^routinal/         && do { $routine_attributes{$name}{'routinal'}=1;         next}; # first arg is function
        /^template/         && do {                                            next}; # will be inherited
        /^pure/             && do { $routine_attributes{$name}{'pure'}=1;             next}; # pure routine
        /^always_pure/      && do { $routine_attributes{$name}{'always_pure'}=1;             # must always be pure (no macro override)
                                    $routine_attributes{$name}{'pure'}=1;             next};
        /^elemental/        && do { $routine_attributes{$name}{'elemental'}=1;               # elemental routine
                                    $routine_attributes{$name}{'pure'}=1;             next};
        /^always_elemental/ && do { $routine_attributes{$name}{'always_elemental'}=1;        # must always be elemental
                                    $routine_attributes{$name}{'pure'}=1;             next};
        /^recursive/        && do { $routine_attributes{$name}{'recursive'}=1;        next}; # recursive
        /^get_from/         && do { /[(]([^)]*)[)]/; # remove brackets
                                    $routine_attributes{$name}{'inherited'}=1;               # inherited routine
                                    $routine_attributes{$name}{'parent_module'}=$1;          # where inherited from
                                    $inherit = "";                             next}; # the inherit match string
        die "\nError : unexpected routine attribute, $_.\nStopped";
      }
    }

    if ($#scope > 2) { $routine_attributes{$name}{'selfless'} = 1; }

    # Analyse the routine arguments
    if (defined $routine_attributes{$name}{'selfless'}) {
      $args = "(" . $args . ")";
    } else {
      if ($args ne '') {        # this routine has arguments ....
	$args = "(self,$args)";       # insert self as first argument
      }
      else {                            # this routine has no arguments
	$args = "(self)";             # insert self as only arg
      }
    }
    $routine_attributes{$name}{'args'}=$args;

    # create and destroy routines automatically get the leaky attribute.
    if ($name =~ m'(?:create)|(?:destroy)') { $routine_attributes{$name}{'leaky'}=1; }
}

################################################################################
sub store_arglist {
# store the arguments for this routine.
    my($args) = @_;
    my($name,$narg);

    $narg = 0;
    while ($args =~ /(\w+)/g) {
       $narg++;
       $arglist{$1} = $narg;
    }
    $name = $current_rout_name;
    if (! (defined $routine_attributes{$name}{'functional'} ||
           defined $routine_attributes{$name}{'routinal'} ||
           defined $routine_attributes{$name}{'selfless'})) {
       $narg++;
       $arglist{'self'} = $narg;
    }
}

################################################################################
sub contains_arg {
    my($name,$arg,$has) = @_;
    $has = 0;
    foreach $arg (keys %arglist) {
       if ($name =~ /(?:^|,)\s*${arg}\s*(::|,)/) {
          $has = 1;
          last;
       }
    }
    ($has);
}

################################################################################
sub contains_arg_fortran {
    my($name,$arg,$has) = @_;
    $has = 0;
    foreach $arg (keys %arglist) {
       if ($name =~ /[:, ]${arg}(,| *$)/) {
          $has = 1;
          last;
       }
    }
    ($has);
}

sub overloaded {
    my($name) = @_;
    $over = 0;
    if (defined $routine_cnt{$name}) { $over = 1; }
    ($over);
}

sub self_field {
    my($name) = @_;
    $is_self_field = 0;
    if (defined $tonto_type{$module_name}{$name}) {
        $is_self_field = 1;
    }
    # Add this variable to the var type table ...
    &has_field('self', $name);
    ($is_self_field);
}

################################################################################
# Return whether the variable has a certain field in its derived type.
# first argument is the variable, second argument is the type component to
# check.
sub has_field {
    my($arg,$name) = @_;
    my($typ,$dotvar,$has_name_as_field,$sub_type,$sub_arry);
    my($arg_type,$is_a_var,$arg_element_type);
    $has_name_as_field = 0;

    # Change fortran back to foo, if applicable.
    $arg =~ s/%/./g;

    ($is_a_var,$arg_type,$arg_element_type) = &is_array_var($arg);
    if ($is_a_var) {
        $sub_type = $arg_type;
        $sub_arry = "";
        if ($arg_type =~ '(VEC)$' or
            $arg_type =~ '(MAT[34567]?)$') {
           $sub_arry = $1;
           $sub_type = $arg_element_type;
        }
        if (defined $tonto_type{$sub_type}{$name} ) {
	    $has_name_as_field = 1;
	    $typ = $tonto_type{$sub_type}{$name};
            if (defined $typ && $typ eq "BSTR") { $typ = "STR" }
	    $typ .= $sub_arry;
	    $dotvar = "${arg}.${name}";
	    $local_type{$dotvar} = $typ;
	}
	return ($has_name_as_field);
    }
    ($is_a_var,$arg_type) = &is_var($arg);
    if ($is_a_var) {
	if (defined $tonto_type{$arg_type}{$name}) {
	    $has_name_as_field = 1;
	    $typ = $tonto_type{$arg_type}{$name};
            if (defined $typ && $typ eq "BSTR") { $typ = "STR" }
	    $dotvar = "${arg}.${name}";
	    $local_type{$dotvar} = $typ;
	}
    }
    return ($has_name_as_field);
}

################################################################################
# Returns whether the preprocessor has previously determined that the argument
# is a variable.  The global variable $arg_type is set to the type of the
# variable.
sub is_var {
    my($arg) = @_;
    my($is_declared_var,$arg_type);
    $is_declared_var = 0;
    $arg_type = "unknown";
    if (exists  $local_type{$arg} && defined $local_type{$arg}) {
	$arg_type = $local_type{$arg};
	$is_declared_var = 1;
    }
    return ($is_declared_var,$arg_type);
}

################################################################################
# Returns whether the preprocessor has previously determined that the argument
# is a variable.  This version converts % to dot before calling is_var.
sub is_var_fortran {
    my($arg) = @_;
    $arg =~ s/%/./g;
    return (&is_var($arg));
}

################################################################################
# In this routine we find out if $arg is an array variable.  Basically, it is
# just making sure that $arg exists in the %local_type hash table. There is some
# tricky stuff to determine what kind of slice is taken from the array and what
# is the type of this slice.
#
# This routine returns whether the argument is an array variable, and if so,
# also the type of the argument and the type of the elements in the array.
sub is_array_var {
    my($arg) = @_;
    my($head,$tail,$split,$yes,$is_a_var,$arg_type,$arg_element_type);
    $is_declared_array_var = 0;
    $arg_type = "unknown";
    ($head,$tail) = &split_by_last_brackets($arg);
    $slice_dim = &slice_dimension($tail); # zero could mean full array !!!!
    if (&is_local_array_var($head)) { # $arg_type is defined in here !!!!!!
        $is_declared_array_var = 1;
        ($is_a_var,$arg_type) = &is_var($head);
        $arg_element_type = $element_type{$arg_type};
        if ($head ne $arg) { # there is a $tail
        if    ($arg_type =~ /VEC$/) {
           if    ($slice_dim==1) { }
           elsif ($slice_dim==0) { $arg_type = $arg_element_type; }
           else { die "Wrong VEC slice dim at line $linenum of $foofile.\nStopped"; }
        }
        elsif ($arg_type =~ /MAT5$/) {
           if    ($slice_dim==5) { }
           elsif ($slice_dim==4) { $arg_type =~ s/MAT5/MAT4/; }
           elsif ($slice_dim==3) { $arg_type =~ s/MAT5/MAT3/; }
           elsif ($slice_dim==2) { $arg_type =~ s/MAT5/MAT/; }
           elsif ($slice_dim==1) { $arg_type =~ s/MAT5/VEC/; }
           elsif ($slice_dim==0) { $arg_type = $arg_element_type; }
           else { die "Wrong MAT5 slice dim at line $linenum of $foofile.\nStopped"; }
        }
        elsif ($arg_type =~ /MAT4$/) {
           if    ($slice_dim==4) { }
           elsif ($slice_dim==3) { $arg_type =~ s/MAT4/MAT3/; }
           elsif ($slice_dim==2) { $arg_type =~ s/MAT4/MAT/; }
           elsif ($slice_dim==1) { $arg_type =~ s/MAT4/VEC/; }
           elsif ($slice_dim==0) { $arg_type = $arg_element_type; }
           else { die "Wrong MAT4 slice dim at line $linenum of $foofile.\nStopped"; }
        }
        elsif ($arg_type =~ /MAT3$/) {
           if    ($slice_dim==3) { }
           elsif ($slice_dim==2) { $arg_type =~ s/MAT3/MAT/; }
           elsif ($slice_dim==1) { $arg_type =~ s/MAT3/VEC/; }
           elsif ($slice_dim==0) { $arg_type = $arg_element_type; }
           else { die "Wrong MAT3 slice dim at line $linenum of $foofile.\nStopped"; }
        }
        elsif ($arg_type =~ /MAT$/) {
           if    ($slice_dim==2) { }
           elsif ($slice_dim==1) { $arg_type =~ s/MAT/VEC/; }
           elsif ($slice_dim==0) { $arg_type = $arg_element_type; }
           else { die "Wrong MAT slice dim at line $linenum of $foofile.\nStopped"; }
        }
        }
    }
    return ($is_declared_array_var,$arg_type,$arg_element_type);
}

sub is_local_array_var {
    my($arg) = @_;
    my($is,$arg_type,$is_a_var);
    $is = 0;
    $arg_type = "unknown";
    ($is_a_var,$arg_type) = &is_var($arg); # get arg_type.
    if ($is_a_var) {
       if (exists  $element_type{$arg_type} &&
           defined $element_type{$arg_type})   { $is = 1; }
    }
    ($is)
}

sub slice_dimension {
   my($tail) = @_;
   # Just add up all the array args or explicit slices
   $s_cnt = 0;
   while ($tail =~ /(.+?)(?:,|\Z)/g) {
      $index = $1;
      if ($index =~ ':')                  { $s_cnt++; }
      elsif (&is_local_array_var($index)) { $s_cnt++; }
   }
   ($s_cnt);
}

sub analyse_types {
    my($X,$type_of) = @_;
    my($function_result,$name);

    if (defined $current_rout_name) {
      $function_result = $routine_attributes{$current_rout_name}{'function_result'};
    }

    # Analyse the type declaration line
    if ($X =~ ' *:: *([A-Z][A-Z0-9_]*)') {
        $typ = $1;
        $dec = $PREMATCH;
        # Look for the declared variables, repeatedly
        while ($dec =~ /([a-zA-Z]\w*)/g) {
            $var = $1;
	    if ($var eq 'DEFAULT_NULL') { last; }
	    if ($var eq 'DEFAULT') { last; }
	    if ($var eq 'NULL') { last; }
	    if ($var eq 'self') { last; }
	    $type_of->{$var} = $typ;    # Add variable to the type table
            if ((defined $function_result) && $var eq $function_result) {
              $function_res_type{"${module_name}_${current_rout_name}"} = $typ;
            }
        }
    }
}

# for unreversed type definitions
#sub analyse_types {
#
#  my($X,$type_of) = @_;
#
#  # Analyse the type declaration line
#  if ($X =~ /([A-Z][A-Z0-9]*)[^:]*::[ ]*/) {
#
#    $typ = $1;
#    $dec = $POSTMATCH;
#
#    # Look for the declared variables, repeatedly
#
#    while ($dec =~ /([a-zA-Z]\w*)/g) {
#      $var = $1;
#      if ($var eq 'DEFAULT_NULL') { last; }
#      if ($var eq 'DEFAULT') { last; }
#      if ($var eq 'NULL') { last; }
#      if ($var eq 'self') { last; }
#      $type_of->{$var} = $typ;    # Add variable to the type table
#    }
#  }
#}

sub split_by_last_brackets {
# This routine breaks up the line into what's before the LAST set of round
# brackets, and what's in the brackets.  For example,
# &split_by_first_brackets("if ((a) + b(c(d)) == 1) (hi)") returns
# "if ((a) + b(c(d)) == 1) " and "hi".
   my($string) = @_;
   my ($head,$tail);
   $head = $string;
   $tail = "";
   if ($string =~ '(.*)([(].*?[)])\Z') {;
       $head = $1;
       $tail = $2;
       while (! &has_matching_brackets($tail)) {
          $head =~ '(.*)([(].*)\Z';
          $head = $1;
          $tail = $2 . $tail;
       }
       $tail =~ '[(](.*)[)]';
       $tail = $1;
   }
   ($head,$tail);
}

sub last_fortran_object {
# This routine returns the last single object of the string.
    $ok = '';
    $i = (length $_[0]) -1;
    while ($i > -1) {
      $this = substr $_[0],$i;
      $char = substr $_[0],$i,1;
      if (&has_matching_brackets($ok)) { # check for an unmatched opening bracket.
        if ($char !~ '[\w%\)]') {last}
      }
      if (! &has_matching_brackets($this)) { # still within closed brackets.
        $ok = $this;
        next;
      }
      # from now on the brackets are closed.
      if ($this =~ '^[\w%(]') { # part of an object
        $ok = $this;
        next;
      } else { # finished.
        return ($ok);
      }
    } continue {
      $i--;
    }
    return ($ok);
}

sub last_foo_object {
# This routine returns the last single object of the string.
    my ($i,$this,$char,$ok);
    $ok = '';
    $i = (length $_[0]) -1;
    while ($i > -1) {
      $this = substr $_[0],$i;
      $char = substr $_[0],$i,1;
      if (&has_matching_brackets($ok)) { # check for an unmatched opening bracket.
        if ($char !~ '[\w\.%\)]') {last}
      }
      if (! &has_matching_brackets($this)) { # still within closed brackets.
        $ok = $this;
        next;
      }
      # from now on the brackets are closed.
      if ($this =~ '^[\w\.%(]') { # part of an object
        $ok = $this;
        next;
      } else { # finished.
        return ($ok);
      }
    } continue {
      $i--;
    }
    return ($ok);
}

sub split_by_first_brackets {
# This routine breaks up the line into what's before the first set of round
# brackets, what's in the brackets, and what's after the brackets.
# For example,
# &split_by_first_brackets("if ((a) + b(c(d)) == 1) (hi)") returns
# "if ", "(a) + b(c(d)) == 1", and " (hi)".
  local ($the_string,$left_string,$middle_string,$right_string);
  local ($left,$right,$this_string,$rest_of_string);
  $the_string = $_[0];
  $left_string = "";
  $middle_string = $the_string;
  $right_string = "";
  $the_string =~ '(.*?)([(].*)';
  $left_string = $1; # what's left of the "("
  $rest_of_string = $2; # what's right of the "("
  $left = length( $left_string ) + 1; # position of the "("
  $length_of_rest = length($rest_of_string);
  LOOP : for ( $n = 1; $n <= $length_of_rest; $n++) {
    $rest_of_string =~ /^(.{\Q$n\E})/;
    $this_string = $1;
    if (&has_matching_brackets($this_string)) {
      $this_string =~ '[(](.*)[)]';
      $middle_string = $1; # what's between the "(" and ")".
      last LOOP;
    }
  }
  $right = $left + length( $middle_string ) + 1; # position of the ")"
  $the_string =~ /.\Q$right\E(.*)/;
  $right_string = $1; # what's right of the ")"

  ($left_string,$middle_string,$right_string)
}

sub has_matching_brackets {
# This routine returns whether the number of "(" equals the number of ")".
  (($_[0] =~ tr/[(]//) == ($_[0] =~ tr/[)]//));
}

################################################################################
# Look for a new scoping unit.
sub find_new_scoping_unit {
  my ($tmp,$newscope);
  if (defined $scopeunit) {$oldscope = $scopeunit;}
  else {$oldscope = '';}
  undef $newscope;
  $lastfoundscope = '';

  # Do the tests based on the parent scope, to save on otherwise pointless
  # tests.  (e.q., can't have a module definition in a do loop).
  if ($oldscope eq 'function' || $oldscope eq 'subroutine' || $oldscope eq 'program') {
    if ($_[0] =~ m'do'o && $_[0] =~ m'(?:^|:) *do *(?: [a-zA-Z]|$|[\#!;])'o) { # do
        $newscope = 'do';
    } elsif ($_[0] =~ m'if'o && $_[0] =~ m'^ *if *[(][^;]*[)] *then(?:(?: *$)|(?: *[\#!;]))'o) { # if
        $newscope = 'if';
    } elsif ($_[0] =~ m'interface'o && $_[0] =~ m'^ *interface *(?:[a-zA-Z]|$)'o) { # interface
        $newscope = 'interface';
    } elsif ($_[0] =~ m'select'o && $_[0] =~ m'^ *select *[a-zA-Z]'o) { # select
        $newscope = 'select';
    } elsif ($_[0] =~ m'forall'o && $_[0] =~ m'^ *forall *[(]'o && $_[0] !~ m'^ *forall *[(].*[)] *.*='o) { # forall
        $newscope = 'forall';
    } elsif ($_[0] =~ m'where'o && $_[0] =~ m'^ *where *[(]'o) { # where
        $newscope = 'where';
    } elsif ($_[0] =~ m'type'o && $_[0] =~ m'^ *type *[a-zA-Z]'o) { # type
        $newscope = 'type';
    } elsif ($_[0] =~ m'contains'o && $_[0] =~ m'^ *contains'o) { # contains
        $newscope = 'contains';
    }
  } elsif ($oldscope =~ 'module') {
    if ($_[0] =~ m'interface'o && $_[0] =~ m'^ *interface *(?:[a-zA-Z]|$)'o) { # interface
        $newscope = 'interface';
    } elsif ($_[0] =~ m'contains'o && $_[0] =~ m'^ *contains'o) { # contains
        $newscope = 'contains';
    } elsif ($_[0] =~ m'type'o && $_[0] =~ m'^ *type *[a-zA-Z]'o) { # type
        $newscope = 'type';
    }
  } elsif ($oldscope eq 'interface') {
    if ($_[0] =~ m'type'o && $_[0] =~ m'^ *type *[a-zA-Z]'o) { # type
        $newscope = 'type';
    } elsif ($#scope > 1) {
      if ($_[0] =~ '\s*result\s*[(](\w+)[)]' ) { # function
        $newscope = 'function';
      } elsif ($_[0] =~ '\w') { # subroutine
        $newscope = 'subroutine';
      }
    }
  } elsif ($oldscope eq 'type') {
  } elsif ($oldscope eq 'do' || $oldscope eq 'if' || $oldscope eq 'select') {
    if ($_[0] =~ m'do'o && $_[0] =~ m'(?:^|:) *do *(?: [a-zA-Z]|$|[\#!;])'o) { # do
        $newscope = 'do';
    } elsif ($_[0] =~ m'if'o && $_[0] =~ m'^ *if *[(][^;]*[)] *then(?:(?: *$)|(?: *[\#!;]))'o) { # if
        $newscope = 'if';
    } elsif ($_[0] =~ m'select'o && $_[0] =~ m'^ *select *[a-zA-Z]'o) { # select
        $newscope = 'select';
    } elsif ($_[0] =~ m'forall'o && $_[0] =~ m'^ *forall *[(]'o && $_[0] !~ m'^ *forall *[(].*[)] *.*='o) { # forall
        $newscope = 'forall';
    } elsif ($_[0] =~ m'where'o && $_[0] =~ m'^ *where *[(]'o) { # where
        $newscope = 'where';
    }
  } elsif ($oldscope eq 'forall' || $oldscope eq 'where') {
  } elsif ($oldscope eq 'contains') {
    if ($_[0] =~ '\s*result\s*[(](\w+)[)]' ) { # function
      $newscope = 'function';
    } elsif ($_[0] =~ '\w') { # subroutine
      $newscope = 'subroutine';
    }
  } else { # must be without a parent scopeunit.
    if ($_[0] =~ m'program'o && $_[0] =~ m'^ *program *[a-zA-Z]'o) { # program
        $newscope = 'program';
    } elsif ($_[0] =~ m'virtual *module'o && $_[0] =~ m'^ *virtual *module *[a-zA-Z]'o) { # virtual module
        $newscope = 'virtual module';
    } elsif ($_[0] =~ m'module'o && $_[0] =~ m'^ *module *[a-zA-Z]'o) { # module
        $newscope = 'module';
    }
  }

  # test for end of scope.
  if ($_[0] =~ m'end'o) {
    if ($_[0] =~ m'^ *end *[a-z](?!.*[=(.])'o) { # named-end
      $newscope = 'named-end';
    } elsif ($_[0] =~ m'^ *end *(?:!|$)'o) { #empty-end
      $newscope = 'empty-end';
    }
  }

  $newscopeunitfound = (defined $newscope);

  if ($newscopeunitfound) {
    # add $newscope to the scope stack.
    $lastfoundscope = $newscope;
    $scopeunit = $newscope;
    push @scope, $newscope;
    if (defined $scope[$#scope-1]) { $parentscope = $scope[$#scope-1]; }
    else { $parentscope = ''; }
  }
}

################################################################################
#Process the new scoping unit, which is a module.
sub do_new_module_scope {
  $is_program = 0;

  # Get the module name #
  $_[0] =~ m/^[ ]*module ([A-Z]\w*)\b/;
  $long_module_name = $1;
  $long_module_name =~ /^([A-Z_0-9]*[A-Z0-9])/;
  $module_name = $1;
  $lc_long_module_name = lc($long_module_name);

  # Check head name is same as module_name
  if ($lc_long_module_name ne $head_name) {
     die "FOO error: module name \"$module_name\" does not match file-head name \"$head_name\".\nStopped";
  }

  # Assign the self type
  $global_type{'self'} = $module_name;

  $finished_this_line = 1;
}

################################################################################
#Process the new scoping unit, which is a module.
sub do_new_virtual_module_scope {
  $is_program = 0;

  # Get the module name #
  $_[0] =~ m/^[ ]*virtual *module ([A-Z]\w*)\b/;
  $long_module_name = $1;
  $long_module_name =~ /^([A-Z_0-9]*[A-Z0-9])/;
  $module_name = $1;
  $lc_long_module_name = lc($long_module_name);

  # Check head name is same as module_name
  if ($lc_long_module_name ne $head_name) {
     die "FOO error: module name \"$module_name\" does not match file-head name \"$head_name\".\nStopped";
  }

  # Assign the self type
  $global_type{'self'} = $module_name;

  $finished_this_line = 1;
}

################################################################################
#Process the new scoping unit, which is a program.
sub do_new_program_scope {

  $is_program = 1;
  $first_active_line = 0; # are these needed?
  $this_is_first_active_line = 0; 

  # Get the module name (pretend program name is module name) #
  $_[0] =~ m/^\s*program\s+(\w+)\b/o;
  $long_module_name = $1;
  $module_name = $1;
  $lc_long_module_name = lc($long_module_name);
  $element_type = " ";
  if (exists  $element_type{$module_name} &&
      defined $element_type{$module_name}) {
     $element_type = $element_type{$module_name};
  }

  # Check head name is same as module_name
  if ($lc_long_module_name ne $head_name) {
     die "FOO error: module name \"$module_name\" does not match file-head name \"$head_name\".\nStopped";
  }

  $finished_this_line = 1;
}

################################################################################
#Process the new scoping unit, which is an interface.
sub do_new_interface_scope {

  # Change the interface declaration
  $_[0] =~ m/^\s*interface\s*([a-z]\w*)/o;
  $name = $1;
}

################################################################################
#Process the new scoping unit, which is a type.
sub do_new_type_scope {
  $type_name = $_[0];
  $type_name =~ s/.*type +//so;         # Get the type name
  $type_name =~ s/_type\b.*//so;
  $type_name = uc($type_name);
}

################################################################################
#Process the new scoping unit, which is an unlabelled end keyword.
sub do_empty_end {
  my($i,$getfile,$found,$name);

  $newscopeunitfound = 0;

  $name = $current_rout_name;
  $lastfoundscope = $scopeunit;
  &pop_scope;
  if ($scopeunit eq 'contains') {
    $lastfoundscope = $scopeunit;
    &pop_scope;
  }

  if (($scopeunit eq 'subroutine' || $scopeunit eq 'function') && $#scope <= 3) {

      # end Module routine. need ns<=3 to eliminate interface routines

      ################################################################
      # Inheritance ##################################################
      ################################################################

      if (defined $routine_attributes{$name}{'inherited'}) {
          $getfile = lc($routine_attributes{$name}{'parent_module'}). ".foo";
          if ($foodir ne "") {
            $getfile = File::Spec->catpath($volume,$foodir,$getfile);
          }
          open(GETFILE, $getfile) || 
             die "\nError : can't find $getfile to inherit routine ...\n\n$inherit.\nStopped";
          @inherit = split('\n',$inherit);
          $inherit[0] =~ s/\s*$//o;
          $inherit[0] =~ s/\s*:::.*//o;
          $i = 0;
          $found = 0;
          while (<GETFILE>) {
             $_  =~ s/\s*$//o;
             $_  =~ s/\s*:::.*//o;
             chomp;
             if    ($_ ne $inherit[$i]) { $i=0 }
             elsif ($i <  @inherit)     { $i++ }
             if    ($i == @inherit)     { $found = 1; last }
          }
          if ($found==0) {
             die "\nError : in $getfile, can't find inherit routine ...\n\n$inherit.\nStopped";
          }
          $oldhandle = *FOOFILE;      # keep old foofile
          $foohandle = *GETFILE;      # foofile is now new get file
          undef $routine_attributes{$name}{'inherited'};
          $being_inherited = 1;
          $inherit = "";
          $first_active_line = 0;     # Real code not yet found
          $this_is_first_active_line = 0; 
                                      # Undo end detection
          push @rout_name_stack, $name;
          $current_rout_name = $name;
          push @scope, $oldscope;

      } else {

          if ($being_inherited) {
            close(GETFILE);             # close parent file
            $foohandle = $oldhandle;    # put back old foofile
            undef $routine_attributes{$name}{'inherited'};
            $being_inherited = 0;
            $inherit = "";
          }

          ################################## Normal uninherited end ######
          # Delete the local type table
          if ($#scope < 3) {
            undef %local_type;
            $local_type{'variable name X'} = 'type of the variable X';
          }

      }
  }

  &pop_scope;
}

################################################################################
#Process the new scoping unit, which is a labelled end keyword.
sub do_named_end {
  $newscopeunitfound = 0;
  $lastfoundscope = $scopeunit;
  &pop_scope;
  if ($scopeunit eq 'contains') {
    $lastfoundscope = $scopeunit;
    &pop_scope;
  }
  &pop_scope;
}

################################################################################
# Add a scoping unit to the scope stack.  Also set $newscopeunitfound,
# $lastfoundscope and $scopeunit.
sub push_scope {
  if (defined $_[0]) {
    $newscopeunitfound = 1;
    $scopeunit = $_[0];
    $lastfoundscope = $scopeunit;
    push @scope, $_[0];
  }
}

################################################################################
# Remove one element from the scope stack, and set $scopeunit to the last
# element on the stack.
sub pop_scope {
  $scopeunit = pop @scope;
  if ($scopeunit =~ '(subroutine)|(function)') {
    $previous_rout_name = $current_rout_name;
    $current_rout_name = pop @rout_name_stack;
    $current_rout_name = $rout_name_stack[$#rout_name_stack];
  }
  $scopeunit = $scope[$#scope];
  if (! defined $scopeunit) { $scopeunit = ''; }
  if ($#scope < -1) {
    die "FOO error: unmatched end at line $linenum of $foofile.\nStopped";
  }
}

################################################################################
# This line is within the scope of a module.
sub process_module {
   my($X);

   ####### Convert SELF_TYPE and ELEMENT_TYPE before anything ######
   ####### Also converts ELEMENT_TYPE_SIZE and SELF_TYPE_SIZE ######
 
   $X = $_[0];
   $X =~ s/\bSELF_TYPE\b/$module_name/g;
   $X =~ s/\bSELF_TYPE_SIZE\b/${module_name}_SIZE/g;
   if (defined $element_type{$module_name} and
       exists  $element_type{$module_name}) {
   $X =~ s/\bELEMENT_TYPE\b/$element_type{$module_name}/g;
   $X =~ s/\bELEMENT_TYPE_SIZE\b/$element_type{$module_name}_SIZE/g;
   }

   # Analyse any module variable declaration lines
   if ($_[0] =~ ' *:: *[A-Z][A-Z0-9_]*[^:]' ) {
      &analyse_types($X,\%global_type);
      # Assign global variable types to local
      %local_type = %global_type
   }
}

################################################################################
# The line is within the scope of an interface within a module.
sub process_module_interface {
}

################################################################################
# The line is within the scope of a subroutine or function.
sub process_routine {
   my($X);
 
   ####### Convert SELF_TYPE and ELEMENT_TYPE before anything ######
   ####### Also converts ELEMENT_TYPE_SIZE and SELF_TYPE_SIZE ######
 
   $X = $_[0];
   $X =~ s/\bSELF_TYPE\b/$module_name/g;
   $X =~ s/\bSELF_TYPE_SIZE\b/${module_name}_SIZE/g;
   if (defined $element_type{$module_name} and
       exists  $element_type{$module_name}) {
   $X =~ s/\bELEMENT_TYPE\b/$element_type{$module_name}/g;
   $X =~ s/\bELEMENT_TYPE_SIZE\b/$element_type{$module_name}_SIZE/g;
   }
 
   # Analyse any variable declaration lines
   if ($_[0] =~ m' *:: *[A-Z][A-Z0-9_]*[^:]'o ) {
      &analyse_types($X,\%local_type);
   }

}

################################################################################
# Determine whether this is the first active line of a routine, i.e. the first
# line which is a true code line, and not a variable declaration. This routine
# must be called just BEFORE a new scope unit is determined and only when the
# oldscope is a routine.
sub check_for_first_active_line {

  # Determine whether the first piece of real code in the routine.
  if ($first_active_line == 0 &&
      ($_[0] =~ '^\s*[a-zA-Z.]' || $_[0] =~ '^\s*#') &&
      (! defined $routine_attributes{$current_rout_name}{'pure'}) &&
#      (! defined $routine_attributes{$current_rout_name}{'inherited'}) &&
      $_[0] !~ '::'            && $_[0] !~ ' function'     &&
      $_[0] !~ ' subroutine'   && $_[0] !~ ' interface'    &&
      $_[0] !~ ' use '         ) {
      $first_active_line = 1;
      $this_is_first_active_line = 1; # this is reset to zero every line
  }
}

################################################################################
# The line is within the scope of a program.
sub process_program {
  my($X);

  ####### Convert SELF_TYPE and ELEMENT_TYPE before anything ######
  ####### Also converts ELEMENT_TYPE_SIZE and SELF_TYPE_SIZE ######

  $X = $_[0];
  $X =~ s/\bSELF_TYPE\b/$module_name/g;
  $X =~ s/\bSELF_TYPE_SIZE\b/${module_name}_SIZE/g;
  if (defined $element_type{$module_name} and
      exists  $element_type{$module_name}) {
  $X =~ s/\bELEMENT_TYPE\b/$element_type{$module_name}/g;
  $X =~ s/\bELEMENT_TYPE_SIZE\b/$element_type{$module_name}_SIZE/g;
  }

  # Analyse any variable declaration lines
  if ($_[0] =~ m' *:: *[A-Z][A-Z0-9_]*[^:]'o ) {
     &analyse_types($X,\%local_type);
  }

  # Determine whether the first piece of real code in the routine.
# if ($first_active_line == 0 &&
#     ($_[0] =~ '^[ ]*[a-zA-Z]' || $_[0] =~ '^[ ]*#') &&
#     (! defined $routine_attributes{$name}{'pure'}) &&
#     $_[0] !~ '::'            && $_[0] !~ ' function'     &&
#     $_[0] !~ ' subroutine'   && $_[0] !~ ' interface'    &&
#     $_[0] !~ ' use '         ) {
#     $first_active_line = 1;
#     $this_is_first_active_line = 1;
# }
}

################################################################################
# The line is within the scope of an interface in a subroutine or function.
sub process_interface {
  # Analyse the routine name
  &analyse_rout_name($_[0]);
  $routine_attributes{$current_rout_name}{'real_name'} = $current_rout_name;
}

################################################################################
# The line is the start of a subroutine or function.
sub do_new_routine_scope {
  my ($pre,$attr,$rout_type,$name,$real_name);

  # Expecting a subroutine/function line
  &analyse_rout_name($_[0]);

  &store_arglist($routine_attributes{$current_rout_name}{'args'});    # arguments

  $name = $current_rout_name;
  $routine_attributes{$name}{'short_name'} = $name;

  if (defined $routine_attributes{$name}{'private'}) {
               $routine_pvt{$name} = "private"; } # Makes the generic interface private
  else {
               $routine_pvt{$name} = "public";  } # This is default

  if (defined $routine_attributes{$name}{'public'}) {    # Makes the specific interface public
      $routine_pub{$name} = "public";
      if (&overloaded($name)) {         # Overloaded routine not allowed
          die "FOO error: overloaded routine cannot be made public\n" .
              "           at line $linenum of $foofile.\nStopped";
      }
  } else {
      $routine_pub{$name} = "private";  # This is default
  }

  # Overloading of routines
  if ($#scope > 2) { # Don't worry about routines within interfaces.
    $real_name = $name;
  } else {
    if (&overloaded($name)) {
      $routine_cnt{$name}++;
      $real_name = $name . "_" . "$routine_cnt{$name}" ;
    } else {
      $routine_cnt{$name} = 0;
      $real_name = $name;
    }
  }

  $routine_attributes{$name}{'real_name'} = $real_name;

  $first_active_line = 0;    # first active line of code not found yet
  $this_is_first_active_line = 0;

  # Assign global variable types to local
  # Don't do this for routines that are within interfaces.
  if ($#scope < 3) {
    %local_type = %global_type;
  }
}

################################################################################
# The line is within the scope of a type.
sub process_type {

  # Analyse a module type declaration body
  if ($_[0] =~ '::' ) {
     $tonto_type{$type_name}{"--exists--"} = 1;      # Create hash
     &analyse_types($_[0],$tonto_type{$type_name});
     delete $tonto_type{$type_name}{"--exists--"};   # Delete token
  }

}

################################################################################
# Return the type of the argument, otherwise return "unknown".
sub foo_type_of_this {
  my ($arg,$tmp,@tmp1,@tmp2,$i,$j,$x,$y,$left,$middle,$right,$is_a_var,$arg_type);
  my($tmp3,$lefttype,$middletype,$righttype);
  $arg = $_[0];

  # convert any % symbols to dots.
  $arg =~ s/%/./g;

  # remove spaces.
  $arg =~ s'[ ]''go;

  # remove outside brackets.
  while ($arg =~ '^[(](.*)[)]$') {$arg = $1}

  # Does it contain derived types?
  # A call to &has_field will add each component to the local hash table if not
  # there already.
  if ($arg =~ /(.*)[.](.*?)$/) {
    $i = 0;
    while ($i < length $arg) {
      $y = substr $arg,$i,1;
      if ($y eq '.') {
        $left = substr $arg,0,$i;
        $right = substr $arg,$i+1;
        &has_field($left,$right);
      }
      $i++;
    }
  }

  # we might be lucky with it being a single variable.
  ($tmp = $arg) =~ s/%/./g;
  ($is_a_var,$arg_type) = &is_var($tmp);
  if ($is_a_var) { return ($arg_type); }

  # we might be lucky with it being an array.
  ($tmp = $arg) =~ s/%/./g;
  ($is_a_var,$arg_type) = &is_array_var($tmp);
  if ($is_a_var) { return ($arg_type); }

  # a real literal constant.
  if ($arg =~ '^([+-])?\d*[.](\d+)?') {
    return ('REAL');
  }

  # a real MACRO constant.
  if ($arg =~ '([+-])?(ONE)|(TWO)|(THREE)|(FOUR)|(FIVE)|(TEN)|(ZERO)') {
    return ('REAL');
  }

  # an integer constant.
  if ($arg =~ '^([+-1234567890]*)$') {
    return ('INT');
  }

  # a string literal constant.
  if ($arg =~ /("[^"]*")|('[^']*')/) {
    return ('STR');
  }

  # perhaps it is a function we know about.
  if ($arg =~ '^(\w+)_[(]([\w\.]+)(.*)') {
  # is above foolproof?
    # maybe do something more fancy with $2 later - recursive call to
    # fortran_type_of_this?
    if (defined $local_type{$2}) {
      if (defined $function_res_type{"$local_type{$2}_$1"}) {
        return ($function_res_type{"$local_type{$2}_$1"});
      }
    }
  }

  # size intrinsic returns an INT.
  if ($arg =~ '^size[(](.*)[)]$') {
    return ('INT');
  }

  # deal with selfless functions; type of the function is the result type
  foreach $i (keys %function_res_type) {
    if ("${module_name}_${arg}" eq $i) {
      return ($function_res_type{$i});
    }
  }

  # min/max function.
  if ($arg =~ '^max[(](.*)[)]$' || $arg =~ '^min[(](.*)[)]$') {
    @tmp2 = split /[,]+/,$1;
    undef $tmp3;
    THAT : foreach $j (@tmp2) {
      $x = &foo_type_of_this($j);
      if (! defined $tmp3) { $tmp3 = $x; }
      if ($tmp3 ne $x) { $tmp3 = "unknown"; last THAT; }
    }
    return ($tmp3);
  }

  # is it a function call?
  if ($arg =~ '^(.*?)[(](.*)[)]$' && &has_matching_brackets($arg)) {
    $tmp3 = $1;
    if (defined $tmp3 && $tmp3 =~ '\w$') {
      return 'unknown';
    }
  }

  # lets get the type of things in brackets.
  if ($arg =~ '[(]' && &has_matching_brackets($arg)) {
    undef $lefttype;
    undef $middletype;
    undef $righttype;

    ($left,$middle,$right) = &split_by_first_brackets($arg);

    # don't want the left to be a function call.
    if (defined $left && $left =~ '\w$') {
      $lefttype = &foo_type_of_this($left . '(' . $middle . ')');
      undef $middletype;
      if (defined $right && $right ne '') { $righttype = &foo_type_of_this($right);}

    } else {
      if (defined $left && $left ne '') { $lefttype = &foo_type_of_this($left);}
      if (defined $middle && $middle ne '') { $middletype = &foo_type_of_this($middle);}
      if (defined $right && $right ne '') { $righttype = &foo_type_of_this($right);}
    }

    undef $tmp3;
    if (! defined $tmp3) { $tmp3 = $lefttype; }
    if (! defined $tmp3) { $tmp3 = $middletype; }
    if (! defined $tmp3) { $tmp3 = $righttype; }

    if (defined $tmp3) {
      if (defined $lefttype) {if ($lefttype ne $tmp3) {return 'unknown'}}
      if (defined $middletype) {if ($middletype ne $tmp3) {return 'unknown'}}
      if (defined $righttype) {if ($righttype ne $tmp3) {return 'unknown'}}
      return $tmp3;
    }
  }

  # maybe a simple combination of variables.
  if ($arg =~ /[=\+\-\*\/]+/) {
    @tmp1 = split /[=\+\-\*\/]+/,$arg;
    if ($#tmp1 >= 0) {
      undef $tmp3;
      THIS : foreach $i (@tmp1) {
        $x = &foo_type_of_this($i);
        if (! defined $tmp3) { $tmp3 = $x; }
        if ($tmp3 ne $x) { $tmp3 = "unknown"; last THIS; }
      }
      if (defined $tmp3) { return ($tmp3); }
    }
  }

  return ('unknown');
}

sub split_by_comment {
# split the line into it's non-comment and comment parts, if applicable.
  my ($x,$y,$i,$left,$right);
  $x = $_[0];
  if ($x !~ '!') { # no comment
    return ($x);
  } else {
    $i = 0;
    while ($i < length $x) {
      $y = substr $x,$i,1;
      if ($y eq '!') {
        $left = substr $x,0,$i;
        $right = substr $x,$i;
        if (&outside_of_string($left)) {
          return ($left,$right);
        }
      }
      $i++;
    }
    return ($x);
  }
}

sub outside_of_string {
# Return whether we are outside of a quoted string.
  my(@tmp,$i,$y,$in_single,$in_double);
    $i = 0;
    $in_single = 0;
    $in_double = 0;
    while ($i < length $_[0]) {
      $y = substr $_[0],$i,1;
      if ($y eq '\'' && ! $in_double) { $in_single = ! $in_single;}
      if ($y eq '"' && ! $in_single) { $in_double = ! $in_double;}
      $i++;
    }
    return (! ($in_single || $in_double));
}

################################################################################
#                          Conversion to HTML

################################################################################
# Process the various options to determine what to print.
# Uses a lot of global variables.
sub html_process_foo_line {
  my($is_comment);

  ($html_out,$comment) = &split_by_comment($inp);

  if (! $being_inherited) { # We only want the original foo, not inserted stuff.
    if ($not_blank) {
      if ($newscopeunitfound) {
        if ($parentscope =~ 'module' && ! $html_done_use_list) { &html_put_use_list; }

        if ($lastfoundscope =~ '(module)|(program)|(type)|(contains)|(routine)')
          { $html_lastline = $lastfoundscope; }

        if ($lastfoundscope =~ '(module)|(program)') { &html_start_module; }
        elsif ($lastfoundscope eq 'interface' && $parentscope =~ 'module') { &html_start_module_interface; }
        elsif ($lastfoundscope eq 'type' && $parentscope =~ 'module') { &html_start_module_type; }
        elsif ($lastfoundscope =~ '(subroutine)|(function)') { &html_start_routine; }
        elsif ($lastfoundscope eq 'contains') { &html_start_contains; }
      }
      elsif ($lastfoundscope =~ 'end') {&html_end_class($scopeunit); }

      elsif ($scopeunit eq 'interface' && $parentscope =~ 'module') { &html_process_module_interface; }
      elsif ($parentscope eq 'interface') { &html_process_routine_interface; }
      if ($scopeunit eq 'program') { &html_process_program; }
      if ($#scope > 1 && grep ('subroutine|function',@scope) &&
        ! ($newscopeunitfound && $lastfoundscope =~ '(subroutine)|(function)'))
        { &html_process_routine; }
      if ($newscopeunitfound) { $html_out .= "<DIV CLASS=\"INDENT\">"; }
    }

    if (defined $comment) { $html_out .= $comment; }
    &html_put_synopsis;
                                            # Print the processed line!
    &html_print($HTMLLONGHANDLE,$html_out);
  }

}

############################################################################
sub html_start {
  -f $htmlshortfile && unlink($htmlshortfile);
  -f $htmllongfile && unlink($htmllongfile);
  open(HTMLSHORTFILE,">".$htmlshortfile) or die "$htmlshortfile: $!.\nStopped";
  open(HTMLLONGFILE,">".$htmllongfile) or die "$htmllongfile: $!.\nStopped";
  $HTMLSHORTHANDLE = *HTMLSHORTFILE;
  $HTMLLONGHANDLE = *HTMLLONGFILE;
  &html_put_header($HTMLSHORTHANDLE);
  &html_put_header($HTMLLONGHANDLE);
  print $HTMLSHORTHANDLE "\n<BR><H2>Synopsis:</H2>";
  $first_interface = 1;
  $html_lastline = "comment";
}

############################################################################
sub html_put_header {
# This routine outputs the HTML header.  The first argument to this routine is
# the output file handle, as a string.
  my ($a);
  $a = $_[0];
  print $a "<HTML>";
  print $a "<HEAD>";
  print $a "  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; CHARSET=iso-8859-1\">";
  print $a "  <META NAME=\"robots\" CONTENT=\"noindex, nofollow\">";
  print $a "  <LINK REL=\"STYLESHEET\" TYPE=\"text/css\" HREF=\"../tonto.css\">";
  print $a "  <TITLE>Documentation for $head_name</TITLE>";
  print $a "</HEAD>";
  print $a "<BODY BGCOLOR=\"#FFFFFF\" CLASS=\"FOODOC\"><BASEFONT SIZE=3>";
  print $a "<DIV CLASS=\"TITLE\"><H1>Documentation for $head_name</H1></DIV>";
  print $a '<BR><IMG SRC="../hr.png" HEIGHT=10 WIDTH=100%><BR>';
}

############################################################################
#
sub html_end {
  print HTMLLONGFILE "\n<BR><IMG SRC=\"../hr.png\" HEIGHT=10 WIDTH=100%><BR>";
  print HTMLLONGFILE "</BODY>";
  print HTMLLONGFILE "</HTML>";
  print HTMLSHORTFILE "\n<BR><IMG SRC=\"../hr.png\" HEIGHT=10 WIDTH=100%><BR>";
  print HTMLSHORTFILE "</BODY>";
  print HTMLSHORTFILE "</HTML>";
}

############################################################################
# Synopsis of various routines/interfaces/etc.
# (this means the comments before the variable definitions.)
sub html_put_synopsis {
  if ($html_out =~ '^\s*[!]' && $html_lastline ne " ") {
    &html_print($HTMLSHORTHANDLE,$html_out);
    $html_lastline = "comment";
  } elsif ($html_out =~ '::' && $html_lastline ne ' ') {
    # leave it as is.
  } elsif (! $newscopeunitfound) {
    $html_lastline = " ";
  }
}

############################################################################
# Expects the global hash %used to contain the unique list of modules that are
# used.
sub html_put_use_list {
  my ($mod,$tmp);
  $html_done_use_list = 1;
  if (keys(%used) > 0) {
    &html_print($HTMLSHORTHANDLE,"<BR><H2>Used Modules:</H2>");
    foreach $mod (keys %used) {
      if ($mod eq 'unknown') {next;}
      $tmp = lc($mod);
      &html_print($HTMLSHORTHANDLE,"   <A HREF=\"${tmp}_short.html\">$mod</A>");
    }
  }
}

############################################################################
sub html_start_module {
  $html_out =~ s/(.*?)((?:program)|(?:module))(.*)/$1<A CLASS=\"KEYWORD\">$2<\/A>$3/i;
}

############################################################################
sub html_process_routine {
  my ($X,@tmp,$word);

  # Separate out the routine body from the routine definition and comments.
  if ($html_lastline eq 'comment' && $html_out !~ '^\s*!') {
    &html_print($HTMLLONGHANDLE,'');
    &html_print($HTMLSHORTHANDLE,'');
    $html_lastline = ' ';
  } elsif ($this_is_first_active_line ) {
    &html_print($HTMLLONGHANDLE,'');
    if ($html_out =~ '(ENSURE)|(DIE_IF)') { &html_print($HTMLSHORTHANDLE,''); }
    $html_lastline = 'stuff';
  }

  # print attributes of routine arguments.
  if ($html_out =~ '::' && &contains_arg($html_out)) {
    @tmp = split /\b/, $html_out;
    foreach $word (@tmp) {
      if ($word =~ /\w/ && grep (/^${word}$/,@types)) {
        $word = '<A HREF="' . (lc $word) . '.html">' . $word . '</A>';
        last;
      }
    }
    $html_out = join('',@tmp);
    $html_out =~ s/^(.*?)::(.*)/<A CLASS="ARGS">$1<\/A>::$2/;
    &html_print($HTMLSHORTHANDLE,$html_out);
  }

  # Print ENSURE/DIE_IF statements that are at the top of the routine.
  $X = $html_out;
  if ($X =~ '(ENSURE)|(DIE_IF)' && ($this_is_first_active_line || $last_was_ensure)) {
    $name = $current_rout_name;
    if (! defined $routine_attributes{$name}{'pure'}) {
      if ($X =~ 'STACK') { $X =~ s/ *STACK[(].*?[)]\n//g; }
      &html_print($HTMLSHORTHANDLE,$X);
      $last_was_ensure = 1;
    }
  }
  else { $last_was_ensure = 0 }

  # Add links to the functions and types.
  &html_add_routine_call_links;
}

############################################################################
sub html_process_program {
  my ($X,@tmp,$word);

  $html_lastline = 'stuff';
  $name = $module_name;
  $current_rout_name = $module_name;
  %routine_attributes = ();
  $routine_attributes{$name}{'real_name'} = $name;

  # print attributes of routine arguments.
  if ($html_out =~ '::' && &contains_arg($html_out)) {
    @tmp = split /\b/, $html_out;
    foreach $word (@tmp) {
      if ($word =~ /\w/ && grep (/^${word}$/,@types)) {
        $word = '<A HREF="' . (lc $word) . '.html">' . $word . '</A>';
        last;
      }
    }
    $html_out = join('',@tmp);
    $html_out =~ s/^(.*?)::(.*)/<A CLASS="ARGS">$1<\/A>::$2/;
    &html_print($HTMLSHORTHANDLE,$html_out);
  }

  # Print ENSURE/DIE_IF statements that are at the top of the routine.
  $X = $html_out;
  if ($X =~ '(ENSURE)|(DIE_IF)' && ($this_is_first_active_line || $last_was_ensure)) {
    if (! defined $routine_attributes{$name}{'pure'}) {
      if ($X =~ 'STACK') { $X =~ s/ *STACK[(].*?[)]\n//g; }
      &html_print($HTMLSHORTHANDLE,$X);
    }
    $last_was_ensure = 1;
  }
  else { $last_was_ensure = 0 }

  # Add links to the functions and types.
  &html_add_routine_call_links;
}

############################################################################
sub html_process_module_interface {
  my($tmp,$tmp1,$word,$new_string,@words,$comma);

  # Expecting a module procedure ###############
  @words = split('\b',$html_out);
  foreach $word (@words) {
    if ($word !~ '\w') {next;}
    $word = "<A HREF=\"#$word\">$word</A>";
  }
  $html_out = join('',@words);
  &html_print($HTMLSHORTHANDLE,$html_out);
}

############################################################################
# This is for an interface block within a routine.
sub html_process_routine_interface {

    if ($is_function) { $lastfoundscope = 'function'; }
    else { $lastfoundscope = 'subroutine'; }
}

############################################################################
# This subroutine outputs the HTML for a subroutine/function declaration.
sub html_start_routine {
  my ($tmp,$attr,$real_name,$name);
  $html_lastline = "routine";
  $name = $current_rout_name;

  #                    space routine   arguments            function result                attributes
  $html_out =~ m/^ (\s*)  (\w+) (?:\(([\s\w,]+)\))? \s* (?:result\s*\(([\s\w]+)\))? (?:\s*:::\s*(.*?))?\s*$/x;
  if (defined $2) {$routname = $2} else {$routname = ''}
  if (defined $3) {$arg = '(<A CLASS="ARGS">'.$3.'</A>)'} else {$arg = ''}
  if (defined $4) {$result = ' result (<A CLASS="ARGS">'.$4.'</A>)'} else {$result = ''}
  if (defined $5) {$attr = ' ::: <A CLASS="ATTR">'.$5.'</A>'} else {$attr = ''}

  # Link get_from to the place where it's inherited from, if applicable.
  if ($attr =~ m'get_from\((.*)\)') {
    $tmp = lc $1;
    $attr = $PREMATCH . "<A HREF=\"${tmp}_short.html#$routname\">get_from($1)<\/A>" . $POSTMATCH;
  }
  $real_name = $routine_attributes{$name}{'real_name'};

  #Long HTML
  $html_out = "\n<BR>";
  $name = $routine_attributes{$name}{'real_name'};
  $html_out .= "<A CLASS=\"ROUTINE\" NAME=\"${real_name}\" HREF=\"${head_name}_short.html#${real_name}\">";
  $html_out .= "${routname}</A>${arg}${result}${attr}";

  #Short HTML
  $tmp = "\n<BR>";
  $tmp .= "<A CLASS=\"ROUTINE\" NAME=\"${real_name}\" HREF=\"${head_name}.html#${real_name}\">";
  $tmp .= "${routname}</A>${arg}${result}${attr}";
  &html_print($HTMLSHORTHANDLE,$tmp);
  &html_print($HTMLSHORTHANDLE,"<DIV CLASS=\"INDENT\">");

  $last_was_ensure = 0;
}

############################################################################
sub html_start_module_type {
  $html_lastline = "type";
  &html_print($HTMLSHORTHANDLE,"\n<BR><A NAME=\"$type_name\"><FONT COLOR=\"#EE0000\">type $type_name:</FONT></A>");
  &html_print($HTMLSHORTHANDLE,"<DIV CLASS=\"INDENT\">");
}

############################################################################
# This routine ends a class.
sub html_end_class {
  if ( $#scope < 4 && ! $being_inherited) {
#    if ($oldscope eq 'interface') {
#      $html_out =~ s/^(\s*)end([\s\w]*)(.*)/$1<A CLASS=\"KEYWORD\">end$2<\/A>$3/;
#    }
    if ( $#scope < 2) { &html_print($HTMLSHORTHANDLE,"</DIV>"); }
  }
  $html_out = "</DIV>" . $html_out;
  $html_lastline = " ";
}

############################################################################
# Write a header for the generic interfaces
sub html_start_module_interface {
  $html_lastline = 'interface';
  if ($first_interface) {
    &html_print($HTMLSHORTHANDLE,"\n<H2>Generic interfaces:</H2>");
  }
  $first_interface = 0;
  if ($html_out =~ /\binterface\b/io) {
    $html_out =~ s/(.*)\binterface\b(\s*)(\w+)(.*)/$1<A CLASS=\"KEYWORD\">interface<\/A>$2<A NAME=\"$3\" HREF=\"${head_name}_short.html#$3\">$3<\/A>$4/g;
    &html_print($HTMLSHORTHANDLE,"\n<BR><A CLASS=\"KEYWORD\">interface</A>$2<A NAME=\"$3\" HREF=\"${head_name}.html#$3\">$name:</A>");
    &html_print($HTMLSHORTHANDLE,"<DIV CLASS=\"INDENT\">");
  }
}

############################################################################
sub html_start_contains {
  &html_put_self_type;
  $html_out =~ s/(.*?)contains(.*)/<\/DIV>$1<A CLASS=\"KEYWORD\">contains<\/A>$2/i;
  &html_print($HTMLSHORTHANDLE,"\n<H2>Method Components:</H2>");
  &html_print($HTMLSHORTHANDLE,"<DIV CLASS=\"INDENT\">");
}

############################################################################
sub html_put_self_type {
  my ($A,$B,$Y,$X,$found,$type_name,@subst_for_this_line,$pattern);
  open(TYPEFILE,$typfile);
  $found = 0;
  while (chop($X = <TYPEFILE>)) {

      last if ($X =~ '^end' );                  # End of types.foo
      if ($found) {
        last if ($X =~ '^ +end');              # End of types def
        if ($X =~ s/^(\s*)!(.*)/!$2/) { }    # a comment.
        elsif ($X =~ '(.*)::(.*)') { # variable declaration
          $left = $1;
          $right = $2;

          #Process left side first.  Make the variables link targets.
          @tmp = split('\b',$left);
          foreach $word (@tmp) {
            if ($word =~ '\w') { $word = "<A NAME=\"$word\" CLASS=\"TYPE\">" . $word . '</A>'; }
          }
          $left = join('',@tmp);

          #Now do right side.  Link to any types we know of.
          @tmp = split('\b',$right);
          foreach $word (@tmp) {
            if ($word =~ '\w' && grep (/^${word}$/,@types)) {
              $word = '<A HREF="' . (lc $word) . '.html">' . $word . '</A>';
            }
          }
          $right = join('',@tmp);

          #Merge them back together.
          $X = $left . '::' . $right;
        }
        $X .= "<BR>";
        &html_print($HTMLSHORTHANDLE,$X);

      } else {
        next if ($X !~ 'type +([a-zA-Z_]\w*)_type\b');  # Look for next type definition
        ($type_name = $X) =~ s/.*type +//s;             # Get the type name
        $type_name =~ s/_type\b.*//s;
        $type_name = uc($type_name);
        if (uc($module_name) eq $type_name) {
          &html_print($HTMLSHORTHANDLE,"\n<BR><A CLASS=\"KEYWORD\">type </A><A CLASS=\"TYPE\" NAME=\"$type_name\">$type_name:</A>");
          &html_print($HTMLSHORTHANDLE,"<DIV CLASS=\"INDENT\">");
          $found = 1;
        }
      }
  }
  close(TYPEFILE);

  if ($found) {
    &html_end_class("type");
  } else {
    $X = uc($module_name);

    if ($X =~ 'STRVEC') {
      foreach $pattern (keys %substs) {  # Do STRVEC's first
        $X =~ s/\b$pattern([^\w(])/$substs{$pattern}$1/;
        $X =~ s/\b$pattern\*\b/$substs{$pattern}\*/;
      }
      if ($X !~ '\bSTRVEC[(](.+),(.+)[)]') {
        $X =~ s/\bSTRVEC[(]([^ ,]+)[)]/STRVEC(STR_SIZE,$1)/;
      }
    }

    # Create an array that contains only the substitutions relevent to this
    # line.  (just an optimisation).
    @subst_for_this_line = grep $X =~ m/$_/, keys %subst;

    # Pattern changes.
    foreach $pattern (@subst_for_this_line) {
    # Try exact match. (neccessary to distinguish BSTR from STR).
      $X =~ s/\b$pattern([^\w(])/$subst{$pattern}$1/;
      $X =~ s/\b$pattern\*\b/$subst{$pattern}\*/;
    }
    foreach $pattern (@subst_for_this_line) {
    # Now the rest. (INTVEC,CPXVEC,INTVEC all have same form).
      if ($X =~ m/(.*)$pattern/) {
        $X =~ s/$pattern([^\w(])/$subst{$pattern}$1/;
        $X =~ s/$pattern\*\b/$subst{$pattern}\*/; # PTR's.
      }
    }

    $X =~ s/,$//g;
    $Y = uc($module_name);
    if ($X =~ '[(](.+)[)]') {
      $A = $+;
      $B = $`;
      foreach $pattern (keys %element_type) {   # Now the rest
        $B =~ s/$pattern/$element_type{$pattern}/g;
      }
    }
    if (defined $B && defined $A) {
      &html_print($HTMLSHORTHANDLE,"\n<BR><A NAME=\"$Y\"><FONT COLOR=\"#EE0000\">Macro $Y may also be written:</FONT></A>");
      &html_print($HTMLSHORTHANDLE,"$X<BR>");
      &html_print($HTMLSHORTHANDLE,"$B, dimension($A)<BR>");
      &html_end_class("type");
    }
  }
}

############################################################################
# This routine modifies the routine and function calls in the long html file to
# be links to the appropriate routine/function definitions.
sub html_add_routine_call_links {
  my($routname,$tmp,$i,$this_char,$left,$right,$ob,$this_mod);
  my($rest_of_left,$new_string,$type);
  if ($html_out =~ '.') {
    $i = 0;
    while ($i <= length($html_out)-1) { # should be $html_html_out;
      $tmp = length($html_out)-1;
      $this_char = substr $html_out,$i,1;
      # Check if it's a . or %.
      if ($this_char eq '.' || $this_char eq '%') {
        $left = substr $html_out,0,$i;
        $right = substr $html_out,$i+1;
        $ob = &last_foo_object($left);
        if ($ob eq '') {$ob = 'self'}
        if ((substr $ob,0,1) eq '.') {$ob = 'self' . $ob}
        # Is the thing on the right of the dot an object?
        if ($right =~ '^(\w+)(?:\(|\s|\Z|\)|,|\.)') {
          $routname = $1;
          $type = &foo_type_of_this($ob);
          $tmp = length($routname);
          $right = substr $right,$tmp;
          # If we know the type, we can add the link...
          if ($type ne 'unknown') {
            $this_mod = lc $type;
            $new_string = ".<A HREF=\"${this_mod}_short.html#$routname\">" . $routname . "</A>";
            $html_out = $left . $new_string . $right;
            $i = length($new_string) + length($left)-1;
            # Remember that length($x) = $#x + 1, so subtract one here so that
            # we're at the end of what we've processed.
          }
        }
      }
    } continue { $i++; }
  }
}

################################################################################
# This routine outputs HTML to the appropriate file.
sub html_print {
  my ($X,$out);
  $file = $_[0];
  $out = $_[1];

  # Collapse multiple spaces down to one, seeing browsers do this anyway.
  $out =~ s/\s{2,}/ /og;

  # Add a manual new line
  if (! $newscopeunitfound) { $out .= '<BR>'; }

  print $file $out;
}


################################################################################
#                          Conversion to Fortran
################################################################################


################################################################################
# Process the information into fortran.
sub fortran_process_foo_line {

  ($fortran_out,$comment) = &split_by_comment($inp);

  if ($not_blank) {

    if ($newscopeunitfound) {
      if ($lastfoundscope =~ '(subroutine)|(function)') { &fortran_start_routine; }
      if ($lastfoundscope eq 'interface' && $parentscope =~ 'module') { &fortran_start_module_interface; }
      if ($lastfoundscope eq 'module') { &fortran_do_new_module_scope; }
      if ($lastfoundscope eq 'virtual module') { &fortran_do_new_virtual_module_scope; }
      if ($lastfoundscope eq 'program') { &fortran_do_new_program_scope; }
    }
    elsif ($lastfoundscope eq 'empty-end') {&fortran_do_empty_end; }
# named-end?
    elsif ($scopeunit eq 'interface' && $parentscope =~ 'module') { &fortran_process_module_interface; }
    elsif ($scopeunit eq 'interface' && $parentscope =~ '(subroutine)|(function)') { &fortran_process_routine_interface; }
    elsif ($scopeunit eq 'module') {&fortran_process_module; }

    if ($#scope >= 0 && $scope[0] eq 'program') { &fortran_process_program; }

    if ($#scope > 1 && grep ('(subroutine)|(function)',@scope) &&
      ! ($newscopeunitfound && $lastfoundscope =~ '(subroutine)|(function)'))
      { &fortran_process_routine; }

    &fortran_add_default_initialisation;
    &fortran_add_include_files;
    &fortran_swap_declaration_order;
    &fortran_misc_changes;
  }


  # Print the processed line!
  if (defined $comment) { $fortran_out .= $comment; }
  print F90FILE $fortran_out;
}

################################################################################
# Change the start of the routine by adding STACK macro.
sub fortran_add_stack_macro {
  my($name,$rout_name);
  $name = $current_rout_name;
  $rout_name = "$module_name:$routine_attributes{$name}{'real_name'}";

  if ($this_is_first_active_line && ! defined $routine_attributes{$name}{'pure'}) {
      $fortran_out =~ s/^(\s*)/$1STACK(\"$rout_name\")\n$1START_TIMER(\"$rout_name\")\n$1/;
      undef %arglist;
      $arglist{"argument name"} = "argument index";
  }

}

################################################################################
# Change the return statement by adding UNSTACK or CHECK macros.
sub fortran_process_return {
  my($name,$rout_name);
  $name = $current_rout_name;
  $rout_name = "$module_name:$routine_attributes{$name}{'real_name'}";

  # found a "return" #######################
  if ($fortran_out =~ '.return') {
    if ($fortran_out =~ '[)] *return *(?:!|$)' ) {
      if (defined $routine_attributes{$name}{'pure'})     { 
         $fortran_out =~ s/[)] *return */) then; STOP_TIMER(\"$rout_name\") return; end if/o; }
      elsif (defined $routine_attributes{$name}{'leaky'}) { 
         $fortran_out =~ s/[)] *return */) then; STOP_TIMER(\"$rout_name\") UNSTACK return; end if/o; }
      else { 
         $fortran_out =~ s/[)] *return */) then; STOP_TIMER(\"$rout_name\") CHECK return; end if/o; }
    }
    if ($fortran_out =~ '(?:^|;) *return *' ) {
      if (defined $routine_attributes{$name}{'pure'})     { 
         $fortran_out =~ s/return/STOP_TIMER(\"$rout_name\") return/o; }
      elsif (defined $routine_attributes{$name}{'leaky'}) { 
         $fortran_out =~ s/return/STOP_TIMER(\"$rout_name\") UNSTACK return/o; }
      else { 
         $fortran_out =~ s/return/STOP_TIMER(\"$rout_name\") CHECK return/o; }
    }
  }
}

################################################################################
# Change case statements appropriately.
sub fortran_process_case_statements {
  my($tmp,$indent,$i,$unknown_arg,$name);

  # Store all case string arguments
  if ($fortran_out =~ /case/) {
    $tmp = $fortran_out;
    if ($tmp =~  'case *\( *("[^"]*")') {
      $tmp =~ s/case *\( *//o;
#      while ( $tmp =~ s/^ *,? *("[^"]*")//o ) {
      while ( $tmp =~ s/^\s*,?\s*(".*?")\s*//o ) {
         $n_case_opt++;
         $case{$n_case_opt} = $1;
      }
    }
  }

  # Dump the case string arguments as a checking construct
  # This can really bloat the code!

  if ($do_unknown eq 1) {
     if ($fortran_out =~ 'UNKNOWN\(.*\)') {
       $fortran_out =~ s/UNKNOWN\((.*)\)/allocate\(tonto\%known_keywords\($n_case_opt\)\)/o;
       $unknown_arg = $1;
       ($indent = $fortran_out) =~ '^(\s*)';
       $indent = $1;
       $fortran_out = $fortran_out . "\n";
       for ($i=1 ; $i <= $n_case_opt; $i++) {
          $fortran_out .= $indent . "tonto\%known_keywords($i) = " . $case{$i} . "\n";
       }
       $name = $current_rout_name;
       if ($do_generic) {
       $fortran_out .= $indent . 
           "call unknown_(tonto,$unknown_arg,\"$module_name:$routine_attributes{$name}{'real_name'}\")\n";
       } else {
       $fortran_out .= $indent . 
           "call SYSTEM_unknown(tonto,$unknown_arg,\"$module_name:$routine_attributes{$name}{'real_name'}\")\n";
       }
       $fortran_out .= $indent . "deallocate(tonto\%known_keywords)";
     }
  }
}

################################################################################
# Change ENSURE/WARN/DIE appropriately.
sub fortran_process_error_management {
  my($error_string,$name);

  $name = $current_rout_name;
  if (defined $routine_attributes{$name}{'pure'}) {
    $fortran_out =~ s/ENSURE.*$//o;
    $fortran_out =~ s/VERIFY.*$//o;
    $fortran_out =~ s/DIE_IF.*$//o;
    $fortran_out =~ s/WARN_IF.*$//o;
    $fortran_out =~ s/DIE\(".*$//o;
    $fortran_out =~ s/WARN\(".*$//o;
  } else {
    $error_string = "$module_name:$routine_attributes{$name}{'real_name'} ... ";
    if ($fortran_out =~ 'ENSURE') { $fortran_out =~ s/(^.*), *"([^"]*)"/$1,"$error_string$2"/m; }
    if ($fortran_out =~ 'VERIFY') { $fortran_out =~ s/(^.*), *"([^"]*)"/$1,"$error_string$2"/m; }
    if ($fortran_out =~ 'DIE_IF') { $fortran_out =~ s/(^.*), *"([^"]*)"/$1,"$error_string$2"/m; }
    if ($fortran_out =~ 'WARN_IF') { $fortran_out =~ s/(^.*), *"([^"]*)"/$1,"$error_string$2"/m; }
    if ($fortran_out =~ 'DIE\("') { $fortran_out =~ s/DIE\("/DIE\("$error_string/m; }
    if ($fortran_out =~ 'WARN\("') { $fortran_out =~ s/WARN\("/WARN\("$error_string/m; }
  }

  # Split the ENSURE & DIE_IF off the STACK; this also bloats the code.
  if ($fortran_out =~ '^ *STACK') {
    $fortran_out =~  s/\)( *)ENSURE\(/\)\n$1ENSURE\(/;
    $fortran_out =~  s/\)( *)ENSURE[\$]\(/\)\n$1ENSURE[\$]\(/;
    $fortran_out =~  s/\)( *)DIE_IF\(/\)\n$1DIE_IF\(/;
  }
}

################################################################################
# Add DEFAULT initialisation.
sub fortran_add_default_initialisation {
  if ((defined $scopeunit) && $scopeunit eq 'type') {
  #  if ($inp =~ ':: +INT *$') {                   $inp .= " DEFAULT(0)"}
  #  if ($inp =~ ':: +REAL *$') {                   $inp .= " DEFAULT(ZERO)"}
  #  if ($inp =~ ':: +STR *$') {                   $inp .= " DEFAULT(\" \")"}
  #  if ($inp =~ ':: +REALVEC[(][^)]*[)] *$') {      $inp .= " DEFAULT(ZERO)"}
  #  if ($inp =~ ':: +CPXVEC[(][^)]*[)] *$') {      $inp .= " DEFAULT(ZERO)"}
  #  if ($inp =~ ':: +INTVEC[(][^)]*[)] *$') {       $inp .= " DEFAULT(0)"}
  #  if ($inp =~ ':: +REALMAT[3-7][(][^)]*[)] *$') { $inp .= " DEFAULT(ZERO)"}
  #  if ($inp =~ ':: +CPXMAT[3-7][(][^)]*[)] *$') { $inp .= " DEFAULT(ZERO)"}
  #  if ($inp =~ ':: +INTMAT[3-7][(][^)]*[)] *$') {  $inp .= " DEFAULT(0)"}
     if ($inp =~ ':: +.*[*] *$') {                 $inp .= " DEFAULT_NULL"}
  }
}

################################################################################
# The line is within the scope of an interface in a subroutine or function.
sub fortran_process_routine_interface {
  my($name,$attr,$args,$pre);
  $name = $current_rout_name;
  $attr = '';
  if    (defined $routine_attributes{$name}{'always_elemental'}) { $attr = 'ALWAYS_ELEMENTAL '; }
  elsif (defined $routine_attributes{$name}{'elemental'})        { $attr = 'ELEMENTAL '; }
  elsif (defined $routine_attributes{$name}{'always_pure'})      { $attr = 'ALWAYS_PURE '; }
  elsif (defined $routine_attributes{$name}{'pure'})             { $attr = 'PURE '; }
  if    (defined $routine_attributes{$name}{'recursive'})        { $attr .= 'recursive '; }

  $args = '';
  $pre = '';
  if (defined $routine_attributes{$name}{'args'}) { $args = $routine_attributes{$name}{'args'}; }
  if (defined $routine_attributes{$name}{'pre'}) { $pre = $routine_attributes{$name}{'pre'}; }

  $pre = $routine_attributes{$name}{'indent'};
  if (defined $routine_attributes{$name}{'function'}) {
    $fortran_out = "${pre}${attr}function ${name}${args}";
  } else {
    $fortran_out = "${pre}${attr}subroutine ${name}${args}";
  }
}

################################################################################
sub fortran_do_new_module_scope {
  my($use_base);
  $fortran_out =~ s/^[ ]*module ([A-Z]\w*)/module $1_MODULE/;
  $fortran_out .= "\n\n" . "#  include \"${fortranusefile}\"";
}

################################################################################
sub fortran_do_new_virtual_module_scope {
  my($use_base);
  $fortran_out =~ s/^[ ]*virtual *module ([A-Z]\w*)/virtual module $1_MODULE ! This is a virtual module/;
  $fortran_out .= "\n\n" . "!  include \"${fortranusefile}\"";
}

################################################################################
sub fortran_do_new_program_scope {
  my($use_base);

  $fortran_out .= "\n\n" . "#  include \"${fortranusefile}\"";
}

################################################################################
# Dump the interface file
sub fortran_dump_interface {
  my ($cnt,$pub,$pvt,$rout);
  if (lc($long_module_name) ne '') {

    -f $fortranintfile && unlink($fortranintfile);
    open(INTFILE,">".$fortranintfile);

    if (! $is_program) {
      if ($module_name ne 'TYPES') {
        print INTFILE "   private";   # All specific interfaces are made private
      } else {
        print INTFILE "   public";    # ... unless this is the TYPES module.
      }
      print INTFILE "";
    }

    my $mod = $module_name;

    foreach $rout (keys %routine_cnt) {
	$cnt = $routine_cnt{$rout};
	$pub = $routine_pub{$rout};
	$pvt = $routine_pvt{$rout};
        if ($do_generic) {
           if ($pub =~ 'public') {       # This specific interface is made public
           print INTFILE "   $pub ${rout}"; # It shouldn't be overloaded ...
           }
	   print INTFILE "   $pvt    ${rout}_";
	   print INTFILE "   interface ${rout}_";
	   print INTFILE "      module procedure ${rout}";
	   for (my $i = 1; $i <= $cnt; $i++) {
	   print INTFILE "      module procedure ${rout}_$i";
	   }
	   print INTFILE "   end interface";
        } else {
	   print INTFILE "   $pvt    ${mod}_${rout}";
           if ($cnt>=1) {
	   print INTFILE "   interface ${mod}_${rout}";
	   print INTFILE "      module procedure ${mod}_${rout}";
	   for (my $i = 1; $i <= $cnt; $i++) {
	   print INTFILE "      module procedure ${mod}_${rout}_$i";
	   }
	   print INTFILE "   end interface";
           }
        }
	   print INTFILE "";
    }
    close INTFILE;
  }
}

################################################################################
# Dump the USE file
sub fortran_dump_use {
  my ($first,$ftime,$rout);
  if (lc($long_module_name) ne '') {

    -f $fortranusefile && unlink($fortranusefile);
    open(USEFILE,">".$fortranusefile);

    # Almost everyone uses TYPES and SYSTEM.
    if ($module_name ne 'TYPES')  {
      print USEFILE "   use TYPES_MODULE";
      if ($module_name ne 'SYSTEM')  {
        print USEFILE "   use SYSTEM_MODULE";
      }
    }

    foreach $mod (keys %used) {
        print USEFILE " ";

        if ($mod eq 'SYSTEM' || $mod eq 'TYPES') {next;}
        $first = 0;
        $ftime = 0;
        foreach $rout (keys %{$used{$mod}}) {
           if ($mod eq "TEXTFILE" and $first==0 and $mod ne $module_name) {
              print USEFILE "   use TEXTFILE_MODULE, only: stdin";
              print USEFILE "   use TEXTFILE_MODULE, only: stdout";
              $first = 1;
           }
           if ($mod eq "TIME"     and $ftime==0 and $mod ne $module_name) {
              print USEFILE "   use TIME_MODULE, only: std_time";
              $ftime = 1;
           }
           if ($mod ne "unknown" and $mod ne $module_name) {
              if ($do_generic) {
              print USEFILE "   use ${mod}_MODULE, only: ${rout}_";
              } else {
              print USEFILE "   use ${mod}_MODULE, only: ${mod}_${rout}";
              }
           } elsif ($mod eq "unknown") {
              if ($do_generic) {
              print USEFILE "!  use ${mod}_MODULE, only: ${rout}_";
              } else {
              print USEFILE "!  use ${mod}_MODULE, only: ?_${rout}";
              }
           }
        }
    }
    close USEFILE;
  }
}

################################################################################
#Process the new scoping unit, which is an interface.
sub fortran_start_module_interface {
  my ($name);

  # Change the interface declaration
  $inp =~ m/^\s*interface\s*([a-z]\w*)/o;
  if ($do_generic) {
     $fortran_out = "   public $1_; interface $1_";
  } else {
     $fortran_out = "   public ${module_name}_$1; interface ${module_name}_$1";
  }
}

################################################################################
# This line is within the scope of a module.
sub fortran_process_module {

   # Fix any explicit only: uses
   # IMPORTANT NOTE: use capital USE is you don't want this
 # $fortran_out =~ s/^ +use +.*only: \w+[^ _]$/$&_/;

   ####### Convert SELF_TYPE and ELEMENT_TYPE before anything ######

   $fortran_out =~ s/\bSELF_TYPE\b/$module_name/g;
   $fortran_out =~ s/\bSELF_TYPE_SIZE\b/${module_name}_SIZE/g;
   if (defined $element_type{$module_name} and
       exists  $element_type{$module_name}) {
   $fortran_out =~ s/\bELEMENT_TYPE\b/$element_type{$module_name}/g;
   $fortran_out =~ s/\bELEMENT_TYPE_SIZE\b/$element_type{$module_name}_SIZE/g;
   }
}

################################################################################
# The line is within the scope of an interface within a module.
sub fortran_process_module_interface {
  my($pre,$name);

  # Expecting a module procedure ###############
  $inp =~ '([a-zA-Z]\w*)';

  my $mod = "";
  if (!$do_generic) { $mod = "${module_name}_"; }

  $pre  = $PREMATCH;
  $name = $POSTMATCH;
  $fortran_out = "${pre}module procedure ${mod}$1";
  while ($name =~ /([a-zA-Z]\w*)/g) {
      $fortran_out .= "\n${pre}module procedure ${mod}$1";
  }
}

################################################################################
# The line is within the scope of a subroutine or function.
sub fortran_process_routine {

  ####### Convert SELF_TYPE and ELEMENT_TYPE before anything ######

  $fortran_out =~ s/\bSELF_TYPE\b/$module_name/g;
  $fortran_out =~ s/\bSELF_TYPE_SIZE\b/${module_name}_SIZE/g;
  if (defined $element_type{$module_name} and
      exists  $element_type{$module_name}) {
  $fortran_out =~ s/\bELEMENT_TYPE\b/$element_type{$module_name}/g;
  $fortran_out =~ s/\bELEMENT_TYPE_SIZE\b/$element_type{$module_name}_SIZE/g;
  }

  ############################# lines that contain a dot. ###########
  $fortran_out = &dots_to_fortran($fortran_out);

  ################################
  # Ad hoc stuff here ....
  ################################

  &fortran_add_stack_macro; 
  &fortran_process_return;
  &fortran_process_case_statements;
  &fortran_process_error_management;
}

################################################################################
# The line is within the scope of a program.
sub fortran_process_program {

  $name = $module_name;
  $current_rout_name = $module_name;
  %routine_attributes = ();
  $routine_attributes{$name}{'real_name'} = $name;

  ############################# lines that contain a dot. ###########
  $fortran_out = &dots_to_fortran($fortran_out);

  ################################
  # Ad hoc stuff here ....
  ################################

  &fortran_process_case_statements;
  &fortran_process_error_management;
}

################################################################################
# The line is within the scope of a contains.
sub fortran_start_routine {
  my ($pre,$attr,$rout_type,$name,$args,$real_name,$result);

  $name = $current_rout_name;
  $pre = $routine_attributes{$name}{'indent'};
  $attr = '';
  if    (defined $routine_attributes{$name}{'always_elemental'}) { $attr = 'ALWAYS_ELEMENTAL '; }
  elsif (defined $routine_attributes{$name}{'elemental'})        { $attr = 'ELEMENTAL '; }
  elsif (defined $routine_attributes{$name}{'always_pure'})      { $attr = 'ALWAYS_PURE '; }
  elsif (defined $routine_attributes{$name}{'pure'})             { $attr = 'PURE '; }
  if    (defined $routine_attributes{$name}{'recursive'})        { $attr .= 'recursive '; }

  $rout_type = '';
  if (defined $routine_attributes{$name}{'function'}) { $rout_type = 'function'; }
  else                                                { $rout_type = 'subroutine'; }
  $args = $routine_attributes{$name}{'args'};
  $real_name = $routine_attributes{$name}{'real_name'};

  $result = '';
  if (defined $routine_attributes{$name}{'function_result'}) {
    $result = " result(" . $routine_attributes{$name}{'function_result'} . ")";
  }

  my $mod = "";
  if (!$do_generic && $parentscope !~ 'interface') { $mod = "${module_name}_"; }

  $fortran_out = "${pre}${attr}${rout_type} ${mod}${real_name}${args}${result}";

  if ( ! (defined $routine_attributes{$name}{'functional'} || 
          defined $routine_attributes{$name}{'routinal'} ||
          defined $routine_attributes{$name}{'selfless'})) {
    $fortran_out .= "\n    $module_name :: self";
  }
}

################################################################################
# Convert the dot notation into Fortran.
# Takes a string as its only argument, and returns the Fortran version.
sub dots_to_fortran {
  my($pre,$first,$left,$middle,$right,$i,$y,$progname);
  my($rout,$post,$fixedpost,$arg,$call,$underscore,$arg_type);

  $X = $_[0];
  if ($X !~ m'[.]'o) {return $X;} # in case of no dots

  # skip "include" lines, don't want any filenames changed.
  if ($X =~ m'include'o && 
      $X =~ m'^(?:\s*|[#])include\s+[\"\'](?:[^\"\']+)[\"\']'o) {
    return $X;
  }

  $i = 0;
  THIS : while ($i < length $X) {
    if ((substr $X,$i,1) eq '.') {
      $left = substr $X,0,$i;
      $right = substr $X,$i+1;
      if (! &outside_of_string($left)) { next THIS; }

      # What is to the right of the dot - object feature and other stuff.
      if ($right !~ m'^([a-zA-Z_]\w*)(.*)'o) {next THIS};
      $rout = $1;
      $post = $2;
      $fixedpost = $2;

      # Separate out the argument from the previous stuff on the line.
      $arg = &last_fortran_object($left);
      if ($arg eq '') {
        # if last object is null, then set it to 'self'.
        $arg = 'self';
        $pre = $left;
      } else {
        $left =~ /(\Q$arg\E)$/;
        $pre = $PREMATCH;
      }

      # determine if subroutine call or not.
      # subroutine call must have a new line, semicolon or right-bracket
      # before it.
      # round bracket?
      if ($pre =~ m'((?:;|^|\))\s*)$'o) {$call = 'call ';}
      else                       {$call = '';}

      if (&has_field($arg, $rout)) {
         # $i does not change if we change the dot to a %.
         $X = join('',$pre,$arg,'%',$rout,$post);

      } else {                                    # routine call ...

        if ($rout =~ m'^dim([1234567]?)$'o) {     # Modify dim statements explicitly
          if (defined $1 && $1 ne '') { $arg .= ",$1"}
          $rout = 'size';
          $underscore = '';
        } elsif ($rout =~ m'^scan$'o) {           # Modify scan statements explicitly
          $rout = 'scan';
          $underscore = '';
        } elsif ($rout =~ m'^verify$'o) {           # Modify verify statements explicitly
          $rout = 'verify';
          $underscore = '';
        } elsif ($rout =~ m'^trim$'o) {           # Modify trim statements explicitly
          $rout = 'trim';
          $underscore = '';
        } elsif ($fixedpost !~ '^[(]' && $rout =~ m'^created$'o) {   # Modify argumentless "created" statements explicitly
          $rout = 'associated';
          $underscore = '';
        } elsif ($fixedpost !~ '^[(]' && $rout =~ m'^destroyed$'o) { # Modify argumentless "destroyed" statements explicitly
          $rout = 'NOT associated';
          $underscore = '';
          # We might have introduced a "NOT NOT".
          if ($pre =~ s/NOT\s*$// || $pre =~ s/\.NOT\.\s*$//) {
            $rout = 'associated';
          }
        } else {                                  # Do a normal routine
          # add to use list.
          $arg_type = &fortran_type_of_this($arg);
          if ($module_name =~ /run_(\w+)/i && $arg_type =~ /^$1$/i) { }
          else { $used{$arg_type}{$rout} = 1; }   # Only use if arg_type /= XXXX
          if ($do_generic) {                      # Underscore, always generic
            $underscore = '_';
          } else {
          # If no generic interfaces are being used, scrub interface
          # and place explicit type in front of routine name
            $rout = $arg_type . '_' . $rout;
            $underscore = '';
          }
        }

        if ($rout =~ m'^trim$'o) {     # Modify trim statements explicitly
          $rout = 'trim';
          $underscore = '';
        }

        # check for other arguments to the function call.
        if ($fixedpost !~ s'^[(]','o)    { $fixedpost = ')' . $fixedpost; }

        $X = join('',$pre,$call,$rout,$underscore,'(',$arg,$fixedpost);
        # for a routine call, $i will increase by the length of ${rout} plus
        # one for the bracket.  If there is no underscore, then decrease by 1.
        $i += (length (join('',$call,$rout,$underscore)) - 1);
      }
    }
  } continue {
    $i++;
  }
  return $X;

}

################################################################################
# Add macros include file, and interface include file 
sub fortran_add_include_files {
  if ($fortran_out =~ 'implicit none') {
    # if (grep 'program',@scope) {
        $fortran_out .= "\n\n" .
             "#  include \"macros\"\n" .
             "#  include \"${fortranintfile}\"\n";
    # } else {
    #   $fortran_out .= "\n\n" .  "#  include \"../macros\"\n" ;
    # }
  }
}

################################################################################
# Swap the order of a declaration statement around.
sub fortran_swap_declaration_order {
  if ($fortran_out =~ '\s+::\s+') {
    $left = $PREMATCH;   # split into before and after the ' :: '.
    $right = $POSTMATCH;
    $left =~ m|^([^ ]* +)([\w, ]+)$|;
    $pre = $1;
    $variables = $2;
    if (defined $pre && defined $variables) {
      if ($right =~ m|^([\w(:),=*+-/%\s]*[\w)*])\s*(\sDEFAULT[(][^!]+[)])\s*$|
       || $right =~ m|^([\w(:),=*+-/%\s]*[\w)*])\s*(\sDEFAULT_NULL)\s*$|
       || $right =~ m|^([\w(:),=*+-/%\s]*[\w)*])( += .*?)\s*$|
       || $right =~ m|^([\w(:),=*+-/%\s]*[\w)*])(.*?)\s*$|)
       {$fortran_out = "$pre$1 :: $variables$2"; }
    }
  }
}

################################################################################
## Miscellaneous small changes.
sub fortran_misc_changes {
  my(@subst_for_this_line,$pattern);

  # Miscelaneous type substitutions, e.g. REALVEC -> REALVEC(:) from %subst hash
  # Also: substitute USE lines ...
  # But NOTE: only lower case USEs are # converted
  if ($fortran_out =~ m'::'o) { # variable declaration line.

    # Substitute STRVEC and BSTRVEC.
    if ($fortran_out =~ m'STRVEC'o) {
      foreach $pattern (keys %substs) {  # Do STRVEC's first
        $fortran_out =~ s/\b$pattern([^\w(])/$substs{$pattern}$1/;
        $fortran_out =~ s/\b$pattern\*\b/$substs{$pattern}\*/;
      }
      if ($fortran_out !~ m'\bSTRVEC[(](.+),(.+)[)]'o) {
        $fortran_out =~ s/\bSTRVEC[(]([^ ,]+)[)]/STRVEC(STR_SIZE,$1)/;
      }
    }

    # Create an array that contains only the substitutions relevent to this
    # line.  (just an optimisation).
    @subst_for_this_line = grep $fortran_out =~ m/$_/, keys %subst;

    # Pattern changes.
    foreach $pattern (@subst_for_this_line) {
    # Try exact match. (neccessary to distinguish BSTR from STR).
      $fortran_out =~ s/\b$pattern([^\w(])/$subst{$pattern}$1/;
      $fortran_out =~ s/\b$pattern\*\b/$subst{$pattern}\*/;
    }
    foreach $pattern (@subst_for_this_line) {
    # Now the rest. (INTVEC,CPXVEC,INTVEC all have same form).
      if ($fortran_out =~ m/(.*)$pattern/) {
        $fortran_out =~ s/$pattern([^\w(])/$subst{$pattern}$1/;
        $fortran_out =~ s/$pattern\*\b/$subst{$pattern}\*/; # PTR's.
      }
    }

    if ($fortran_out =~ m'\*'o) {
      $fortran_out =~ s'\* *::', PTR ::'o;      # Add remaining PTR conversion
      $fortran_out =~ s/([^=])\*, */$1, PTR, /;
    }
  }
  elsif ($fortran_out =~ ' use ') {
      $fortran_out =~ s/^[ ]*use[ ]+([A-Z]\w*)/   use $1_MODULE/s;
      if ($do_generic) {
         $fortran_out =~ s/^ +use +.*only: *\w+[^_]/$&/;
      } else {
         $fortran_out =~ s/^ +use +(\w+)_MODULE *, *only: *(\w+[^_]) *$/   use $1_MODULE, only: $1_$2/;
      }
  }

                                      # Add NULL() statements for local PTR's
##  if ($scope{$ns} eq 'subroutine' || $scope{$ns} eq 'function' ||
##      $scope{$ns} eq 'module'     || $scope{$ns} eq 'program') {
##     if ($fortran_out =~ /, PTR/ && $fortran_out =~ '::' && &contains_arg($fortran_out) == 0 )
##        { &add_nullify_stmt }
##  }
                                        # Add DEFAULT_NULL statements for type defs
##  if ($scope{$ns} eq 'type') {
##     if ($fortran_out =~ /, PTR/ && $fortran_out =~ '::' )
##        { $fortran_out = $fortran_out . "DEFAULT_NULL" }
##  }

  $fortran_out =~ s/([^\\])\[/$1(\//go; # brackets [ ] are array constructors
  $fortran_out =~ s/([^\\])\]/$1\/)/go; # 
  $fortran_out =~ s/\\\[/[/go;          # ... unless preceeded by back slash
  $fortran_out =~ s/\\\]/]/go;  
}

################################################################################
# Return the type of the argument, otherwise return "unknown".
sub fortran_type_of_this {
  my ($arg,$tmp,@tmp1,@tmp2,$i,$j,$x,$y,$left,$middle,$right,$is_a_var,$arg_type);
  my($tmp3,$lefttype,$middletype,$righttype);
  $arg = $_[0];

  # remove spaces.
  $arg =~ s'[ ]''go;

  # remove outside brackets.
  while ($arg =~ '^[(](.*)[)]$') {$arg = $1}

  # Does it contain derived types?
  # A call to &has_field will add each component to the local hash table if not
  # there already.
  if ($arg =~ /(.*)[%](.*?)$/) {
    $i = 0;
    while ($i < length $arg) {
      $y = substr $arg,$i,1;
      if ($y eq '%') {
        $left = substr $arg,0,$i;
        $right = substr $arg,$i+1;
        &has_field($left,$right);
      }
      $i++;
    }
  }

  # we might be lucky with it being a single variable.
  ($tmp = $arg) =~ s/%/./g;
  ($is_a_var,$arg_type) = &is_var($tmp);
  if ($is_a_var) { return ($arg_type); }

  # we might be lucky with it being an array.
  ($tmp = $arg) =~ s/%/./g;
  ($is_a_var,$arg_type) = &is_array_var($tmp);
  if ($is_a_var) { return ($arg_type); }

  # a real literal constant.
  if ($arg =~ '^([+-])?\d*[.](\d+)?') {
    return ('REAL');
  }

  # a real constant.
  if ($arg =~ '([+-])?(ONE)|(TWO)|(THREE)|(FOUR)|(FIVE)|(TEN)|(ZERO)') {
    return ('REAL');
  }

  # an integer constant.
  if ($arg =~ '^([+-1234567890]+)$') {
    return ('INT');
  }

  # a string literal constant.
  if ($arg =~ /("[^"]*")|('[^']*')/) {
    return ('STR');
  }

  # perhaps it is a function we know about.
  if ($arg =~ '^(\w+)_[(]([\w%]+)(.*)') {
  # is above foolproof?
    # maybe do something more fancy with $2 later - recursive call to
    # fortran_type_of_this?
    if (defined $local_type{$2}) {
      if (defined $function_res_type{"$local_type{$2}_$1"}) {
        return ($function_res_type{"$local_type{$2}_$1"});
      }
    }
  }

  # size intrinsic returns an INT.
  if ($arg =~ '^size[(](.*)[)]$') {
    return ('INT');
  }

  # deal with selfless functions; type of the function is the result type
  foreach $i (keys %function_res_type) {
    if ("${module_name}_${arg}" eq $i) {
      return ($function_res_type{$i});
    }
  }

  # min/max function.
  if ($arg =~ '^max[(](.*)[)]$' || $arg =~ '^min[(](.*)[)]$') {
    @tmp2 = split /[,]+/,$1;
    undef $tmp3;
    THAT : foreach $j (@tmp2) {
      $x = &fortran_type_of_this($j);
      if (! defined $tmp3) { $tmp3 = $x; }
      if ($tmp3 ne $x) { $tmp3 = "unknown"; last THAT; }
    }
    return ($tmp3);
  }

  # is it a function call?
  if ($arg =~ '^(.*?)[(](.*)[)]$' && &has_matching_brackets($arg)) {
    $tmp3 = $1;
    if (defined $tmp3 && $tmp3 =~ '\w$') {
      return 'unknown';
    }
  }

  # lets get the type of things in brackets.
  if ($arg =~ '\(' && &has_matching_brackets($arg)) {
    undef $lefttype;
    undef $middletype;
    undef $righttype;

    ($left,$middle,$right) = &split_by_first_brackets($arg);

    # This first bit is for arrays.
    if (defined $left && $left =~ '\w$') {
      $lefttype = &fortran_type_of_this($left . '(' . $middle . ')');
      undef $middletype;
      if (defined $right && $right ne '') { $righttype = &fortran_type_of_this($right);}

    } else {
      if (defined $left && $left ne '') { $lefttype = &fortran_type_of_this($left);}
      if (defined $middle && $middle ne '') { $middletype = &fortran_type_of_this($middle);}
      if (defined $right && $right ne '') { $righttype = &fortran_type_of_this($right);}
    }

    undef $tmp3;
    if (! defined $tmp3) { $tmp3 = $lefttype; }
    if (! defined $tmp3) { $tmp3 = $middletype; }
    if (! defined $tmp3) { $tmp3 = $righttype; }

    if (defined $tmp3) {
      if (defined $lefttype) {if ($lefttype ne $tmp3) {return 'unknown'}}
      if (defined $middletype) {if ($middletype ne $tmp3) {return 'unknown'}}
      if (defined $righttype) {if ($righttype ne $tmp3) {return 'unknown'}}
      return $tmp3;
    }
  }

  # maybe a simple combination of variables.
  if ($arg =~ /[=\+\-\*\/]+/) {
    @tmp1 = split /[=\+\-\*\/]+/,$arg;
    if ($#tmp1 >= 0) {
      undef $tmp3;
      THIS : foreach $i (@tmp1) {
        $x = &fortran_type_of_this($i);
        if (! defined $tmp3) { $tmp3 = $x; }
        if ($tmp3 ne $x) { $tmp3 = "unknown"; last THIS; }
      }
      if (defined $tmp3) { return ($tmp3); }
    }
  }

  return ('unknown');
}

################################################################################
# Open the fortran output file.
sub fortran_start {
  -f $fortranfile && unlink($fortranfile);
  open(F90FILE,">".$fortranfile);
}

################################################################################
# End the fortran stuff, and close the fortran file.
sub fortran_end {
  close(F90FILE);
  &fortran_dump_interface;
  &fortran_dump_use; 
}

################################################################################
#Process the new scoping unit, which is an unlabelled end keyword.
sub fortran_do_empty_end {
  my($i,$getfile,$found,$name,$rout_name);
  $name = $previous_rout_name;

  # need ns<3 to eliminate interface routines
  if (($oldscope eq 'subroutine' || $oldscope eq 'function') && $#scope < 3) {
    if ($being_inherited) {
      $fortran_out = "   ! The following code is inherited from " .
                $routine_attributes{$name}{'parent_module'};
    } else {
      $rout_name = "$module_name:$routine_attributes{$name}{'real_name'}";
      if (defined $routine_attributes{$name}{'pure'}) { 
       # $fortran_out =~ s/end */end $oldscope/; 
         $fortran_out =~ s/(\s*)end */$1  STOP_TIMER(\"$rout_name\")\n$1end $oldscope/; }
      elsif (defined $routine_attributes{$name}{'leaky'}) { 
         $fortran_out =~ s/(\s*)end */$1  STOP_TIMER(\"$rout_name\")\n$1   UNSTACK\n$1end $oldscope/; }
      else { 
         $fortran_out =~ s/(\s*)end */$1  STOP_TIMER(\"$rout_name\")\n$1   CHECK\n$1end $oldscope/; }
    }
  } else {
    $fortran_out =~ s/end */end $oldscope/;
  }
}

