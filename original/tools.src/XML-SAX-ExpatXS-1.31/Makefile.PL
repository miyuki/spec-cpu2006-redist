# This Makefile.PL contains portions borrowed 
# from XML::Parser and XML::SAX

use ExtUtils::MakeMaker;
use Config;

$expat_libpath = '';
$expat_incpath = '';

my @replacement_args;

foreach (@ARGV) {
  if (/^EXPAT(LIB|INC)PATH=(.+)/) {
    if ($1 eq 'LIB') {
      $expat_libpath = $2;
    }
    else {
      $expat_incpath = $2;
    }
  }
  else {
    push(@replacement_args, $_);
  }
}

@ARGV = @replacement_args;

if (not $expat_libpath and $] >= 5.006001) {
  require ExtUtils::Liblist;		# Buggy before this
  ($expat_libpath) = ExtUtils::Liblist->ext('-lexpat');
}

unless ($expat_libpath) {
  # Test for existence of libexpat
  my $found = 0;
  foreach (split(/\s+/, $Config{libpth})) {
    if (-f "$_/libexpat." . $Config{so}) {
      $found = 1;
      last;
    }
  }

  unless ($found) {
      die <<'Expat_Not_Installed;';

Expat must be installed prior to building XML::SAX::ExpatXS and I can't 
find it in the standard library directories. You can download expat from:

http://expat.sourceforge.net/

If expat is installed, but in a non-standard directory, then use the
following options to Makefile.PL:

    EXPATLIBPATH=...  To set the directory in which to find libexpat

    EXPATINCPATH=...  To set the directory in which to find expat.h

For example:

    perl Makefile.PL EXPATLIBPATH=/home/me/lib EXPATINCPATH=/home/me/include

Note that if you build against a shareable library in a non-standard location
you may (on some platforms) also have to set your LD_LIBRARY_PATH environment
variable at run time for perl to find the library.

Expat_Not_Installed;
  }
}

my $libs = "-lexpat";
$libs = "-L$expat_libpath $libs"
  if $expat_libpath;

@extras = ();

push(@extras, INC => "-I$expat_incpath")
  if $expat_incpath;

push(@extras,
     CAPI => 'TRUE')
    if ($PERL_VERSION >= 5.005 and $OSNAME eq 'MSWin32'
	and $Config{archname} =~ /-object\b/i);

push(@extras,
     ABSTRACT_FROM => 'lib/XML/SAX/ExpatXS.pm',
     AUTHOR => 'Matt Sergeant <matt AT sergeant DOT org>')
    if ($ExtUtils::MakeMaker::Version >= 5.4301);

# building XML::SAX::ExpatXS::Preload;
&build_xml_sax_expatxs_preload;

WriteMakefile(
    NAME => 'XML::SAX::ExpatXS',
    VERSION_FROM => 'lib/XML/SAX/ExpatXS.pm',
    PREREQ_PM => {
		    'XML::SAX' => 0.13,
		   },
    LIBS => $libs,
    @extras
);


sub MY::install {
    package MY;
    my $script = shift->SUPER::install(@_);
    if (ExtUtils::MakeMaker::prompt("Do you want to alter ParserDetails.ini?", "Y") =~ /^y/i) {
        $script =~ s/install :: (.*)$/install :: $1 install_expat_xs/m;
        $script .= <<"INSTALL";

install_expat_xs :
\t\@\$(PERL) -MXML::SAX -e "XML::SAX->add_parser(q(XML::SAX::ExpatXS))->save_parsers()"

INSTALL

    }

    return $script;
}


sub build_xml_sax_expatxs_preload {
    
    print "Writing XML::SAX::ExpatXS::Preload\n";

    my $code = <<'EOHEADER';
package XML::SAX::ExpatXS::Preload;

#-----------------------------------------------------#
# STOP!!!!!
#
# This file is generated by the 'Makefile.PL' file
# that ships with the XML::SAX::ExpatXS distribution.
# If you need to make changes, patch that file NOT
# this one.
#-----------------------------------------------------#

use strict;
use vars qw($VERSION);

$VERSION = '0.01';

EOHEADER

    my %EVENT_SPEC = (
		      start_element => [qw(ContentHandler DocumentHandler Handler)],
		      end_element   => [qw(ContentHandler DocumentHandler Handler)],
		      characters    => [qw(ContentHandler DocumentHandler Handler)],
		      comment       => [qw(DocumentHandler LexicalHandler Handler)],
                     );

    for my $ev (keys %EVENT_SPEC) {
        $code .= <<"        EOTOPCODE";
sub get_$ev {
    my \$self = shift;

    my \$method;
    my \$callbacks;
    if (exists \$self->{ParseOptions}) {
        \$callbacks = \$self->{ParseOptions};
    }
    else {
        \$callbacks = \$self;
    }

    if (0) { # dummy to make elsif's below compile
    }
        EOTOPCODE

       my ($can_string, $aload_string);
       for my $h (@{$EVENT_SPEC{$ev}}) {
            $can_string .= <<"            EOCANBLOCK";
    elsif (defined \$callbacks->{'$h'} and \$method = \$callbacks->{'$h'}->can('$ev') ) {
        my \$handler = \$callbacks->{'$h'};
        \$self->{Methods}->{'$ev'} = sub { \$method->(\$handler, \@_) };
    }
            EOCANBLOCK
            $aload_string .= <<"            EOALOADBLOCK";
    elsif (defined \$callbacks->{'$h'} and \$callbacks->{'$h'}->can('AUTOLOAD') ) {
        my \$res = eval { \$callbacks->{'$h'}->$ev(\@_) };
        if (\$@) {
            die \$@;
        }
        else {
            my \$handler = \$callbacks->{'$h'};
            \$self->{Methods}->{'$ev'} = sub { \$handler->$ev(\@_) };
        }
    }
            EOALOADBLOCK
        }

        $code .= $can_string . $aload_string;

            $code .= <<"            EOFALLTHROUGH";
    else {
        \$self->{Methods}->{'$ev'} = sub { };
    }

            EOFALLTHROUGH

        $code .= "return 1;\n}\n\n";
    }

    $code .= <<'BODY';

1;

BODY

    $code .= "__END__\n";

    $code .= <<'FOOTER';

=head1 NAME

XML::SAX::ExpatXS::Preload - Helper class for XML-SAX-ExpatXS

=head1 AUTHOR

Petr Cimprich (petr@gingerall.cz) based on an original code of XML::SAX::Base 
by Kip Hampton (khampton@totalcinema.com).

=head1 SEE ALSO

L<XML::SAX::ExpatXS>

=cut

FOOTER

    open(BASE, ">" . File::Spec->catdir("lib", "XML", "SAX", "ExpatXS", "Preload.pm")) || die "Cannot write Preload.pm: $!";
    print BASE $code;
    close BASE;
}
