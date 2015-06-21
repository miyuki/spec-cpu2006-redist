#
# CPUFlagsParser.pm
#
# This class implements a Perl XML::SAX-compliant parser for SPEC CPU-style
# flag description files.  The DTD for such files can be found at
# http://www.spec.org/dtd/cpuflags2.dtd
#
# Copyright 2005-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: CPUFlagsParser.pm 6416 2011-04-07 03:22:22Z cloyce $

package CPUFlagsParser;
use base qw(XML::SAX::Base);

my $version = '$LastChangedRevision: 6416 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'CPUFlagsParser.pm'} = $version;

# Master list of all available classes; this must match the DTD for things to
# make sense.
my %classlist = map { $_ => 1 } qw(mandatory forbidden
				   portability optimization
				   compiler other
				   unknown
                                   default); # For <header>

sub new {
  my $class = shift;
  my %options = @_;
  my $fp = { 'flaglist' => undef,
             'flagmap'  => {},
             'inforef'  => {},
             'elemstack' => [],
             'charstack' => [],
             'currelem' => undef,
             'chars'    => undef,
             'seenflags'=> {},
           };
  return bless $fp, $class;
}

sub get_flag_ref {
  my ($self) = @_;
  return { %{$self->{'inforef'}},
           'flag' => $self->{'flaglist'},
           'flagmap' => $self->{'flagmap'} };
}

sub start_element {
  my ($self, $element) = @_;

  my $elem = $element->{LocalName};
  if ($elem eq 'flagsdescription') {    # Top-level container
    if (defined($self->{'flaglist'})) {
      # One file per parser instance, please.
      die "Duplicate <flagsdescription> seen.\n";
    } else {
      $self->{'flaglist'} = [];
    }
    
    # If there are things on the element stack, stuff in the accumulator,
    # etc that might indicate that the previous file was not complete, THEN
    # definitely throw an error.  This should not happen.
    if (defined($self->{'currelem'})
        || defined($self->{'chars'})
        || @{$self->{'elemstack'}}
        || @{$self->{'charstack'}}) {
        die "Saw <flagsdescription> before finishing previous processing!\n";
    }

  } elsif (   $elem eq 'title'
           || $elem eq 'style'
           || $elem eq 'header'
           || $elem eq 'platform_settings'	# <= v1.1
           || $elem eq 'submit_command'		# >= v1.2
           || $elem eq 'fdo_settings'		# >= v1.2
           || $elem eq 'sw_environment'		# >= v1.2
           || $elem eq 'os_tuning'		# >= v1.2
           || $elem eq 'virtual_machine'	# >= v1.2
           || $elem eq 'firmware'		# >= v1.2
           || $elem eq 'parts'			# >= v1.2
           || $elem eq 'filename'
           || $elem eq 'dumpit'
           ) {
    if ($elem ne 'header' && exists($self->{'inforef'}->{$elem})) {
      die "<$elem> section may only occur once!\n";
    }
    $self->push_current();
    if ($elem eq 'header') {
      my ($name, $attrhash) = each %{$element->{Attributes}};
      my $value = $attrhash->{Value};
      if (!defined($name)) {
        $name = 'class';
        $value = 'default';
      }
      die "<header> has no attribute other than class!\n" if $name !~ /class$/;
      # Make sure that the class is proper
      if (!exists($classlist{$value})) {
        die "\"$value\" is not a valid class for <header>!\n";
      }
      $self->{'inforef'}->{$elem} = {} unless ref($self->{'inforef'}->{$elem}) eq 'HASH';
      $self->{'inforef'}->{$elem}->{$value} = '';
      $elem .= ':'.$value;
    } else {
      $self->{'inforef'}->{$elem} = '';
    }
    $self->{'currelem'} = $elem;
    $self->{'chars'} = '';

  } elsif ($elem eq 'flag') {
    if (defined($self->{'currelem'})) {
      die "Saw beginning of flag description inside another element!\n";
    }
    $self->{'currelem'} = {};
    $self->{'chars'} = '';
    # Set the attributes
    while ( my ($name, $value) = (each (%{$element->{Attributes}}))) {
        $self->{'currelem'}->{$value->{LocalName}} = $value->{Value};
    }
    # Check the class (if it's set)
    if (exists $self->{'currelem'}->{'class'}) {
        my $class = $self->{'currelem'}->{'class'};
	if (!exists $classlist{$class}) {
	    die "Illegal value \"$class\" found in class attribute for flag element\n";
	}
    }

  } elsif ($elem eq 'ex_replacement') {
    if (ref($self->{'currelem'}) ne 'HASH') {
      die "<$elem> element found outside of a flag description!\n";
    }
    if (ref($self->{'currelem'}->{$elem}) ne 'ARRAY') {
      $self->{'currelem'}->{$elem} = [];
    }
    $self->push_current();
    $self->{'currelem'} = $elem;
    $self->{'chars'} = '';

  } elsif (   $elem eq 'include'
	   || $elem eq 'display'
	   || $elem eq 'example'
	  ) {
    if (ref($self->{'currelem'}) ne 'HASH') {
      die "<$elem> element found outside of a flag description!\n";
    }
    my %tmphash = ();
    while ( my ($name, $value) = (each (%{$element->{Attributes}}))) {
        $name = $value->{LocalName};
        if (   (($elem eq 'include') && ($name !~ /^(flag|flagtext|text)$/o))
	    || (($elem eq 'display') && ($name ne 'enable')
            ||  ($elem eq 'example'))
	   ) {
	  die "Unknown attribute '$name' found in $elem element!\n";
	}
	if ($elem eq 'include') {
          $tmphash{$name} = $value->{Value};
	} elsif ($elem eq 'display') {
	  my $text = $value->{Value};
	  if (   $text !~ /^(?:0|1)$/
	      && $text !~ s/^\s*(?:1|on|yes)\s*$/1/i
              && $text !~ s/^\s*(?:0|off|no)\s*$/0/i) {
	    die "Illegal value \"$text\" found in $name attribute for $elem element\n";
	  }
	  $self->{'currelem'}->{$elem} = $text;
	}
    }

    # Arrange for the flagtext (if any) to stay with its flag
    if ($elem eq 'include') {
      if (exists $tmphash{'text'}) {
        if (ref($self->{'currelem'}->{'inc_text'}) ne 'ARRAY') {
          $self->{'currelem'}->{'inc_text'} = [];
        }
        # Textual substitution; no flagtext allowed
        if (exists $tmphash{'flagtext'}) {
          die "<include> attributes 'text' and 'flagtext' may not appear together!\n";
        }
        push @{$self->{'currelem'}->{'inc_text'}}, $tmphash{'text'};
      } elsif (exists $tmphash{'flag'}) {
        if (ref($self->{'currelem'}->{'inc_flag'}) ne 'ARRAY') {
          $self->{'currelem'}->{'inc_flag'} = [];
        }
        push @{$self->{'currelem'}->{'inc_flag'}}, [ $tmphash{'flag'}, $tmphash{'flagtext'} ];
      }
    }

    $self->push_current();
    $self->{'currelem'} = $elem;
    $self->{'chars'} = undef;

  } else {
    # It's an unknown element.  It's most likely a typo or some unprotected
    # HTML.  Either way, it's VERY not likely to be what the user intended.
    if (each %{$element->{Attributes}}) {
      print "$elem: Attributes for <$element->{LocalName}>\n";
      while ( my ($name, $value) = (each (%{$element->{Attributes}}))) {
        print "  $value->{LocalName} : $value->{Value}\n";
      }
    }
    if (defined($self->{'currelem'}) && exists($self->{'currelem'}->{'name'})) {
      die "Unrecognized element <$elem> found while processing description\n for \"".$self->{'currelem'}->{'name'}."\".\n";
    } else {
      die "Unrecognized element <$elem> found.\n";
    }
  }

}

sub end_element {
  my ($self, $element) = @_;

  my $elem = $element->{LocalName};
  if ($elem eq 'flagsdescription') {    # Top-level container
    # If there are things on the element stack, stuff in the accumulator,
    # etc, then processing was NOT complete and the file is bad.
    if (defined($self->{'chars'}) && $self->{'chars'} =~ /\S/) {
      die "flagsdescription element may not contain character data!\n";
    }
    if (defined($self->{'currelem'})
        || @{$self->{'elemstack'}}
        || @{$self->{'charstack'}}) {
        die "Saw </flagsdescription> with other elements still open.  XML file is bad.\n";
    }

  } elsif (   $elem eq 'title'
           || $elem eq 'style'
           || $elem eq 'header'
           || $elem eq 'platform_settings'	# <= v1.1
           || $elem eq 'submit_command'		# >= v1.2
           || $elem eq 'fdo_settings'		# >= v1.2
           || $elem eq 'sw_environment'		# >= v1.2
           || $elem eq 'os_tuning'		# >= v1.2
           || $elem eq 'virtual_machine'	# >= v1.2
           || $elem eq 'firmware'		# >= v1.2
           || $elem eq 'parts'			# >= v1.2
           || $elem eq 'filename'
           || $elem eq 'dumpit'
           ) {
    if (!defined($self->{'currelem'}) || $self->{'currelem'} !~ /^$elem/) {
      die "Close tag </$elem> found, but $elem is not the current element!\n";
    }
    if ($elem eq 'header') {
      my ($hdr, $class) = split(/:/, $self->{'currelem'}, 2);
      $self->{'inforef'}->{$hdr}->{$class} = $self->{'chars'};
    } else {
      $self->{'inforef'}->{$elem} = $self->{'chars'};
    }
    $self->pop_current();

  } elsif ($elem eq 'flag') {
    if (ref($self->{'currelem'}) ne 'HASH') {
      die "Flag description closing tag found, but the current element is not a flag desc!\n";
    }
    $self->{'currelem'}->{'description'} = $self->{'chars'};
    $self->{'currelem'}->{'display'} = 1 unless exists($self->{'currelem'}->{'display'});
    push @{$self->{'flaglist'}}, $self->{'currelem'};
    $self->{'flagmap'}->{$self->{'currelem'}->{'name'}} = $self->{'currelem'};
    $self->pop_current();       # It'll clear things out

  } elsif ($elem eq 'ex_replacement') {
    if (!defined($self->{'currelem'}) || $self->{'currelem'} ne $elem) {
      die "Close tag </$elem> found, but $elem is not the current element!\n";
    }
    my $text = $self->{'chars'};
    $self->pop_current();
    push @{$self->{'currelem'}->{'ex_replacement'}}, $text;

  } elsif (   $elem eq 'include'
	   || $elem eq 'display'
	   || $elem eq 'example'
	  ) {
    if (!defined($self->{'currelem'}) || $self->{'currelem'} ne $elem) {
      die "Close tag </$elem> found, but $elem is not the current element!\n";
    }
    my $text;
    if (defined($self->{'chars'}) && $self->{'chars'} =~ /\S/) {
      if ($elem eq 'example') {
        $text = $self->{'chars'};
      } else {
        die "$elem element may not contain character data!\n";
      }
    }
    $self->pop_current();
    $self->{'currelem'}->{$elem} = $text if defined($text);

  } else {
    print "unhandled end_element $elem!\n";
    if ($self->{'currelem'} ne $elem) {
      die "Close tag for $elem found, but $elem is not the current element!\n";
    }
    # Try to not let it screw other things up
    $self->pop_current();
  }
}

sub characters {
  my ($self, $data) = @_;

  if (!defined($self->{'chars'})) {
    $self->{'chars'} = $data->{Data};
  } else {
    $self->{'chars'} .= $data->{Data};
  }
}

sub push_current {
  my ($self) = @_;

  # If there's a current element being worked on, save it and its characters
  # so far.
  if (defined($self->{'currelem'})) {
      push @{$self->{'elemstack'}}, $self->{'currelem'};
      push @{$self->{'charstack'}}, $self->{'chars'};
  }
  $self->{'currelem'} = undef;
  $self->{'chars'} = undef;
}

sub pop_current {
  my ($self) = @_;

  $self->{'currelem'} = pop @{$self->{'elemstack'}};
  $self->{'chars'} = pop @{$self->{'charstack'}};
}

1;
