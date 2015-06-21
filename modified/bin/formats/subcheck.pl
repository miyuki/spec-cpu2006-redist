#
#  subcheck.pl - raw file submission checker
#  Copyright 2005-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Cloyce D. Spradling
#
# $Id: subcheck.pl 6419 2011-04-08 01:06:49Z cloyce $

use IO::File;
use strict;

use vars qw($name $extension $synonyms $prefix %syntax $first $last);
do "formats/${main::lcsuite}.syntax" || die "\n  Error reading ${main::lcsuite}.syntax file:\n    $@\n";

$name      = 'Submission Check';
$extension = undef;
$synonyms  = { map { lc($_) => 1 } ($name, qw(check chk subcheck subtest test sub)) };

$Spec::Format::subcheck::non_default = 1;       # You must ask for it by name
my $subcheck_version = '$LastChangedRevision: 6419 $ '; # Make emacs happier
$subcheck_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::subcheck::non_default = 1;  # You must _sometimes_ ask for it
$::tools_versions{'subcheck.pl'} = $subcheck_version;

sub format {
    my($me, $r, $fn, $written) = @_;
    my $rawfile = $fn.$Spec::Format::raw::extension;
    my @errors = ();
    my @data = ();
    my %check_lines = ();
    my @syntax_types = ();
    my %syntax_lists = ();
    my @written = ::isa($written, 'ARRAY') ? @{$written} : ( $written );

    # Look for the raw file that's been generated
    if (!-f $rawfile || !grep { /$rawfile$/ } @written) {
      # Okay... there might not be a raw file if this is a fake report.
      # Those _should_ be checked, so work on the in-memory copy if it's
      # available.  (But do warn about it -- no raw file could mean out of
      # disk space.)
      if (!exists $r->{'compraw'}) {
	::Log(0, "FAILED (no raw file!)\n");
	return ([], []);
      } else {
        my $rawdata = ::decode_decompress($r->{'compraw'});
        if (!defined($rawdata)) {
          ::Log(0, "FAILED (no raw file!)\n");
          return ([], []);
        }
        @data = split(/\n/, $rawdata);
      }
    } else {
      my $ifh = new IO::File $rawfile;
      if (!defined($ifh)) {
          ::Log(0, "FAILED (could not read raw file)\n");
          return ([], []);
      }
      @data = <$ifh>;
    }

    # Tack on ephemeral information about bad flags used so that it can be
    # handled via the syntax file.
    foreach my $type (qw(forbidden unknown)) {
        my $val = (exists $r->{"${type}used"}) ? $r->{"${type}used"} : 0;
        push @data, "$prefix${type}used: $val";
    }

    # Read in the values from all the lines so that they can be referred to
    # later.
    my %values = ();
    foreach my $line (@data) {
        next unless $line =~ /^\Q$prefix\E(\S+): (.*)/;
        my ($key, $val) = ($1, $2);
        $key =~ s/0*([1-9]\d*)$/$1 + 0/ego;     # Collapse indices
        $values{$key} = $val;
    }

    # This is ripped nearly directly from submit_functions.pl
    if ($::lcsuite eq 'mpi2007') {
        # All of the different parts have different syntax lists...

        foreach my $item (qw(node interconnect)) {
            next unless exists($r->{$item}) && ::isa($r->{$item}, 'HASH');
            my $iref = $r->{$item};

            # Get a list of things; order them by primarily  by 'order'
            # and secondarily by 'label' (lexically)
            foreach my $system (sort {
                     $iref->{$a}->{'order'} <=> $iref->{$b}->{'order'} ||
                     $iref->{$a}->{'label'} cmp $iref->{$b}->{'label'}
                                     } keys %{$iref}) {
                my $name = "${item}_${system}_";
                # Put it on the list to be processed
                push @syntax_types, $name;
                # Choose the correct syntax table; somewhat deep copy because
                # it may be modified.
                $syntax_lists{$name} = { %{$syntax{$item}} };
                # Get only the lines that apply to this system
                $check_lines{$name} = [ grep { /^$prefix\Q$name\E/ } @data ];

                # Check for special variable-count items
                my @subs = grep { ::isa($syntax{$item}->{$_}, 'HASH') } keys %{$syntax{$item}};
                foreach my $sub_syntax (@subs) {
                    my $special_name = $name.$sub_syntax.'_';
                    delete $syntax_lists{$name}->{$sub_syntax};
                    # Figure out which ones are there
                    my %subitems = ();
                    foreach my $line (@{$check_lines{$name}}) {
                        next unless $line =~ /${special_name}([a-zA-Z0-9]+)_/;
                        $subitems{$1}++;
                    }
                    if (%subitems == 0) {
                        # No item was found; Fudge up a dummy so that the
                        # syntax checker will have something to complain about
                        $subitems{'MISSING'} = 1;
                    }
                    foreach my $subitem (keys %subitems) {
                        my $subname = "${special_name}${subitem}";
                        # Add a syntax entry for it
                        push @syntax_types, $subname;
                        $syntax_lists{$subname} = $syntax{$item}->{$sub_syntax};
                        $check_lines{$subname} = [ grep { /^$prefix\Q$subname\E/ } @{$check_lines{$name}} ];
                        $check_lines{$name} = [ grep { !/^$prefix\Q$subname\E/ } @{$check_lines{$name}} ];
                    }
                }

                # Remove them from the general pool
                @data = grep { !/^$prefix\Q$name\E/ } @data;
            }
        }
        # Ensure that the rest of the lines are processed
        push @syntax_types, '';
        $syntax_lists{''} = $syntax{'general'};
        $check_lines{''} = \@data;

    } else {
        # Normal, not too complicated...
        @syntax_types = ('');
        %syntax_lists = ( '' => \%syntax );
        %check_lines =  ( '' => \@data );
    }

    foreach my $syntax_type (@syntax_types) {
      if (!exists($syntax_lists{$syntax_type})) {
        ::Log(0, "\n\nNo syntax list for type '$syntax_type'!\nThis should never happen.\n");
        next;
      }
      my $syntax_ref = $syntax_lists{$syntax_type};
      if (!::isa($syntax_ref, 'HASH')) {
        ::Log(0, "\n\nEntry in syntax_lists for \"$syntax_type\" is not a hash!\nThis should never happen.\n");
        next;
      }
      my $data_ref = $check_lines{$syntax_type};
      if (!::isa($data_ref, 'ARRAY')) {
        ::Log(0, "\n\nEntry in check_lines for \"$syntax_type\" is not an array!\nThis should never happen.\n");
        next;
      }

      foreach my $key (sort keys %{$syntax_ref}) {
	my @lines = grep(/^\Q$prefix$syntax_type\E$key[\s=]/, @{$data_ref});

        my ( $re, $explanation, $one_match_sufficient, $complain_once, $logic, $conditions, $default );
        if (::isa($syntax_ref->{$key}, 'ARRAY')) {
          # Pre-compiled regexp and explanation are provided as an array
          ($re, $explanation, $one_match_sufficient, $complain_once, $logic, $conditions) = @{$syntax_ref->{$key}};
          $explanation = [ $explanation ] unless ::isa($explanation, 'ARRAY');
          $default = 0;
        } elsif ( ::isa( $syntax_ref->{$key}, 'HASH' ) ) {
          # Pre-compiled regexp and explanation are provided as a hash
          $re                   = $syntax_ref->{$key}->{'regexp'};
          $explanation          = $syntax_ref->{$key}->{'text'};
          $one_match_sufficient = $syntax_ref->{$key}->{'one_match'};
          $complain_once        = $syntax_ref->{$key}->{'complain_once'};
          $logic                = $syntax_ref->{$key}->{'custom_test'};
          $conditions           = $syntax_ref->{$key}->{'conditions'};
          $explanation = [ $explanation ] unless ::isa($explanation, 'ARRAY');
          $default = 0;
        } else {
          $re = qr/$syntax_ref->{$key}/;
          $explanation = [ 'Invalid syntax: $line',
                           '\tUnable to match regular expression: $re'
                         ];
          $one_match_sufficient = 0;
          $complain_once = 0;
          $logic = undef;
          $conditions = undef;
          $default = 1;
        }

        # If the field constraints for this item don't match, skip it.
        if (defined($conditions) && ::isa($conditions, 'ARRAY')) {
            my $all_ok = 1;
            my @conditions = @{$conditions};
            if (!::isa($conditions[0], 'ARRAY')) {
                # Must be an older-style single field constraint.  Wrap it
                # up so it can be unwrapped in just a second...
                @conditions = ([ @conditions ]);
            }
            foreach my $constraintref (@conditions) {
                my $constraint_ok = 0;
                my @constraints = @{$constraintref};
                my $field = shift (@constraints); # Not really NEEDED here...
                if (!exists($values{$field})) {
                    push @errors, " - Could not locate field '$field' in result\n";
                    last;
                }
                foreach my $constraint (@constraints) {
                    my ($min, $max, $regex) = (undef, undef, undef);
                    if (::isa($constraint, 'ARRAY')) {
                        ($min, $max) = @{$constraint};
                    } else {
                        $regex = qr/$constraint/;
                    }
                    if ((defined($regex) && $values{$field} =~ /$regex/)
                        ||
                        (defined($min) && $values{$field} >= $min && $values{$field} <= $max)) {
                        $constraint_ok = 1;
                        last;
                    }
                }
                $all_ok = 0 unless $constraint_ok;
                last unless $all_ok;
            }
            next unless $all_ok;
        }

	if (@lines+0 <= 0) {

            # No lines, so no matches are possible.
            # Strip the colon from the key and try to make it otherwise
            # presentable:
            $key =~ s/://;
            my $k = $key;
            $k =~ s/\\.*//;
            $k = "${syntax_type}$k" if $syntax_type ne '';
            my $line = '';
            my $value = '';
            my @tmpprobs = ( "- The \"$k\" field (any field matching \"$key\") is missing.", '' );
            if ( !$default ) {
                push @tmpprobs, map { eval "return \"$_\"" } @{$explanation};
            }
            push @tmpprobs, '';
            push @errors, @tmpprobs;

	} else {

            my $matched = 0;
            my $complained = 0;
            foreach my $line (@lines) {
                $line =~ tr/\012\015//d;  # Pesky line endings
                my ($k, @slirp) = ($line =~ /^\Q${prefix}\E(\Q${syntax_type}\E${key})\s*(?:[\s=](.*)|$)/i);
                $k = $key if $k eq ''; # Failsafe
                $k =~ s/:$//;
                my $value = pop @slirp;
                if ( $value !~ /$re/ &&
                    ( !$matched || ( $matched && !$one_match_sufficient ) ) &&
                    ( !$complained || ( $complained && !$complain_once ) ) ) {
                    my @tmpprobs = map { eval "return \"$_\"" } @{$explanation};
                    if ( !$default && $tmpprobs[0] !~ /^-/ ) {
                        unshift @tmpprobs, "- The \"$k\" field is invalid.";
                    }
                    push @tmpprobs, '';
                    $complained++ if @tmpprobs;
                    push @errors, @tmpprobs;
                } else {
                    $matched = 1;
                }
                # Let the custom code (if any) have a crack at it too.
                if (ref($logic) eq 'CODE') {
                    my @tmpprobs = &{$logic}($line, $k, $value, $re, $matched, $one_match_sufficient, $complained, $complain_once, \%values);
                    $complained++ if @tmpprobs;
                    push @errors, @tmpprobs;
                }
            }

        }
      }
    }

    if (@errors) {
	::Log(0, "FAILED.  Found the following errors:\n");
	::Log(0, "                 ".join("\n                 ", @errors)."\n\n");
    } else {
	::Log(0, "PASSED syntax check\n");
        if (!$::website_formatter) {
            ::Log(0, "\n             Note: although this result has PASSED subcheck\n");
            ::Log(0, "                   you should be aware that:\n\n");
            ::Log(0, "                 - SPEC may have updated requirements in the version of the\n");
            ::Log(0, "                   checker at the SPEC submission website.\n");
            ::Log(0, "                 - Much (though not all) of SPEC's version is downloadable,\n");
            ::Log(0, "                   and can be synchronized with your version; see:\n");
            ::Log(0, "                   http://www.spec.org/$::lcsuite/Docs/runspec.html#newflags\n\n");
        }
    }
    return([],[]);

}
