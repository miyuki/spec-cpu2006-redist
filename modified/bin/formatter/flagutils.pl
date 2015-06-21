#
# flagutils.pl
#
# Copyright 2005-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: flagutils.pl 6553 2011-06-16 20:35:19Z CloyceS $

use strict;
use Safe;
use XML::SAX;
use CPUFlagsParser;
use UNIVERSAL qw(isa);
use LWP::UserAgent;
#use LWP::Debug qw(+);

require 'util.pl';
require 'flagutils_common.pl';

use vars qw($ua);

my %seen_ids = ();

# Master list of all available classes; this must match the DTD and the list
# in CPUFlagsParser.pm for things to make sense.
my @classlist = qw(mandatory forbidden
                   portability optimization
                   compiler other
                   unknown);

my $version = '$LastChangedRevision: 6553 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'flagutils.pl'} = $version;

sub flags_list {
    # Build a data structure detailing what's known about all the various
    # flags used for a particular benchmark.  The results are stored into
    # the benchmark's configuration area.
    my ($config, $benchopt, $bench, $tune, @flagkeys) = @_;

    return undef unless isa($config->{'flaginfo'}, 'HASH');

    my $s = new Safe 'retmp';
    if ($^V ge v5.10.0) {
      $s->permit(qw(:load entereval sort print));
    } else {
      $s->deny_only('entereval'); # Deny eval-based regexp stuff
    }

    # Get the option lines into an array, being careful to preserve their
    # order.
    ${$s->varglob('opts')}      = [ split(/\n+/, $benchopt) ];
    ${$s->varglob('flagkeys')}  = [ @flagkeys ];
    ${$s->varglob('benchmarks')}= $config->benchmarks;
    ${$s->varglob('flaginfo')}  = $config->{'flaginfo'};
    ${$s->varglob('flaglist')}  = { 'passes' => [] };
    ${$s->varglob('bench')}     = $bench;
    ${$s->varglob('tune')}      = $tune;
    ${$s->varglob('classlist')} = [ @classlist ];
    $s->share_from('main', [ 'check_elem', 'isa', 'Log', 'istrue' ]);
    $s->share('do_replacements');
    $s->share('deep_copy');
    $s->share('add_implicit_ref');
    $s->share('find_benchmark');
    my ($opts, $flaglist) = $s->reval(q% 
#-----------------------------------------------------------------------------
    use bytes;    # No UTF-8 in regexps or flag strings; needed because
                  # otherwise we can't find utf8::SWASHNEW
    no re 'eval'; # The default, but just in case

    sub kill_flag {
      my ($flaglist, $inforef, $name) = @_;
      my $i = 0;
      for(; $i < @$inforef && $inforef->[$i]->{'name'} ne $name; $i++) {
        # Whee!
      }
      splice @$inforef, $i, 1;
      $flaglist->{'seen_errors'}++;
    }

    sub adjust_flag_pos {
      my ($flaglist, $line, $start, $len) = @_;

      foreach my $class (@{$classlist}) {
        next unless (exists($flaglist->{$class}) && isa($flaglist->{$class}, 'ARRAY'));
        foreach my $flag (@{$flaglist->{$class}}) {
          next unless $flag->[2]->{'line'} == $line;
          next unless $flag->[2]->{'pos'} > $start;
          $flag->[2]->{'pos'} += $len;
        }
      }
    }

    my $prev_match = {};
    $flaglist->{'seen_errors'} = 0;
    $flaglist->{'parallel'}    = 0;

    my $tmpcnt = 0;
    Log(98, "\nOption strings for $bench $tune:\n  ".join("\n  ", map { $tmpcnt++.": $_" } @{$opts})."\n" );

    my $matches = {};
    my @currcompiler = ();
    # Look for options
    foreach my $infokey (@{$flagkeys}) {
	next unless check_elem('ARRAY', $flaginfo, $infokey, 'flag');
	my $inforef = $flaginfo->{$infokey}->{'flag'};

        my $not_done_yet = 1;
        while ($not_done_yet) {
          # This just allows the matching code to ask for a re-run of the
          # current flag list, if some new flag text was inserted.
          $not_done_yet = 0;
          my $count = 0;

          # Search the compilers first (regardless of their interleaving order)
          # and other flags next.
          foreach my $currflag ((grep { $_->{'class'} eq 'compiler' } @{$inforef}),
                                (grep { $_->{'class'} ne 'compiler' } @{$inforef})) {
              my $flagdesc = $currflag->{'name'};
              $count++;
              my $re = $currflag->{'regexp'};
              my @compilers = split(/[\s+,]+/, $currflag->{'compilers'});

              if (!defined($re)) {
                  # This won't happen, since we check for it while
                  # parsing the flags files.
                  Log(0, "\nERROR: Flag \"$flagdesc\" has no regular expression!\n");
                  next;
              }

              my $mode = undef;
              my $currcompiler = undef;
              my @passes = @{$flaglist->{'passes'}};
              for(my $i = 0; $i <= $#{$opts}; $i++) {
                # Skip LINK and COMP lines
                if ($opts->[$i] =~ /^COMP\d*:/) {
                  $mode = 'comp';
                  $currcompiler = undef if $currcompiler eq 'specpp';
                  next;
                } elsif ($opts->[$i] =~ /^FPP\d*:/) {
                  $mode = 'comp';
                  $currcompiler = $currcompiler[$i] = 'specpp';
                  next;
                } elsif ($opts->[$i] =~ /^LINK\d*:/) {
                  $mode = 'link';
                  next;
                } elsif ($opts->[$i] =~ /^ONESTEP(\d*):/) {
                  my $pass = $1;
                  if (!exists($prev_match->{'O::ONESTEP:ONESTEP'})) {
                    my $flagref =
                      [ [ '' ],            # Variables it was found in
                        'ONESTEP',         # Flag text
                        {                  # Flag description entry
                          'name' => 'ONESTEP',
                          'regexp' => 'ONESTEP\b',# keep one_match happy
                          'nomap' => 1,           # Don't try to link to it
                          'display' => 1,         # Do display it
                          'found_in' => 'O',      # A lie of convenience  
                          'origin' => [ undef, 'suite' ], # It came from the suite
                          'pos' => -1,     # Where in the string it was found
                          'line' => $i,     # Where in the string it was found
                        },
                        [ 'comp', $pass ],
                        [ 'link', $pass ]
                      ];
                    # ONESTEP should always be the first flag
                    unshift @{$flaglist->{'optimization'}}, $flagref;
                    $prev_match->{'O::ONESTEP:ONESTEP'} = $flagref;
                  } else {
                    # When ONESTEP is first, it saves a lot of time looking
                    # for it. :)
                    push @{$flaglist->{'optimization'}->[0]}, [ 'comp', $pass ], [ 'link', $pass ];
                  }
                  $mode = 'both';
                  next;
                }
                
                # If the current compiler is set for this line, it should be
                # used for matching subsequent lines, until another compiler
                # is seen.
                if (defined($currcompiler[$i])) {
                  $currcompiler = $currcompiler[$i];
                }

                # Skip empty lines.  This is done here so that even when
                # compiler lines are gone, their settings can still be used.
                next unless defined($opts->[$i]);

                # If the current flag specifies a compiler list, make sure
                # that it ONLY matches flags when that compiler has been
                # seen.
                next if (
                         @compilers
                         && (   (
                                 defined($currcompiler)
                                 && $currcompiler ne ''
                                 && !grep { $_ eq $currcompiler } @compilers
                                 )
                             || (!defined($currcompiler)
                                 || $currcompiler eq ''
                                )
                             )
                        );

                # Go ahead and decompose the option line into the type,
                # the variable name, and the contents.
                my ($type, $pass, $var, $thing) = ($opts->[$i] =~ /^([A-Z]+)(\d*): ([^=]+)=\"(.*)\"$/);
                push @passes, $pass if defined($pass) && $pass ne '' && !grep { $pass == $_ } @passes;
                my @usedin = ( [ $mode, $pass ] );
                if ($mode eq 'both') {  
                  # Cursed ONESTEP!
                  @usedin = ( [ 'comp', $pass ], [ 'link', $pass ]);
                }

                next unless (defined($thing) && $thing ne '');
                if (!defined($type) ||
                    $type !~ /^[CPO]$/) {
                    Log(0, "\nERROR: Unknown variable type \"$type\" found.  Skipping option line.\n");
                    next;
                }
                if (!defined($var) || $var eq '') {
                    Log(0, "\nERROR: Variable name is empty.  Skipping option line.\n");
                    next;
                }

                # Find a marker to use.
                my $marker = '';
                Log(99, "Looking for marker to use in \"$thing\"; starting at YxY${marker}XyX\n");
                while ($thing =~ /YxY${marker}XyX/ && $marker - $$ < 1000) {
                  $marker++;
                }
                if ($marker >= 1000) {
                  Log(0, "ERROR: Could not find unique marker to use in \"$thing\"\n");
                  last;     # $flag is already undef
                }
                $marker = "YxY${marker}XyX";
                Log(99, "Chose marker \"$marker\"\n");

                my ($flag, @replacements) = (1);
                while (defined($re) && defined($flag)) {
                    # This is the same thing as below in one_match.  If I
                    # could get it inlined, I would, because when I break
                    # it out into a sub there's a noticeable performance
                    # degradation (here, not there).
                    $flag = undef;
                    @replacements = ();
                    my $pos = undef;
                    my $len = 0;

                    if ($thing =~ s/($re)/$marker/) {
                        no strict 'refs';
                        $flag = $1;
                        $pos = $-[0];
                        $len = length($flag);

                        # Loop to add all captured groups
                        # to the list of values to return
                        for(my $j = 2; $j <= $#+; $j++) {
                            push @replacements, ${$j};
                        }
                    }
                    last unless defined($flag); # No match => no marker
                    Log(97, "Flag named \"$flagdesc\" matched: \"$flag\" in $type:$var (line $i; $mode pass $pass)");
                    Log(97, " (replacements: '".join("', '", @replacements)."')") if (@replacements);
                    Log(97, "\n");

                    # NOT GOOD if the matched bit is empty
                    if ($flag eq '') {
                      Log(0, "\n\nERROR: The regexp for \"$flagdesc\" matched, but matched no text.  This\n");
                      Log(0, "      can cause infinite loops, so \"$flagdesc\" will be removed.\n");
                      Log(0, "      Reformatting the results with a fixed flag description file should\n");
                      Log(0, "      result in readable results.\n\n");
                      # It's important to delete this flag if the set will
                      # be re-run.  So...
                      kill_flag($flaglist, $inforef, $flagdesc);
                      undef $re;
                      # Remove the marker
                      $thing =~ s/$marker//;
                      last;
                    }

                    if ($currflag->{'class'} eq 'compiler') {
                        $currcompiler[$i] = $currflag->{'name'};
                    }

                    # Deal with the auto-parallel
                    $flaglist->{'parallel'}++ if istrue($currflag->{'parallel'});

                    # Handle included text, if any.
                    # This is done even if the flag has matched before so that
                    # split-out flags will still have their used-in information
                    # saved properly.
                    my $inserted_len = 0;
                    if (exists($currflag->{'inc_text'})
                        && isa($currflag->{'inc_text'}, 'ARRAY')) {
                        Log(95, "Flag text substitution: pre-sub: \"$thing\"\n");
                        my $subcount = 1;
                        foreach my $repl (@{$currflag->{'inc_text'}}) {
                          my $newthing = do_replacements($repl, @replacements);
                          if ($newthing eq $flag) {
                              # It is a BAD THING for a flag to put back in
                              # the exact text it matches -- infinite loop!
                              Log(0, "\n\nERROR: The replacement text for \"$flagdesc\" ($flag)\n");
                              Log(0, "      is exactly the same as the text that was matched.  This causes an\n");
                              Log(0, "      infinite loop, so \"$flagdesc\" will be removed.\n");
                              Log(0, "      Reformatting the results with a fixed flag description file should\n");
                              Log(0, "      result in readable results.\n\n");
                              # It's important to delete this flag if the set
                              # will be re-run.  So...
                              kill_flag($flaglist, $inforef, $flagdesc);
                              undef $re;
                              last;
                          } else {
                              $thing =~ s/$marker/ $newthing$marker/;
                              Log(95, "Flag text substitution: repl $subcount (\"$repl\"): \"$thing\"\n");
                              $inserted_len += length($newthing) + 1;
                              $subcount++;
                          }
                        }
                        Log(95, "Flag text substitution: post-sub: \"$thing\"\n");
                        $not_done_yet++;
                    }

                    if ($inserted_len > $len) {
                      # We inserted more than we removed, so just remove the
                      # marker and adjust the stored position information
                      # for other flags from the same line
                      $thing =~ s/$marker//;
                      Log(99, "Adjusting flag pos information for flags past $pos on line $i ($var)\n");
                      adjust_flag_pos($flaglist, $i, $pos, $inserted_len - $len);
                    } elsif ($inserted_len < $len) {
                      # Insert some padding so that the string length doesn't
                      # change.
                      my $pad = ' ' x ($len - $inserted_len);
                      $thing =~ s/$marker/$pad/;
                    } else {
                      # Just remove the marker; no adjustments necessary
                      $thing =~ s/$marker//;
                    }

                    # Keep track of how many times this regexp matches
                    $matches->{$flagdesc}++;
                    my $max_flag_matches = 200;
                    if ($matches->{$flagdesc} >= $max_flag_matches) {
                        # There shouldn't be 100 flags for a particular
                        # benchmark, let alone $max_flag_matches matches, so...
                        Log(0, "\n\nERROR: Flag \"$flagdesc\" has matched more than $max_flag_matches times; it will be\n");
                        Log(0, "      removed, and the currently generated results will be bogus.\n");
                        Log(0, "      Reformatting the results with a fixed flag description file should\n");
                        Log(0, "      result in readable results.\n\n");
                        # It's important to delete this flag if the set will
                        # be re-run.  So...
                        kill_flag($flaglist, $inforef, $flagdesc);
                        undef $re;
                        last;
                    }

                    # Don't store the matched flag if this rule has
                    # matched the same thing on a previous instance of
                    # this line.
                    # This can often happen when using non-language-
                    # specific flags like OPTIMIZE, which will show up
                    # in both compilation and link sections.
                    if (exists($prev_match->{"$type:$var:$flag:".$currflag->{'name'}})
                        && $prev_match->{"$type:$var:$flag:".$currflag->{'name'}}) {
                        Log(97, "  *** Avoiding duplication of flag match ($mode pass $pass)\n");
                        push @{$prev_match->{"$type:$var:$flag:".$currflag->{'name'}}}, @usedin;
                        next;
                    }


                    # Make a copy of the flag description, and fix up the
                    # description.
                    my $flagdesc_copy = deep_copy($currflag);
                    $flagdesc_copy->{'description'} = do_replacements($flagdesc_copy->{'description'}, @replacements);
                    $flagdesc_copy->{'found_in'} = $type;
                    $flagdesc_copy->{'compiler_used'} = $currcompiler;
                    $flagdesc_copy->{'pos'} = $pos;
                    $flagdesc_copy->{'line'} = $i;
                    # Handle replacements in included flags
                    if (exists($flagdesc_copy->{'inc_flag'})
                        && isa($flagdesc_copy->{'inc_flag'}, 'ARRAY')) {
                      for(my $i = 0; $i < @{$flagdesc_copy->{'inc_flag'}}; $i++) {
                          Log(95, "Included flag text substitution: pre-sub: \"$flagdesc_copy->{'inc_flag'}->[$i]->[1]\"\n");
                          $flagdesc_copy->{'inc_flag'}->[$i]->[1] = do_replacements($flagdesc_copy->{'inc_flag'}->[$i]->[1], @replacements);
                          Log(95, "Included flag text substitution: post-sub: \"$flagdesc_copy->{'inc_flag'}->[$i]->[1]\"\n");
                      }
                    }

                    # List it
                    my $flagref = [ [ $var ], $flag, $flagdesc_copy, @usedin ];
                    push @{$flaglist->{$currflag->{'class'}}}, $flagref;
                    $flaglist->{'seenflags'}->{$currflag}->{'flag'} = $flagref;
                    $prev_match->{"$type:$var:$flag:".$currflag->{'name'}} = $flagref;

                    if ($var eq 'LD' &&
                        isa($flaglist->{'optimization'}->[0], 'ARRAY') &&
                        $flaglist->{'optimization'}->[0]->[1] eq 'ONESTEP') {
                      # Cursed ONESTEP!  Make another copy with LD replaced
                      # by the proper language compiler.
                      # This will allow the compiler invocation to be merged
                      # if both base and peak are run, and only one uses
                      # ONESTEP.
                      my $fakedflag = deep_copy($flagref);
                      # Figure out what compiler variable WOULD have been
                      # used were ONESTEP not involved.
                      if (exists($benchmarks->{$bench})) {
                        my ($benchlang) = @{$benchmarks->{$bench}->{'allBENCHLANG'}};
                        if ($::lcsuite =~ /cpu(?:2006|v6)/ && $benchlang eq 'F77') {
                          # No F77 in SPEC CPU
                          $benchlang = 'F';
                        }
                        if ($benchlang && $benchlang ne '?') {
                          $var = $benchlang.'C';
                          $fakedflag->[0] = [ $var ];
                          push @{$flaglist->{$currflag->{'class'}}}, $fakedflag;
                          $prev_match->{"$type:$var:$flag:".$currflag->{'name'}} = $fakedflag;
                        }
                      }
                    }

                    if ($inforef eq 'user') {
                        # For user flags, store (for later extraction and
                        # display) the included flags (all of them)
                        foreach my $refflag (find_flags_refs($inforef, $currflag)) {
                            $flaglist->{'seenflags'}->{$refflag}->{'flag'} = $refflag;
                            add_implicit_ref($flaglist, $refflag, $currflag, $bench, $tune);
                        }
                    }
                }

                if ($thing !~ /^\s*$/) {
                  # Put the option line back together for the benefit of
                  # subsequent checks...
                  $opts->[$i] = "$type$pass: $var=\"$thing\"";
                } else {
                  # Mark it so that it can be skipped quickly
                  $opts->[$i] = undef;
                }
              }
              $flaglist->{'passes'} = [ sort { $a <=> $b } @passes ];
          }
          Log(96, "Saw $count flags for $bench from $infokey flags\n");
        }
    }

    return ($opts, $flaglist);
#-----------------------------------------------------------------------------
%);
    if ($@) {
      Log(0, "ERROR: While parsing flags, the compartment returned:\n        $@\n\n");
      return undef;
    }

    # Fix up the positions listed for each variable, in case one of the
    # formatters wants to use them.  In case they don't, each set of flags
    # will be sorted into this order anyway.
    foreach my $class (@classlist) {
      next unless isa($flaglist->{$class}, 'ARRAY');
      my $i = 0;
      foreach my $flag (sort by_variable @{$flaglist->{$class}}) {
        $flag->[2]->{'pos'} = $i;
        $i++;
      }
      @{$flaglist->{$class}} = sort { $a->[2]->{'pos'} <=> $b->[2]->{'pos'} } @{$flaglist->{$class}};
    }

    # Go through the lines again and put any remainders into "unknown"
    # Only do one instance of each variable per type, since the "flag"
    # is everything that's left.
    foreach my $type (qw(C P O)) {
	my %seen = ();
        my $mode = undef;
	foreach my $unk (grep { /^(?:LINK\d*:|COMP\d*:|ONESTEP\d*:|$type\d*: \S+=\"\s*[^\"])/ } @{$opts}) {
            # Skip LINK and COMP lines
            if ($unk =~ /^(?:COMP|FPP)\d*:/) {
              $mode = 'comp';
              next;
            } elsif ($unk =~ /^LINK\d*:/) {
              $mode = 'link';
              next;
            } elsif ($unk =~ /^ONESTEP\d*:/) {
              $mode = 'both';
              next;
            }
	    if ($unk =~ /^$type(\d*): (\S+)=\"\s*(.*?)\s*\"$/) {
                my ($pass, $var, $val) = ($1, $2, $3);
		next if $val eq ''; # No flags left
		if ($seen{$var}) { # Already processed this one... maybe
                  # Find ones that match var and val and append mode and pass
                  # info to them
                  my $gotone = 0;
                  foreach my $seenref (grep { match_var($_, $var) && $_->[1] eq $val } @{$flaglist->{$type}->{'unknown'}}) {
                    push @$seenref, [ $mode, $pass ];
                    $gotone++;
                  }
                  next if $gotone;      # Fall through if not
                }
                my $flagref = [ [ $var ], $val, { 'display' => 1 } ];
                if ($mode eq 'both') {
                  # Cursed ONESTEP!
                  push @$flagref, [ 'comp', $pass ], [ 'link', $pass ];
                } else {
                  push @$flagref, [ $mode, $pass ];
                }
		$seen{$var} = $flagref;
		push @{$flaglist->{'unknown'}}, $flagref
	    }
	}
    }

    # Now go through and add one dummy "No flags used" to any sections that
    # don't have any flags.
    foreach my $class (@classlist) {
      next if $class =~ /^(?:unknown|forbidden)$/o;
      next if $class eq 'portability' && !::istrue($config->{'review'});

      # This is inside the loop so that it's a different instance for each
      # class.
      my $noflag = [ [ '' ],            # Variables it was found in
                     'No flags used',   # Flag text
                      {                 # Flag description entry
                       'name' => '__noflags_YYY',
                       'regexp' => 'No flags used\b', # For one_match
                       'nomap' => 1,                  # Don't try to link to it
                       'display' => 1,                # Do display it
                       'origin' => [ undef, 'suite' ] # It came from the suite
                     }
                   ];

      if (isa($flaglist->{$class}, 'ARRAY')) {
        # Sections (like optimization) that are an EMPTY array still get the
        # no flag marking.  It's done this way to not change the ref (in case
        # anyone else is looking).
        @{$flaglist->{$class}} = ( $noflag ) unless @{$flaglist->{$class}};
      } else {
        $flaglist->{$class} = [ $noflag ];
      }
    }

#print Data::Dumper->Dump([$flaglist],[qw(flaglist)])."\n"; exit;
    return $flaglist;
}

sub reduce_flags {
    # Given a set of flags (as assembled by flags_list), aggregate the common
    # flags by language for all tuning levels as well as each level
    # individually.
    # Returns a hash struct keyed by flag class, much like the input.
    my ($r) = @_;
    my %seen_bench = ();
    my %seen_tune = ();
    my %class2type = ( 'portability' => 'P', 'optimization' => 'O',
                       'compiler' => 'C' );
    my $rc = { 'var2desc' => {
                               'CC'        => 'C benchmarks',
		               'CC CXX'    => 'Benchmarks using both C and C++',
		               'CC CXXC'    => 'Benchmarks using both C and C++',
                               'CXX'       => 'C++ benchmarks',
                               'CXXC'      => 'C++ benchmarks',
		               'CXX CC'    => 'Benchmarks using both C and C++',
		               'CXXC CC'    => 'Benchmarks using both C and C++',
                               'F77'       => 'FORTRAN77 benchmarks',
                               'F77C'      => 'FORTRAN77 benchmarks',
                               'FC'        => 'Fortran benchmarks',
		               'CC FC'     => 'Benchmarks using both Fortran and C',
		               'CC F77'    => 'Benchmarks using both Fortran and C',
		               'CC F77C'   => 'Benchmarks using both Fortran and C',
		               'CXX FC'    => 'Benchmarks using both Fortran and C++',
		               'CXXC FC'   => 'Benchmarks using both Fortran and C++',
		               'CXX F77'   => 'Benchmarks using both Fortran and C++',
		               'CXXC F77C' => 'Benchmarks using both Fortran and C++',
			   }
             };

    # Get a list of benchmark counts by language.  This can't be done using the
    # flags list, as there may be some benchmarks (like those neither built
    # nor run) that don't have any entries in that structure at all.
    foreach my $bench (keys %{$r->{'benchmarks'}}) {
        # The hash gymnastics are to ensure that only one copy of each
        # langauge is entered into the key.
        my %langs = map { $_ => 1 } @{$r->{'benchmarks'}->{$bench}->{'allBENCHLANG'}};
        if ($::lcsuite =~ /cpu(?:2006|v6)/ && exists($langs{'F77'})) {
            # There's no F77 in SPEC CPU; only F
            $langs{'F'}++;
            delete $langs{'F77'};
        }
        my $langs = join(' ', map { ($_ eq 'F77' || $_ eq 'CXX') ? $_ : "${_}C" } sort keys %langs);
        $rc->{'benchcounts'}->{$langs}++;
    }

    # First, go through all flags and build a per-benchmark per-tune list
    # of flags used.  Each entry will have the flags concatenated as one
    # string (for comparison), the languages used, a list of the individual
    # flag text, and a list of references to the flag structures.
    foreach my $bench (keys %{$r->{'results'}}) {
	next unless defined($r->{'results'}->{$bench}) && isa($r->{'results'}->{$bench}, 'HASH');
	my $benchref = $r->{'results'}->{$bench};
	foreach my $tune (keys %{$benchref}) {
	    next unless defined($benchref->{$tune}) && isa($benchref->{$tune}, 'HASH');
	    my $tuneref = $benchref->{$tune};
	    next unless exists($tuneref->{'flags'}) && isa($tuneref->{'flags'}, 'HASH');
            my @passes = ::get_pass_list($r, $bench, $tune);
	    $tuneref = $tuneref->{'flags'};
	    foreach my $class (@classlist) {
		next unless (::check_elem('ARRAY', $tuneref, $class) &&
                             @{$tuneref->{$class}});
		my $type = $class2type{$class};
		my $classref = $tuneref->{$class};
		$rc->{'benchlist'}->{$bench}++;
		$rc->{'maxbench'} = length($bench) if length($bench) > $rc->{'maxbench'};
		$rc->{'tunelist'}->{$tune}++;
		$rc->{'maxtune'} = length($tune) if length($tune) > $rc->{'maxtune'};
		$rc->{'classlist'}->{$class}++;
		foreach my $flag (@{$classref}) {
                    my @names = @{$flag->[0]};
                    if (grep { /^(CC|CXXC?|F77C?|FC)$/ } @names) {
                        # Deal with old (pre-kit 93 CPU2006) results.
                        # All newer results will have just
                        # "CXX" instead of "CXXC".
                        map { s/(CXX|F77)C/$1/g } @names;
                        push @{$rc->{'compilers'}->{$bench}->{$tune}}, @names;
                        map { $rc->{'compilers'}->{'all'}->{$_}++ } @names;
                    }
		    my ($mismatch, $tmpflag, $markup);
		    if ($class eq 'unknown' || $class eq 'forbidden') {
			$tmpflag = "\"$flag->[1]\" (in ".join(',', @{$flag->[0]}).")";
			$mismatch = 0;
                        $markup = '';
		    } else {
			($mismatch, $tmpflag, $markup) = ::markup_flag($flag, 'passes' => \@passes, 'seen_errors' => $tuneref->{'seen_errors'}, 'classes' => [ $type ]);
		    }
		    push @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}}, $tmpflag;
		    push @{$rc->{'markup'}->{$class}->{$bench}->{$tune}}, $markup;
		    push @{$rc->{'flaglist'}->{$class}->{$bench}->{$tune}}, $flag;
		    push @{$rc->{'mismatches'}->{$class}->{$bench}->{$tune}}, $mismatch;
		    $rc->{'mismatch'}->{$class}->{$bench}->{$tune} += $mismatch;
		}
                $rc->{'string'}->{$class}->{$bench}->{$tune} = join(' ', @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}});
	    }
	    if (isa($rc->{'compilers'}->{$bench}->{$tune}, 'ARRAY')) {
                # Make sure that the list of compilers is unique.  Sure, you'd
                # never have two compilers listed on the same line on PURPOSE,
                # but this is probably a point of confusion that n00bs will
                # hit often.  (Hopefully not more than once per n00b.)
                my %compilers = map { $_ => 1 } @{$rc->{'compilers'}->{$bench}->{$tune}};
                @{$rc->{'compilers'}->{$bench}->{$tune}} = sort keys %compilers;
		my $langs = join(' ', @{$rc->{'compilers'}->{$bench}->{$tune}});
		$rc->{'langs'}->{$bench}->{$tune} = $langs;
		$rc->{'langlist'}->{$langs}++;
		if (!exists($rc->{'var2desc'}->{$langs})) {
		    Log(0, "ERROR: No var2desc member for \"$langs\"; please update formatter/flagutils.pl\n");
		} else {
		    my $langlen = length($rc->{'var2desc'}->{$langs});
		    $rc->{'maxlang'} = $langlen if $langlen > $rc->{'maxlang'};
		}
	    }
	}
    }

    # Next, if basepeak is used, fake up (or replace) peak flag records.
    # Do it on a per-benchmark basis and let the flag coalescing do its thing.
    if ($r->{'basepeak'} && grep { $_ eq 'peak' } @{$r->{'tunelist'}}) {
      my @bp_bench = ();
      if ($r->{'basepeak'} == 1) {
        # Whole-suite basepeak
        @bp_bench = keys %{$r->{'results'}};
      } elsif ($r->{'basepeak'} == 2) {
        for my $bench (keys %{$r->{'results'}}) {
          next unless ::check_elem('ARRAY', $r, 'results', $bench, 'peak', 'data');
          next unless isa($r->{'results'}{$bench}{'peak'}{'data'}->[0], 'HASH');
          next unless $r->{'results'}{$bench}{'peak'}{'data'}->[0]->{'basepeak'};
          push @bp_bench, $bench;
        }
      }
      my $onetune = 'base';     # (keys %{$rc->{'tunelist'}})[0];
      $rc->{'tunelist'}->{'peak'}++;
      $rc->{'classlist'}->{'optimization'}++;
      foreach my $bench (@bp_bench) {
        # Fix up the optimization flag section for basepeak benchmarks.
        # In v1.0 all other classes for peak were also eliminated, but there
        # were objections that doing so made it look like no portability flags
        # were used for a particular benchmark in peak.
        # "Unknown" and "forbidden" flags are still removed, because those
        # really _do_ need to be fixed in only one place for the listed
        # benchmarks.
        foreach my $tmpclass (qw(unknown forbidden)) {
          foreach my $thing (qw(string stringlist markup flaglist
                                mismatches mismatch)) {
            next unless ::check_elem('HASH', $rc, $thing, $tmpclass, $bench);
            delete $rc->{$thing}->{$tmpclass}->{$bench}->{'peak'};
          }
        }
        $rc->{'langs'}->{$bench}->{'peak'} = $rc->{'langs'}->{$bench}->{$onetune};
        $rc->{'stringlist'}->{'optimization'}->{$bench}->{'peak'} = [ 'basepeak = yes' ];
        $rc->{'markup'}->{'optimization'}->{$bench}->{'peak'} = [ undef ];
        # Fake up a flag description:
        $rc->{'flaglist'}->{'optimization'}->{$bench}->{'peak'} = [
          [ [ '' ],            # Variables it was found in
            'basepeak = yes',  # Flag text
            {                  # Flag description entry
              'name' => 'basepeak',
              'regexp' => 'basepeak = yes\b',# Necessary, or one_match complains
              'nomap' => 1,                  # Don't try to link to it
              'display' => 1,                # Do display it
              'origin' => [ undef, 'suite' ] # It came from the suite
            }
          ]
        ];
        $rc->{'string'}->{'optimization'}->{$bench}->{'peak'} = 'basepeak = yes';
        $rc->{'mismatches'}->{'optimization'}->{$bench}->{'peak'} = [ 0 ];
        $rc->{'mismatch'}->{'optimization'}->{$bench}->{'peak'} = 0;
      }
    }

    # Third, go through and check for instances of the same flag being used
    # in more than one variable.  For those cases, eliminate the duplicates,
    # keeping one structure that has all of the "used in" variables listed,
    # as well as a unified list of phases in which it was used.
    foreach my $bench (keys %{$rc->{'benchlist'}}) {
      foreach my $tune (keys %{$rc->{'tunelist'}}) {
        # It's okay to do this one class at a time, since a given flag will
        # always only be in one class.
        foreach my $class (keys %{$rc->{'classlist'}}) {
          next unless check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
          my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
          my @kill = ();        # List of indices to remove

          # Go through the flag list and get counts of each flag used.
          my %counts = ();
          for(my $i = 0; $i < @{$flaglistref}; $i++) {
            my $flag = $flaglistref->[$i];
            next unless isa($flag, 'ARRAY');
            if ($class =~ /(?:forbidden|unknown)/) {
              # These are treated specially because they're not real flags
              my $key = $flag->[1].' (in '.join(',', @{$flag->[0]}).')';
              $counts{$key}->[0]++;
              $counts{$key}->[1] = $key;        # Why not?
              push @{$counts{$key}->[2]}, $i;
            } else {
              next unless isa($flag->[2], 'HASH');
              next if $flag->[2]->{'name'} =~ /^__.+?YYY$/;
              # Because all of the flag name, matched text, description, and
              # mismatch must match, make that the key.
              my $key = $flag->[2]->{'name'}.$flag->[1].$rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i].$flag->[2]->{'description'};
              $counts{$key}->[0]++;
              $counts{$key}->[1] = $flag->[2]->{'name'}; # Redundant, yes.
              push @{$counts{$key}->[2]}, $i;
            }
          }

          # Now iterate over the flags that have more than one instance.
          foreach my $key (sort keys %counts) {
            my ($count, $flagname, $indices) = @{$counts{$key}};
            if ($count <= 1) {
              delete $counts{$key};
              next;
            }
            # Here's a flag that matched multiple times; mash it all together
            my $dstidx = $counts{$key}->[2]->[0];
            my %langs = map { $_ => 1 } @{$flaglistref->[$dstidx]->[0]};
            for(my $i = 1; $i < @{$counts{$key}->[2]}; $i++) {
              my $srcidx = $counts{$key}->[2]->[$i];
              foreach my $var (@{$flaglistref->[$srcidx]->[0]}) {
                $langs{$var}++;
              }
              merge_passlist($flaglistref, $dstidx, $srcidx);
              push @kill, $srcidx;
            }
            @{$flaglistref->[$dstidx]->[0]} = sort keys %langs;
          }

          # Now for the indices listed in @kill, kill!
          # Strings and markup will be handled in a bit.
          my $killed = 0;
          foreach my $killidx (sort { $b <=> $a } @kill) {
            foreach my $thing (qw(flaglist mismatches)) {
              splice @{$rc->{$thing}->{$class}->{$bench}->{$tune}}, $killidx, 1;
            }
            $killed++;
          }

          if ($killed) {
            my $type = $class2type{$class};
            my @passes = ::get_pass_list($r, $bench, $tune);
            # Regenerate the other stuff
            foreach my $thing (qw(stringlist markup)) {
              @{$rc->{$thing}->{$class}->{$bench}->{$tune}} = ();
            }
            for(my $i = 0; $i < @{$flaglistref}; $i++) {
              my ($mismatch, $tmpflag, $markup);
              if ($class =~ /(?:unknown|forbidden)/) {
                # Once again, not real flags
                $tmpflag = '"'.$flaglistref->[$i]->[1].'" (in '.join(', ', @{$flaglistref->[$i]->[0]}).')';
                $markup = '';
              } else {
                ($mismatch, $tmpflag, $markup) = ::markup_flag($flaglistref->[$i], 'passes' => \@passes, 'classes' => [ $type ]);
              }
              push @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}}, $tmpflag;
              push @{$rc->{'markup'}->{$class}->{$bench}->{$tune}}, $markup;
            }
            $rc->{'string'}->{$class}->{$bench}->{$tune} = join(' ', @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}});
          }
        }
      }
    }

    # Handy shorthand; no need to sprinkle keys %{...} around everywhere
    $rc->{'tunecount'}  = (keys %{$rc->{'tunelist'}}) +0;
    $rc->{'benchcount'} = (keys %{$rc->{'benchlist'}})+0;
    $rc->{'classcount'} = (keys %{$rc->{'classlist'}})+0;
    $rc->{'langcount'}  = (keys %{$rc->{'langlist'}}) +0;

    # For each class of flags, make a count of each set seen.
    my %counts = ();
    my %bench2key = ();
    foreach my $class (keys %{$rc->{'classlist'}}) {
	foreach my $bench (keys %{$rc->{'benchlist'}}) {
	    foreach my $tune (keys %{$rc->{'tunelist'}}) {
		next unless check_elem(undef, $rc, 'langs', $bench, $tune);
		next unless check_elem(undef, $rc, 'string', $class, $bench, $tune);
		next unless check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
		my $langs = $rc->{'langs'}->{$bench}->{$tune};
                # It's not sufficient to use just the string, as it's not
                # guaranteed to be unique.
		my $optstring = $rc->{'string'}->{$class}->{$bench}->{$tune};
                my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
                my $key = '';
                # All this stuff is to make sure that different flags with the
                # same text aren't counted together.
                for(my $i = 0; $i <= $#{$flaglistref}; $i++) {
                  $key .= $flaglistref->[$i]->[2]->{'name'};
                  $key .= $flaglistref->[$i]->[1];
                  $key .= $rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i];
                  $key .= $flaglistref->[$i]->[2]->{'description'};
                  $key .= ' ';
                }
                $bench2key{$class.$bench.$tune} = $key;
		$counts{$class}->{$tune}->{$langs}->{$key}++;
	    }
	}
    }

    # Now for each class + language combo, find the string with the highest
    # count.  If it is higher than 1, remove all the flags that use it.
    # For optimization flags, matching flags will not be completely removed;
    # rather they'll be replaced by 'Same as nnn.YYYbench' messages.  For
    # optimization, this will be done for _all_ common sets, not just the
    # most popular.
    # This is per-language flag aggregation.
    foreach my $class (keys %counts) {
	if ($class eq 'unknown' ||
	    $class eq 'forbidden' ||
	    $class eq 'portability') {
	    # Repetition is a form of emphasis; do NOT collapse unknown
	    # or forbidden flags.  Portability flags are unlikely to
            # collapse anyway.
	    next;
	}
	foreach my $tune (keys %{$counts{$class}}) {
	    foreach my $langs (keys %{$counts{$class}->{$tune}}) {
		next unless defined($langs) && $langs ne '';
		my @popular = sort { $counts{$class}->{$tune}->{$langs}->{$b} <=> $counts{$class}->{$tune}->{$langs}->{$a} } keys %{$counts{$class}->{$tune}->{$langs}};
                my $popular_count = $counts{$class}->{$tune}->{$langs}->{$popular[0]};

		if (@popular == 1 &&
                    $popular_count == $rc->{'benchcounts'}->{$langs}) {
		    # There is one common set of flags used, and it is used
                    # for ALL benchmarks of the current language.  That means
		    # that for this tuning level in this class, everything is
		    # the same.
		    $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 1;
		} else {
		    if ($popular_count > 1 &&
                        $popular_count < $rc->{'benchcounts'}->{$langs}) {
                      # There are at least _some_ common flags, but not all
                      # of the benchmarks use them.
                      $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 2;
                    } else {
                      $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 0;
                    }
		}
                # For non-optimization classes, the idea is to remove entries
                # that match the most popular one ONLY if the "most popular"
                # one means more than one benchmark (langmatch==2) OR
                # 'langmatch' is set.
                # For optimization flags, entries will not be removed.  Rather,
                # common sets will have one fully represented and the others
                # will be "Same as nnn.YYYbench".
                foreach my $popular (@popular) {
                    # No sense dealing with a clique of 1
                    next if $counts{$class}->{$tune}->{$langs}->{$popular} < 2;
                    # Don't coalesce 'basepeak = yes' lines
                    next if ($popular =~ /basepeakbasepeak = yes0/);
                    my $saved = undef;
                    if ($popular ne '' &&
                        $rc->{'langmatch'}->{$class}->{$tune}->{$langs}) {
                        # Go through and remove entries in $rc which match the
                        # most popular string.
                        foreach my $bench (sort keys %{$rc->{'benchlist'}}) {
                            next unless check_elem(undef, $rc, 'string', $class, $bench, $tune);
                            # In order to be eliminated, the benchmark's
                            # 1. language must match
                            # 2. flag text must match
                            # 3. flag descriptions must match
                            # It's easiest to compare the "key" generated earlier
                            # with the contents of $popular in order to match
                            # #2 and #3.
                            if ($rc->{'langs'}->{$bench}->{$tune} eq $langs &&
                                $bench2key{$class.$bench.$tune} eq $popular) {
                                if ($class eq 'optimization' && !defined($saved) &&
                                    $rc->{'langmatch'}->{$class}->{$tune}->{$langs} != 1) {
                                    $saved = $bench;
                                    next;
                                }

                                # It's a match!
                                if (defined($saved) && $saved ne '') {
                                    # Point to $saved
                                    $rc->{'stringlist'}->{$class}->{$bench}->{$tune} = [ 'Same as '.$saved ];
                                    $rc->{'string'}->{$class}->{$bench}->{$tune} = 'Same as '.$saved;
                                    $rc->{'markup'}->{$class}->{$bench}->{$tune} = [ undef ];
                                    $rc->{'mismatches'}->{$class}->{$bench}->{$tune} = [ 0 ];
                                    $rc->{'mismatch'}->{$class}->{$bench}->{$tune} = 0;
                                    # Fake up a flag description:
                                    $rc->{'flaglist'}->{$class}->{$bench}->{$tune} = [
                                      [ [ '' ],            # Variables it was found in
                                        'Same as '.$saved, # Flag text
                                        {                  # Flag description entry
                                          'name' => 'identical',
                                          'regexp' => 'Same as '.$saved.'\b',# Necessary, or one_match complains
                                          'nomap' => 1,                  # Don't try to link to it
                                          'display' => 1,                # Do display it
                                          'origin' => [ undef, 'suite' ] # It came from the suite
                                        }
                                      ]
                                    ];
                                } else {
                                    # Remove it.
                                    foreach my $thing (qw(markup stringlist string flaglist mismatch mismatches)) {
                                        next unless check_elem(undef, $rc, $thing, $class, $bench, $tune);
                                        $rc->{'bylang'}->{$thing}->{$class}->{$tune}->{$langs} = $rc->{$thing}->{$class}->{$bench}->{$tune};
                                        delete $rc->{$thing}->{$class}->{$bench}->{$tune};
                                        if (keys %{$rc->{$thing}->{$class}->{$bench}} == 0) {
                                            # No more elements, so remove it too.
                                            delete $rc->{$thing}->{$class}->{$bench};
                                        }
                                    }
                                }
                            }
                        }
                    }
                    # For all classes other than "optimization" only do this
                    # for the most popular.
                    last unless $class eq 'optimization';
                }
	    }
	}
    }

    # See if the various tuning level flags are all the same.  If so, make
    # a notation in the appropriate section.  It's up to the formatters
    # whether or not they output collapsed or per-tuning sections.

    # Do the per-language comparison
    foreach my $langs (keys %{$rc->{'langlist'}}) {
	my $reftune = (keys %{$rc->{'tunelist'}})[0];
	foreach my $tune (keys %{$rc->{'tunelist'}}) {
	    next if $tune eq $reftune;
	    foreach my $class (keys %{$rc->{'classlist'}}) {
		next if $class eq 'compilers';  # Note: not 'compiler'

                # Make sure that there are flag lists for each tune.
		my $reftuneok = ::check_elem('HASH', $counts{$class}, $reftune, $langs);
		my $tuneok =    ::check_elem('HASH', $counts{$class}, $tune,    $langs);
                # If one has counts and the other doesn't, they don't match!
                # If neither have counts, then they don't match!
                if ($reftuneok != $tuneok || $reftuneok == 0 || $tuneok == 0) {
                  $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                  next;
                }

                my @refflags = sort keys %{$counts{$class}->{$reftune}->{$langs}};
                my @tuneflags = sort keys %{$counts{$class}->{$tune}->{$langs}};
                # If there are different numbers of flags, they definitely
                # don't match
                if ($#refflags != $#tuneflags) {
                  $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                  next;
                }
		if (!defined($rc->{'langmatch'}->{$class}->{'alltune'}->{$langs})) {
		    $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 1;
		}
                for(my $i = 0; $i < @refflags; $i++) {
                  if ($refflags[$i] ne $tuneflags[$i]) {
		    $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                    last;
                  }
                }
	    }
	}
    }

    # Try to get rid of all the fully consolidated sections that are just
    # "No flags used".
    foreach my $langs (keys %{$rc->{'langlist'}}) {
	foreach my $tune (keys %{$rc->{'tunelist'}}) {
	    foreach my $class (keys %{$rc->{'classlist'}}) {
		next if $class eq 'compilers';  # Note: not 'compiler'
                if (   $rc->{'langmatch'}->{$class}->{$tune}->{$langs} > 0
                    && check_elem('ARRAY', $rc, 'bylang', 'stringlist', $class, $tune, $langs)
                    && $rc->{'bylang'}->{'stringlist'}->{$class}->{$tune}->{$langs}->[0] eq 'No flags used')
                {
                    foreach my $thing (qw(markup stringlist string flaglist mismatch mismatches)) {
                        next unless check_elem(undef, $rc, 'bylang', $thing, $class, $tune);
                        delete $rc->{'bylang'}->{$thing}->{$class}->{$tune}->{$langs};
                        if (keys %{$rc->{'bylang'}->{$thing}->{$class}->{$tune}} == 0) {
                            # No more elements, so remove it too.
                            delete $rc->{'bylang'}->{$thing}->{$class}->{$tune};
                        }
                    }
                }
            }
        }
    }

    # Do the per-benchmark comparison
    foreach my $bench (keys %{$rc->{'benchlist'}}) {
	my $reftune = (keys %{$rc->{'tunelist'}})[0];
	foreach my $tune (keys %{$rc->{'tunelist'}}) {
	    next if $tune eq $reftune;
	    my $langs = $rc->{'langs'}->{$bench}->{$tune};
	    foreach my $class (keys %{$rc->{'classlist'}}) {
		next if $class eq 'compilers';  # Note: not 'compiler'

                # Make sure that there are flag lists for each tune.
                my $reftuneok = ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $reftune);
                my $tuneok =    ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);

                # If one has flags and the other doesn't, they don't match!
                # If neither of them has flags, then they don't match!
		if ($reftuneok != $tuneok || $reftuneok == 0 || $tuneok == 0) {
                  $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
                  next;
                }

                my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench};
                # If there are different numbers of flags, they definitely
                # don't match
                if ($#{$flaglistref->{$reftune}} != $#{$flaglistref->{$tune}}) {
                  $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
                  next;
                }

		if (!defined($rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'})) {
		    $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 1;
		}

                my $refref = '';
                my $classref = '';
                # All this stuff is to make sure that different flags with the
                # same text are not merged.  For example, -DSPEC_CPU_SOLARIS for
                # 400.perlbench is different than -DSPEC_CPU_SOLARIS for
                # 403.gcc.
                for(my $i = 0; $i < @{$flaglistref->{$reftune}}; $i++) {
                  $refref .= $flaglistref->{$reftune}->[$i]->[2]->{'name'};
                  $refref .= $flaglistref->{$reftune}->[$i]->[1];
                  $refref .= $rc->{'mismatches'}->{$class}->{$bench}->{$reftune}->[$i];
                  $refref .= $flaglistref->{$reftune}->[$i]->[2]->{'description'};
                  $refref .= ' ';
                  $classref .= $flaglistref->{$tune}->[$i]->[2]->{'name'};
                  $classref .= $flaglistref->{$tune}->[$i]->[1];
                  $classref .= $rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i];
                  $classref .= $flaglistref->{$tune}->[$i]->[2]->{'description'};
                  $classref .= ' ';
                }
		if ($refref ne $classref) {
		    $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
		}
	    }
	}
    }

    # Finally, go through benchmatch and langmatch.  If ANY 'alltune' is set
    # to 0, then a single unified section cannot be output.
    foreach my $class (keys %{$rc->{'classlist'}}) {
      if ($rc->{'tunecount'} <= 1) {
        # Don't "consolidate" sections for runs with only 1 tuning level
        $rc->{'allmatch'}->{$class} = 0;
        next;
      }
      if (!exists($rc->{'allmatch'}->{$class})) {
        $rc->{'allmatch'}->{$class} = 1;
      }
      next if $rc->{'allmatch'}->{$class} == 0; # Don't waste time

      # Go through all of the languages in langmatch
      if (::check_elem('HASH', $rc, 'langmatch', $class, 'alltune')) {
        foreach my $lang (keys %{$rc->{'langmatch'}->{$class}->{'alltune'}}) {
          if (!defined($rc->{'langmatch'}->{$class}->{'alltune'}->{$lang}) ||
              $rc->{'langmatch'}->{$class}->{'alltune'}->{$lang} == 0) {
            $rc->{'allmatch'}->{$class} = 0;
            last;
          }
        }
      }

      # Go through all of the benchmarks in benchmatch
      if (::check_elem('HASH', $rc, 'benchmatch', $class)) {
        foreach my $bench (keys %{$rc->{'benchmatch'}->{$class}}) {
          if (!::check_elem(undef, $rc, 'benchmatch', $class, $bench, 'alltune') ||
              $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} == 0) {
            $rc->{'allmatch'}->{$class} = 0;
            last;
          }
        }
      }
    }

    # Now that everything's settled out and been reduced, go through and
    # re-mark the ones which have common flag text with another flag in
    # the same class + (language / benchmark).  This is to allow the
    # compiler to be added to the marking.
    foreach my $class (keys %{$rc->{'classlist'}}) {
      # Go through all of the languages
      if (::check_elem('HASH', $rc, 'bylang', 'flaglist', $class)) {
        foreach my $tune (keys %{$rc->{'bylang'}->{'flaglist'}->{$class}}) {
          next unless ::check_elem('HASH', $rc, 'bylang', 'flaglist', $class, $tune);
          my $tuneref = $rc->{'bylang'}->{'flaglist'}->{$class}->{$tune};
          foreach my $lang (keys %{$tuneref}) {
            next unless isa($tuneref->{$lang}, 'ARRAY');
            # Build a hash with counts of the flag text
            my %counts = ();
            map { $counts{$_->[1]}++ } grep { isa($_, 'ARRAY') } @{$tuneref->{$lang}};
            # Weed out the ones that aren't duplicates
            map { delete $counts{$_} } grep { $counts{$_} <= 1 } keys %counts;
            foreach my $dupflag (keys %counts) {
              munge_markup($dupflag, $tuneref->{$lang}, $r,
                           $rc->{'bylang'}->{'markup'}->{$class}->{$tune}->{$lang},
                           $rc->{'bylang'}->{'stringlist'}->{$class}->{$tune}->{$lang},
                           \$rc->{'bylang'}->{'string'}->{$class}->{$tune}->{$lang});
            }
          }
        }
      }

      # Now, all the benchmarks (if any)
      if (::check_elem('HASH', $rc, 'flaglist', $class)) {
        foreach my $bench (keys %{$rc->{'flaglist'}->{$class}}) {
          next unless ::check_elem('HASH', $rc, 'flaglist', $class, $bench);
          foreach my $tune (keys %{$rc->{'flaglist'}->{$class}->{$bench}}) {
            next unless ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
            my $tuneref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
            # Build a hash with counts of the flag text
            my %counts = ();
            map { $counts{$_->[1]}++ } grep { isa($_, 'ARRAY') } @{$tuneref};
            # Weed out the ones that aren't duplicates
            map { delete $counts{$_} } grep { $counts{$_} <= 1 } keys %counts;
            foreach my $dupflag (keys %counts) {
              munge_markup($dupflag, $tuneref, $r,
                           $rc->{'markup'}->{$class}->{$bench}->{$tune},
                           $rc->{'stringlist'}->{$class}->{$bench}->{$tune},
                           \$rc->{'string'}->{$class}->{$bench}->{$tune});
            }
          }
        }
      }
    }

#print Data::Dumper->Dump([$rc], ['rc'])."\n"; exit;
    return $rc;
}

sub munge_markup {
  my ($dupflag, $flaglist, $r, $markuplist, $stringlist, $stringref) = @_;

  my $munged = 0;
  for(my $i = 0; $i < @{$flaglist}; $i++) {
    next unless isa($flaglist->[$i], 'ARRAY');
    my $flagref = $flaglist->[$i];
    next unless $flagref->[1] eq $dupflag;
    # If there's no compiler name, then there's really not
    # anything that can be done.
    next unless (defined($flagref->[2]->{'compiler_used'}) ||
                 $flagref->[2]->{'compiler_used'} ne '');
    my $markup = $markuplist->[$i];
    my $compiler = $flagref->[2]->{'compiler_used'};
    my $compiler_name;
    if (::check_elem('HASH', $r, 'flaginfo', 'user', 'flagmap', $compiler)) {
      # Get the example text from the flag named in the
      # 'compiler_used' field.
      $compiler_name = $r->{'flaginfo'}->{'user'}->{'flagmap'}->{$compiler}->{'example'};
    }
    if (!defined($compiler_name) || $compiler_name eq '') {
      # What else can we do?  There's no entry for it!
      $compiler_name = $compiler;
    }
    # Fix up the markup
    $markuplist->[$i] = "($compiler_name)".$markup;
    # Fix up the stringlist
    $stringlist->[$i] = $flagref->[1]."($compiler_name)".$markup;
    $munged++;
  }
  if ($munged) {
    # Regenerate the class string
    $$stringref  = join(' ', @{$stringlist});
  }
}

sub find_flags_refs {
    my ($flaghash, $flag, $seen) = @_;

    $seen = {} unless isa($seen, 'HASH');

    return unless exists($flag->{'includes'});

    foreach my $flagname (@{$flag->{'includes'}}) {
	next if ($seen->{$flagname});
	if (!exists($flaghash->{$flagname})) {
	    Log(0, "\nWARNING: Flag '".$flag->{'name'}."' references unknown flag '$flagname'\n");
	    next;
	}
	if (!isa($flaghash->{$flagname}, 'HASH')) {
	    Log(0, "\nERROR: Flag '$flagname' isn't a HASH.  How did that happen?\n");
	    next;
	}
	$seen->{$flagname}++;
	if (exists($flaghash->{$flagname}->{'includes'})) {
	    find_flags_refs($flaghash, $flaghash->{$flagname}, $seen);
	}
    }

    return map { $flaghash->{$_} } sort keys %{$seen};
}

sub add_implicit_ref {
    my ($r, $refflag, $currflag, $bench, $tune) = @_;
    my $refs = undef;

    $r->{'seenflags'}->{$refflag}->{'refs'} = [] unless (ref($r->{'seenflags'}->{$refflag}->{'refs'}) eq 'ARRAY'); 
    $refs = $r->{'seenflags'}->{$refflag}->{'refs'}; 
    if (!grep { $_->[3] eq $bench } @{$refs}) {
        push @{$refs}, [ 'implicit', '', $currflag, $bench, $tune ];
    }
}

sub search_reduced_flags_byclass {
    my ($result, $class, $bench, $tune) = @_;

    return undef unless ::check_elem('HASH', $result, 'reduced_flags');
    my $rf = $result->{'reduced_flags'};

    my @bench = (defined($bench) && $bench ne '') ? ($bench) : (keys %{$rf->{'benchlist'}});
    # Return true if there are any flags in $class in the result
    foreach my $bench (@bench) {
      next unless ::check_elem('HASH', $rf, 'flags', $bench);
      my @tunes = (defined($tune) && $tune ne '') ? ($tune) : (keys %{$rf->{'tunelist'}});
      foreach my $tune (@tunes) {
        next unless ::check_elem('HASH', $rf, 'flags', $bench, $tune);
        if (exists($rf->{'flags'}->{$bench}->{$tune}->{$class})) {
          ::Log(97, "Found \"$class\" flags for $bench:$tune\n");
          return 1;
        }
      }
    }
    return 0;
}

sub get_flags_byclass {
    my ($result, $class, $bench, $tune, @types) = @_;

    # Return list of references to flags of class $class
    return () unless ::check_elem('ARRAY', $result, 'results', $bench, $tune, 'flags', $class);

    my @rc = ();
    if (@types) {
      foreach my $type (@types) {
        push @rc, grep { $_->{'found_in'} eq $type } @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{$class}};
      }
    } else {
      push @rc, @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{$class}};
    }
    return @rc;
}

sub get_pass_list {
    my ($result, $bench, $tune) = @_;

    # Return list of references to flags of class $class
    return () unless ::check_elem('ARRAY', $result, 'results', $bench, $tune, 'flags', 'passes');
    return @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{'passes'}};
}

sub markup_flag {
  my ($flag, %opts) = @_;
  # Mark a flag if its class is mismatched, whether it is used in all
  # link/compilation passes, etc.
  my $mismatch = 0;
  my $markup = '';
  my $seen_error = $opts{'seen_error'};
  my $passes = $opts{'passes'};
  $passes = [ $passes ] if (!isa($passes, 'ARRAY'));
  my @classes = grep { defined } @{$opts{'classes'}} if isa($opts{'classes'}, 'ARRAY');
  if (@classes && !grep { /^[CP]$/ } @classes) {
    # Only portability flags may not appear on compiler lines
    push @classes, 'C';
  }

  # Skip the exercise if it's just for a placeholder.
  return(0, $flag->[1], '') if $flag->[1] eq 'No flags used';

  # Check to see if the flag was in a variable in one of the allowed
  # classes (if any are specified).
  if (@classes && !grep { $flag->[2]->{'found_in'} eq $_ } @classes) {
    $mismatch = 1;
    $markup .= '(*)';
  }

  # Mark is as a non-displayed flag if it is.
  if (!istrue($flag->[2]->{'display'})) {
    $markup .= '(NODISPLAY)';
  }

  return ($mismatch, $flag->[1].$markup, $markup) if !isa($passes, 'ARRAY');

  my $passcount = scalar(grep { defined } @{$passes});
  $passcount++ if $passcount == 0;

  # Now try to figure out whether and how to mark up which phase(s) and
  # pass(es) a particular flag was used.

  # Make an array, indexed by pass, with a bitmap indicating which phases
  # were seen for each pass.
  my @usage = ();
  my %phasemap = ( 'comp' => 1, 'link' => 2 );
  my %add = ('comp' => [], 'link' => [] );
  my %sum;
  my %seen;
  foreach my $passref (@{$flag}[3..$#{$flag}]) {
    next if !isa($passref, 'ARRAY');
    my ($phase, $pass) = @$passref;
    if (!exists $phasemap{$phase}) {
      ::Log(0, "WARNING: Unknown build phase ($phase) found in flag structure.\n");
      next;
    }
    next if exists($seen{$phase.$pass});
    $seen{$phase.$pass} = 1;
    $usage[$pass+0] |= $phasemap{$phase};
    push @{$add{$phase}}, $pass;
    $sum{$phase}++;
  }

  if ($sum{'comp'} == $passcount && $sum{'link'} == $passcount) {
    # Done!  Nothing to add.
    return ($mismatch, $flag->[1].$markup, $markup);
  }

  my %used = ('comp' => 0, 'link' => 0);
  # Figure out how to concisely display exactly where each flag was used.
  foreach my $phase (qw(comp link)) {
    next unless @{$add{$phase}};
    $used{$phase} = 1;
    if ($sum{$phase} == $passcount) {
      $sum{$phase} = '';
      next;
    }
    my ($multiple, $str) = ::nicejoin(@{$add{$phase}});
    if ($str ne '') {
      $used{$phase} = 2;
      $sum{$phase}  = 'pass';
      $sum{$phase} .= 'es' if $multiple;
      $sum{$phase} .= ' '.$str;
    }
  }

  if ($used{'comp'} == 1 && $used{'link'} == 0) {
    # Compilation only
    if (0) {
      # Removed -- too much detail
      $markup .= '(compilation)';
    }
  } elsif ($used{'comp'} == 0 && $used{'link'} == 1) {
    # Link only
    if (0) {
      # Removed -- too much detail
      $markup .= '(link)';
    }
  } elsif (($used{'comp'} == 1 && $used{'link'} == 2) ||
           ($used{'comp'} == 2 && $used{'link'} == 1)) {
    # One with no pass listed, and one with.  This is a problem.
    ::Log(0, "ERROR: Flag options error.  A pass-usage mismatch was detected\n");
    ::Log(0, "       for \"$flag->[1]\" in ".join(',', @{$flag->[0]})."\n");

    ::Log(0, "        The flag was used in ".pluralize($used{'comp'}, 'compilation phase')." and ".pluralize($used{'link'}, 'link phase')."\n");
    if ($seen_error) {
      ::Log(0, "  Normally, this error should never occur.  Flag parsing errors seen\n");
      ::Log(0, "  previously may cause this error, so the flag file MUST be corrected and\n");
      ::Log(0, "  this result reformatted");
    } else {
      ::Log(0, "  This is probably an internal tools error that should never happen.\n");
      ::Log(0, "  Please report this error to ${main::lcsuite}support\@spec.org.  Please\n");
      ::Log(0, "  include the config file used to generate this result, the command line\n");
      ::Log(0, "  used to generate the result, and the flags file used.\n");
    }
  } elsif ($used{'comp'} == 2 && $used{'link'} == 2 &&
      $sum{'comp'} eq $sum{'link'}) {
    # Used in both phases; list only the passes.
    $markup .= '('.$sum{'comp'}.')';
  } elsif ($sum{'comp'} ne '' && $sum{'link'} eq '') {
    # Compilation only
    if (0) {
      # Removed -- too much detail
      $markup .= '(compilation';
      $markup .= ' '.$sum{'comp'} if $sum{'comp'} ne '';
      $markup .= ')';
    } else {
      $markup .= '('.$sum{'comp'}.')' if $sum{'comp'} ne '';
    }
  } elsif ($sum{'comp'} eq '' && $sum{'link'} ne '') {
    # Link only
    if (0) {
      # Removed -- too much detail
      $markup .= '(link';
      $markup .= ' '.$sum{'link'} if $sum{'link'} ne '';
      $markup .= ')';
    } else {
      $markup .= '('.$sum{'link'}.')' if $sum{'link'} ne '';
    }
  } elsif ($sum{'comp'} eq '' && $sum{'link'} eq '') {
    ::Log(0, "ERROR: Flag accounting error; a flag was found that was not used for\n");
    ::Log(0, "       compilation or link.\n");
    ::Log(0, "  This is probably an internal tools error that should never happen.\n");
    ::Log(0, "  Please report this error to ${main::lcsuite}support\@spec.org.  Please\n");
    ::Log(0, "  include the config file used to generate this result, the command line\n");
    ::Log(0, "  used to generate the result, and the flags file used.\n");
  }

  return ($mismatch, $flag->[1].$markup, $markup);
}

sub nicejoin {
  my (@list) = @_;
  my @rc = ();
  my $multiple = @list > 1;
  return @list unless @list;
  return (0, @list) if !$multiple;

  # Make sure that the list is ordered.  This ensures that the code below
  # for making backwards ranges will never be used, but I'm not going to
  # rip it out (it works) because I might want to be able to recognize
  # descending ranges someday
  @list = sort { $a <=> $b } @list;

  # Now look for sequences
  my $start = 0;
  my $reverse = $list[0] > $list[1];
  my @ranges = ();
  for(my $i = 0; $i < $#list - 1; $i++) {
      $start = $i if (!defined($start));
      if ((!$reverse && ($list[$i]+1 != $list[$i+1]) ||
           ($reverse && ($list[$i]-1 != $list[$i+1])))) {
          # This is the end of the current sequence
          if ($i == $start) {
              # Singleton
              push @ranges, [ $list[$i] ];
          } elsif (abs($i - $start) == 1) {
              # Range of two; record them as singletons
              push @ranges, [ $list[$start] ], [ $list[$i] ];
          } else {
              # Range
              push @ranges, [ $list[$start], $list[$i] ];
          }
          $start = undef;
      }
  }
  # Take care of the last element
  if (!defined($start) || $#list == $start) {
      # Singleton
      push @ranges, [ $list[$#list] ];
  } elsif (abs($#list - $start) == 1) {
      # Range of two; record them as singletons
      push @ranges, [ $list[$start] ], [ $list[$#list] ];
  } else {
      # Range
      push @ranges, [ $list[$start], $list[$#list] ];
  }

  # Now render it
  foreach my $rref (@ranges) {
    push @rc, join('-', @{$rref}) if ref($rref) eq 'ARRAY';
  }

  return ($multiple, join(', ', @rc));
}

sub makeanchor {
    my ($anchor, $used) = @_;
    $anchor =~ s/C\+\+/CXX/g;
    $anchor =~ tr/A-Za-z0-9//cd;
    $anchor = "_$anchor" unless $anchor =~ /^[0-9A-Za-z:_]/;

    if (   $anchor eq ''
        || !::isa($used, 'HASH')
        || !exists($used->{$anchor})
        || $used->{$anchor} == 0) {
        $used->{$anchor}++ unless $anchor eq '';
        return $anchor;
    }

    my $tmpadd = 0;
    while($used->{$anchor.$tmpadd} > 0) {
        $tmpadd++;
    }
    $used->{$anchor.$tmpadd}++;
    return $anchor.$tmpadd;
}

sub check_same_flags {
  # Given a hash ref whose values are array refs which contain strings,
  # determine if all the lists are the same.
  my ($lines) = @_;

  return 0 unless isa($lines, 'HASH');

  my $same = 1;
  # Make sure all the strings are the same.
  my $reference = undef;
  my @items = keys %{$lines};
  my $ref_item = $items[0];
  while ($same && @items) {
    my $item = shift(@items);
    if (!isa($lines->{$item}, 'ARRAY')) {
      # Something is messed up.
      $same = 0;
      next;
    }
    if (!defined($reference)) {
      $reference = join(' ', @{$lines->{$item}});
    } else {
      my $candidate = join(' ', @{$lines->{$item}});
      $same = 0 if ($reference ne $candidate);
    }
  }
  return $same ? $lines->{$ref_item} : undef;
}

sub match_var {
  # Check to see if $var is in a particular flag's list of "used in" variables
  my ($flag, $var) = @_;

  return grep { $_ eq $var } @{$flag->[0]};
}

sub merge_passlist {
  # Merge the list of phases/passes used by two flags
  my ($flaglist, $dstidx, $srcidx) = @_;
  my %passes = ();

  foreach my $flagidx ($srcidx, $dstidx) {
    for(my $i = 3; $i < @{$flaglist->[$flagidx]}; $i++) {
      $passes{join(':', @{$flaglist->[$flagidx]->[$i]})}++;
    }
  }

  # Remove the current list of phases/passes
  splice @{$flaglist->[$dstidx]}, 3;

  foreach my $phase (sort keys %passes) {
    push @{$flaglist->[$dstidx]}, [ split(/:/, $phase, 2) ];
  }
}

# Variable order:
# CC CXXC FC LD BENCH_FPPFLAGS COBJOPT CXXOBJOPT FOBJOPT CPUFLAGS
# BENCH_FLAGS BENCH_CFLAGS BENCH_CXXFLAGS BENCH_FFLAGS LDCFLAGS LDCXXFLAGS
# LDFFLAGS PASSn_FLAGS PASSn_CFLAGS PASSn_CXXFLAGS PASSn_FFLAGS
# PASSn_LDFLAGS PASSn_LDCFLAGS PASSn_LDCXXFLAGS PASSn_LDFFLAGS OPTIMIZE
# COPTIMIZE CXXOPTIMIZE FOPTIMIZE FDO_OPTIMIZE FDO_COPTIMIZE
# FDO_CXXOPTIMIZE FDO_FOPTIMIZE PORTABILITY CPORTABILITY CXXPORTABILITY
# FPORTABILITY LDPORTABILITY EXTRA_CFLAGS EXTRA_CXXFLAGS EXTRA_FFLAGS
# EXTRA_LDFLAGS EXTRA_OPTIMIZE EXTRA_COPTIMIZE EXTRA_CXXOPTIMIZE
# EXTRA_FOPTIMIZE EXTRA_PORTABILITY EXTRA_CPORTABILITY EXTRA_CXXPORTABILITY
# EXTRA_FPORTABILITY FDO_LDFLAGS FDO_LDCFLAGS FDO_LDCXXFLAGS FDO_LDFFLAGS
# LIBS OS_LIBS MATH_LIBS OPTIMIZATION_LIBS OPTIMIZATION_CLIBS
# OPTIMIZATION_CXXLIBS OPTIMIZATION_FLIBS PORTABILITY_LIBS
# PORTABILITY_CLIBS PORTABILITY_CXXLIBS PORTABILITY_FLIBS EXTRA_LIBS
# EXTRA_CLIBS EXTRA_CXXLIBS EXTRA_FLIBS PASSn_LDOPT LDOPT LDOUT
# FPPPORTABILITY EXTRA_FPPFLAGS

my %variable_order = (
  'CC'                   => 1,
  'CXXC'                 => 2,
  'CXX'                  => 2,
  'FC'                   => 3,
  'LD'                   => 4,
  'BENCH_FPPFLAGS'       => 5,
  'COBJOPT'              => 6,
  'CXXOBJOPT'            => 7,
  'FOBJOPT'              => 8,
  'CPUFLAGS'             => 9,
  'BENCH_FLAGS'          => 10,
  'BENCH_CFLAGS'         => 11,
  'BENCH_CXXFLAGS'       => 12,
  'BENCH_FFLAGS'         => 13,
  'LDCFLAGS'             => 14,
  'LDCXXFLAGS'           => 15,
  'LDFFLAGS'             => 16,
  'PASSn_FLAGS'          => 17,
  'PASSn_CFLAGS'         => 18,
  'PASSn_CXXFLAGS'       => 19,
  'PASSn_FFLAGS'         => 20,
  'PASSn_LDFLAGS'        => 21,
  'PASSn_LDCFLAGS'       => 22,
  'PASSn_LDCXXFLAGS'     => 23,
  'PASSn_LDFFLAGS'       => 24,
  'OPTIMIZE'             => 25,
  'COPTIMIZE'            => 26,
  'CXXOPTIMIZE'          => 27,
  'FOPTIMIZE'            => 28,
  'FDO_OPTIMIZE'         => 29,
  'FDO_COPTIMIZE'        => 30,
  'FDO_CXXOPTIMIZE'      => 31,
  'FDO_FOPTIMIZE'        => 32,
  'PORTABILITY'          => 33,
  'CPORTABILITY'         => 34,
  'CXXPORTABILITY'       => 35,
  'FPORTABILITY'         => 36,
  'LDPORTABILITY'        => 37,
  'EXTRA_CFLAGS'         => 38,
  'EXTRA_CXXFLAGS'       => 39,
  'EXTRA_FFLAGS'         => 40,
  'EXTRA_LDFLAGS'        => 41,
  'EXTRA_OPTIMIZE'       => 42,
  'EXTRA_COPTIMIZE'      => 43,
  'EXTRA_CXXOPTIMIZE'    => 44,
  'EXTRA_FOPTIMIZE'      => 45,
  'EXTRA_PORTABILITY'    => 46,
  'EXTRA_CPORTABILITY'   => 47,
  'EXTRA_CXXPORTABILITY' => 48,
  'EXTRA_FPORTABILITY'   => 49,
  'FDO_LDFLAGS'          => 50,
  'FDO_LDCFLAGS'         => 51,
  'FDO_LDCXXFLAGS'       => 52,
  'FDO_LDFFLAGS'         => 53,
  'LIBS'                 => 54,
  'OS_LIBS'              => 55,
  'MATH_LIBS'            => 56,
  'OPTIMIZATION_LIBS'    => 57,
  'OPTIMIZATION_CLIBS'   => 58,
  'OPTIMIZATION_CXXLIBS' => 59,
  'OPTIMIZATION_FLIBS'   => 60,
  'PORTABILITY_LIBS'     => 61,
  'PORTABILITY_CLIBS'    => 62,
  'PORTABILITY_CXXLIBS'  => 63,
  'PORTABILITY_FLIBS'    => 64,
  'EXTRA_LIBS'           => 65,
  'EXTRA_CLIBS'          => 66,
  'EXTRA_CXXLIBS'        => 67,
  'EXTRA_FLIBS'          => 68,
  'PASSn_LDOPT'          => 69,
  'LDOPT'                => 70,
  'LDOUT'                => 71,
  'FPPPORTABILITY'       => 72,
  'EXTRA_FPPFLAGS'       => 73,
);

sub by_variable {
  my ($avarname, $bvarname, $avar, $bvar, $apos, $bpos, $apass, $bpass);

  $apass = $bpass = 0;

  # Figure out which of the variables listed are _last_ (if multiple
  # variables are listed)
  if (isa($a, 'ARRAY')) {
    my @avars = @{$a->[0]};
    @avars = map { s/^PASS\d+_/PASSn_/; $_ } @avars;  # Don't modify the source
    $avarname = (reverse sort by_variable_order @avars)[0];
    if ($avarname =~ /^PASS(\d+)(_.+)$/) {
      $apass = $1;
      $avarname = 'PASSn'.$2;
    }
    $avar = $variable_order{$avarname};
    $apos = $a->[2]->{'pos'};
  }
  if (isa($b, 'ARRAY')) {
    my @bvars = @{$b->[0]};
    @bvars = map { s/^PASS\d+_/PASSn_/; $_ } @bvars;  # Don't modify the source
    $bvarname = (reverse sort by_variable_order @bvars)[0];
    if ($bvarname =~ /^PASS(\d+)(_.+)$/) {
      $bpass = $1;
      $bvarname = 'PASSn'.$2;
    }
    $bvar = $variable_order{$bvarname};
    $bpos = $b->[2]->{'pos'};
  }

  #if (!defined($avar)) { print "A is undef: \"$avarname\" ($a->[1]) $avar $apos\n"; }
  #if (!defined($bvar)) { print "B is undef: \"$bvarname\" ($b->[1]) $bvar $bpos\n"; }

  # Unknown variables go at the beginning
  return -1 if !defined($avar);
  return 1 if !defined($bvar);

  return $avar <=> $bvar || $apass <=> $bpass || $apos <=> $bpos;
}

sub by_variable_order {
  # Sort two strings by their order in %variable_order, taking into account
  # PASSn_* flags.
  my $avar = $a;
  my $bvar = $b;
  $avar =~ s/^PASS\d+_/PASSn_/;
  $bvar =~ s/^PASS\d+_/PASSn_/;
  return $variable_order{$avar} <=> $variable_order{$bvar};
}
1;
