#
#  raw.pl - produces RAW output
#  Copyright 1995-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: raw.pl 6513 2011-06-01 23:31:07Z CloyceS $

use strict;
use IO::File;
use UNIVERSAL qw(isa);

use vars qw($name $extension $synonyms $prefix);

$name      = 'raw';
$extension = 'rsf';
$synonyms  = { map { lc($_) => 1 } ($name, $extension) };
$prefix = "spec.$::lcsuite";

my $version = '$LastChangedRevision: 6513 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'output_raw.pl'} = $version;
$Spec::Format::raw::part_of_all = 1;    # Part of '-o all' (just to be clear)

sub format {
    my($me, $r, $fn) = @_;
    my @output;
    my $written = [];
    # Some keys should not be put into the raw file, so mark them as already
    # dumped.
    my %dumped = map { $_ => 1 } qw(
                                     benchmarklist datestr rawfile compraw
                                     rawhash flagsinfo output orig_raw_config
                                     do_dump graph_min graph_max graph_auto
                                     forbiddenused unknownused
                                     allow_extension_override output_root
                                     expid review topdir flags test train ref
                                     size_class mpi_items orig_raw_good
                                     poweranalyzerrawrecords tempmeterrawrecords
                                     speed_multiplier rate_multiplier
                                   );
    $dumped{$$.'auto2par'} = 1;
    # Figure out whether to dump or ignore the sw_auto_parallel setting
    if (   exists($r->{'sw_parallel_other'})
        || $::lcsuite eq 'mpi2007') {
        # Presence of sw_parallel_other implies that either
        # a) this is a new (1.1+) result or
        # b) it's an old result that's been refitted with the new fields
        $dumped{'sw_auto_parallel'} = 1;  # Keep it out of the editable section
        if ( exists($r->{'sw_parallel_other'}) ) {
            # Make sure it's below the fence
            push @{$r->{'do_dump'}}, 'sw_auto_parallel';
        }
    }

    my @keys = grep { !/(^refs|^benchmarks|auto2par)$/ } $r->list_keys;

    # Make sure that the suite versions exist and are integrated properly
    if (!grep { /^suitever$/o } @keys) {
	$r->{'suitever'} = $::suite_version;
	push @keys, 'suitever';
    }
    if (!grep { /^runspecver$/o } @keys) {
	$r->{'runspecver'} = $::version;
	push @keys, 'runspecver';
    }
    if (!grep { /^toolset$/o } @keys) {
	$r->{'toolset'} = $::toolset_name;
	push @keys, 'toolset';
    }


    # Likewise, power measurements
    if (   ($::lcsuite eq 'mpi2007' && $r->{'suitever'} < 1.1)
        || ($::lcsuite eq 'cpu2006' && $r->{'suitever'} < 1.2)
        || ($::lcsuite eq 'omp2001')
       ) {
        $dumped{'power'} = 1;
    }

    if ($::lcsuite =~ /cpu(?:2006|v6)/) {
        # These shouldn't be present, but just in case...
        $dumped{'ranks'} = 1;
        $dumped{'base_ranks'} = 1;
        $dumped{'system_class'} = 1;
        $dumped{'hw_total_nodes'} = 1;
        $dumped{'hw_total_chips'} = 1;
        $dumped{'hw_total_cores'} = 1;
        $dumped{'hw_total_threads'} = 1;
        $dumped{'hw_total_memory'} = 1;
        $dumped{'max_ranks'} = 1;
        $dumped{'max_peak_ranks'} = 1;
    } elsif ($::lcsuite eq 'mpi2007') {
        # These shouldn't be present, but just in case...
        $dumped{'copies'} = 1;
        $dumped{'clcopies'} = 1;
        $dumped{'base_copies'} = 1;
    } elsif ($::lcsuite =~ /^omp20(01|12)$/) {
        # These shouldn't be present, but just in case...
        $dumped{'copies'} = 1;
        $dumped{'clcopies'} = 1;
        $dumped{'base_copies'} = 1;
    }
    my $base_fn = $fn;
    $base_fn =~ s/\.${extension}$//;
    $base_fn =~ s/\.${extension}$// if $::rawformat_time && $::from_runspec;

    # Generated fields should not be dumped, or people will think that they
    # can be edited in the raw file.
    foreach my $genfield (keys %::generated_fields) {
        $dumped{$genfield} = 1 if ($::generated_fields{$genfield} == 1);
    }

    # Dump configuration info
    for my $key (sort main::bytrailingnum @keys) {
	next if $dumped{$key};
	next if exists $::generated_fields{$key} && $::generated_fields{$key};
	next unless ($key =~ /$::info_re/);
        next if $key =~ /^$::mpi_desc_re_id?notes/;   # Notes are special now.
        next if $key =~ /^flagsurl\d*$/;                  # As are flagsurls
	$dumped{$key} = 1;
	my $val = $r->accessor($key);
        # Fix up dynamic entries for missing items
        if ($key =~ /!/) {
            $key =~ s/!/MISSING/;
            $dumped{$key} = 1;
        }
	if (::isa($val, 'ARRAY')) {
	    for (my $i = 0; $i < @$val; $i++) {
                next unless defined($val->[$i]);        # Handle holes
		push(@output, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@output, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
    }

    # Dump the notes.  We do this separately so that the numbers can be spaced
    # It should always be an array, but just in case it isn't, give it the
    # full treatment.
    # 'nowarn' because there might not be any notes
    foreach my $system ('', keys %{$r->{'mpi_items'}}) {
        foreach my $section (map { $_->[0] } @::notes_info) {
            next unless ::isa($r->{$system.$section}, 'HASH');
            $dumped{$system.$section} = 1;
            next if "$system$section" =~ /notes_auto2par/;
            foreach my $key (sort keys %{$r->{$system.$section}}) {
                my $val = $r->{$system.$section}->{$key};
                next unless ::isa($val, 'ARRAY');
                for (my $i = 0; $i < @$val; $i++) {
                    my ($tag, $text) = @{$val->[$i]};
                    next unless defined($tag);
                    # Look in the line for ATTACH statements, and deal with
                    # the files if necessary
                    $text = ::handle_ATTACH($text, $base_fn, $written);
                    push @output, "$prefix.$tag: $text";
                }
            }
        }
    }

    # Trim trailing whitespace from all fields.  This shouldn't be necessary,
    # as the config file and raw file parsers should remove them.  But just
    # in case someone manages to sneak one through...
    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
        # Strip all trailing whitespace
        $line =~ s/\s+$//;
        # Put one space back if it needs to be there
        $line =~ s/^(${prefix}\.[\.A-Za-z0-9_-]+:)$/$1 /;
    }

    # Print a little delimiter
    push @output, '# =============== do not edit below this point ===================';

    # From here on in, we make an MD5 hash of all the lines
    my $ctx = new Digest::MD5;
    my @md5list = (); # Here's where we store the info to be hashed

    if ($r->{'invalid'} == 0) {
        # If any of the individual runs were not valid, the whole result should
        # be not valid
        my %seen = ();
        for my $bench (sort keys %{$r->{'results'}}) {
            for my $tune (keys %{$r->{'results'}{$bench}}) {
                next unless ::isa($r->{'results'}{$bench}{$tune}{'data'}, 'ARRAY');
                foreach my $res (@{$r->{'results'}{$bench}{$tune}{'data'}}) {
                    next unless ::isa($res, 'HASH');
                    if ($res->{'valid'} ne 'S') {
                        if (!$seen{$bench.$tune}) {
                            push @{$r->{'errors'}}, "Run of $bench ($tune) was not valid; status is $res->{'valid'}";
                            $seen{$bench.$tune} = 1;
                            $r->{'invalid'} = 1;
                            $r->{'valid'} = 'X';
                            if (!grep { /^errors$/o } @keys) {
                                push @keys, 'errors';
                            }
                        }
                    }
                }
            }
        }
    }

    if (!grep { /^invalid$/o } @keys) {
	# Make sure that invalid results have an invalid tag:
	if (!exists($r->{'invalid'})) {
	    if (exists($r->{'errors'})) {
		if ((ref($r->{'errors'}) eq 'ARRAY') &&
		    (@{$r->{'errors'}}+0) > 0) {
		    $r->{'invalid'} = 1;
		    $r->{'valid'} = 'X';
		}
	    } else {
		$r->{'invalid'} = 0;
	    }
	} else {
	    $r->{'invalid'} = 0 if (!defined($r->{'invalid'}));
	}
	push @keys, 'invalid';
    }

    # If the result is otherwise fine, but forbidden or unknown flags were
    # used, mark it as 'temporarily invalid'.  This will get stored in the
    # raw file so that the submission handler can see it, but is treated by
    # the formatter the same as if it were set to 0.
    if ($r->{'invalid'} == 0) {
        $r->{'invalid'} = 2 if ($r->{'forbiddenused'} || $r->{'unknownused'});
    }

    if (0
    # CVT2DEV: || 1
       ) {
        if (!exists($r->{'errors'}) ||
            (exists($r->{'errors'}) && ::isa($r->{'errors'}, 'ARRAY') &&
             !grep { /This result was run using a development tree/ } @{$r->{'errors'}})) {
            push @{$r->{'errors'}}, 'This result was run using a development tree; results from this may not be',
                                    'published or used externally.';
            if (!grep { /^errors$/o } @keys) {
                push @keys, 'errors';
            }
        }
    }
    if (!grep { /^lognum$/o } @keys) {
	$r->{'lognum'} = $::global_config->accessor_nowarn('lognum') || 'Unknown';
	push @keys, 'lognum';
    }

    # Put all the cfidx items together.  In most cases it will make them take
    # less space.
    my @cfidx = ();
    for my $key (sort ::bytag grep { /^cfidx_/ } @keys) {
	my $index = $r->accessor($key);
	$key =~ s/^cfidx_//;
	push @cfidx, "$key:$index";
	$dumped{$key}++;
    }
    my ($raw, $comp, $enc) = ::compress_encode(join(',', @cfidx));
    my @complines = split(/\n/, (defined($enc) && $enc ne '') ? $enc : $raw);
    $r->{'cfidx'} = [ @complines ];
    push @keys, 'cfidx';

    if (!exists($r->{'toolvers'})) {
	# Mash up the various file versions
	($raw, $comp, $enc) = ::compress_encode(join(',', map { "$_:$::tools_versions{$_}" } sort keys %::tools_versions));
	@complines = split(/\n/, (defined($enc) && $enc ne '') ? $enc : $raw);
	$r->{'toolvers'} = [ @complines ];
	push @keys, 'toolvers';
    }

    # Some things simply _must_ be dumped
    foreach my $important_thing (@{$r->{'do_dump'}}) {
        delete $dumped{$important_thing};
    }

    # Dump non-configuration, non-result info (leftovers)
    for my $key (sort @keys) {
	next if $dumped{$key};
	my $val = $r->accessor($key);
        if ($key eq 'size') {
            my $class = $r->accessor('size_class');
            $val = "$val ($class)" if ($val ne $class);
        }
        # For flagsurls, put a little warning comment above them
        if ($key eq 'flagsurl') {
            if (::isa($val, 'ARRAY')) {
                # Eliminate holes
                $val = [ grep { defined($_) && $_ ne '' } @$val ];
            }
            if (   (::isa($val, 'ARRAY') && @$val >= 1)
                || (!::isa($val, 'ARRAY') && $val ne '')) {
                if (::isa($val, 'ARRAY') && @$val > 1) {
                    push @md5list, '# This result was formatted with the flags files mentioned on the following';
                    push @md5list, '# lines. Please do NOT attempt to apply a new flags file by changing them;';
                } else {
                    push @md5list, '# This result was formatted with the flags file mentioned on the following';
                    push @md5list, '# line. Please do NOT attempt to apply a new flags file by changing it;';
                }
                push @md5list, '# to load new flag definitions, you need to use "rawformat --flagsurl".';
            }
        }

	if (::isa($val, 'ARRAY')) {
	    for (my $i = 0; $i < @$val; $i++) {
		push(@md5list, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@md5list, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
	$dumped{$key} = 1;
    }

    # Make sure there are stored compile options...
    if (!exists($r->{'compile_options'})) {
	Log(0, "\nERROR: No saved compile options in the result object!\n");
	main::do_exit(1);
    }

    # Dump result info
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchname = $bench;
	$benchname =~ y/\./_/;
	for my $tune (keys %{$r->{'results'}{$bench}}) {
	    next unless ::isa($r->{'results'}{$bench}{$tune}{'data'}, 'ARRAY');
            my @tmp = sort { $a->{'iteration'} <=> $b->{'iteration'} } @{$r->{'results'}{$bench}{$tune}{'data'}};
            for (my $i = 0; $i < @tmp; $i++) {
                for my $key (sort keys %{$tmp[$i]}) {
                    next if $key eq 'ref' || $key eq 'refs' || $key eq 'tune';
                    if (ref $tmp[$i]->{$key} eq 'ARRAY') {
                        my @tmp2 = @{$tmp[$i]->{$key}};
                        for (my $j = 0; $j < @tmp2; $j++) {
                            push (@md5list, 
                                sprintf "%s.results.%s.%s.%03d.%s%03d: %s", 
                                        $prefix, $benchname, $tune, $i, $key, 
                                        $j, $tmp[$i]->{$key}->[$j]);
                        }
                    } else {
                        push (@md5list, 
                            sprintf "%s.results.%s.%s.%03d.%s: %s", 
                                    $prefix, $benchname, $tune, $i, $key, 
                                    $tmp[$i]->{$key});
                    }
                }
            }

	    # Dump the compile options so that flag reports can be generated
	    # with just the raw file.
	    if (exists($r->{'compile_options'}->{$bench}) &&
		exists($r->{'compile_options'}->{$bench}->{$tune})) {
		my $opts = $r->{'compile_options'}->{$bench}->{$tune};
		if ($opts =~ /:/o) {
		    # It's probably not compressed or anything...
		    $opts = ::compress_encode($opts);
		}
		my @optlist = split(/\n+/, $opts);
		for (my $i = 0; $i < @optlist; $i++) {
		    push (@md5list, 
			sprintf "%s.compopts%03d.%s.%s: %s", 
				$prefix, $i, $benchname, $tune,
				$optlist[$i]);
		}
	    }
	}
    }

    foreach my $line (@md5list) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
        # Also remove trailing spaces, etc...
        $line =~ s/\s+$//;
        # Put one space back if it needs to be there
        $line =~ s/^(${prefix}\.[\.A-Za-z0-9_-]+:)$/$1 /;
    }
    $ctx->add(@md5list);
    push @output, "$prefix.rawhash: ".$ctx->hexdigest();
    push @output, @md5list;

    # Make sure we save a copy of the rawfile in the result object:
    if (!exists $r->{'compraw'}) {
        my (undef, $comp, $enc) = ::compress_encode(join("\n", @output)."\n");
	if (defined($comp)) {
	    $r->{'rawfile'} = undef;
	    $r->{'compraw'} = $enc;
	} else {
	    $r->{'rawfile'} = $enc;
	    $r->{'compraw'} = undef;
	}
    }

    # If forbidden or unknown flags were used, add a note to errors.
    # This isn't done before, because we don't want to permanently mark
    # them as invalid (for that reason only) in the raw file.
    if ($r->{'forbiddenused'}) {
      push @{$r->{'errors'}}, 'Forbidden flags were used!';
    }
    if ($r->{'unknownused'}) {
      if (!exists $r->{'errors'} || !::isa($r->{'errors'}, 'ARRAY') ||
          @{$r->{'errors'}} == 0) {
        push @{$r->{'errors'}}, 'Your run was marked invalid because it has one or more flags in the',
                                '"unknown" category. You might be able to resolve this problem without',
                                're-running your test; see',
                                "     http://www.spec.org/${main::lcsuite}/Docs/runspec.html#flagsurl",
                                'for more information.';
      } else {
        # There are other errors, so do not hold out the false hope of
        # correction without a re-run.
        push @{$r->{'errors'}}, 'Unknown flags were used! See',
                                "     http://www.spec.org/${main::lcsuite}/Docs/runspec.html#flagsurl",
                                'for information about how to get rid of this error.';
      }
    }

    if (exists($r->{'flags'}) && $r->{'flags'} ne '' &&
        $r->{'flags'} =~ /<flagsdescription>/) {
      # Save off a copy of the flags file source
      my $flag_file = $base_fn.'.xml';
      my $ofh = new IO::File '>'.$flag_file;
      if (defined($ofh)) {
        $ofh->print($r->{'flags'});  
        $ofh->close();
        push @{$written}, $flag_file;
      } else {
        ::Log(0, "Couldn't open $flag_file for writing: $!\n");
      }
    }

    return (\@output, $written);
}

sub Log { main::Log(@_) }

