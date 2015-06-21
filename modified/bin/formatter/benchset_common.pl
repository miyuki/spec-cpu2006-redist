#
# benchset_common.pl
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchset_common.pl 6364 2011-03-05 00:41:51Z cloyce $
#

use strict;
use UNIVERSAL qw(isa);

my $version = '$LastChangedRevision: 6364 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'benchset_common.pl'} = $version;

# Information about what's in the config entries

# All of the hardware_info items are printed
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@::hardware_info=(
#    Field name      Field label        Display in detail box?
    ['hw_cpu_name' , 'CPU Name'           , 1 ],
    ['hw_cpu_char' , 'CPU Characteristics', 1 ],
    ['hw_cpu_mhz'  , 'CPU MHz'            , 1 ],
    ['hw_fpu'      , 'FPU'                , 1 ],
    ['hw_ncpu'     , 'CPU(s) enabled'     , 1 ],
    ['hw_ncpuorder', 'CPU(s) orderable'   , 1 ],
    ['hw_pcache'   , 'Primary Cache'      , 1 ],
    ['hw_scache'   , 'Secondary Cache'    , 1 ],
    ['hw_tcache'   , 'L3 Cache'           , 1 ],
    ['hw_ocache'   , 'Other Cache'        , 1 ],
    ['hw_memory'   , 'Memory'             , 1 ],
    ['hw_disk'     , 'Disk Subsystem'     , 1 ],
    ['hw_other'    , 'Other Hardware'     , 1 ],
    ['idle_avg_watts', 'Idle Power (w)'   , 1 ],
);

# All of the software_info items are printed
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@::software_info=(
#    Field name           Field label        Display in detail box?
    ['sw_os'            , 'Operating System', 1 ],
    ['sw_compiler'      , 'Compiler'        , 1 ],
#   ['sw_auto_parallel' , 'Auto Parallel'   , 1 ], Only for non-MPI2007
    ['sw_file'          , 'File System'     , 1 ],
    ['sw_state'         , 'System State'    , 1 ],
    ['sw_base_ptrsize'  , 'Base Pointers'   , 1 ],
    ['sw_peak_ptrsize'  , 'Peak Pointers'   , 1 ],
    ['sw_other'         , 'Other Software'  , 1 ],
);

if ($::lcsuite ne 'mpi2007') {
  # MPI2007 doesn't believe in AUTOMATIC parallelism
  splice @::software_info, 2, 0, 
    ['sw_auto_parallel' , 'Auto Parallel'   , 1 ];
}

# Only *some* of the extra_info items are printed, and only some of the time!
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@::extra_info = (
	       [ 'test_sponsor' ,  'Test Sponsor'    ], # Submits the result
	       [ 'license_num'  ,  'License Number'  ],
	       [ 'tester'       ,  'Tested by'       ], # Runs the test
	       [ 'test_date'    ,  'Test Date'       ],
	       [ 'hw_avail'     ,  'Hardware Avail'  ],
	       [ 'sw_avail'     ,  'Software Avail'  ],
	       [ 'prepared_by'  ,  'Preparer Name'   ],
);
if ($::lcsuite ne 'mpi2007') {
    push @::extra_info,
               [ 'hw_vendor'    ,  'Hardware Vendor' ],
               [ 'hw_model'     ,  'Model Name'      ];
} else {
    push @::extra_info,
               [ 'system_vendor',  'System Vendor'   ],
               [ 'system_name'  ,  'System Name'     ];
}

# MPI system description info
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
%::mpi_info = (
  'node'         => [
                      [ 'label'             , 'System Label'                 ],
                      [ 'order'             , 'Place in Node List'           ],
                      [ 'count'             , 'Number of nodes'       , 'hw' ],
                      [ 'purpose'           , 'Uses of the node'      , 'hw' ],
                      [ 'hw_vendor'         , 'Vendor'                , 'hw' ],
                      [ 'hw_model'          , 'Model'                 , 'hw' ],
                      [ 'hw_cpu_name'       , 'CPU Name'              , 'hw' ],
                      [ 'hw_ncpuorder'      , 'CPU(s) orderable'      , 'hw' ],
                      [ 'hw_nchips'         , 'Chips enabled'         , 'hw' ],
                      [ 'hw_ncores'         , 'Cores enabled'         , 'hw' ],
                      [ 'hw_ncoresperchip'  , 'Cores per chip'        , 'hw' ],
                      [ 'hw_nthreadspercore', 'Threads per core'      , 'hw' ],
                      [ 'hw_cpu_char'       , 'CPU Characteristics'   , 'hw' ],
                      [ 'hw_cpu_mhz'        , 'CPU MHz'               , 'hw' ],
                      [ 'hw_pcache'         , 'Primary Cache'         , 'hw' ],
                      [ 'hw_scache'         , 'Secondary Cache'       , 'hw' ],
                      [ 'hw_tcache'         , 'L3 Cache'              , 'hw' ],
                      [ 'hw_ocache'         , 'Other Cache'           , 'hw' ],
                      [ 'hw_memory'         , 'Memory'                , 'hw' ],
                      [ 'hw_disk'           , 'Disk Subsystem'        , 'hw' ],
                      [ 'hw_other'          , 'Other Hardware'        , 'hw' ],
                      # The ! entries are special because they will be
                      # replicated as many times as necessary to cover the
                      # number of adapters.
                      [ 'hw_adapter_!_model'       , 'Adapter'           , 'hw,sw' ],
                      [ 'hw_adapter_!_count'       , 'Number of Adapters', 'hw' ],
                      [ 'hw_adapter_!_slot_type'   , 'Slot Type'         , 'hw' ],
                      [ 'hw_adapter_!_data_rate'   , 'Data Rate'         , 'hw' ],
                      [ 'hw_adapter_!_ports_used'  , 'Ports Used'        , 'hw' ],
                      [ 'hw_adapter_!_interconnect', 'Interconnect Type' , 'hw' ],
                      [ 'hw_adapter_!_driver'      , 'Adapter Driver'    , 'sw' ],
                      [ 'hw_adapter_!_firmware'    , 'Adapter Firmware'  , 'sw' ],
                      [ 'sw_os'             , 'Operating System'      , 'sw' ],
                      [ 'sw_localfile'      , 'Local File System'     , 'sw' ],
                      [ 'sw_sharedfile'     , 'Shared File System'    , 'sw' ],
                      [ 'sw_state'          , 'System State'          , 'sw' ],
                      [ 'sw_other'          , 'Other Software'        , 'sw' ],
                    ],
  'interconnect' => [
                      [ 'label'       , 'Interconnect Label'       ],
                      [ 'order'       , 'Place in List'            ],
                      [ 'hw_vendor'   , 'Vendor'            , 'hw' ],
                      [ 'hw_model'    , 'Model'             , 'hw' ],
                      # The ! entries are special because they will be
                      # replicated as many times as necessary to cover the
                      # number of switches.
                      [ 'hw_switch_!_model'    , 'Switch Model'      , 'hw' ],
                      [ 'hw_switch_!_count'    , 'Number of Switches', 'hw' ],
                      [ 'hw_switch_!_ports'    , 'Number of Ports'   , 'hw' ],
                      [ 'hw_switch_!_data_rate', 'Data Rate'         , 'hw' ],
                      [ 'hw_switch_!_firmware' , 'Firmware'          , 'hw' ],
                      [ 'hw_topo'     , 'Topology'          , 'hw' ],
                      [ 'purpose'     , 'Primary Use'       , 'hw' ],
                    ],
  'hardware'     => [
                      [ 'system_class'     , 'Type of System'         , 1    ],
                      [ 'hw_total_nodes'   , 'Total Compute Nodes'    , 1    ],
                      [ 'hw_total_chips'   , 'Total Chips'            , 1    ],
                      [ 'hw_total_cores'   , 'Total Cores'            , 1    ],
                      [ 'hw_total_threads' , 'Total Threads'          , 1    ],
                      [ 'hw_total_memory'  , 'Total Memory'           , 1    ],
                      [ 'base_ranks'       , 'Base Ranks Run'         , 1    ],
                      [ 'min_peak_ranks'   , 'Minimum Peak Ranks'     , 1    ],
                      [ 'max_peak_ranks'   , 'Maximum Peak Ranks'     , 1    ],
                      [ 'idle_avg_watts'   , 'Idle Power (w)'         , 1    ],
                    ],
  'software'     => [
                      [ 'sw_c_compiler'    , 'C Compiler'             , 1    ],
                      [ 'sw_cxx_compiler'  , 'C++ Compiler'           , 1    ],
                      [ 'sw_f_compiler'    , 'Fortran Compiler'       , 1    ],
                      [ 'sw_base_ptrsize'  , 'Base Pointers'          , 1    ],
                      [ 'sw_peak_ptrsize'  , 'Peak Pointers'          , 1    ],
                      [ 'sw_mpi_library'   , 'MPI Library'            , 1    ],
                      [ 'sw_mpi_other'     , 'Other MPI Info'         , 1    ],
                      [ 'sw_preprocessors' , 'Pre-processors'         , 1    ],
                      [ 'sw_other'         , 'Other Software'         , 1    ],
                    ],
);

# Match the node/interconnect prefixes that can be used to
# describe an MPI2007 setup.
# First is one that saves the type and tag
$::mpi_desc_re    = qr/^(node|interconnect)_([a-zA-Z0-9]+)_/;
# ...and a faster one that doesn't
$::mpi_desc_re_id = qr/^(?:node|interconnect)_[a-zA-Z0-9]+_/;

# The various different notes sections, and the order in which they appear
# Note that the name MUST begin with 'notes', or all sorts of other things
# will break.
@::notes_info = (
    [ 'notes_comp',	'Compiler Invocation Notes'	        	],
    [ 'notes_port',	'Portability Notes'		        	],
    [ 'notes_base',	'Base Tuning Notes'		        	],
    [ 'notes_peak',	'Peak Tuning Notes'		        	],
    [ 'notes_auto2par',	'Automatic Parallelization Notes' 	  	],
    [ 'notes_submit',	'Submit Notes'			 	  	],
    [ 'notes_os',	'Operating System Notes'	        	],
    [ 'notes_plat',	'Platform Notes'		        	],
    [ 'notes_part',	'Component Notes'		        	],
    [ 'notes',		'General Notes', qr/^($::mpi_desc_re_id?notes)_*(.*?)_*([0-9]*[0-9])$/io ],
);

# Fill in the regexps for the ones that don't already have it
foreach my $notes_info_ref (@::notes_info) {
    # Make the regexps for them ahead of time
    next unless ref($notes_info_ref) eq 'ARRAY';
    next if defined($notes_info_ref->[2]);
    my $tag = $notes_info_ref->[0];
    $notes_info_ref->[2] = qr/^($::mpi_desc_re_id?${tag})_*(.*?)_*([0-9]*[0-9])$/i;
}

@::notes_regexps = map { $_->[2] } @::notes_info;
if ($::lcsuite ne 'mpi2007') {
    # Simplicity itself
    @::field_order = map { $_->[0] } (@::hardware_info, @::software_info, @::extra_info);
} else {
    # XXX This really isn't much of a fix -- the node and adapter tags would
    # XXX still need to be fixed up.  At least this way there's something
    # XXX TO fix up.
    my %tmp_info = ( 'hw' => [ @::hardware_info, @{$::mpi_info{'hardware'}} ],
                     'sw' => [ @::software_info, @{$::mpi_info{'software'}} ],
                     'extra' => [ @::extra_info ] );
    @::field_order = ();
    foreach my $type (qw(hw sw extra)) {
        push @::field_order, map { $_->[0] } @{$tmp_info{$type}};
        foreach my $thing (qw(node interconnect)) {
            push @::field_order, map { "${thing}_!_".$_->[0] } grep { $_->[2] =~ /$type/ } @{$::mpi_info{$thing}};
        }
    }
}

# Regexp to match informational field tags
$::info_re = qr/^$::mpi_desc_re_id?(?:hw_|sw_|license_num|test.|prepared|notes|flagsurl|label|interconnect|count|order|purpose|system_|nc|nc_is_na|nc_is_cd)/o;

# Fields which are allowed to be empty (i.e. will not be set to '--')
%::empty_fields = ('hw_cpu_char' => 1);

# Fields which are generated, and should therefore not be output to raw or
# back-propagated into config files.
# A value of "2" means that the value is always generated, even on reformatting,#  but that the value is also put in the raw file (below the fence) for use by
#  the indexer (and anything else that might want to look at it).
# A value of "1" means that the value is always generated, even on reformatting
# A value of "0" means that the value is generated once (at run time) and will
#  be output to the raw file, where it is editable.  (It must still be present
#  in the result object to be output... obviously.)
%::generated_fields = (
                       'hw_ncpu'            => 1,
                       'hw_total_nodes'     => 2,
                       'hw_total_chips'     => 2,
                       'hw_total_cores'     => 2,
                       'hw_total_threads'   => 2,
                       'hw_total_memory'    => 2,
                       'max_ranks'          => 2,
                       'max_peak_ranks'     => 2,
                       'min_peak_ranks'     => 2,
                       'test_date'          => 0,
                      );

sub new {
    my ($class, $config, %opts) = @_;
    my $me       = bless {}, $class;

    no strict 'refs';
    $me->{'name'}           = ${"${class}::name"};
    $me->{'units'}          = ${"${class}::units"};
    $me->{'metric'}         = ${"${class}::metric"};
    $me->{'benchmarklist'}  = [@{"${class}::benchmarks"}];
    # Don't make benchsets with no benchmarks!
    if (@{$me->{'benchmarklist'}} == 0 && $opts{'empty_ok'} != 1) {
        ::Log(0, "\nERROR: Benchset \"$me->{'name'}\" contains no benchmarks!\n");
        return undef;
    }
    $me->{'output'}         = defined(${"${class}::output"}) ? ${"${class}::output"} : 0;
    $me->{'no_output'}      = defined(${"${class}::no_output"}) ? ${"${class}::no_output"} : {};
    $me->{'mach'} = 'default';
    $me->{'ext'}  = 'default';
    $me->{'size'} = 'default';
    $me->{'size_class'} = 'default';
    $me->{'rate'} = 0;
    $me->{'time'} = time;
    $me->{'mean_anyway'} = istrue($config->mean_anyway);
    $me->{'errors'} = [];
    $me->{'calc_errors'} = [];
    $me->{'config'} = $config;
    $me->{'review'} = istrue($config->review);
    $me->{'valid'} = 'X';
    $me->{'power'} = 0;
    $me->{'speed_multiplier'} = defined(${"${class}::speed_multiplier"}) ? ${"${class}::speed_multiplier"} : $::speed_multiplier;
    $me->{'rate_multiplier'} = defined(${"${class}::rate_multiplier"}) ? ${"${class}::rate_multiplier"} : $::rate_multiplier;

    # Workloads to use for reportable runs, as well as for the mandatory test
    # and train runs.  Note that this does _not_ constrain the workload with
    # which binaries can be trained.
    foreach my $workload (qw(ref train test)) {
        $me->{$workload} = defined(${"${class}::$workload"}) ? ${"${class}::$workload"} : $workload;
    }

    my $tmp = {};
    for (@{$me->{'benchmarklist'}}) {
	$tmp->{$_}++;
    }
    $me->{'benchmarks'}     = $tmp;
    $me->{'refs'} = [ $me ];

    return $me;
}

sub info_format {
    my ($me, @format) = @_;
    my @rc = ();
    for my $format (@format) {
	next if (ref($format) ne 'ARRAY');
	if (ref($format[0]) eq 'ARRAY') {
	    for my $ref (grep { defined($_->[2]) ? $_->[2] : 1 } @$format) {
                # XXX This seems a hokey place to put this exclusion.
                next if (!istrue($me->power) && $ref->[0] =~ /watts/);
		my @keys  = $me->match_keys($ref->[0]);
		my @field = ($ref->[1]);
		if (@keys) {
		    for my $key (@keys) {
			my $val = $me->accessor($key);
			my @vals = ($val);
			@vals = grep { defined } @$val if ::isa($val, 'ARRAY');
			for $val (@vals) {
                            if (::isa($val, 'HASH')) {
                                push @field, $val;
                            } elsif ($val =~ /^\s*$/o) {
				# It contains only whitespace, which would
				# get stomped by the split.
				push @field, ' ';
			    } else {
				push @field, split(/\n/, $val);
			    }
			}
		    }
		} else {
		    push @field, exists($::empty_fields{$ref->[0]}) ? '' : '--';
		}
		push (@rc, [@field]);
	    }
	} elsif (!defined($format->[2]) ||
                 (defined($format->[2]) && $format->[2])) {
	    my @keys  = $me->match_keys($format->[0]);
	    my @field = ($format->[1]);
	    if (@keys) {
		for my $key (@keys) {
		    push (@field, split(/\n/, $me->accessor($key)));
		}
	    } else {
		push @field, exists($::empty_fields{$format->[0]}) ? '' : '--';
	    }
	    push (@rc, [@field]);
	}
    }
    return @rc;
}

sub hardware {
    my ($me) = @_;
    return $me->info_format(\@::hardware_info);
}

sub software {
    my ($me) = @_;
    return $me->info_format(\@::software_info);
}

sub notes {
    my ($me, $system) = @_;
    $system = '' unless defined($system);

    # What a pain these notes are!
    # Okay, each notes item will be a hash with arbitrary keys.  Each of
    # the items in the hash is an array, just like the old notes used to
    # be.  The notes items will be presented in order of appearance in the
    # array.  The arrays (values in the hashes) will be processed in the
    # lexical order of the arbitrary keys in the hashes.

    my @notes = ();
    foreach my $sectionref (@::notes_info) {
        my $notesref = $me->notes_section($system, $sectionref->[0]);
        push @notes, [ $sectionref->[1], $notesref ] if (@{$notesref});
    }
    return \@notes;
}

sub notes_section {
    my ($me, $system, $section) = @_;
    my @notes = ();

    # See the comments for 'notes' above.

    my $notesref = $me->accessor_nowarn($system.$section);
    return \@notes unless ref($notesref) eq 'HASH';
    for my $key (sort keys %{$notesref}) {
        next unless ref($notesref->{$key}) eq 'ARRAY';
        for my $note (@{$notesref->{$key}}) {
            if (ref($note) eq 'ARRAY') {
                push @notes, $note->[1];
            } else {
                push @notes, $note;
            }
        }
    }
    return \@notes;
}

sub baseunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    return $me->units . $rate . '_base' . $::year;
}

sub peakunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    if ($::lcsuite eq 'mpi2007') {
        return $me->units . $rate. '_peak' . $::year;
    } else {
        return $me->units . $rate. $::year;
    }
}

sub datestr {
    my ($me) = shift;
    my $tmp = main::ctime($me->{'time'});
    $tmp =~ tr/\015\012//d;
    return $tmp;
}

sub errors {
    my ($me) = shift;
    my @errors;

    push (@errors, @{$me->{'errors'}}) if ref $me->{'errors'} eq 'ARRAY';

    my $ref = $me->{'results'};
    for my $bench (sort keys %$ref) {
	for my $tune (sort keys %{$ref->{$bench}}) {
	    next if ref($ref->{$bench}{$tune}{'data'}) ne 'ARRAY';
	    for my $res (@{$ref->{$bench}{$tune}{'data'}}) {
		for (@{$res->{'errors'}}) {
		    push (@errors, "Error $bench: $_");
		}
	    }
	}
    }

    grep (s/\s+$//,@errors);

    return @errors;
}

sub invalid_results {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';

    my @which = ();
    for my $bench (keys %{$me->benchmarks}) {
	next unless (   exists $me->{'results'}{$bench}
                     && exists $me->{'results'}{$bench}{$tune}
                     && exists $me->{'results'}{$bench}{$tune}{'data'}
                     && ::isa($me->{'results'}{$bench}{$tune}{'data'}, 'ARRAY'));
        foreach my $res (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
            next unless ::isa($res, 'HASH');
            if ($res->{'valid'} ne 'S') {
                push @which, $bench;
                last;
            }
        }
    }
    return @which;
}

sub insufficient_data {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';

    my @which = ();
    for my $bench (keys %{$me->benchmarks}) {
	if (!exists $me->{'results'}{$bench} ||
            !exists $me->{'results'}{$bench}{$tune} ||
            !exists $me->{'results'}{$bench}{$tune}{'data'} ||
	    ref($me->{'results'}{$bench}{$tune}{'data'}) ne 'ARRAY' ||
	    @{$me->{'results'}{$bench}{$tune}{'data'}}+0 < $main::global_config->min_report_runs) {
	    push (@which, $bench);
	}
    }
    return @which;
}

sub add_results {
    my ($me, $bench, $config) = @_;
    my @tunes = ($bench->tune);

    for my $tune (@tunes) {
	my @tmp;
	if (istrue($me->rate)) {
	    @tmp = $bench->result_list;
	} else {
	    @tmp = $bench->result_list(1);
	}
        foreach my $rec (@tmp) {
            # Clean out the power/temp meter raw records
            delete $rec->{'poweranalyzerrawrecords'};
            delete $rec->{'tempmeterrawrecords'};
        }
	push (@{$me->{'results'}{$bench->benchmark}{$tune}{'data'}}, @tmp);
	$me->{'compile_options'}->{$bench->benchmark}->{$tune} = $bench->accessor_nowarn('compile_options');
        my $baggage = $bench->accessor_nowarn('baggage');
	push @{$me->{'baggage'}}, $baggage if (defined($baggage) && $baggage ne '');
	$me->{'results'}->{$bench->benchmark}->{$tune}->{'flags'} = $config->{$bench->benchmark}->{$tune}->{'flags'};
        $me->{'submit'}++ if (grep { $_->{'submit'} > 0 } @tmp);
    }
    $me->{'benchmarks'}->{$bench->benchmark}->{'basepeak'} = $bench->basepeak;
}

sub reference {
    my ($me, $bench) = @_;
    return $me->{'reference'}{$bench};
}

sub valid {
    my ($me, $bench, $tune) = @_;
    return 0 unless exists $me->{'results'}{$bench};
    return 0 unless exists $me->{'results'}{$bench}{$tune};
    my $valid = '?';
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	$valid = $_->{'valid'};
    }
    return $valid;
}

sub ratio {
    my ($me, $bench, $tune) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'ratio'};
	$count++;
    }
    $rc /= $count if $count;
    return $rc;
}

sub copies {
    my ($me, $bench, $tune) = @_;

    # Base # copies are always the same
    my $copies = $me->accessor_nowarn('base_copies');
    return $copies if (defined($copies) && $tune eq 'base');

    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
        if (defined($_->{'clcopies'}) && $_->{'clcopies'} ne '') {
            return $_->{'clcopies'};
        } else {
            return $_->{'copies'};
        }
    }
    return '';
}

sub ranks {
    my ($me, $bench, $tune) = @_;

    # Base # ranks are always the same
    return $me->base_ranks if ($tune eq 'base');

    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
        if (defined($_->{'clranks'}) && $_->{'clranks'} ne '') {
            return $_->{'clranks'};
        } else {
            return $_->{'ranks'};
        }
    }
    return '';
}

sub runtime {
    my ($me, $bench, $tune, $round) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'reported_time'};
	$count++;
    }
    if ($count) {
	$rc /= $count;
	$rc = int($rc + 0.5) if ($round);
    }
    return $rc;
}

sub calc_mean_rate {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = scalar($me->insufficient_data($tune)) == 0;

    my $per_copy = {};
    for my $bench (keys %{$me->benchmarks}) {
        next unless exists($me->{'results'}->{$bench});
        next unless exists($me->{'results'}->{$bench}->{$tune});
        next unless exists($me->{'results'}->{$bench}->{$tune}->{'data'});
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next unless ($obj->valid eq 'S');
	    push (@{$per_copy->{$obj->copies}{$bench}{'data'}}, $obj);
	}
    }
    my $num_benchmarks = (keys %{$me->benchmarks})+0;
    if ($tune eq 'base') {
	my $nextbest_product = 0;
	my $nextbest_copies  = 0;
	my $best_product = 0;
	my $best_copies  = 0;
        my $lowdp = undef;
	for my $copies (sort { $a <=> $b } keys %$per_copy) {
	    my $copyref = $per_copy->{$copies};
	    my $product = 1;
	    my $count = 0;
	    my $valid = 'S';
	    for my $bench (keys %{$me->benchmarks}) {
		if (! exists $per_copy->{$copies}{$bench}) {
		    $valid = 'X';
		    next;
		}
		my $benchref = $per_copy->{$copies}{$bench};
		$valid = 'X' if (@{$benchref->{'data'}} < $main::global_config->min_report_runs);
		my ($tmp, $dp) = ::median_ratio(1, @{$benchref->{'data'}});
		if (defined($tmp) && ($tmp > 0)) {
		    $product *= $tmp;
		    $count ++;
                    $lowdp = $dp if (($dp >= 0) && (!defined($lowdp) || ($dp < $lowdp)));
		}
	    }
	    if ($count) {
		$product = ::round($product ** (1/$count), $lowdp);
		if ($valid eq 'X' && $product > $nextbest_product) {
		    $nextbest_product = $product;
		    $nextbest_copies  = $copies;
		}
		if ($product > $best_product) {
		    $best_product = $product;
		    $best_copies  = $copies;
		}
	    }
	}
	if ($best_copies == 0) {
	    if ($main::global_config->action ne 'report') {
                $me->{'valid'} = 'X';
                $me->add_error("There is no set of valid runs with the same number of copies for base");
            }
            $nextbest_product = 0 if $nextbest_product < 0; # For fake reports
            return $nextbest_product if (istrue($main::global_config->mean_anyway) || $sufficient);
            # '--' means 'no'
            return '--';
	}
	return $best_product if (istrue($main::global_config->mean_anyway) || $sufficient);
        return '--';
    } else {
	my $best_product = {};
	my $best_copies = {};
        my $lowdp = undef;
	for my $copies (keys %$per_copy) {
	    for my $bench (keys %{$me->benchmarks}) {
		next unless exists $per_copy->{$copies}{$bench};
		my $benchref = $per_copy->{$copies}{$bench};
		# median_ratio wouldn't do the right thing for benchmarks
                # with copies that varied per-iteration.  But that doesn't
                # happen, so this is safe:
		my ($product, $dp) = ::median_ratio(1, @{$benchref->{'data'}});
		if (defined($product) && ($product > 0) &&
		    ($product > $best_product->{$bench})) {
		    $best_product->{$bench} = $product;
		    $best_copies->{$bench}  = $copies;
                    $lowdp = $dp if (($dp >= 0) && (!defined($lowdp) || ($dp < $lowdp)));
		}
	    }
	}
	my $product = 1;
	my $count = 0;
	for my $bench (keys %{$me->benchmarks}) {
	    if (exists $best_product->{$bench}) {
		$product *= $best_product->{$bench};
		$count ++;
	    } elsif ($main::global_config->action ne 'report') {
		$me->{'valid'} = 'X';
		$me->add_error("Complete set of valid runs for peak rate unavailable ($bench missing)");
	    }
	}
	if ($count) {
	    $product = ::round($product ** (1/$count), $lowdp);
	} else {
	    $product = 0;
	}
	return $product if (istrue($main::global_config->mean_anyway) || $sufficient);
        return '--';
    }
}

sub add_error {
    my $me = shift;
    push (@{$me->{'errors'}}, @_);
}

sub calc_mean_speed {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = $me->insufficient_data($tune) == 0;

    my $product = 1;
    my $count = 0;
    my $lowdp = undef;
    for my $bench (keys %{$me->benchmarks}) {
	my @results = ();
        next unless exists($me->{'results'}->{$bench});
        next unless exists($me->{'results'}->{$bench}->{$tune});
        next unless exists($me->{'results'}->{$bench}->{$tune}->{'data'});
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next if ($obj->valid ne 'S' || $obj->copies != 1);
	    push (@results, $obj);
	}
	my ($tmp, $dp) = ::median_ratio(1, @results);
	if (defined $tmp && $tmp > 0) {
	    $product *= $tmp;
	    $count++;
            $lowdp = $dp if (($dp >= 0) && (!defined($lowdp) || ($dp < $lowdp)));
	}
    }
    if ($count == 0) {
      # '--' means 'no'
      return 0 if (istrue($main::global_config->mean_anyway) || $sufficient);
      return '--';
    }
    $product = ::round($product ** (1/$count), $lowdp);
    return (istrue($main::global_config->mean_anyway) || $sufficient) ? $product : '--';
}

sub bench_in {
    my ($me, $bench) = @_;
    return exists $me->benchmarks->{$bench->benchmark} &&
	    $me->mach eq $bench->mach && $me->ext eq $bench->ext &&
	    $me->size eq $bench->size;
}

sub Log    { main::Log(@_); }
sub jp     { main::joinpaths(@_); }
sub istrue { main::istrue(@_); }
sub src    { my $me = shift; jp($me->path, $me->srcdir); }

sub print_bench {
  # This just lists which benchmarks and tuning levels live in the 'results'
  # item of a result object.  It's just for debugging.
  my ($r, $tag) = @_;

  if (!exists($r->{'results'})) {
    print "$tag: none\n";
    return;
  }
  if (ref($r->{'results'}) ne 'HASH') {
    print "$tag: NOT HASH\n";
    return;
  }
  print "$tag: ";
  foreach my $bench (sort keys %{$r->{'results'}}) {
    print "$bench (";
    if (ref($r->{'results'}->{$bench}) ne 'HASH') {
      print "NOT HASH";
    } else {
      print join(', ', sort keys %{$r->{'results'}->{$bench}});
    }
    print "); ";
  }
  print "\n";
}

1;
