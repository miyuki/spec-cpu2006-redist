#
# locate.pm
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: locate.pl 6500 2011-05-11 12:37:59Z CloyceS $

use strict;
use IO::Dir;
use Time::HiRes qw(time);

require 'vars.pl';
require 'flagutils.pl';

my $version = '$LastChangedRevision: 6500 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'formatter_locate.pl'} = $version;

sub locate_benchmarks {
    my ($config) = @_;

    if (keys %fm::bmarklist == 0) {
        ::Log(0, "\n\nERROR: The formatter has no benchmark information for $::suite\n\n");
        ::do_exit(1);
    }

    $config->{'formats'} = {} if !exists $config->{'formats'};

    my $benchdir = jp($config->top, $config->benchdir);
    my %seen;
    # We're just formatting, so don't bother to look for the
    # benchmark directories; use the hardcoded list instead
    foreach my $bset (keys %fm::bmarklist) {
        my $setclass = "Spec::Benchset::${bset}";
        eval "package $setclass; \@${setclass}::ISA = qw(Spec::Benchset);";
        my $setobj = $setclass->new($config, 'empty_ok' => 1);
        for my $var (keys %{$fm::bmarklist{$bset}}) {
            next if ($var eq 'benchmarklist');
            $setobj->{$var} = $fm::bmarklist{$bset}->{$var};
        }
        $config->{'benchsets'}->{$setobj->name} = $setobj;
        foreach my $bmarkref (@{$fm::bmarklist{$bset}->{'benchmarklist'}}) {
            my ($bmark, $benchlang);
            if (isa($bmarkref, 'ARRAY')) {
              ($bmark, $benchlang) = @{$bmarkref};
            } else {
              $bmark = $bmarkref;
              $benchlang = '?';
            }
            $bmark =~ /(\d+).(.*)/o;
            my ($num, $name) = ($1, $2);
            my $bmarkclass = "Spec::Benchmark::${name}${num}";
            # Strip off leading zeroes to prevent the number from being treated
            # as octal...
            my $tmpnum = $num;
            $tmpnum =~ s/^0+//;
            eval "
                  package $bmarkclass;
                  \@${bmarkclass}::ISA = qw(Spec::Benchmark);
                  \$${bmarkclass}::benchname = \'$name\';
                  \$${bmarkclass}::benchnum = $tmpnum;
                  \$${bmarkclass}::benchlang = \'$benchlang\';
                  \@${bmarkclass}::base_exe = (\'foo\');
                 ";
            if ($@) {
                Log(0, "\nError building benchmark class for '$bmark': $@\n");
                next;
            }
            my $bmobj = $bmarkclass->new("$num.$name", $config, $num, $name);
            push @{$setobj->{'benchmarklist'}}, $bmark;
            $config->{'benchmarks'}->{$bmark} = $bmobj;
            my $flags_file;
            if (defined($::website_formatter) && $::website_formatter) {
              $flags_file = jp($::flag_base, $bmark.'.flags.xml');
            } else {
              $flags_file = jp($benchdir, $setobj->{'topdir'}, $bmark, 'Spec', 'flags.xml');
            }
            if (-e $flags_file) {
                my $objectstarttime = Time::HiRes::time;
                (undef, $config->{'flaginfo'}->{$bmark}) =
                    main::get_flags_file($flags_file, $bmark);
                Log(90, sprintf("    Reading + parsing $flags_file took %8.9fs\n", Time::HiRes::time - $objectstarttime));
            }
            if (!-e $flags_file ||
                !defined($config->{'flaginfo'}->{$bmark})) {
                Log(0, "\nERROR: The flags file for $bmark ($flags_file) could not be parsed.\n");
                do_exit(1);
            }
        }
    }
    my $error = 0;
    for my $set (keys %{$config->{'benchsets'}}) {
	my $obj = $config->{'benchsets'}{$set};
	my $ref = {};
	$config->{'benchsets'}{$set}{'benchmarks'} = $ref;
	my @benchmarks = @{$obj->{'benchmarklist'}};
	for my $bench (@benchmarks) {
	    if (!exists $config->{'benchmarks'}{$bench}) {
		Log(0, "\nBenchmark Set '$set' calls for nonexistent benchmark '$bench'\n");
		$obj->{'valid'} = 'X';
                $error++;
	    } else {
	        $ref->{$bench} = $config->{'benchmarks'}{$bench};
            }
	}
    }
    ::do_exit(1) if $error;
    $config->{'setobjs'} = [ map {$config->{'benchsets'}{$_}} keys %{$config->{'benchsets'}} ];
}

sub locate_formats {
    my ($config, $quiet) = @_;

    my @formats = list_files(jp($config->top, 'bin', 'formats'));
    @formats = grep (m|\/[^.][^/\\]*\.p[lm]$|o, @formats);

    my $logged = 0;
    for my $pm (@formats) { ## for each format .pl file
	my ($name) = $pm =~ m|([^/]+)\.p[lm]$|o;
        my $ok = 0;
        eval "\$ok = ::${name}_ok();";
	$ok = 1 if ($@ && $@ =~ /undefined subroutine/i);
        if (($@ && $@ !~ /undefined subroutine/i) || !$ok) {
            Log(2, ', ') if $logged && !$quiet;
            Log(2, "$name (DISABLED)") unless $quiet;
            Log(8, " [$@]");
            $logged++;
            next;
        }
	eval "package Spec::Format::${name}; \@Spec::Format::${name}::ISA = qw(Spec::Format); require '$pm';";
	if ($@) {
	    Log(102, "\nError requiring $pm for $name format: $@\nContinuing with output formats...");
            $logged = 0;
	    next;
	}

	my $class= "Spec::Format::${name}";

	if (!$class->can('new')) {
	    Log(12, "\nNo 'new' function for class '$class' in '$pm'\n");
	    next;
	}

	## run the creation method for the object, i.e., "new"
	my $obj = $class->new;
	if (!defined($obj) || !ref($obj)) {
	    Log(12, "\nError initializing format object\n");
	    next;
	}
	$config->{'formats'}{$name} = $obj;
        Log(2, ', ') if $logged && !$quiet;
	Log(2, $obj->{'name'}) unless $quiet;
        $logged++;
    }
    Log(2, "\n") unless $quiet;
}

1;
