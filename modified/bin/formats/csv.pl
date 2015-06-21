#
#  csv.pl - produces ASCII CSV output
#  Copyright 2004-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: csv.pl 6364 2011-03-05 00:41:51Z cloyce $

use strict;

use vars qw($name $extension $synonyms);
use Text::CSV_XS;
use UNIVERSAL qw(isa);

$name      = 'CSV';
$extension = 'csv';
$synonyms  = { map { lc($_) => 1 } ($name, qw(spreadsheet)) };

my $csv_version = '$LastChangedRevision: 6364 $ '; # Make emacs happier
$csv_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::csv::non_default = 1; # You must ask for it by name
$Spec::Format::csv::part_of_all = 1; # Included in '-o all'
$::tools_versions{'csv.pl'} = $csv_version;


my $debug = 0;

sub format {
    my($me, $r, $fn) = @_;
    my (@output, @errors);
    my (%seen, $temp, $name, @values, @errmsg);
    my @nc = ::allof($r->{'nc'});
    my $is_nc = 0;
    my $nc_str = '';
    if (@nc) {
        if (istrue($r->{'nc_is_na'})) {
            $is_nc = 2; # NA
            $nc_str = 'NA';
        } elsif (istrue($r->{'nc_is_cd'})) {
            $is_nc = 3; # CD
            $nc_str = 'CD';
        } else {
            $is_nc = 1; # NC
            $nc_str = 'NC';
        }
    }

    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));
    my $csv = new Text::CSV_XS({
                                'binary' => 1,
                                'always_quote' => 0,
                               });

    my $sizestr = ($r->{'size'} eq $r->{'size_class'}) ? $r->{'size'} : "$r->{'size'}($r->{'size_class'})";

    if ($invalid) {
	# The \# are for emacs' benefit
	push @errors, csv_string($csv, '');
	push @errors, csv_string($csv, 'ERRORS');
	push @errors, csv_string($csv, '');
	push @errors, csv_string($csv, '#' x 78);
	push @errors, csv_string($csv, sprintf("#   %-72s \#", 'INVALID RUN ' x 6));
	push @errors, csv_string($csv, sprintf("# %-74s \#", ''));

	for ($r->errors) {
	    push @errors, csv_string($csv, sprintf("# %-74s \#", $_));
	}

	push @errors, csv_string($csv, sprintf("# %-74s \#", ''));
	push @errors, csv_string($csv, sprintf("# %-74s \#", '  ' . 'INVALID RUN ' x 6));
	push @errors, csv_string($csv, '#' x 78);
    }

    # Output validity information as the first line
    my @valid_line = ('valid', ($invalid || $is_nc) ? '0' : '1');
    if ($is_nc == 3) {
        push @valid_line, '(CODE DEFECT)';
    } elsif ($is_nc == 2) {
        push @valid_line, '(NOT AVAILABLE)';
    } elsif ($is_nc) {
        push @valid_line, '(NON-COMPLIANT)';
    }
    push @output, csv_string($csv, @valid_line);

    # Make a list of possible tuning levels
    my @tunelist = grep { !/^base$/oi } @{$::global_config->valid_tunes};
    unshift @tunelist, 'base';

    # Now do the column headings
    my $bmode;
    if ($::lcsuite eq 'mpi2007') {
      $bmode = '# Ranks';
    } else {
      $bmode = istrue($r->rate) ? '# Copies' : 'Ref Time';
    }
    my $rmode = istrue ($r->rate) ? 'Rate' : 'Ratio';
    my $est = $invalid ? 'Est. ' : '';

    my @headings = ('Benchmark');
    foreach my $tune (@tunelist) {
      my $label = ucfirst($tune);
      # Do the headings
      push @headings, "$label $bmode", "${est}$label Run Time", "${est}$label $rmode", "$label Selected", "$label Status";
    }
    push @headings, 'Description';

    push @output, '', csv_string($csv, 'Full Results Table'), '';
    push @output, csv_string($csv, @headings);

    # And do the data
    my $table    = {};
    my $results  = {};
    my %benchseen = ();
    my %tuneseen = ();

    # Scan through the list of results to figure out the maximum number of
    # iterations.  Otherwise the base holes in peak-only results cannot be
    # made.
    my $iter = 0;
    for my $bench (sort keys %{$r->{'results'}}) {
      my $benchres = $r->{'results'}{$bench};
      for my $tune (@tunelist) {
	next unless ::check_elem('ARRAY', $benchres, $tune, 'data');
	$benchres = $r->{'results'}{$bench}{$tune}{'data'};
	$iter = @{$benchres} if ($iter < @{$benchres});
      }
    }

    # Go through the benchmarks that have results.
    my %selected = ();
    for my $bench (sort keys %{$r->benchmarks}) {
        if (!::check_elem('HASH', $r, 'results', $bench)) {
            # No results for this benchmark, so make up a whole bunch of
            # empty rows
            for(my $i = 0; $i < $iter; $i++) {
                push @output, csv_string($csv, $bench, ('','','','','NR') x (@tunelist+0));
            }
            next;
        }
        if ($is_nc) {
            # No compliant results for this benchmark
            for(my $i = 0; $i < $iter; $i++) {
                my @cols = ();
                foreach my $tune (@tunelist) {
                    if (::check_elem('ARRAY', $r, 'results', $bench, $tune, 'data')) {
                        # There's data; mark it appropriately;
                        push @cols, $nc_str, $nc_str, $nc_str, $nc_str, $nc_str;
                    } else {
                        # No data; leave it blank
                        push @cols, '', '', '', '', '';
                    }
                }
                push @output, csv_string($csv, $bench, @cols);
            }
            next;
        }
        my $benchres = $r->{'results'}{$bench};
	my $reslist = [];
	foreach(my $j = 0; $j < @tunelist; $j++) {
            my $tune = $tunelist[$j];
	    if (::check_elem('ARRAY', $r, 'results', $bench, $tune, 'data')) {
		$benchres = $r->{'results'}{$bench}{$tune}{'data'};
	    } else {
		$benchres = [ ];
	    }
	    
	    for(my $i = 0; $i < $iter; $i++) {
		if (defined($benchres->[$i])) {
                    my $res = $benchres->[$i];
                    # If we don't get here, we haven't "seen" them...
                    $benchseen{$bench} = 1 unless exists $benchseen{$bench};
                    $tuneseen{$tune} = 1 unless exists $tuneseen{$tune};
                    my @numbers;
                    if ($::lcsuite eq 'mpi2007') {
                        @numbers = ($res->ranks,     $res->reported_time, $res->ratio, $res->selected, $res->valid);
                    } elsif (istrue($r->rate)) {
                        @numbers = ($res->copies,    $res->reported_time, $res->ratio, $res->selected, $res->valid);
                    } else {
                        @numbers = ($res->reference, $res->reported_time, $res->ratio, $res->selected, $res->valid);
                    }
                    push @{$reslist->[$i]}, @numbers;
                    $selected{$bench}->{$tune} = [ $i+1, @numbers ] if istrue($res->selected);
                } else {
		    push @{$reslist->[$i]}, ('') x 5;
                }
                push @{$reslist->[$i]}, "$sizestr iteration #".($i+1) if $j == (@tunelist - 1);
	    }
	}
	foreach my $res (@{$reslist}) {
	    push @output, csv_string($csv, $bench, @{$res});
	}
    }
    push @output, csv_string($csv, '');

    # Now do just the selected revisions
    push @output, csv_string($csv, 'Selected Results Table'), '';
    push @output, csv_string($csv, @headings);
    for my $bench (sort keys %{$r->benchmarks}) {
        my $desc = 'SelectedIteration (';
        my @res = ();
        for(my $i = 0; $i < @tunelist; $i++) {
            if (@nc > 0 && ::check_elem('ARRAY', $r, 'results', $bench, $tunelist[$i], 'data')) {
                # Non-compliant results
                push @res, (($nc_str) x 5);
            } elsif (!exists($selected{$bench}) ||
                !exists($selected{$bench}->{$tunelist[$i]}) ||
                !isa($selected{$bench}->{$tunelist[$i]}, 'ARRAY')) {
                # This can happen if (for example) you only run base
                push @res, (('') x 4), 'NR';
            } else {
                my ($iter, @numbers) = @{$selected{$bench}->{$tunelist[$i]}};
                $desc .= '; ' if ($i > 0);
                $desc .= "$tunelist[$i] \#$iter";
                push @res, @numbers;
            }
        }
        if (grep { $_ > 0 } @res) {
            # Only mark selected benchmark runs as 'SelectedIteration'
            push @res, $desc.')';
        }
        push @output, csv_string($csv, $bench, @res);
    }
    push @output, csv_string($csv, '');

    # Now the rest of the stuff that nobody using CSV is interested in...

    # Now the means
    # This seems kind of dumb; maybe it'll be changed someday
    my %means = ( 'base' => [ $r->baseunits, $r->basemean ],
                  'peak' => [ $r->peakunits, $r->peakmean ] );
    my $pad = 1;
    foreach my $tune (@tunelist) {
        if (exists($means{$tune})) {
            my ($unit, $mean) = @{$means{$tune}};
            $mean = $nc_str if (@nc > 0 && $mean =~ /\d/);
            push @output, csv_string($csv, $unit, $mean, ('') x $pad, $mean);
            $pad += 5;
        }
    }
    push @output, '';

    # Identifying information
    push @output, csv_string($csv, 'Run number:', $r->accessor_nowarn('lognum'));
    push @output, csv_string($csv, '');

    # The system description
    my %id_map = ($::lcsuite eq 'mpi2007') ?
                    (
                      'vendor' => 'system_vendor',
                      'vendor_title'  => 'System Vendor:',
                      'model'  => 'system_name',
                      'model_title'  => 'System Name:',
                    ) :
                    (
                      'vendor' => 'hw_vendor',
                      'vendor_title'  => 'Hardware Vendor:',
                      'model'  => 'hw_model',
                      'model_title'  => 'Hardware Model:',
                    );
    push @output, csv_strings($csv, $id_map{'vendor_title'}, join(' ', ::allof($r->accessor_nowarn($id_map{'vendor'}))));
    push @output, csv_strings($csv, $id_map{'model_title'}, join(' ', ::allof($r->accessor_nowarn($id_map{'model'}))));
    push @output, csv_strings($csv, 'Date tested:',     $r->test_date);


    # Note some important stuff
    push @output, csv_strings($csv, "$::suite License:" , join(' ', ::allof($r->accessor_nowarn('license_num'))));
    push @output, csv_strings($csv, 'Test sponsor:'     , join(' ', ::allof($r->accessor_nowarn('test_sponsor'))));
    push @output, csv_strings($csv, 'Tested by:'        , join(' ', ::allof($r->accessor_nowarn('tester'))));
    push @output, csv_strings($csv, 'Hardware avail:'   , join(' ', ::allof($r->accessor_nowarn('hw_avail'))));
    push @output, csv_strings($csv, 'Software avail:'   , join(' ', ::allof($r->accessor_nowarn('sw_avail'))));
    push @output, csv_string($csv, '');

    # Note the reason for NC, if any.
    if ($is_nc) {
        if ($is_nc == 3) {
            push @output, csv_string($csv, 'EXPLANATION OF CODE DEFECT');
        } elsif ($is_nc == 2) {
            push @output, csv_string($csv, 'REASON FOR NON-AVAILABILITY');
        } else {
            push @output, csv_string($csv, 'REASON FOR NON-COMPLIANCE');
        }
	push @output, csv_string($csv, '---------------------------------------------------------------------------');
	foreach my $ncline (@nc) {
	    push @output, csv_string($csv, $ncline);
	}
	push @output, csv_string($csv, '---------------------------------------------------------------------------');
	push @output, csv_string($csv, '');
    }

    if ($::lcsuite ne 'mpi2007') {
        push @output, format_info('HARDWARE', $csv, [ $r->hardware ]);
        push @output, format_info('SOFTWARE', $csv, [ $r->software ]);
    } else {
        # MPI is very very special
        foreach my $item (qw(node interconnect)) {
            next unless exists($r->{$item}) && isa($r->{$item}, 'HASH');
            my $iref = $r->{$item};

            # Get a list of things; interconnects are ordered primarily by
            # 'order' and secondarily by 'label' (lexically).  Nodes are the
            # same, but the most primary key is whether or not purpose contains
            # "compute".
            my @itemlist;
            if ($item eq 'node') {
                @itemlist = sort {
                     $iref->{$a}->{'purpose'} !~ /compute/i <=> $iref->{$b}->{'purpose'} !~ /compute/i ||
                     $iref->{$a}->{'order'} <=> $iref->{$b}->{'order'} ||
                     $iref->{$a}->{'label'} cmp $iref->{$b}->{'label'}
                                     } keys %{$iref};
            } else {
                @itemlist = sort {
                     $iref->{$a}->{'order'} <=> $iref->{$b}->{'order'} ||
                     $iref->{$a}->{'label'} cmp $iref->{$b}->{'label'}
                                     } keys %{$iref};
            }

            foreach my $system (@itemlist) {
                push @output, '';
                my $label = ucfirst($item).' Description: '.$iref->{$system}->{'label'};
                push @output, csv_string($csv, $label);

                my ($hw_info, $sw_info) = ::mpi_info_munge($r, $item, $system);

                push @output, format_info('HARDWARE', $csv, [ $r->info_format($hw_info) ]);
                push @output, format_info('SOFTWARE', $csv, [ $r->info_format($sw_info) ]);

                # Do the notes for this thing...
                my @notes = @{$r->notes("${item}_${system}_")};
                push @output, '';
                foreach my $sectionref (@notes) {
                    my ($section, $notesref) = @{$sectionref};
                    next unless isa($notesref, 'ARRAY');
                    push @output, format_info($section, $csv, $notesref);
                }
            }
        }

        # And now, the "benchmark details"
        push @output, format_info('BENCHMARK DETAILS', $csv, [ $r->info_format($::mpi_info{'hardware'}), $r->info_format($::mpi_info{'software'}) ]);
    }

    my @notes = @{$r->notes};
    foreach my $sectionref (@notes) {
        my ($section, $notesref) = @{$sectionref};
        next unless ref($notesref) eq 'ARRAY';
        push @output, format_info($section, $csv, $notesref);
    }

    push @output, csv_flags($csv, $r, @tunelist);

    push @output, @errors;

    push @output, csv_string($csv, '----------------');
    push @output, csv_string($csv, 'For questions about',
                                   'this result, please',
                                   'contact the tester.',
                                   '');
    push @output, csv_string($csv, 'For other inquiries,',
                                   'please contact',
                                   'webmaster@spec.org.',
                                   '');
    push @output, csv_string($csv, 'Copyright '.::copyright_dates().' Standard',
                                   'Performance Evaluation',
                                   'Corporation',
                                   '');
    push @output, csv_string($csv, "Tested with SPEC $::suite ",
                                   'v'.$r->{'suitever'}.'.  Report',
                                   'generated on ',
                                   &::ctime(time),
                                   "by SPEC $::suite CSV",
                                   "formatter v$csv_version.");

    return (\@output, []);
}

sub format_info {
    my ($title, $csv, $ref) = @_;
    return () if (ref($ref) ne 'ARRAY') || !@$ref;
    my $isnotes = ($title =~ / NOTES/i);

    my @output;
    push @output, csv_string($csv, '');
    push @output, csv_string($csv, $title);
    push @output, csv_string($csv, '');
    foreach my $item (@{$ref}) {
        my ($name, @vals);
        if (ref($item) eq 'ARRAY') {
          ($name, @vals) = @$item;
        } elsif ($isnotes) {
          $name = '';
          @vals = munge_links($item);
        } else {
          # Ignore it
          next;
        }

        if (!@vals) {
            if (!$isnotes) {
                push @output, csv_string($csv, "${name}:", $name);
            } else {
                push @output, csv_string($csv, '--');
            }
        } else {
            my $val = shift @vals;
            if (!$isnotes) {
                push @output, csv_string($csv, $name, $val);
            } else {
                push @output, csv_string($csv, $val);
            }

            while (@vals) {
                $val = shift @vals;
                if (ref $val eq '') {
                    if (!$isnotes) {
                        push @output, csv_string($csv, '', $val);
                    } else {
                        push @output, csv_string($csv, $val);
                    }
                } elsif (ref $val eq 'ARRAY') {
                    unshift (@vals, @{$val});
                }
            }
        }
    }

    return @output;
}

sub csv_flags {
    my ($csv, $r, @tunes) = @_;
    my @output = ();

    # Output the flags as a straight dump.  In order to ensure that everything
    # is in the same relative position, always dump flags for each benchmark
    # for ALL classes.
    my %class2title = ( 'compiler' => 'Compiler Invocation',
                        'portability' => 'Portability Flags',
                        'optimization' => 'Optimization Flags',
                        'other' => 'Other Flags',
                        'forbidden' => 'Forbidden Flags',
                        'unknown' => 'Unknown Flags',
                        );
    foreach my $tune (@tunes) {
        foreach my $class (qw(compiler portability optimization other
                              forbidden unknown)) {
            my $title = ucfirst($tune).' '.$class2title{$class};
            push @output, '', csv_string($csv, $title);
            for my $bench (sort keys %{$r->benchmarks}) {
                my @items = ($bench);
                if (::check_elem('ARRAY', $r, 'results', $bench, $tune, 'flags', $class)) {
                    my $flaglist = $r->{'results'}->{$bench}->{$tune}->{'flags'}->{$class};
                    foreach my $flag (@{$flaglist}) {
                        next unless (istrue($flag->[2]->{'display'}) || $r->{'review'});
                        push @items, ' '.$flag->[1];
                    }
                }
                push @output, csv_string($csv, @items);
            }
        }
    }
    return @output;
}

sub csv_string {
    my ($csv, @items) = @_;

    my $status = $csv->combine(@items);
    if ($status) {
	return $csv->string();
    } else {
	Log(0, "Error creating CSV string: ".$csv->error_input()."\n");
	return '';
    }
}

sub csv_strings {
    my ($csv, @items) = @_;
    my @things = ();

    # Things in @items might be array refs; deal with that eventuality.
    foreach my $item (@items) {
        if (isa($item, 'ARRAY')) {
            push @things, @{$item};
        } else {
            push @things, $item;
        }
    }
    my $status = $csv->combine(@things);
    if ($status) {
	return $csv->string();
    } else {
	Log(0, "Error creating CSV string: ".$csv->error_input()."\n");
	return '';
    }
}

# This is taken straight from asc.pl
sub munge_links {
    my (@lines) = @_;
    my @newlines = ();

    foreach my $line (@lines) {
        # LINKs are treated the same no matter where they're formatted
        $line =~ s/LINK\s+(\S+)\s+AS\s+(?:\[([^]]+)\]|(\S+))/$2$3 ($1)/go;

        my $count = 0;
        my $temp = $line;
        while ($count < 40 && $line =~ /(ATTACH\s+(\S+)\s+AS\s+(?:\[([^]]+)\]|(\S+)))/g) {
            my ($section, $url, $text) = ($1, $2, $3.$4);
            $text =~ s/^\[(.*?)\]$/$1/;
            $temp =~ s/\Q$section\E/$text ($url)/;
            $count++;
        }
        push @newlines, $temp;
    }
    return @newlines;
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub Log { main::Log(@_); }
sub istrue { main::istrue(@_); }

1;
