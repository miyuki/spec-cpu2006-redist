#
#  asc.pl - produces ASCII output
#  Copyright 1999-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: asc.pl 6400 2011-03-31 18:21:54Z cloyce $

use strict;
use UNIVERSAL qw(isa);
require 'util.pl';
require 'flagutils.pl';

use vars qw($name $extension $synonyms);

$name      = 'ASCII';
$extension = 'txt';
$synonyms  = { map { lc($_) => 1 } ($name, $extension, qw(text asc)) };
my $asc_version = '$LastChangedRevision: 6400 $ '; # Make emacs happier
$asc_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::asc::part_of_all = 1;

$::tools_versions{'asc.pl'} = $asc_version;

my $debug = 0;
my %trademarks_done = ();
my %code2mark = ( 'r' => '(R)',
                  't' => '(TM)',
                  's' => '(SM)',
                );

sub format {
    my($me, $r, $fn) = @_;
    my (@output, @errors);
    my (%seen, $temp, $bench, $name, @values, @errmsg);
    my @nc = ::allof($r->{'nc'});
    $r->{'table'} = 0 if (@nc);
    my $invalid = ($r->{'invalid'} ||
		   (isa($r->{'errors'}, 'ARRAY') && @{$r->{'errors'}}));
    %trademarks_done = ();
    if ($invalid) {
	push (@errors, '#' x 78);
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN -- ' x 4 . 'INVALID RUN'));
	push (@errors, sprintf ("# %-74s #", ''));

	for ($r->errors) {
	    push (@errors, sprintf ("# %-74s #", $_));
	}

	push (@errors, sprintf ("# %-74s #", ''));
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN -- ' x 4 . 'INVALID RUN'));
	push (@errors, '#' x 78);
    }

    # Collect (possibly multi-line) info
    my %id_map = ($::lcsuite eq 'mpi2007') ?
                    (
                      'vendor' => 'system_vendor',
                      'model'  => 'system_name',
                    ) :
                    (
                      'vendor' => 'hw_vendor',
                      'model'  => 'hw_model',
                    );
    my @hw_vendor    = ::allof($r->accessor_nowarn($id_map{'vendor'}));
    my @hw_model     = ::allof($r->accessor_nowarn($id_map{'model'}));
    my @license_num  = ::allof($r->accessor_nowarn('license_num'));
    my @test_date    = ::allof($r->accessor_nowarn('test_date'));
    my @hw_avail     = ::allof($r->accessor_nowarn('hw_avail'));
    my @sw_avail     = ::allof($r->accessor_nowarn('sw_avail'));
    my @tester       = ::allof($r->accessor_nowarn('tester'));
    my @test_sponsor = ::allof($r->accessor_nowarn('test_sponsor'));

    my $header_string = 'SPEC ' . $r->metric .  ' Summary';
    push @output, center( fixup_trademarks($header_string) );
    if (@hw_vendor > 1 || @hw_model > 1) {
	push @output, map { center($_) } @hw_vendor;
	push @output, map { center($_) } @hw_model;
    } else {
        push @output, center($hw_vendor[0].' '.$hw_model[0]);
    }
    # Print the test sponsor close to the hardware vendor if
    # they're not the same
    if (join(' ', @hw_vendor) ne join(' ', @test_sponsor)) {
        my @tmp = @test_sponsor;
        push @output, center ('Test Sponsor: '.shift(@tmp));
        while (@tmp) {
            push @output, center('              '.shift(@tmp));
        }
    }
    push @output, center( $r->datestr );
    push (@output, '');

    # Mash things up so that they don't look like the other results, but
    # so that the dates match up.
    # License number and test date
    push @output, sprintf('%s License: %-5s                                   Test date: %-8s',
                           $::suite, shift(@license_num), shift(@test_date));
    while (@license_num || @test_date) {
        push @output, sprintf('%*s          %-5s                                              %-8s',
                               length($::suite), ' ', shift(@license_num),
                               shift(@test_date));
    }

    # Test sponsor and hardware availability
    push @output, sprintf('Test sponsor: %-29s  Hardware availability: %-8s',
                          shift(@test_sponsor), shift(@hw_avail));
    while (@test_sponsor || @hw_avail) {
        push @output, sprintf('              %-29s                         %-8s',
                              shift(@test_sponsor), shift(@hw_avail));
    }

    # Tester and software availability
    push @output, sprintf('Tested by:    %-29s  Software availability: %-8s',
                          shift(@tester), shift(@sw_avail));
    while (@tester || @sw_avail) {
        push @output, sprintf('              %-29s                         %-8s',
                              shift(@tester), shift(@sw_avail));
    }

    push (@output, '');

    # Note the reason for NC, NA, or CD (if any).
    if (@nc) {
	push @output, '', '---------------------------------------------------------------------------', '';
	push @output, map { center($_) } @nc;
	push @output, '', '---------------------------------------------------------------------------', '', '';
    }

    push @output, screen_format($me, $r, $fn, 1, $invalid, \@nc);

    if ($::lcsuite ne 'mpi2007') {
        push @output, format_info('HARDWARE', [ $r->hardware ]);
        push @output, format_info('SOFTWARE', [ $r->software ]);
    } else {
        # MPI is very very special

        # The "benchmark details"
        push @output, format_info('BENCHMARK DETAILS', [ $r->info_format($::mpi_info{'hardware'}), $r->info_format($::mpi_info{'software'}) ]);

        # System descriptions
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
                push @output, center($label), center('=' x length($label));

                my ($hw_info, $sw_info) = ::mpi_info_munge($r, $item, $system);

                push @output, format_info('HARDWARE', [ $r->info_format($hw_info) ]);
                push @output, format_info('SOFTWARE', [ $r->info_format($sw_info) ]);

                # Do the notes for this thing...
                my @notes = @{$r->notes("${item}_${system}_")};
                push @output, '';
                foreach my $sectionref (@notes) {
                    my ($section, $notesref) = @{$sectionref};
                    next unless isa($notesref, 'ARRAY');
                    push @output, '', center($section), center('-' x length($section));
                    push @output, munge_links(map { '    '.$_ } @{$notesref});
                }
            }
        }
    }

    # Do the notes
    my @notes = @{$r->notes};
    push @output, '';
    foreach my $sectionref (@notes) {
        my ($section, $notesref) = @{$sectionref};
        next unless isa($notesref, 'ARRAY');
        push @output, '', center($section), center('-' x length($section));
        push @output, munge_links(map { '    '.$_ } @{$notesref});
    }

    # These will be handy for the flags section
    my $rf = $r->{'reduced_flags'};
    return undef unless isa($rf, 'HASH');
    my @benches = sort keys %{$rf->{'benchlist'}};
    my @tunes = sort keys %{$rf->{'tunelist'}};
    my @classes = sort keys %{$rf->{'classlist'}};

    # Do the unknown and forbidden flags; they are uncollapsed, because
    # repetition is a form of emphasis.
    my $maxtitle = 0;
    foreach my $class (qw(forbidden unknown)) {
	next unless ::check_elem(undef, $rf, 'stringlist', $class);
        # Flags of the class exist for at least one benchmark, so
        # make lists of them.  They'll be formatted and output later.
	my $maxtitle = $rf->{'maxbench'} + 3; # 2 = ' ' + ': '
        my $classref = $rf->{'stringlist'}->{$class};
        for my $tune (sort ::bytune @tunes) {
          my $title_printed = 0;
          my $title = ucfirst($tune).' '.ucfirst($class).' Flags';
          for my $bench (sort keys %{$classref}) {
	    my $printed = 0;
            next unless ::check_elem('ARRAY', $classref, $bench, $tune);
            if (!$title_printed) {
                push @output, '', center($title), center('-' x length($title));
                $title_printed = 1;
            }
            push @output, dump_lines(" $bench: ", $maxtitle, $classref->{$bench}->{$tune}), '';
	  }
	}
    }

    # Do all the other flags in a way that aggregates as much as possible.
    # Well, maybe.  Sometimes they're a LITTLE more expanded than they could
    # be.

    # First, figure out which form we'll use.  Will it be 0ld sk00l
    # Compiler (merged)
    # Portability (merged)
    # Base Optimization   -+- Maybe merged
    # Peak Optimization   -+
    # Other (merged)
    # ?
    # Or will it be the new style
    # Base Compiler Invocation
    # Base Portability Flags
    # Base Optimization
    # Base Other Flags
    # Peak Compiler Invocation (maybe with a back-ref to base)
    # Peak Portability Flags (maybe with a back-ref to base)
    # Peak Optimization (maybe with a back-ref to base)
    # Peak Other (maybe with a back-ref to base)
    # ?
    my $section_order = 1; # 0ld Sk00l by default
    foreach my $class (qw(compiler portability other)) {
        next unless exists $rf->{'allmatch'}->{$class};
        $section_order = $rf->{'allmatch'}->{$class};
        last unless $section_order;
    }
    # If any of the above sections don't match for all languages across all
    # tuning levels, we'll go to the "new style" order.

    my %class2title = ( 'compiler' => 'Compiler Invocation',
                        'portability' => 'Portability Flags',
                        'optimization' => 'Optimization Flags',
                        'other' => 'Other Flags' );
    my $onetune = $tunes[0];
    foreach my $tune (@tunes) {
        foreach my $class (qw(compiler portability optimization other)) {
            # Skip this tuning level pass if we're doing the old order, and EITHER
            # 1. it's the first trip through and the class is 'other'
            # or
            # 2. it's the second trip through and the class is 'compiler' or 'portability'
            # or
            # 3. it's the second trip through, the class is 'optimization', and allmatch is set
            # This is done so that the merged "other" section can come after optimization
            next if ($section_order == 1 &&
                     (($tune eq $onetune && $class eq 'other') ||
                     ($tune ne $onetune && ($class eq 'compiler' || $class eq 'portability')) ||
                     ($tune ne $onetune && $class eq 'optimization' && $rf->{'allmatch'}->{$class} == 1)));
            my $mismatch = 0;
            my $printed_title = 0;
            my %langstodo = map { $_ => 1 } keys %{$rf->{'langlist'}};
            my %donebench = ();
            my $title = $class2title{$class};

            # Easy case first -- if we're doing new section order and allmatch
            # for this class is set and this isn't the base tuning, just
            # output the "Same as ..." message
            if ($section_order == 0 &&
                exists($rf->{'allmatch'}->{$class}) &&
                $rf->{'allmatch'}->{$class} == 1 &&
                $tune ne $onetune) {
                $title = ucfirst($tune).' '.$title;
                push @output, '', center($title), center('-' x length($title));
                push @output, 'Same as '.ucfirst($onetune).' '.$class2title{$class},'';
                next;
            }

            # Go through the langs and print the ones that match.
            foreach my $lang (sort ::bylang keys %langstodo) {
                last if $class eq 'portability'; # Portability is by benchmark
                my $printed_lang = 0;

                # Completely merged sections are only output for 0ld sk00l order
                if ($section_order == 1) {
                    # First dump all class flags that are common across all tuning levels
                    if ($rf->{'allmatch'}->{$class} == 1 &&
                        ::check_elem('HASH', $rf, 'langmatch', $class, 'alltune') &&
                        ::check_elem('HASH', $rf, 'bylang', 'stringlist', $class, $onetune)) {
                        if (exists($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang}) &&
                            $rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} &&
                            # There might _not_ be an entry for a particular language if, for
                            # the same flag (like -DSPEC_CPU_WINDOWS) one benchmark calls
                            # it portability and another calls it mandatory.  This is
                            # incorrect, but it's no fault of the user.
                            ::check_elem('ARRAY', $rf, 'bylang', 'stringlist', $class, $onetune, $lang) &&
                            @{$rf->{'bylang'}->{'stringlist'}->{$class}->{$onetune}->{$lang}}) {
                            if (!$printed_title) {
                                push @output, '', center($class2title{$class}), center('-' x length($class2title{$class}));
                                $printed_title = 1;
                            }
                            my @strings = ();
                            my $flags = $rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang};
                            my $strings = $rf->{'bylang'}->{'stringlist'}->{$class}->{$onetune}->{$lang};
                            for(my $i = 0; $i < @{$flags}; $i++) {
                                next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
                                push @strings, $strings->[$i];
                            }
                            my $langtitle = $rf->{'var2desc'}->{$lang};
                            if ($rf->{'langmatch'}->{$class}->{$onetune}->{$lang} == 2) {
                                $langtitle .= ' (except as noted below)';
                            }
                            push @output, dump_lines($langtitle.': ', 5, \@strings, { 'title_alone' => 1 }), '';
                            $printed_lang = 1;
                            delete $langstodo{$lang};
                            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $onetune, $lang)) {
                                $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$onetune}->{$lang};
                            }
                        }
                    }

                    # Do the benchmarks of $lang that matched across tuning levels
                    if ($rf->{'allmatch'}->{$class} == 1 &&
                        ::check_elem('HASH', $rf, 'stringlist', $class)) {
                        my $classref = $rf->{'stringlist'}->{$class};
                        foreach my $bench (sort keys %{$classref}) {
                            next unless # the following six conditions are true:
                               (
                                $rf->{'langs'}->{$bench}->{$onetune} eq $lang &&
                                ::check_elem(undef, $rf, 'benchmatch', $class, $bench, 'alltune') &&
                                $rf->{'benchmatch'}->{$class}->{$bench}->{'alltune'} &&
                                ::check_elem('ARRAY', $rf, 'flaglist', $class, $bench, $onetune) &&
                                isa($rf->{'flaglist'}->{$class}->{$bench}->{$onetune}, 'ARRAY') &&
                                @{$rf->{'flaglist'}->{$class}->{$bench}->{$onetune}}
                               );
                            if (!$printed_title) {
                                push @output, '', center($class2title{$class}), center('-' x length($class2title{$class}));
                                $printed_title = 1;
                            }
                            if (!$printed_lang) {
                                push @output, $rf->{'var2desc'}->{$lang}.':', '';
                                $printed_lang = 1;
                            }
                            my @strings = ();
                            my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$onetune};
                            my $strings = $classref->{$bench}->{$onetune};
                            for(my $i = 0; $i < @{$flags}; $i++) {
                                next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
                                push @strings, $strings->[$i];
                            }
                            push @output, dump_lines(" $bench: ", $rf->{'maxbench'} + 3, \@strings), '';
                            if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $onetune)) {
                                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$onetune};
                            }
                            $donebench{$bench}++;
                        }
                    }
                }
            }

            # Next dump class flags by tuning level, with the common per-language
            # set at the top, followed by benchmark-specific settings
            my $printed_tune = 0;
            my $classref = undef;
            if (::check_elem('HASH', $rf, 'bylang', 'stringlist', $class, $tune)) {
                $classref = $rf->{'bylang'}->{'stringlist'}->{$class}->{$tune};
            }
            foreach my $lang (sort ::bylang keys %langstodo) {
                last if $class eq 'portability'; # Portability is by benchmark
                my $printed_lang = 0;

                # First check for by-language list
                if (defined($classref) &&
                    ::check_elem('ARRAY', $classref, $lang) &&
                    @{$classref->{$lang}}) {
                    if (!$printed_tune) {
                        my $title = ucfirst($tune).' '.$class2title{$class};
                        push @output, '', center($title), center('-' x length($title));
                        $printed_tune = 1;
                    }
                    my @strings = ();
                    my $flags = $rf->{'bylang'}->{'flaglist'}->{$class}->{$tune}->{$lang};
                    for(my $i = 0; $i < @{$flags}; $i++) {
                        next if (!istrue($flags->[$i]->[2]->{'display'}) && !istrue($r->{'review'}));
                        push @strings, $classref->{$lang}->[$i];
                    }
                    my $langtitle = $rf->{'var2desc'}->{$lang};
                    if ($rf->{'langmatch'}->{$class}->{$tune}->{$lang} == 2) {
                        $langtitle .= ' (except as noted below)';
                    }
                    push @output, dump_lines($langtitle.': ', 5, \@strings, { 'title_alone' => 1 }), '';
                    $printed_lang = 1;
                    if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $tune, $lang)) {
                        $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$tune}->{$lang};
                    }
                }

                # Now do the benchmark-specific list (if any)
                if (::check_elem('HASH', $rf, 'stringlist', $class)) {
                    my $classref = $rf->{'stringlist'}->{$class};
                    foreach my $bench (sort keys %{$classref}) {
                        next if $donebench{$bench};
                        next if $rf->{'langs'}->{$bench}->{$tune} ne $lang;
                        next unless ::check_elem('ARRAY', $classref, $bench, $tune);
                        next unless @{$classref->{$bench}->{$tune}};
                        if (!$printed_tune) {
                            my $title = ucfirst($tune).' '.$class2title{$class};
                            push @output, '', center($title), center('-' x length($title));
                            $printed_tune = 1;
                        }
                        if (!$printed_lang) {
                            push @output, $rf->{'var2desc'}->{$lang}.':', '';
                            $printed_lang = 1;
                        }
                        my @strings = ();
                        my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$tune};
                        my $strings = $classref->{$bench}->{$tune};
                        for(my $i = 0; $i < @{$flags}; $i++) {
                            next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
                            push @strings, $strings->[$i];
                        }
                        push @output, dump_lines(" $bench: ", $rf->{'maxbench'} + 3, \@strings), '';
                        if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                            $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
                        }
                    }
                }
            }

            if ($class eq 'portability') {
                # Do the portability flags on a per-benchmark basis; this is mostly
                # a copy of the code above.
                my @port_tunes = ($tune);
                my @titles = ( ucfirst($tune).' '.$class2title{$class} );
                if ($section_order == 1) {
                    # 0ld sk00l order means we have to do all tuning outputs
                    # here
                    if (!exists($rf->{'allmatch'}->{$class}) ||
                        $rf->{'allmatch'}->{$class} != 1) {
                        # ... but only if they shouldn't be merged.
                        @port_tunes = @tunes;
                        @titles = map { ucfirst($_).' '.$class2title{$class} } @port_tunes;
                    } else {
                        # Old order, but the section is merged (as it should
                        # always be, in the old order)
                        @titles = ( $class2title{$class} );
                    }
                }
                foreach my $port_tune (@port_tunes) {
                    my $title = shift(@titles);
                    $printed_tune = 0;
                    my $did_output = 0;
                    if (::check_elem('HASH', $rf, 'stringlist', $class)) {
                        my $classref = $rf->{'stringlist'}->{$class};
                        foreach my $bench (sort keys %{$classref}) {
                            next if $donebench{$bench};
                            next unless ::check_elem('ARRAY', $classref, $bench, $port_tune);
                            next unless @{$classref->{$bench}->{$port_tune}};
                            if (!$printed_tune) {
                                push @output, '', center($title), center('-' x length($title));
                                $printed_tune = 1;
                            }
                            my @strings = ();
                            my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$port_tune};
                            my $strings = $classref->{$bench}->{$port_tune};
                            for(my $i = 0; $i < @{$flags}; $i++) {
                                next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
                                push @strings, $strings->[$i];
                            }
                            push @output, dump_lines(" $bench: ", $rf->{'maxbench'} + 3, \@strings);
                            $did_output++ if @strings;
                            if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $port_tune)) {
                                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$port_tune};
                            }
                        }
                    }
                    if ($mismatch) {
                        push @output, '(*) Indicates portability flags found in non-portability variables';
                        $mismatch = 0;
                    }
                    push @output, '' if $did_output;
                }
            } else {
                # Portability is taken care of above...

                if ($mismatch) {
                    if ($class eq 'optimization') {
                        push @output, '(*) Indicates optimization flags found in portability variables';
                    } elsif ($class eq 'portability') {
                        push @output, '(*) Indicates portability flags found in non-portability variables';
                    } elsif ($class eq 'compiler') {
                        push @output, '(*) Indicates compiler flags found in non-compiler variables';
                    }
                }
                $mismatch = 0;
            }
        }
    }

    if (defined($::website_formatter) && $::website_formatter &&
        defined($r->{'flagsurl'}) && $r->{'flagsurl'} ne '') {
      my $urls = $r->{'flagsurl'};
      if (!isa($urls, 'ARRAY')) {
          # Shouldn't happen, but just in case
          $urls = [ $urls ];
      }
      my $plural = undef;
      my (@html_output, @xml_output);
      foreach my $url (@{$urls}) {
          my $html_url = $url;
          $html_url =~ s/\.xml$/\.html/;
          push @html_output, $html_url;
          push @xml_output, $url;
          $plural = 's' if defined($plural);
          $plural = '' unless defined($plural);
      }

      if (@{$urls} > 1) {
        push @output, '', 'The flags files that were used to format this result can be browsed at', @html_output;
      } else {
        push @output, '', 'The flags file that was used to format this result can be browsed at', @html_output;
      }
      push @output, '', "You can also download the XML flags source${plural} by saving the following link${plural}:", @xml_output;
    }

    push @output, '';

    push @output, footer();

    push @output, @errors;
    unshift @output, @errors;
    push @output, '-----------------------------------------------------------------------------';
    push @output, 'For questions about this result, please contact the tester.';
    push @output, 'For other inquiries, please contact webmaster@spec.org.';
    push @output, 'Copyright '.::copyright_dates().' Standard Performance Evaluation Corporation';
    push @output, "Tested with SPEC $::suite v".$r->{'suitever'}.'.';
    push @output, 'Report generated on '.&::ctime(time)." by $::suite ASCII formatter v$asc_version.";

    return (\@output, []);
}

sub format_table {
    my ($table, $timelog, $ratiolog, $wattslog, $resultobj, $is_nc) = @_;
    my @rc;
    for my $benchname (sort keys %$table) {
	my $tr = $table->{$benchname};
	my $array = { 'base' => [], 'peak' => [] };
	$array->{'base'} = [@{$tr->{'base'}}] if isa($tr->{'base'}, 'ARRAY');
	$array->{'peak'} = [@{$tr->{'peak'}}] if isa($tr->{'peak'}, 'ARRAY');
	my ($base, $peak);

	while (@{$array->{'base'}} || @{$array->{'peak'}}) {
	    my $line = sprintf '%-14s ', $benchname;
	    for my $tune (qw(base peak)) {
		my $ref = $array->{$tune};
		if (@$ref) {
		    my ($first, $time, $ratio, $selected, $invalid, $watts) = @{shift @$ref};
		    $first = sprintf('%d', $first) if ($first + 0 > 0);
		    $time  = significant(6,3,$timelog->{$tune}, $time,  1, $is_nc) if ($time + 0 > 0);
		    $ratio = significant(6,3,$ratiolog->{$tune},$ratio, 0, $is_nc) if ($ratio + 0 > 0);
                    # XXX This may not be what we want for power
		    $watts = significant(6,3,$wattslog->{$tune},$watts, 0, $is_nc) if ($watts + 0 > 0);
		    my $selectchar = $invalid;
		    if ($resultobj->size_class eq 'ref') {
			$selectchar = $selected ? '*' : $invalid;
		    }
		    if ($invalid ne 'S') {
			$selectchar = ($invalid == 1) ? 'NR' : $invalid;
			$ratio = '';
		    }
		    $selectchar = ' ' if ($is_nc);
                    if (istrue($resultobj->power)) {
                        $line .= sprintf '%5s %7s %7s %7s %-2s',  
                                    $first, $watts, $time, $ratio, 
                                    $selectchar;
                    } else {
                        $line .= sprintf '%6s  %9s  %9s %-2s ',  
                                    $first, $time, $ratio, 
                                    $selectchar;
                    }
		} else {
                    # This amount must match the length of the output in the
                    # sprintf above.
                    $line .= ' ' x 32;
		}
	    }
	    push (@rc, $line);
	}
    }
    return @rc;
}

sub screen_format {
    # This does the screen format, which is really just the summary table
    my ($me, $r, $fn, $isasc, $invalid, $nc) = @_;
    my $is_nc = 0;

    if (@{$nc}) {
        if (istrue($r->{'nc_is_cd'})) {
            $is_nc = 3; # CD
        } elsif (istrue($r->{'nc_is_na'})) {
            $is_nc = 2; # NA
        } else {
            $is_nc = 1; # NC
        }
    }
    my @output = ();
    my $what_ = ' Ref. ';
    my $rmode = ' Ratio';
    if (istrue($r->rate)) {
       $what_ = 'Copies';
       $rmode = ' Rate ';
    } elsif ($::lcsuite eq 'mpi2007') {
       $what_ = ' Ranks';
       $rmode = ' Ratio';
    }

    # The field widths that these need to match are in format_table()
    if (istrue($r->power)) {
        push (@output,
        '                           Estimated                         Estimated'
        ) if $invalid;
        push (@output,
        '               Base   Base    Base    Base      Peak    Peak    Peak    Peak',
        "Benchmarks    $what_  Power  RunTime $rmode    $what_   Power  RunTime $rmode",
        '-------------- ----- ------- ------- -------   ------  ------- ------- -------');
    } else {
        push (@output,
        '                                  Estimated                       Estimated'
        ) if $invalid;
        push (@output,
        '                Base     Base       Base        Peak     Peak       Peak',
        "Benchmarks     $what_  Run Time    $rmode      $what_  Run Time    $rmode",
        '-------------- ------  ---------  ---------    ------  ---------  ---------');
    }

    my $table    = {};
    my $results  = {};
    my $smallest = { 'base' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 }, # Mr. Big Number
                     'peak' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 } # Mr. Big Number
		 };
    my %benchseen = ();
    my %tuneseen = ();

    # Go through the benchmarks that have results.  We'll catch the missed
    # ones afterward.
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchres = $r->{'results'}{$bench};
	for my $tune (sort keys %{$r->{'results'}{$bench}}) {
	    $benchres = $r->{'results'}{$bench}{$tune}{'data'};
	    my $tmp;
	    for my $res (@{$benchres}) {
		# If we don't get here, we haven't "seen" them...
		$benchseen{$bench} = 1 unless exists $benchseen{$bench};
		$tuneseen{$tune} = 1 unless exists $benchseen{$tune};
                my $watts = (istrue($res->power) ? $res->avg_watts : 0);
		if (istrue($r->rate)) {
		    $tmp = [$res->copies,    $res->reported_time, $res->ratio];
		} elsif ($::lcsuite eq 'mpi2007') {
		    $tmp = [$res->ranks,     $res->reported_time, $res->ratio];
                } else {
		    $tmp = [$res->reference, $res->reported_time, $res->ratio];
		}
                # Common fields
                push @$tmp, ($res->selected, $res->valid, $watts);
		print "\@tmp = (",join(', ', @{$tmp}),")\n" if ($debug & 2);
		if ($tmp->[1] && ($smallest->{$tune}{'time'}  > $tmp->[1])) {
		    $smallest->{$tune}{'time'}  = $tmp->[1];
		}
		if ($tmp->[2] && ($smallest->{$tune}{'ratio'} > $tmp->[2])) {
		    $smallest->{$tune}{'ratio'} = $tmp->[2];
		}
		if ($tmp->[5] && ($smallest->{$tune}{'watts'} > $tmp->[5])) {
		    $smallest->{$tune}{'watts'} = $tmp->[5];
		}
		if ($debug & 2) {
		    print "smallest->{$tune}{time} = ".$smallest->{$tune}{'time'}."\n";
		    print "smallest->{$tune}{ratio} = ".$smallest->{$tune}{'ratio'}."\n";
		    print "smallest->{$tune}{watts} = ".$smallest->{$tune}{'watts'}."\n";
		}
		push (@{$table->{$bench}{$tune}}, $tmp);
		if ($res->selected) {
		    $tmp->[3] = '*' if ($tmp->[4] eq 'S' && $tmp->[3] ne 'X');
		    push (@{$results->{$bench}{$tune}}, $tmp);
		    $benchseen{$bench} = 'selected';
		}
	    }
	}
    }
    for my $bench (sort keys %{$r->benchmarks}) {
	next if (exists($benchseen{$bench}) &&
		 ($benchseen{$bench} eq 'selected'));
	for my $tune (sort keys %tuneseen) {
	    my $tmp = [ '', '', '', '', 'NR', '' ];
	    push (@{$table->{$bench}{$tune}}, $tmp) unless $benchseen{$bench};
	    push (@{$results->{$bench}{$tune}}, $tmp);
	}
    }
    my $timelog = { 'base' => $smallest->{'base'}{'time'},
		    'peak' => $smallest->{'peak'}{'time'} };
    my $ratiolog = { 'base' => $smallest->{'base'}{'ratio'},
		     'peak' => $smallest->{'peak'}{'ratio'} };
    my $wattslog = { 'base' => $smallest->{'base'}{'watts'},
		     'peak' => $smallest->{'peak'}{'watts'} };
    $timelog->{'base'}  = log($timelog->{'base'})/log(10) if ($timelog->{'base'} > 0);
    $ratiolog->{'base'} = log($ratiolog->{'base'})/log(10) if ($ratiolog->{'base'} > 0);
    $wattslog->{'base'} = log($wattslog->{'base'})/log(10) if ($wattslog->{'base'} > 0);
    $timelog->{'peak'}  = log($timelog->{'peak'})/log(10) if ($timelog->{'peak'} > 0);
    $ratiolog->{'peak'} = log($ratiolog->{'peak'})/log(10) if ($ratiolog->{'peak'} > 0);
    $wattslog->{'peak'} = log($wattslog->{'peak'})/log(10) if ($wattslog->{'peak'} > 0);
    if ($debug) {
        print "\@timelog=($timelog->{'base'}, $timelog->{'peak'})  ";
        print "\@ratiolog=($ratiolog->{'base'}, $ratiolog->{'peak'})  ";
        print "\@wattslog=($wattslog->{'base'}, $wattslog->{'peak'})\n";
    }


    if ($isasc && istrue($r->table)) {
	push (@output, format_table($table, $timelog, $ratiolog, $wattslog, $r, $is_nc));
	push (@output, '=' x 78);
    }
    push (@output, format_table($results, $timelog, $ratiolog, $wattslog, $r, $is_nc));

    my $est;
    $est = 'Est. ' if ($invalid);

    my $basemean = $r->basemean;
    if ($basemean =~ /\d/) {
      $basemean = significant(8, 3, $ratiolog->{'base'}, $r->basemean, 0, $is_nc);
    }
    push @output, sprintf (" %-34s%8s", 
                           $est . fixup_trademarks($r->baseunits), $basemean);

    my $peakmean = $r->peakmean;
    if ($peakmean =~ /\d/) {
      $peakmean = significant(8, 3, $ratiolog->{'peak'}, $r->peakmean, 0, $is_nc);
    }
    push @output, sprintf (" %-34s%40s",
                           $est . fixup_trademarks($r->peakunits), $peakmean);

    return @output;
}

sub format_info {
    my ($title, $ref) = @_;
    return () if !@$ref;

    my @output;
    push (@output, '', '', center($title), center('-' x length($title)));
    for my $item (@{$ref}) {
	my ($name, @vals) = @$item;
	if (!@vals) {
	    push (@output, sprintf ('%20.20s: --', $name));
	} else {
	    my $val = shift @vals;
	    push (@output, sprintf ('%20.20s: %s', $name, $val));

	    while (@vals) {
		$val = shift @vals;
		if (ref($val) eq '') {
		    push (@output, sprintf ('%20.20s  %s', '', $val));
		} elsif (::isa($val, 'ARRAY')) {
		    unshift @vals, @{$val};
		}
	    }
	}
    }
    return @output;
}

sub dump_lines {
    my ($title, $len, $strings, $opts) = @_;
    my @output = ();
    my $line = '';
    my $printed = 0;
    $opts = {} unless isa($opts, 'HASH');
    return () unless isa($strings, 'ARRAY') && @$strings;

    if ($opts->{'title_alone'}) {
      $printed = 1;
      push @output, $title;
    }

    foreach my $string (@{$strings}) {
	if ($line eq '') {
	    $line = $string;
	} elsif (length($line.', '.$string) + $len > 78) {
	    push @output, sprintf "%*s%s", $len, ($printed) ? '' : $title, $line;
	    $printed++;
	    $line = $string;
	} else {
            if (0) {
              # No commas; too "confusing"
              $line .= ", $string";
            } else {
              $line .= " $string";
            }
	}
    }
    if ($line ne '') {
	push @output, sprintf "%*s%s", $len, ($printed) ? '' : $title, $line;
    }

    return @output;
}

sub footer {
  return ::trademark_lines('  ', %trademarks_done);
}

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

# Look in the input string for trademarks and mark them up as appropriate.
# Also keep track of which ones were used so that they can be mentioned in
# the result page footer.
sub fixup_trademarks {
  my ($str) = @_;

  foreach my $tm (sort { length($b) <=> length($a) } keys %::trademarks) {
    next if exists($trademarks_done{$tm});
    my $tmre = qr/\b${tm}((?=[^a-zA-Z])|\b)/;
    my $tmcode = $::trademarks{$tm};
    if (isa($::trademarks{$tm}, 'ARRAY')) {
        if ($::trademarks{$tm}->[1]) {
            $tmre = qr/\b${tm}/;
            $tmcode = $::trademarks{$tm}->[0];
        }
    }
    if ($str =~ /$tmre/) {
        $trademarks_done{$tm}++;
        $str =~ s/$tmre/${tm}$code2mark{$tmcode}/;
    }
  }

  return $str;
}

# significant -- format a floating point value into N significant digits
# width of text string to return
# log of minimum output (how many decimal places do we want)
# log of smallest overall value (how many decimals will we be using for sure)
# value to format
sub significant {
    my ($width, $min_log, $low_log, $value, $hack, $is_nc) = @_;
    print "significant($width, $min_log, $low_log, $value, $debug)\n" if ($debug & 4);
    my ($real_dp, $wanted_dp, $dp, $space, $log);

    return 'CD' if ($is_nc == 3);
    return 'NA' if ($is_nc == 2);
    return 'NC' if ($is_nc);

    if ($value == 0) {
	if ($value eq '0' || $value !~ m/^\s*(\+|-)?[0-9.eE+]/) {
	    print "Returning '0.00'\n" if ($debug & 4);
	    return '0.00';
	}
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    $min_log--;
    print "  log=$log  min_log=$min_log\n" if ($debug & 4);
    if ($log > $min_log &&
        (!$hack || $value < 1000)) {
        # Return of the STUPID STUPID HACK, but only if $hack is set.
        $value = int($value / (10**($log-$min_log))+.5) * (10**($log-$min_log));
    }
    print "  value=$value\n" if ($debug & 4);
    $dp        = ($low_log>=$min_log)?0:3-$low_log;
    $wanted_dp = ($log>=$min_log)?0:$min_log-$log;
    print "  dp=$dp   wanted_dp=$wanted_dp\n" if ($debug & 4);
    if ($dp > $wanted_dp) {
	$space = $dp - $wanted_dp;
	$real_dp = $wanted_dp;
    } else {
	$space = 0;
	$real_dp = $dp;
    }
    if ($real_dp == 0 && $dp > 0) {
	$space++;
    }
    print "  space=$space  real_dp=$real_dp\n" if ($debug & 4);
    my $retval = sprintf('%*.*f%s', $width-$space, $real_dp, $value, ' ' x $space);
    print "sprintf('%*.*f%s', \$width-\$space, \$real_dp, \$value, ' ' x \$space) =\n" if ($debug & 4);
    print "  sprintf('%*.*f%s', ".($width-$space).", $real_dp, $value, '".(' ' x $space)."') =\n" if ($debug & 4);
    print "    '$retval'\n" if ($debug & 4);
    return $retval;
}

sub floor {
    my ($temp) = @_;
    my $inttemp = int($temp);
    if ($temp != $inttemp) { #  This is a bad test.
	if ($temp > 0) {
	    $temp = $inttemp;
	} else {
	    $temp = $inttemp-1;
	}
    }
    return $temp;
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub istrue { main::istrue(@_); }

1;
