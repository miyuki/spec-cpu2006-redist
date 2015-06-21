#
#  flags.pl - compilation/link flag summary reports
#  Copyright 2005-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author: Cloyce D. Spradling
#
# $Id: flags.pl 6417 2011-04-07 04:36:26Z cloyce $

use strict;
use IO::File;
use File::Basename;
use URI;
use UNIVERSAL qw(isa);
use Data::Dumper;

require 'util.pl';
require 'flagutils.pl';

use vars qw($name $extension $synonyms);

$name      = 'flags';
$extension = 'flags.html';
$synonyms  = { map { lc($_) => 1 } ($name, 'flag') };

my $flags_version = '$LastChangedRevision: 6417 $ '; # Make emacs happier
$flags_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::flags::non_default = 1;  # You must _sometimes_ ask for it
$::tools_versions{'flags.pl'} = $flags_version;
my $debug = 0;

sub format {
  my($me, $r, $fn) = @_;
  my (@output, @errors);
  my $written = [];
  my @afters = ();

  my $userhead = $r->{'flaginfo'}->{'user'};
  my $suitehead = $r->{'flaginfo'}->{'suite'};
  my $title = "$::suite Result Flag Description";
  push @output, xhtml_header($title, $userhead->{'style'});
  push @output, '<body>';

  my %id_map = ($::lcsuite eq 'mpi2007') ?
                  (
                    'vendor' => 'system_vendor',
                    'model'  => 'system_name',
                  ) :
                  (
                    'vendor' => 'hw_vendor',
                    'model'  => 'hw_model',
                  );
  my $vendor = join(' ', ::allof($r->accessor_nowarn($id_map{'vendor'})));
  my $model  = join(' ', ::allof($r->accessor_nowarn($id_map{'model'})));
  push @output, "<h1>$::suite Flag Description<br />$vendor $model</h1>";

  my $company = join(' ', ::allof($r->accessor_nowarn('test_sponsor')));
  if ($company !~ /^(|--)$/ && $company ne $vendor) {
      push @output, "<h2>Test sponsored by $company</h2>";
  }

  push @output, $userhead->{'header'}->{'default'} if exists($userhead->{'header'}->{'default'});
  push @output, '<hr />';

  # These will be handy for the flags section
  my $rf = $r->{'reduced_flags'};
  return undef unless isa($rf, 'HASH');
  my @benches = sort keys %{$rf->{'benchlist'}};
  my @tunes = sort keys %{$rf->{'tunelist'}};
  my @classes = sort keys %{$rf->{'classlist'}};

  # Do the unknown and forbidden flags; they are uncollapsed, because
  # repetition is a form of emphasis.
  foreach my $class (qw(forbidden unknown)) {
      next unless ::check_elem(undef, $rf, 'flaglist', $class);
      # Flags of the class exist for at least one benchmark, so
      # make lists of them.  They'll be formatted and output later.
      my $classref = $rf->{'flaglist'}->{$class};
      for my $tune (sort ::bytune @tunes) {
        my $title_printed = 0;
        my $title = ucfirst($tune).' '.ucfirst($class).' Flags';
        for my $bench (sort keys %{$classref}) {
            my $printed = 0;
            next unless ::check_elem('ARRAY', $classref, $bench, $tune);
            if (!$title_printed) {
                push @output, '<div class="'.$class.'flags">';
                push @output, '<h2>'.$title.'</h2>';
                $title_printed = 1;
            }
            push @output, "<h3><a name=\"$class$bench$tune\">$bench ($tune)</a></h3>";
            push @output, '<ul class="flagDescList">';

            foreach my $flag (@{$classref->{$bench}->{$tune}}) {
                if ($class eq 'unknown') {
                    # Unknown, so no flag description.  Just print the flag
                    # and its location
                    push @output, "<li><a name=\"".join('', @{$flag->[0]})."$bench$tune\">".join(', ', @{$flag->[0]})."</a>: <span class=\"monospace\">$flag->[1]</span></li>";
                } else {
                    # Forbidden, so we've got the full deal.
                    my %opts = ( 'exclude_regexp' => 1,
                                 'from_bench' => $tune,
                                 'used_in' => join(', ', @{$flag->[0]}) );
                    if (exists $flag->[2]->{'nomap'}) {
                      # This is a pseudo-flag added by the tools, so just
                      # display the name.  (This should never happen here.)
                      $opts{'flagref'} = $flag->[2];
                      $opts{'skip_all'} = 1;
                    }
                    my $origin = $flag->[2]->{'origin'}->[1];
                    $opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
                    my ($afters, @flagdesc) = dump_single_flag($r->{'flaginfo'}->{$origin},
                                                      $flag->[2]->{'name'},
                                                      $flag->[1],
                                                      %opts,
                                                      'sub_text' => 1,
                                                      'flagref' => $flag->[2]);
                    push @output, @flagdesc;
                    push @afters, @{$afters} if isa($afters, 'ARRAY');
                }
            }
            push @output, '</ul>';
	  }
          if ($title_printed) {
            push @output, '</div>';
            push @output, '<hr />';
          }
	}
    }

    # Do all the other flags in a way that aggregates as much as possible.
    my %class2title = ( 'compiler' => 'Compiler Invocation',
                        'portability' => 'Portability Flags',
                        'optimization' => 'Optimization Flags',
                        'other' => 'Other Flags' );
    my %mismatchtype = ( 'portability' => 'po', 'optimization' => 'op',
                         'compiler' => 'c' );
    foreach my $class (qw(compiler portability optimization other)) {
      my $mismatch = 0;
      my $printed_title = 0;
      my $onetune = $tunes[0];
      my %langstodo = map { $_ => 1 } keys %{$rf->{'langlist'}};
      my %donebench = ();

      # Go through the langs and print the ones that match.
      foreach my $lang (sort ::bylang keys %langstodo) {
        last if $class eq 'portability'; # Portability is by benchmark
        my $printed_lang = 0;
        my $langname = $lang;
        $langname =~ s/ /_/g;

        # First dump all class flags that are common across all tuning levels
        if ($rf->{'allmatch'}->{$class} == 1 &&
            ::check_elem('HASH', $rf, 'langmatch', $class, 'alltune') &&
            ::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $onetune)) {
          # Go through the langs and print the ones that match.
          if (exists($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang}) &&
              $rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} &&
              # There might _not_ be an entry for a particular language if, for
              # the same flag (like -DSPEC_CPU_WINDOWS) one benchmark calls
              # it portability and another calls it mandatory.  This is
              # incorrect, but it's no fault of the user.
              ::check_elem('ARRAY', $rf, 'bylang', 'flaglist', $class, $onetune, $lang) &&
              @{$rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}}) {
            if (!$printed_title) {
                push @output, '', '<div class="'.$class.'flags">';
                push @output, '<h2>'.$class2title{$class}.'</h2>';
                $printed_title = 1;
            }
            my $langtitle = $rf->{'var2desc'}->{$lang};
            if ($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} == 2) {
              $langtitle .= ' (except as noted below)';
            }
            push @output, "<h3><a name=\"${langname}ALL\">$langtitle</a></h3>";
            $printed_lang = 1;
            push @output, '<ul class="flagDescList">';
            my $flags = $rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
              my $flag = $flags->[$i];
              my $string = $rf->{'bylang'}->{'stringlist'}->{$class}->{$onetune}->{$lang}->[$i];
              my $flagmismatch = $rf->{'bylang'}->{'mismatches'}->{$class}->{$onetune}->{$lang}->[$i] ? $mismatchtype{$class} : '';
              $flagmismatch = '' if !defined($flagmismatch);
              my $origin = $flag->[2]->{'origin'}->[1];
              my ($afters, @flagdesc) = dump_single_flag(
                                                $r->{'flaginfo'}->{$origin},
                                                $flag->[2]->{'name'},
                                                $flag->[1],
                                                'mismatch' => $flagmismatch,
                                                'exclude_regexp' => 1,
                                                'from_bench' => 'ALL'.$lang,
                                                'used_in' => join(', ', @{$flag->[0]}),
                                                'sub_text' => 1,
                                                'flagref' => $flag->[2]);
              push @output, @flagdesc;
              push @afters, @{$afters} if isa($afters, 'ARRAY');
            }
            push @output, '</ul>';
            delete $langstodo{$lang};
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $onetune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$onetune}->{$lang};
            }
          }
        }

        # Do the benchmarks of $lang that matched across tuning levels
        if ($rf->{'allmatch'}->{$class} == 1 &&
            ::check_elem('HASH', $rf, 'flaglist', $class)) {
          my $classref = $rf->{'flaglist'}->{$class};
          foreach my $bench (sort keys %{$classref}) {
            next unless # the following six conditions are true
               (
                $rf->{'langs'}->{$bench}->{$onetune} eq $lang &&
                ::check_elem(undef, $rf, 'benchmatch', $class, $bench, 'alltune') &&
                $rf->{'benchmatch'}->{$class}->{$bench}->{'alltune'} &&
                ::check_elem('ARRAY', $rf, 'flaglist', $class, $bench, $onetune) &&
                isa($rf->{'flaglist'}->{$class}->{$bench}->{$onetune}, 'ARRAY') &&
                @{$rf->{'flaglist'}->{$class}->{$bench}->{$onetune}}
               );
            if (!$printed_title) {
              push @output, '', '<div class="'.$class.'flags">';
              push @output, '<h2>'.$class2title{$class}.'</h2>';
              $printed_title = 1;
            }
            if (!$printed_lang) {
              push @output, "<h3><a name=\"${langname}ALL$class\">$rf->{'var2desc'}->{$lang}</a></h3>";
              $printed_lang = 1;
            }
            push @output, "<h4><a name=\"$class${bench}ALL\">$bench</a></h4>";
            push @output, '<ul class="flagDescList">';
            my @flags = ();
            my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$onetune};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next unless (istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
              my $flag = $flags->[$i];
              my $markup = $rf->{'markup'}->{$class}->{$bench}->{$onetune}->[$i];
              my %opts = ( 'exclude_regexp' => 1,
                           'from_bench' => 'ALL'.$lang,
                           'used_in' => join(', ', @{$flag->[0]}) );
              if ($rf->{'mismatches'}->{$class}->{$bench}->{$onetune}->[$i]) {
                $opts{'mismatch'} = $mismatchtype{$class};
              }
              if (exists $flag->[2]->{'nomap'}) {
                # This is a pseudo-flag added by the tools, so just display
                # the name.
                $opts{'flagref'} = $flag->[2];
                $opts{'skip_all'} = 1;
              }
              my $origin = $flag->[2]->{'origin'}->[1];
              my ($afters, @flagdesc) = dump_single_flag(
                                                $r->{'flaginfo'}->{$origin},
                                                $flag->[2]->{'name'},
                                                $flag->[1],
                                                %opts,
                                                'sub_text' => 1,
                                                'flagref' => $flag->[2]);
              push @output, @flagdesc;
              push @afters, @{$afters} if isa($afters, 'ARRAY');
            }
            push @output, '</ul>';
            if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $onetune)) {
              $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$onetune};
            }
            $donebench{$bench}++;
          }
        }
      }
      if ($mismatch) {
        if ($class eq 'optimization') {
          push @output, '<p><span class="opmismatch">Some optimization flags</span> were found in portability variables.</p>';
        } elsif ($class eq 'portability') {
          push @output, '<p><span class="pomismatch">Some portability flags</span> were found in optimization variables.</p>';
        }
      }
      $mismatch = 0;
      if ($printed_title) {
        push @output, '</div>';
        push @output, '<hr />';
      }

      # Next dump class flags by tuning level, with the common per-language
      # set at the top, followed by benchmark-specific settings
      foreach my $tune (@tunes) {
        my $printed_tune = 0;
        my $classref = undef;
        if (::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $tune)) {
          $classref = $rf->{'bylang'}->{'flaglist'}->{$class}->{$tune};
        }
        foreach my $lang (sort ::bylang keys %langstodo) {
          last if $class eq 'portability'; # Portability is by benchmark
          my $printed_lang = 0;
          my $langname = $lang;
          $langname =~ s/ /_/g;

          # First check for by-language list
          if (defined($classref) &&
              ::check_elem('ARRAY', $classref, $lang) &&
              @{$classref->{$lang}}) {
            if (!$printed_tune) {
              push @output, '', '<div class="'.$class.'flags">';
              push @output, '<h2>'.ucfirst($tune).' '.$class2title{$class}.'</h2>';
              $printed_tune = 1;
            }
            if (!$printed_lang) {
              my $langtitle = $rf->{'var2desc'}->{$lang};
              if ($rf->{'langmatch'}->{$class}->{$tune}->{$lang} == 2) {
                $langtitle .= ' (except as noted below)';
              }
              push @output, "<h3><a name=\"${langname}$tune$class\">$langtitle</a></h3>";
              $printed_lang = 1;
            }
            push @output, '<ul class="flagDescList">';
            for(my $i = 0; $i < @{$classref->{$lang}}; $i++) {
              my $flag = $classref->{$lang}->[$i];
              next unless (istrue($flag->[2]->{'display'}) || $r->{'review'});
              my $string = $rf->{'bylang'}->{'stringlist'}->{$class}->{$tune}->{$lang}->[$i];
              my %opts = ( 'exclude_regexp' => 1,
                           'from_bench' => $langname.$tune,
                           'used_in' => join(', ', @{$flag->[0]}) );
              if ($rf->{'bylang'}->{'mismatches'}->{$class}->{$tune}->{$lang}->[$i]) {
                $opts{'mismatch'} = $mismatchtype{$class};
              }
              if (exists $flag->[2]->{'nomap'}) {
                # This is a pseudo-flag added by the tools, so just display
                # the name.
                $opts{'flagref'} = $flag->[2];
                $opts{'skip_all'} = 1;
              }
              my $origin = $flag->[2]->{'origin'}->[1];
              my ($afters, @flagdesc) = dump_single_flag(
                                                $r->{'flaginfo'}->{$origin},
                                                $flag->[2]->{'name'},
                                                $flag->[1],
                                                %opts,
                                                'sub_text' => 1,
                                                'flagref' => $flag->[2]);
              push @output, @flagdesc;
              push @afters, @{$afters} if isa($afters, 'ARRAY');
            }
            push @output, '</ul>';
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $tune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$tune}->{$lang};
            }
          }

          # Now do the benchmark-specific list (if any)
          if (::check_elem('HASH', $rf, 'flaglist', $class)) {
            my $classref = $rf->{'flaglist'}->{$class};
            foreach my $bench (sort keys %{$classref}) {
              next if $donebench{$bench};
              next if $rf->{'langs'}->{$bench}->{$tune} ne $lang;
              next unless ::check_elem('ARRAY', $classref, $bench, $tune);
              next unless @{$classref->{$bench}->{$tune}};
              if (!$printed_tune) {
                push @output, '', '<div class="'.$class.'flags">';
                push @output, '<h2>'.ucfirst($tune).' '.$class2title{$class}.'</h2>';
                $printed_tune = 1;
              }
              if (!$printed_lang) {
                push @output, "<h3><a name=\"${langname}$tune$class\">$rf->{'var2desc'}->{$lang}</a></h3>";
                $printed_lang = 1;
              }
              push @output, "<h4><a name=\"$class$bench$tune\">$bench</a></h4>";
              push @output, '<ul class="flagDescList">';
              for(my $i = 0; $i < @{$classref->{$bench}->{$tune}}; $i++) {
                my $flag = $classref->{$bench}->{$tune}->[$i];
                next unless (istrue($flag->[2]->{'display'}) || $r->{'review'});
                my $string = $rf->{'stringlist'}->{$class}->{$bench}->{$tune}->[$i];
                my %opts = ( 'exclude_regexp' => 1,
                             'from_bench' => $tune.join('', @{$flag->[0]}),
                             'used_in' => join(', ', @{$flag->[0]}) );
                if ($rf->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i]) {
                  $opts{'mismatch'} = $mismatchtype{$class};
                }
                if (exists $flag->[2]->{'nomap'}) {
                  # This is a pseudo-flag added by the tools, so just display
                  # the name.
                  $opts{'flagref'} = $flag->[2];
                  $opts{'skip_all'} = 1;
                }
                my $origin = $flag->[2]->{'origin'}->[1];
                $opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
                my ($afters, @flagdesc) = dump_single_flag(
                                                  $r->{'flaginfo'}->{$origin},
                                                  $flag->[2]->{'name'},
                                                  $flag->[1],
                                                  %opts,
                                                  'sub_text' => 1,
                                                  'flagref' => $flag->[2]);
                push @output, @flagdesc;
                push @afters, @{$afters} if isa($afters, 'ARRAY');
              }
              push @output, '</ul>';
              if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
              }
            }
          }
        }

        if ($class eq 'portability') {
          # Do the portability flags on a per-benchmark basis; this is mostly
          # a copy of the code above.
          if (::check_elem('HASH', $rf, 'flaglist', $class)) {
            my $classref = $rf->{'flaglist'}->{$class};
            foreach my $bench (sort keys %{$classref}) {
              next if $donebench{$bench};
              next unless ::check_elem('ARRAY', $classref, $bench, $tune);
              next unless @{$classref->{$bench}->{$tune}};
              if (!$printed_tune) {
                push @output, '', '<div class="'.$class.'flags">';
                push @output, '<h2>'.ucfirst($tune).' '.$class2title{$class}.'</h2>';
                $printed_tune = 1;
              }
              push @output, "<h4><a name=\"$class$bench$tune\">$bench</a></h4>";
              push @output, '<ul class="flagDescList">';
              for(my $i = 0; $i < @{$classref->{$bench}->{$tune}}; $i++) {
                my $flag = $classref->{$bench}->{$tune}->[$i];
                next unless (istrue($flag->[2]->{'display'}) || $r->{'review'});
                my $string = $rf->{'stringlist'}->{$class}->{$bench}->{$tune}->[$i];
                my %opts = ( 'exclude_regexp' => 1,
                             'from_bench' => $tune.join('', @{$flag->[0]}),
                             'used_in' => join(', ', @{$flag->[0]}) );
                if ($rf->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i]) {
                  $opts{'mismatch'} = $mismatchtype{$class};
                }
                if (exists $flag->[2]->{'nomap'}) {
                  # This is a pseudo-flag added by the tools, so just display
                  # the name.
                  $opts{'flagref'} = $flag->[2];
                  $opts{'skip_all'} = 1;
                }
                my $origin = $flag->[2]->{'origin'}->[1];
                $opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
                my ($afters, @flagdesc) = dump_single_flag(
                                                  $r->{'flaginfo'}->{$origin},
                                                  $flag->[2]->{'name'},
                                                  $flag->[1],
                                                  %opts,
                                                  'sub_text' => 1,
                                                  'flagref' => $flag->[2]);
                push @output, @flagdesc;
                push @afters, @{$afters} if isa($afters, 'ARRAY');
              }
              push @output, '</ul>';
              if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
              }
            }
          }
        }

        if ($mismatch) {
          if ($class eq 'optimization') {
            push @output, '<p><span class="opmismatch">Some optimization flags</span> were found in portability variables.</p>';
          } elsif ($class eq 'portability') {
            push @output, '<p><span class="pomismatch">Some portability flags</span> were found in optimization variables.</p>';
          }
        }
        $mismatch = 0;

        if ($printed_tune) {
          push @output, '</div>';
          push @output, '<hr />';
        }
      }
    }

    # Dump the flags that were included by flags above, and must appear in
    # this file.  This _SHOULD_ only be user flags.
    my %seen_flags = ();
    if (@afters) {
      push @output, '<h2>Implicitly Included Flags</h2>';
      push @output, '<p>This section contains descriptions of flags that were included implicitly';
      push @output, 'by other flags, but which do not have a permanent home at SPEC.</p>';
      push @output, '<ul class="flagDescList">';
      while (@afters) {
        my $incref = shift(@afters);
        next unless isa($incref, 'ARRAY');
        my ($flag, $text) = @{$incref};
        next unless (istrue($flag->{'display'}) || $r->{'review'}); # Shouldn't happen
        my $id = ::make_flag_id($flag, undef, $text);
        next if exists $seen_flags{$id};
        $seen_flags{$id}++;
        my $origin = $flag->{'origin'}->[1];
        my ($afters, @flagdesc) = dump_single_flag($r->{'flaginfo'}->{$origin},
                                   $flag->{'name'},
                                   $text,
                                   'exclude_regexp' => 1,
                                   'sub_text' => 1,
                                   'flagref' => $flag);
        push @output, @flagdesc;
        unshift @afters, @{$afters};
      }
      push @output, '</ul>';
      push @output, '<hr />';
    }

    push @output, dump_text_sections($userhead);

    # Explain the flag markings
    push @output, '<div class="flagorigins">';
    push @output, ' <p>Flag description origin markings:</p>';
    # This used to be done with a dictionary list, but it looked really bad
    # so I'm committing The Sin and using a table:
    push @output, ' <table>';
    push @output, '  <tr>';
    push @output, "   <td><a name=\"userOrigin\"><img src=\"${main::spec_flags_base}user.png\" alt=\"[user]\" /></a></td>";
    push @output, '   <td>Indicates that the flag description came from the user flags file.</td>';
    push @output, '  </tr>';
    push @output, '  <tr>';
    push @output, "   <td><a name=\"suiteOrigin\"><img src=\"${main::spec_flags_base}suite.png\" alt=\"[suite]\" /></a></td>";
    push @output, '   <td>Indicates that the flag description came from the suite-wide flags file.</td>';
    push @output, '  </tr>';
    push @output, '  <tr>';
    push @output, "   <td><a name=\"benchmarkOrigin\"><img src=\"${main::spec_flags_base}benchmark.png\" alt=\"[benchmark]\" /></a></td>";
    push @output, '   <td>Indicates that the flag description came from a per-benchmark flags file.</td>';
    push @output, '  </tr>';
    push @output, ' </table>';
    push @output, '</div>';

    if (defined($::website_formatter) && $::website_formatter &&
        defined($r->{'flagsurl'}) && $r->{'flagsurl'} ne '') {
      my $urls = $r->{'flagsurl'};
      if (!isa($urls, 'ARRAY')) {
          # Shouldn't happen, but just in case
          $urls = [ $urls ];
      }
      my $count = @{$urls};
      my $plural = ($count > 1) ? 's' : '';
      my (@html_output, @xml_output);
      foreach my $url (@{$urls}) {
          my $html_url = $url;
          $html_url =~ s/\.xml$/\.html/;
          my $br = ($count < @{$urls}) ? '<br />' : '';
          my $punct = ($count == 1) ? '.' : ',';
          push @html_output, qq|   $br<a href="$html_url">|.::escape_HTML($html_url)."</a>$punct";
          push @xml_output,  qq|   $br<a href="$url">|.::escape_HTML($url)."</a>$punct";
          $count--;
      }

      push @output, '<hr />';
      push @output, '<div class="notes flagfooter">';
      my $flag_link = ($::lcsuite =~ /(cpu(2006|v6)|mpi2007|omp20(01|12))/) ? 'faq.html#flagsfile' : 'flag-description.html';
      if (@{$urls} > 1) {
          push @output, qq|  <p>The <a href="http://www.spec.org/auto/$::lcsuite/Docs/$flag_link">flags files</a> that were used to format this result can be browsed at<br />|;
      } else {
          push @output, qq|  <p>The <a href="http://www.spec.org/auto/$::lcsuite/Docs/$flag_link">flags file</a> that was used to format this result can be browsed at<br />|;
      }
      push @output, @html_output,
                    qq|  </p>|;
      push @output, qq|  <p style="font-size: 75%">You can also download the XML flags source${plural} by saving the following link${plural}:<br />|;
      push @output, @xml_output,
                    qq|  </p>|;
      push @output, '</div>';
    }

    # Now do the footer
    push @output, '<hr />';
    push @output, '<p style="text-align: left">For questions about the meanings of these flags, please contact the tester.<br />';
    push @output, 'For other inquiries, please contact <a href="mailto:webmaster@spec.org">webmaster@spec.org</a><br />';
    push @output, 'Copyright '.::copyright_dates().' Standard Performance Evaluation Corporation<br />';
    push @output, "Tested with SPEC $::suite v".$r->{'suitever'}.'.<br />';
    push @output, 'Report generated on '.&::ctime(time)." by SPEC $::suite flags formatter v$flags_version.</p>";
    push @output, '</body>';
    push @output, '</html>';

    return (\@output, $written);
}

sub dump_single_flag {
    # This dumps one single flag as a list element.  Enclosing <ul> or <ol>
    # tags are not generated.
    # Options recognized:
    #  mismatch:        puts the flag title into one of the "mismatched"
    #                   classes.  These are "po" (for portability option in
    #                   a non-portability variable) and "op" (optimization in
    #                   a portability variable), and "c" (compiler in non-
    #                   compiler variable)
    # exclude_regexp:   Omit the output of the regular expression
    # exclude_include:  Do not link to included flags
    # include_class:    Output the flag's classification
    # exclude_compiler: If the flag specifies that it goes with a particular
    #                   compiler, do not output the name of the compiler that
    #                   it goes with.
    # do_replacements:  Use the contents of the flags ex_replacements array
    #                   to do replacements in the description text.
    # no_origin:        Do not use the origin icon for the flag
    # from_bench:       If the flag is part of a set of benchmark flags,
    #                    put the benchmark name in this option.  Used when
    #                    generating flag IDs.
    # used_in:          If this option contains a non-empty string, it will
    #                    be used as the name of the variable in which the
    #                    flag was found.
    # flagref:          If set to a reference to a flag, the flag to dump
    #                    will not be looked up by name in the flagmap.
    # skip_all:         Print only the flag name
    # no_link_flagtext: Do not generate the link based on the title
    # sub_text:         Use the flag's regexp to match against $title and do
    #                    substitutions into the description.  Normally this
    #                    happens at match time, but it needs to be re-done
    #                    for aggregated flags and for included flags with
    #                    parameters.

    my ($flags, $flagname, $title, %opts) = @_;
    my @output = ();
    my @afters = ();

    return ([], '') unless defined($flagname);

    my $flag;
    if (exists($opts{'flagref'}) && isa($opts{'flagref'}, 'HASH')) {
      $flag = $opts{'flagref'};
    } else {
      $flag = $flags->{'flagmap'}->{$flagname};
    }
    return ([], '') unless isa($flag, 'HASH');

    my @replacements = ();

    # Decide on the descriptive text
    my $desc = $flag->{'description'};
    if (defined($title) && $title ne '' && $opts{'sub_text'}) {
      @replacements = ::one_match($flag, $title);
      $desc = ::do_replacements($desc, @replacements);
      $opts{'do_replacements'} = 0;     # Already done!
    }

    # Do title, if it doesn't have one
    $title = $flag->{'example'} unless $title ne '';

    my $nameclass = 'flagName';
    if (exists $opts{'mismatch'} && $opts{'mismatch'} ne '') {
      $nameclass .= " $opts{'mismatch'}class";
    }

    my $tag_text = $title unless $opts{'no_link_flagtext'};
    my $id = ::make_flag_id($flag, $opts{'from_bench'}, $tag_text, 1);
    push @output, '<li>';
    push @output, '<div class="flagDesc">';
    push @output, '<ul class="flagDesc">';

    push @output, "<li class=\"$nameclass\"><a name=\"$id\" id=\"$id\">".escape_html_entities($title)."</a></li>";

    if ($opts{'skip_all'}) {
      # Finish what we started
      push @output, '</ul>';
      push @output, '</div>';
      push @output, '</li>';
      return ([], @output);
    }

    # Indicate the origin
    if (!(exists($opts{'no_origin'}) && $opts{'no_origin'}) &&
        @{$flag->{'origin'}}) {
      my $origin;
      if ($flag->{'origin'}->[1] eq 'user') {
        $origin = 'user';
      } elsif ($flag->{'origin'}->[1] eq 'suite') {
        $origin = 'suite';
      } elsif ($flag->{'origin'}->[1] =~ /^\d{3}\.\S+$/) {
        $origin = 'benchmark';
      } else {
        ::Log(0, "WARNING: Unknown flag origin \"$flag->{'origin'}->[1]\"\n");
        $origin = 'unknown';
      }
      push @output, "<li class=\"flagOrigin\"><a href=\"#${origin}Origin\"><img src=\"${main::spec_flags_base}$origin.png\" alt=\"[$origin]\" /></a></li>";
    }
    
    if ($opts{'include_class'}) {
        push @output, '<li class="flagDescClass">'.$flag->{'class'}.'</li>';
    }
    if (!$opts{'exclude_compiler'} && $flag->{'compilers'} ne '') {
        push @output, '<li class="flagDescCompiler">'.$flag->{'compilers'}.'</li>';
    }
    if (!$opts{'exclude_regexp'} && $flag->{'regexp'} ne '') {
        push @output, '<li class="flagDescRE"><span class="tt">'.escape_html_entities($flag->{'regexp'}).'</span></li>';
    }
    if (!$opts{'exclude_parallel'} && $flag->{'parallel'} ne '') {
        push @output, '<li class="flagDescParallel"><span class="tt">'.(istrue($flag->{'parallel'}) ? 'Yes' : 'No').'</span></li>';
    }

    if (exists $opts{'used_in'} && $opts{'used_in'} ne '') {
      push @output, '<li class="flagVar">'.$opts{'used_in'}.'</li>';
    }

    push @output, '<li class="descText">';
    if ($opts{'do_replacements'}
        && ::isa($flag->{'ex_replacement'}, 'ARRAY')
        && @{$flag->{'ex_replacement'}}) {
        my $tmpdesc = ::do_replacements($flag->{'description'}, @{$flag->{'ex_replacement'}});
        push @output, $tmpdesc;
    } else {
        push @output, $desc;
    }
    push @output, '</li>';

    if (!$opts{'exclude_include'} && ref($flag->{'inc_flag'}) eq 'ARRAY'
        && @{$flag->{'inc_flag'}}) {
        push @output, '<li class="flagDescInc"><span class="flagDescInc">Includes:</span>';
        push @output, ' <ul class="flagTOC">';
        foreach my $incref (@{$flag->{'inc_flag'}}) {
            next unless isa($incref, 'ARRAY');
            my ($name, $text) = @{$incref};
            if (   $opts{'do_replacements'}
                && @replacements == 0
                && isa($flag->{'ex_replacement'}, 'ARRAY')
                && @{$flag->{'ex_replacement'}}) {
                @replacements = @{$flag->{'ex_replacement'}};
            }
            if ($opts{'do_replacements'} || $opts{'sub_text'}) {
                $text = ::do_replacements($text, @replacements);
            }
            my $isuser = ($flag->{'origin'}->[1] eq 'user');
	    push @output, flag_link_tree('  ', $flags, $name,
					 'with_name' => 1,
					 'internal_link' => $isuser,
                                         'link_text' => $text,
                                         'do_replacements' => $opts{'do_replacements'},
                                         'sub_text' => $opts{'sub_text'},
                                         'no_link_flagtext' => $opts{'no_link_flagtext'});
            push @afters, [ $flags->{'flagmap'}->{$name}, $text ] if $isuser;
        }
        push @output, ' </ul>';
        push @output, '<div class="clear"></div>';
        push @output, '</li>';
    }

    push @output, '</ul>';
    push @output, '</div>';
    push @output, '</li>';

    return (\@afters, @output);
}

sub flags_to_html {
    my ($flags, $fh, $review) = @_;

    if (ref($flags) ne 'HASH') {
	::Log(0, "\nflags_to_html: flags data structure is not of the correct type!\n");
	return;
    } elsif (!exists($flags->{'flag'})) {
	::Log(0, "\nflags_to_html: flags list contains no flag descriptions!\n");
	return;
    } elsif (ref($flags->{'flag'}) ne 'ARRAY') {
	::Log(0, "\nflags_to_html: flags list is not of the correct type!\n");
	return;
    }

    if (!defined($fh)) {
	# Dump it to stdout
	$fh = new IO::Handle;
	if (!$fh->fdopen(fileno(STDOUT),'w')) {
	    ::Log(0, "\nCouldn't open STDOUT for writing: $!\n");
	    return;
	}
    }
    my $title = (exists($flags->{'title'}) ? $flags->{'title'} :
		 "SPEC $::suite flag description file");
    $fh->print(join("\n", xhtml_header($title, $flags->{'style'})));
    $fh->print("<body>\n");
    $fh->print("<h1>$title</h1>\n");

    if (exists $flags->{'header'}->{'default'}) {
	$fh->print($flags->{'header'}->{'default'});
    }

    # Get a list of all the classes used
    my @flags = @{$flags->{'flag'}};
    my %classes = map { $_ => 1 } map { $_->{'class'} } @flags;

    # Here's the order in which to present the known classes.
    my @do_classes = qw(optimization portability
			compiler other
                        mandatory forbidden
			unknown);

    foreach my $class (@do_classes) {
	delete $classes{$class};
    }

    # In case there were some not listed in @do_classes...
    push @do_classes, sort keys %classes;

    # Make the section TOC
    my $section_TOC_header_printed = 0;
    foreach my $class (@do_classes) {
      if (grep { $_->{'class'} eq $class } @flags) {
        if (! $section_TOC_header_printed) {
            $fh->print("<div class=\"NoPrint\">\n");
            $fh->print("<h3>Sections</h3>\n");
            $fh->print("<p>Selecting one of the following will take you directly to that section:</p>\n");
            $fh->print("<ul>\n");
            $section_TOC_header_printed = 1;
        }
        $fh->print("<li><a href=\"#Section_$class\">".ucfirst($class)." Flags</a></li>\n");
      }
    }
    if ($section_TOC_header_printed) {
        $fh->print("</ul>\n");
        $fh->print("</div>\n\n");
        $fh->print("<hr />\n\n");
    }

    foreach my $class (@do_classes) {
	# Make a list of flags of this class, in the order in which they appear
        # in the original XML file.
	my @flags_in_class = grep { $_->{'class'} eq $class } @flags;
	next unless (@flags_in_class);

	$fh->print("<div class=\"flagClass\" id=\"$class\">\n");
	$fh->print("<h2><a name=\"Section_$class\" id=\"Section_$class\">".ucfirst($class)." Flags</a></h2>\n");
        if (exists $flags->{'header'}->{$class}) {
            $fh->print($flags->{'header'}->{$class});
        }

        # Dump a TOC of flag links
        $fh->print("<div class=\"flagTOC\">\n");
        $fh->print("<ul class=\"flagTOC\">\n");
	foreach my $flagref (@flags_in_class) {
	    next unless (istrue($flagref->{'display'}) || $review);
	    $fh->print(join("\n", flag_link_tree(' ', $flags,
						 $flagref->{'name'},
						 'with_name' => 1,
                                                 'do_replacements' => 1,
						 'internal_link' => 1,
                                                 'no_link_flagtext' => 1))."\n");
        }
        $fh->print("</ul>\n");
        $fh->print("<div class=\"clear\"></div>\n");
        $fh->print("</div>\n\n");

        $fh->print("<ul class=\"flagDescList\">\n");
        # Now dump the flags themselves
	foreach my $flagref (@flags_in_class) {
	  $fh->print("<!--\n") unless (istrue($flagref->{'display'}) || $review);
          my @lines = dump_single_flag($flags, $flagref->{'name'}, undef,
                                       'do_replacements' => 1,
                                       'no_origin' => 1,
                                       'no_link_flagtext' => 1);
          shift @lines; # No "afters" wanted
          $fh->print(join("\n", @lines)."\n");
	  $fh->print("-->\n") unless (istrue($flagref->{'display'}) || $review);
	}
        $fh->print("</ul>\n\n");
	$fh->print("</div>\n");
	$fh->print("<hr />\n");
	delete $classes{$class};
    }

    my $tmp = join("\n", dump_text_sections($flags));
    $fh->print($tmp."\n") if ($tmp ne '');
      
    # Undocumented feature... :) If there's a "dumpit" element and it's non-
    # zero and non-empty, you get a nice Data::Dumper dump of everything.
    if (exists($flags->{'dumpit'}) && $flags->{'dumpit'}) {
	my @save = ($Data::Dumper::Indent, $Data::Dumper::Useqq,
		    $Data::Dumper::Purity, $Data::Dumper::Sortkeys);
	$Data::Dumper::Indent = 1;
	$Data::Dumper::Useqq = 1;
	$Data::Dumper::Purity = 0;
	$Data::Dumper::Sortkeys = 1;
	$fh->print("<pre>\n");
	$fh->print(escape_html_entities(Data::Dumper->Dump([$flags],['flags']))."\n");
	$fh->print("</pre>\n");
	($Data::Dumper::Indent, $Data::Dumper::Useqq,
	 $Data::Dumper::Purity, $Data::Dumper::Sortkeys) = @save;
    }

    $fh->print("</body>\n</html>\n");
}

sub xhtml_header {
    my ($title, $style) = @_;
    my @output = ();

    my %html_style;
    my @common_style;
    if (!defined $::website_formatter || !$::website_formatter) {
      @common_style = map { tr/\012\015//d; $_ } ::read_file(::jp($ENV{'SPEC'}, 'Docs', 'css', "${main::lcsuite}result.css"));
    } else {
      @common_style = ('  @import url(http://www.spec.org/includes/css/'.${main::lcsuite}.'result.css);');
    }
    foreach my $media (qw(screen print)) {
        @{$html_style{$media}} = ('<style type="text/css" id="internal'.ucfirst($media)."\" media=\"$media\">", '<!--', @common_style);
        my $done = 0;
        if (!defined $::website_formatter || !$::website_formatter) {
            # Try to read it in from a file
            my $path = jp($ENV{'SPEC'}, 'Docs', 'css', "${main::lcsuite}${media}.css");
            my @tmp_style = map { tr/\012\015//d; $_ } ::read_file($path);
            if (@tmp_style) {
                push @{$html_style{$media}}, @tmp_style;
                $done = 1;
            }
        }
        if (!$done) {
          push @{$html_style{$media}}, '  @import url(http://www.spec.org/includes/css/'.${main::lcsuite}.${media}.'.css);';
        }
        push @{$html_style{$media}}, '-->', '</style>';
    }

    # It's too bad that Internet Explorer sucks SO hard... we could almost
    # enter the late '90s!
    if (0) {
      push @output, '<?xml version="1.0" encoding="utf-8"?>';
      push @output, '<?xml-stylesheet href="http://www.w3.org/StyleSheets/TR/W3C-REC.css" type="text/css"?>';
      push @output, '<?xml-stylesheet href="#internalScreen" type="text/css" media="screen"?>';
      push @output, '<?xml-stylesheet href="#internalPrint" type="text/css" media="print"?>';
    }
    push @output, '<!DOCTYPE html';
    push @output, '     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"';
    push @output, '     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">';
    push @output, '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">';
    push @output, '<head>';
    push @output, "<meta name=\"generator\" content=\"SPEC $::suite Tools $::version (flags v$flags_version)\" />";
    push @output, qq|<meta http-equiv="Content-type" content="text/html;charset=utf-8" />|;
    push @output, @{$html_style{'screen'}};
    push @output, @{$html_style{'print'}};
    if ($style ne '') {
	push @output, '<style type="text/css" id="user">';
        push @output, $style;
        push @output, '</style>';
    }
    push @output, "<title>$title</title>";
    push @output, '</head>';

    return @output;
}

sub get_linktext {
    my ($flag, %opts) = @_;

    my $linktext = '';
    if ($opts{'with_name'}) {
        if ($opts{'escape_name'}) {
            $linktext .= escape_html_entities($flag->{'example'});
        } else {
            $linktext .= $flag->{'example'};
        }
    }
    $linktext = $opts{'link_text'} if ($opts{'link_text'} ne '');
    $linktext = $linktext.$opts{'link_text_after'} if ($opts{'link_text_after'} ne '');
    $linktext = $opts{'link_text_before'}.$linktext if ($opts{'link_text_before'} ne '');
    # Strip leading and trailing whitespace
    $linktext =~ s/^\s+//s;
    $linktext =~ s/\s+$//s;
    return $linktext;
}

sub link_to_flag {
    # Make a link to a flag
    my ($flag, %opts) = @_;
    my ($url, $source) = @{$flag->{'origin'}};

    my $linktext = '>'.get_linktext($flag, %opts, 'escape_name' => 1).'</a>';

    if (defined($opts{'target_url'}) && $opts{'target_url'} ne '') {
        $url = $opts{'target_url'};
        $source = 'user';       # So that it will be used to make links
    }

    my $href = '#'.::make_flag_id($flag, $opts{'from_bench'}, $opts{'tag_text'});
    if ($opts{'internal_link'} || $opts{'link_from'} eq $source) {
        # That's it!
        return "<a href=\"$href\"".$linktext;
    } elsif ($opts{'file_link'}) {
        if ($url =~ /^https?:/) {
            # This should be an error, since we can't convert from external
            # ref to file, but just leave it as-is
            return "<a href=\"$url$href\"$linktext";
        } else {
            return "<a href=\"file://$url$href\"$linktext";
        }
    } else { # It's an external link
        if ($source ne 'user') {
            my $uri = URI->new($url);
            my @path_segs = $uri->path_segments;
            if ($path_segs[-1] eq 'flags.xml' && $path_segs[-3] =~ /^\d{3}\.\S+$/) {
                # It's from a benchmark, so the location is easy!
                return "<a href=\"${main::spec_flags_base}$source.flags.html$href\"$linktext";
            } elsif ($source eq 'suite') {
                # It's the suite flags, so it's easy again!
                return "<a href=\"${main::spec_flags_base}${main::suite}.flags.html$href\"$linktext";
            }
        } else {
            if ($url =~ /^https?:/) {
                # Already from a URL; just use that
                return "<a href=\"$url$href\"$linktext";
            } else {
                # It's from a user's flag file on the local filesystem...
                return "<a href=\"file://$url$href\"$linktext";
            }
        }
    }
}

sub flag_link_tree {
    my ($prefix, $flags, $flagname, %opts) = @_;
    my @output;
    $opts{'seen'} = {} unless isa($opts{'seen'}, 'HASH');

    return () unless exists($flags->{'flagmap'}->{$flagname}) && isa($flags->{'flagmap'}->{$flagname}, 'HASH');
    my $flag = $flags->{'flagmap'}->{$flagname};

    # Deal with any link text we've gotten (as from included flags with
    # parameters)
    my $tagtext = undef;
    if (exists $opts{'link_text'}) {
      if (!defined($opts{'link_text'}) || $opts{'link_text'} eq '') {
        delete $opts{'link_text'};
      } elsif (!exists $opts{'no_link_flagtext'} ||
               !$opts{'no_link_flagtext'}) {
        $tagtext = $opts{'link_text'};
      }
    }

    my $start = "$prefix<li>".link_to_flag($flag, %opts, 'tag_text' => $tagtext);
    my $link_text = $opts{'link_text'};
    delete $opts{'link_text'};
    if (exists($flag->{'inc_flag'}) && isa($flag->{'inc_flag'}, 'ARRAY') &&
        # Weed out flags that have already been seen, in order to avoid loops
        grep { !exists($opts{'seen'}->{$_->[0]}) } @{$flag->{'inc_flag'}}) {
      push @output, $start;
      push @output, "$prefix <ul class=\"SubMenu\">";
      my %seen = %{$opts{'seen'}};      # Save it
      my @replacements = ();
      foreach my $incref (@{$flag->{'inc_flag'}}) {
          my ($name, $text) = @{$incref};
          next if exists($opts{'seen'}->{$name});
          # Check to see whether or not this text wants substitutions
          if ($opts{'do_replacements'} || $opts{'sub_text'}) {
              if ($text =~ /(?!<=\\)\$\d/) {
                  # It does.  Prepare the replacements, if necessary
                  if (@replacements == 0) {
                      if ($link_text eq '' && $opts{'do_replacements'}) {
                          if (   ::isa($flag->{'ex_replacement'}, 'ARRAY')
                              && @{$flag->{'ex_replacement'}}) {
                              @replacements = @{$flag->{'ex_replacement'}};
                          }
                      }
                      # Give it another chance here, in case ex_replacements
                      # is empty.
                      if ($opts{'sub_text'} || @replacements == 0) {
                          my $tmpflagtext;
                          if ($link_text ne '') {
                              $tmpflagtext = $link_text;
                          } else {
                              $tmpflagtext = get_linktext($flag, %opts);
                          }
                          @replacements = ::one_match($flag, $tmpflagtext, 1);
                      }
                  }
                  $text = ::do_replacements($text, @replacements);
              }
          }
          $opts{'seen'}->{$name}++;
          push @output, flag_link_tree($prefix.'  ', $flags, $name, %opts,
                                       'link_text' => $text);
          %{$opts{'seen'}} = %seen;         # Restore it
      }
      push @output, $prefix.' </ul>';
      push @output, $prefix.'</li>';
    } else {
      push @output, $start.'</li>';
    }

    return @output;
}

sub dump_text_sections {
    my ($desc) = @_;
    my @output = ();

    if (exists $desc->{'platform_settings'}) {
        # This must be a pre-v1.2 flags file
        push @output, '<h2><a name="otherTuningInfo">System and Other Tuning Information</a></h2>';
        push @output, $desc->{'platform_settings'};
        push @output, '<hr />';
    } else {
        # If a pre-v1.2 flags file has no platform_settings section, it also
        # won't have any of the new sections, and no output will be generated
        foreach my $secref (
            [ 'submit_command',	'<h2><a name="submitCommand">Commands and Options Used to Submit Benchmark Runs</a></h2>' ],
            [ 'sw_environment',	'<h2><a name="softwareEnvironment">Shell, Environment, and Other Software Settings</a></h2>' ],
            [ 'os_tuning',	'<h2><a name="OStuning">Operating System Tuning Parameters</a></h2>' ],
            [ 'virtual_machine',	'<h2><a name="VMinfo">Virtualization Settings</a></h2>' ],
            [ 'firmware',	'<h2><a name="firmwareSettings">Firmware / BIOS / Microcode Settings</a></h2>' ],
            [ 'parts',		'<h2><a name="systemParts">Hardware and Software Parts or Options Used</a></h2>' ],
            ) {
            my ($var, $head) = @{$secref};
            if (exists $desc->{$var}) {
              push @output, $head;
              push @output, $desc->{$var};
              push @output, '<hr />';
            }
        }
    }

    return @output;
}

sub escape_html_entities {
    my ($text) = @_;

    $text =~ s/\&/\&amp;/g;
    $text =~ s/</\&lt;/g;
    $text =~ s/>/\&gt;/g;

    return $text;
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub istrue { main::istrue(@_); }
sub Log { main::Log(@_); }

1;
