#
#  html.pl - produces HTML output
#  Copyright 1999-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author:  Cloyce D. Spradling
#
# $Id: html.pl 6400 2011-03-31 18:21:54Z cloyce $

use strict;
use File::Basename;
use File::Path;
use File::Spec;
use IO::File;
use Cwd;
use GD;
use MIME::Base64;
use UNIVERSAL qw(isa);
use URI;
#use Data::Dumper;

require 'util.pl';
require 'flagutils.pl';

use vars qw($name $extension $synonyms $sortdir @image_data %seen_anchors);

$name      = 'HTML';
$extension = 'html';
$synonyms  = { map { lc($_) => 1 } ($name, qw(xhtml web www)) };

my $prefix = $Spec::Format::raw::prefix;
$Spec::Format::html::part_of_all = 1;   # Part of '-o all'
my $html_version = '$LastChangedRevision: 6400 $ '; # Make emacs happier
$html_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'html.pl'} = $html_version;
my $image_url = (defined($::website_formatter) &&
		 $::website_formatter) ? $::image_url_path : '';

my $logo_location = "logo037.gif";
my $basebar = "basebar.gif";
my $peakbar = "peakbar.gif";
my @logo_size = (37,55);	# We could use Image::Size,
                                # but it's not going to change.
my $table_width = 700;		# Table height will be figgerd out later
my $written = [];		# List of files written
$sortdir = 0;			# Used in number_tick and byspot
my $debug = 0;
my %trademarks_done = ();
my %code2mark = ( 'r' => '&reg;',
                  't' => '&trade;',
                  's' => '<sup>SM</sup>',
                );

sub format {
    my ($me, $r, $fn) = @_;
    $written = [];
    %seen_anchors = ();

    return HTML_report($me, $r, $fn);
}

sub HTML_report {
    my ($me, $r, $fn) = @_;
    my @output = ();
    my $tmpstr = '';
    my $invalid = $r->{'invalid'} ||
	(isa($r->{'errors'}, 'ARRAY') && @{$r->{'errors'}}+0 > 0);
    my @nc = ::allof($r->{'nc'});
    my $is_nc = 0;
    if (@nc) {
        if (::istrue($r->{'nc_is_cd'})) {
            $is_nc = 3; # CD
        } elsif (::istrue($r->{'nc_is_na'})) {
            $is_nc = 2; # NA
        } else {
            $is_nc = 1; # NC
        }
    }
    my ($barename, $outputpath);
    my $base_url;
    if (defined($::website_formatter) && $::website_formatter) {
      # Get a non-relative path.
      $fn = ::unrel_path($fn);
      $outputpath = dirname($fn);
      # Strip the base off of the path, leaving the filename and whatever
      # hierarchy it's in.
      ($barename = $fn) =~ s#^$::report_base/*##;
      if (defined($::format_for_publication) && $::format_for_publication) {
        # Strip out the _second_ path component so that the URL will come
        # out right.
        my @pathcomp = split(m#/#, $barename);
        splice @pathcomp, 1, 1;
        $barename = ::jp(@pathcomp);
      }
      $barename =~ s/\.${extension}$//i;
      $base_url = $::report_url_base.'/'.$barename;
      $barename = basename($fn, '.'.$extension);
    } else {
      if ($fn =~ /^\./) {
          $fn = File::Spec->abs2rel(::unrel_path($fn));
      }
      $base_url = $barename = basename($fn, '.'.${extension});
      $outputpath = dirname($fn).'/';
      $outputpath = '' if ($outputpath =~ /^\.[\/\\]$/o);
    }
    %trademarks_done = ();

    push @output, HTML_head($r, $invalid, $is_nc, $outputpath);
    # Start the body
    push @output, '<body>';
    push @output, '<div class="resultpage">';

    # Make the top title bar thing
    push @output, HTML_title($r, $invalid, $is_nc);

    # Make the result bar
    push @output, HTML_result($r, $invalid, $is_nc);

    # Make the info bar that shows the dates
    push @output, HTML_date($r);

    if ($is_nc) {
	# Make the NC/NA explanation section
	push @output, '<div class="ncsection">';
        push @output, break_lines(' <p>', \@nc, '</p>', 0);
        push @output, '</div>', '';
    }

    # Make the graph
    my ($fname, @tmp_output) = HTML_graph($r, $invalid, $is_nc, $barename, $outputpath);
    return undef unless defined($fname);

    push @output, @tmp_output;
    push @{$written}, $fname if $fname ne '';


    if ($::lcsuite ne 'mpi2007') {
        # Make the hardware and software tables
        push @output, HTML_info($r, { 'system' => 'the system under test' },
                                [ 'Hardware', $r->hardware ],
                                [ 'Software', $r->software ]);
    }

    # Do errors
    push @output, HTML_errors($r, $invalid, $is_nc);

    # Make the table of results
    push @output, HTML_table($r, $invalid, $is_nc);

    if ($::lcsuite eq 'mpi2007') {
        push @output, HTML_mpi_info($r);
    }

    # Do notes
    push @output, HTML_notes($r->notes, $invalid, $is_nc);

    # Do flags
    push @output, HTML_flags($r, $invalid, $is_nc,
                             $barename.'.flags.html', $base_url.'.flags.html');

    push @output, ' <div class="notes footer">';
    push @output, footer();
    push @output, ' </div>';

    push @output, ' <div class="notes footer">';
    push @output, '  <p>For questions about this result, please contact the tester.<br />';
    push @output, '   For other inquiries, please contact <a href="mailto:webmaster@spec.org">webmaster@spec.org</a><br />';
    push @output, '   Copyright '.::copyright_dates().' Standard Performance Evaluation Corporation<br />';
    push @output, "   Tested with SPEC $::suite v".$r->{'suitever'}.'.<br />';
    push @output, '   Report generated on '.&::ctime(time)." by SPEC $::suite HTML formatter v$html_version.</p>";
    push @output, ' </div>';

    if (!defined($::format_for_publication) || !$::format_for_publication) {
        push @output, ' <div class="w3cbutton3">';
        push @output, '   <a href="http://validator.w3.org/check?uri=referer">';
        push @output, '     <span class="w3c">W3C</span>';
        push @output, '     <span class="spec">XHTML 1.0</span>';
        push @output, '   </a>';
        push @output, ' </div>';
        push @output, ' <div class="w3cbutton3">';
        push @output, '   <a href="http://jigsaw.w3.org/css-validator/check/referer">';
        push @output, '     <span class="w3c">W3C</span>';
        push @output, '     <span class="spec">CSS</span>';
        push @output, '   </a>';
        push @output, ' </div>';
    }
    push @output, '</div>';
    push @output, '</body>';
    push @output, '</html>';

    if ($is_nc == 0) {
      push @output, '<!-- The following is an encoded version of the raw file that was used to', '     produce this result. Use the extract_raw script to retrieve it. -->';
      if (defined($r->{'compraw'})) {
	  my %whatcomp = ( '*' => 'BASE64', '&' => 'BZIP2', '@' => 'GZIP' );
	  my $whatcomp = 'BASE64';
	  if ($r->{'compraw'} =~ /^([\@\&\*])/) {
	      $whatcomp = $whatcomp{$1};
	  }
          push @output, "<!-- BEGIN $whatcomp $barename.rsf", split("\n", $r->{'compraw'});
      } elsif (defined($r->{'rawfile'})) {
          # The uncompressed raw file will be BASE64 encoded so that HTML
          # comments in the raw file (notes, for example) won't screw up the
          # page output.
          my $tmp = '*'.encode_base64($r->{'rawfile'});
          push @output, "<!-- BEGIN BASE64 $barename.rsf", split("\n", $tmp);
      } else {
          push @output, '<!-- BEGIN', 'There must have been a problem with the encoding.  This is not a raw file.';
      }
      push @output, 'END -->';
    }

    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    return (\@output, $written);
}

sub HTML_table {
    # Make the table of results
    my ($r, $invalid, $nc) = @_;
    my @output = ();
    my ($rate, $what) = (::istrue($r->{'rate'}), 'copies');
    if ($::lcsuite eq 'mpi2007') {
        ($rate, $what) = (1, 'ranks');
    }
    my $tunewidth = $rate ? 7 : 6;
    my $nc_str = '';
    if ($nc == 3) {
        $nc_str = 'CD';
    } elsif ($nc == 2) {
        $nc_str = 'NA';
    } elsif ($nc) {
        $nc_str = 'NC';
    }

    push @output,   ' <div class="resultstable">';
    push @output,   '  <h2>'.linkto('Results Table').'</h2>';
    push @output,   '  <table summary="Detailed per-benchmark result data">';
    push @output,   '   <colgroup>';
    push @output,   '    <col id="benchmark" />';
    push @output, qq|    <col id="base$what" />| if $rate;
    push @output,   '    <col id="baseres" span="6" />';
    push @output, qq|    <col id="peak$what" />| if $rate;
    push @output,   '    <col id="peakres" span="6" />';
    push @output,   '   </colgroup>';

    push @output,   '   <thead>';
    push @output,   '    <tr>';
    push @output,   '     <th rowspan="2">'.linkto('Benchmark').'</th>';
    push @output, qq|     <th class="basecol" colspan="${tunewidth}">Base</th>|;
    push @output, qq|     <th class="peakcol" colspan="${tunewidth}">Peak</th>|;
    push @output,   '    </tr>';
    push @output,   '    <tr>';
    foreach my $tune (qw(base peak)) {
      push @output,   qq|     <th class="${tune}col">|.linkto(ucfirst($what)).'</th>' if $rate;
      for(my $i = 0; $i < 3; $i++) {
        # Three sets of results per tune
        push @output, qq|     <th class="${tune}col secs" >|.linkto('Seconds').'</th>';
        push @output, qq|     <th class="${tune}col ratio" >|.linkto('Ratio').'</th>';
      }
    }
    push @output, '    </tr>';
    push @output, '   </thead>';
    push @output, '   <tfoot>';
    push @output, '    <tr>';
    push @output, qq|     <td colspan="|.($tunewidth * 2 + 1).qq|">Results appear in the <a href="http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#RunOrder">order in which they were run</a>. Bold underlined text <a href="http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#Median">indicates a median measurement</a>.</td>|;
    push @output, '    </tr>';
    push @output, '   </tfoot>';
    push @output, '   <tbody>';
    my %results = ();
    my @dp = (-10) x 14;
    foreach my $bench (sort keys %{$r->benchmarks}) {
      my $residx = 0;
      foreach my $tune (qw(base peak)) {
        if ($nc) {
            my $str = (grep { /^${tune}$/ } @{$r->tunelist}) ? $nc_str : '';
            $results{$bench}->[$residx++] = [ $str, "${tune}col bm" ] if $rate;
            for(my $i = 0; $i < 3; $i++) {
                $results{$bench}->[$residx++] = [ $str, "${tune}col secs" ];
                $results{$bench}->[$residx++] = [ $str, "${tune}col ratio" ];
            }
        } elsif (grep { /^${tune}$/ } @{$r->tunelist}) {
            my @results = $r->benchmark_results_list($bench, $tune);
            if ($rate) {
                my $copies;
                if ($::lcsuite eq 'mpi2007') {
                    $copies = $r->ranks($bench, $tune);
                } else {
                    $copies = $r->copies($bench, $tune);
                }
                $dp[$residx] = undef;
                $results{$bench}->[$residx] = [ $copies, "${tune}col bm", undef, 1, 0 ];
                $residx++;
            }
            my @idx = (0, 1, 2);
            if (@results+0 > 3) {
              # Sigh.  Find min/median/max.  We are in _trouble_ if
              # more than (n-3) of the results are exactly the same.
              my ($min, $median, $max) = (1 << 31, undef, -1);
              my $count = 0;
              foreach my $res (sort { $a->{'iteration'} <=> $b->{'iteration'} } @results) {
                  if ($res->{'selected'} && !defined($idx[1])) {
                      $idx[1] = $count;
                  }
                  if ($res->{'ratio'} < $min) {
                      $min = $res->{'ratio'};
                      $idx[0] = $count;
                  } 
                  if ($res->{'ratio'} > $max) {
                      $max = $res->{'ratio'};
                      $idx[2] = $count;
                  }
                  $count++;
              }
            }
            foreach my $idx (sort @idx) {
                if (!defined($idx) || !isa($results[$idx], 'HASH')) {
                    $results{$bench}->[$residx++] = [ '', "${tune}col secs" ];
                    $results{$bench}->[$residx++] = [ '', "${tune}col ratio"];
                    next;
                }

                my $res = $results[$idx];
                my $time = $res->{'reported_time'};
                my $ratio = ($r->size_class eq 'ref') ? $res->{'ratio'} : undef;
                if (!$r->valid($bench, $tune)) {
                    $time = $ratio = 'X';
                }
                my $dp = significant($time, undef, 2, 1);
                my $selected = $res->{'selected'} ? ' selected' : '';
                $dp[$residx] = $dp if ($dp > $dp[$residx]);
                $results{$bench}->[$residx] = [ $time, "${tune}col secs$selected", $dp, 0, 1 ];
                $residx++;
                if (defined($ratio)) {
                  $dp = significant($ratio, undef, 2, 0);
                  $dp[$residx] = $dp if ($dp > $dp[$residx]);
                } else {
                  $dp = undef;
                }
                $results{$bench}->[$residx] = [ $ratio, "${tune}col ratio$selected", $dp, 0, 0 ];
                $residx++;
            }
        } else {
            # These should probably also have classes attached, but it seems
            # like such a waste.  Do it only if they need to be styled.
            $results{$bench}->[$residx++] = [ '', "${tune}col bm" ] if $rate;
            for(my $i = 0; $i < 3; $i++) {
                $results{$bench}->[$residx++] = [ '', "${tune}col secs" ];
                $results{$bench}->[$residx++] = [ '', "${tune}col ratio" ];
            }
        }
      }
    }
    # Now just dump out the results
    foreach my $bench (sort keys %results) {
        push @output,   '    <tr>';
        push @output, qq|     <td class="bm">|.linkto($bench, 1).'</td>';
        for(my $i = 0; $i < @{$results{$bench}}; $i++) {
            my ($val, $class, $dp, $no_pad, $hack) = @{$results{$bench}->[$i]};
            my $selected = ($class =~ / selected/) ? 1 : 0;
            if ($val ne 'NC' && $val ne 'NA' && $val ne 'CD' && !$no_pad) {
                my $pad_class = 'selected' if $selected;
                $val = pad_rear($val, $dp[$i], $pad_class, $hack) if defined($val) && $val ne '';
            }
            $val = qq|<a href="http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#Median">$val</a>| if ($selected && $val =~ /\d/);
            $class = qq| class="$class"| if defined($class) && $class ne '';
            push @output, "     <td$class>$val</td>";
        }
        push @output,   '    </tr>';
    }
    push @output, '   </tbody>';
    push @output, '  </table>';
    push @output, ' </div>';

    return @output;

}

sub HTML_info {
    # Make the hardware and software tables
    my ($r, $config, @sections) = @_;
    my $system = $config->{'system'};
    my $summary = $config->{'summary'};
    my @output = ();
    
    push @output,     ' <div class="infobox">' unless $system eq 'XXstandaloneXX';

    for my $foo (@sections) {
      my ($heading, @refs) = @{$foo};
      if (@refs > 0) {
          push @output, qq|  <table id="|.::makeanchor($heading, \%seen_anchors).'"';
          if (!defined($summary) || $summary eq '') {
              push @output, qq|         summary="Details of |.lc($heading).' configuration for '.::escape_HTML($system).'">';
          } else {
              push @output, qq|         summary="|.::escape_HTML($summary).'">';
          }
          push @output,   '   <thead>';
          push @output, qq|    <tr><th colspan="2">|.linkto($heading, 0, $config->{'anchor_prefix'}).'</th></tr>';
          push @output,   '   </thead>';
          push @output,   '   <tbody>';
          foreach my $ref (@refs) {
            my ($name, @vals) = @$ref;
            push @output, '    <tr>';
            push @output, '     <th>'.linkto($name, 0, $config->{'anchor_prefix'}).':</th>';
            push @output, break_lines('     <td>', \@vals, '</td>', 1);
            push @output, '    </tr>';
          }
          push @output,   '   </tbody>';
          push @output,   '  </table>';
      }
    }
    push @output,     ' </div>' unless $system eq 'XXstandaloneXX';
    
    return @output;
}

sub HTML_mpi_info {
    my ($r) = @_;
    my @output = ();

    # First make a list of all the sections.  This is pre-done so that
    # anchor names can be uniquified if necessary.
    my @items = ();
    my %anchors = ();
    for my $item (qw(node interconnect)) {
        next unless exists($r->{$item}) && ::isa($r->{$item}, 'HASH');
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
            push @items, [ $item, $system ];
            my $anchor = ::makeanchor($r->{$item}->{$system}->{'label'}, \%seen_anchors);
            if ($anchor eq '') {
                $anchor = ::makeanchor($item.'_'.$system, \%seen_anchors);
            }
            $anchors{$item.$system} = $anchor;
        }
    }


    # Do the benchmark description and the node/interconnect TOC

    # It's desired that the system TOC fit into the "hardware summary".  So
    # fake up the necessary fields and tweak up the list of fields to make
    # it happen.
    my @hw_fields = @{$::mpi_info{'hardware'}};
    # Find the splice position; it's right after "system_class"
    my $i = 1;
    for(; $i <= @hw_fields; $i++) {
        last if $hw_fields[$i-1]->[0] eq 'system_class';
    }

    my %tmp_sections = ();
    foreach my $item (qw(node interconnect)) {
        my @systems = map { $_->[1] } grep { $_->[0] eq $item } @items;
        my @itemnames = ($item);
        foreach my $system (@systems) {
            my $label = $r->{$item}->{$system}->{'label'} || $system;
            my @subsystems = ($item);
            if ($item eq 'node') {
                # Nodes additionally need to be differentiated by purpose
                @subsystems = map { lc($_)."_$item" } split(/(?:\s+(?!server)|,)+/i, $r->{$item}->{$system}->{'purpose'});
            }
            my $desc = '<a href="#'.$anchors{$item.$system}.'">'.::escape_HTML($label).'</a>';
            foreach my $subsys (@subsystems) {
                push @{$tmp_sections{$subsys}}, $desc;
            }
        }
    }
    # Order for sections is compute nodes, interconnects, everything else
    # (sorted by purpose, I guess...)
    my @added = ();
    foreach my $sec (qw(compute_node interconnect), sort keys %tmp_sections) {
        if (exists $tmp_sections{$sec}) {
            my $title = $sec;
            $title =~ s/_/ /g;
            # Capitalize all the words
            $title =~ s/(\S+)/ucfirst($1)/ge;
            $title .= 's' if (@{$tmp_sections{$sec}} > 1);

            # Fix up the fileserver->file server mess
            $title =~ s/^Fileserver/File Server/i;

            splice @hw_fields, $i, 0, [ "XXXadded_$sec", $title, 1 ];
            $i++;
            for(my $j = 0; $j < @{$tmp_sections{$sec}}; $j++) {
                my $newitem = sprintf "XXXadded_%s%03d", $sec, $j;
                $r->{$newitem} = $tmp_sections{$sec}->[$j];
                push @added, $newitem;
            }
            delete $tmp_sections{$sec};
        }
    }

    push @output, HTML_info($r, { 'summary' =>
                            'Benchmark configuration details for the run' },
                            [ 'Hardware Summary', $r->info_format(\@hw_fields) ],
                            [ 'Software Summary', $r->info_format($::mpi_info{'software'}) ],
                           );

    # Now remove the added fields, so that they don't pollute other results
    foreach my $tmpitem (@added) {
        delete $r->{$tmpitem};
    }

    # Do the system descriptions and notes
    for my $iref (@items) {
        my ($item, $system) = @{$iref};
        my $inforef = $r->{$item}->{$system};

        my ($hw_info, $sw_info) = ::mpi_info_munge($r, $item, $system);

        push @output, qq| <div class="sysdesc" id="$anchors{$item.$system}">|;
        push @output, qq|  <h2>|.ucfirst($item).' Description: '.$inforef->{'label'}.'</h2>';

        push @output, HTML_info($r, {
                                'system' => $item.': '.$inforef->{'label'},
                                'summary' => "Configuration details for $item $inforef->{'label'}",
                                'anchor_prefix' => ucfirst($item),
                                    },
                                 [ 'Hardware', $r->info_format($hw_info) ],
                                 [ 'Software', $r->info_format($sw_info) ]);
        #...

        # Do the notes for this thing...
        push @output, HTML_notes($r->notes("${item}_${system}_"), ucfirst($item));

        push @output, qq| </div>|;
    }

    return @output;
}

sub HTML_notes {
    # Print all the notes
    my ($notesref, $anchor_prefix) = @_;
    my @output = ();

    return () unless isa($notesref, 'ARRAY');
    my @notes = @{$notesref};
    foreach my $sectionref (@notes) {
        next unless ref($sectionref) eq 'ARRAY';
        my ($section, $notesref) = @{$sectionref};
        next unless ref($notesref) eq 'ARRAY';

        push @output, ' <div class="notes">';
        push @output, '  <h2>'.linkto($section, 0, $anchor_prefix).'</h2>';
        push @output, '  <pre>';
        push @output, munge_links(map { auto_link($_, 0) } @{$notesref});
        push @output, '</pre>';	# Shouldn't be indented; causes extra line
        push @output, ' </div>';
    }

    return @output;
}

sub HTML_graph {
    # Make the graph.  Fun!
    # Even more fun: graphs used to be made of alternating gray and white
    # "bars", but let's try all white.  Nevertheless, the code still thinks
    # there are bars, as reflected in various variable names.  -j.henning 9/2007
    my ($r, $invalid, $nc, $name, $path) = @_;
    $name .= '.gif';                     # PNG someday...
    my $inch         = 72;               # dpi.  Yeah, right.
    my $border       = $inch / 32;
    my $bar_height   = 22;               # one bar (base or peak) - about 1/3in
    my $label_height = $bar_height * 2;  # two bars (one benchmark)
    my $label_width  = 0;                # Figured later (space for bench name)
    my $what         = 'ratio';          # What's on the graph?
    my $tick_height  = $bar_height * 0.375;
    my %max = ();
    my %min = ();
    my ($smallest, $largest) = ({}, {});
    my $maxcopies = 0;

    my %benchmarks = map { $_ => {} } keys %{$r->benchmarks};
    my $num_benchmarks = (keys %benchmarks)+0;
    return (undef, []) unless $num_benchmarks;

    my @output = ();
    my $height = ($bar_height * 2 * $num_benchmarks) + ($bar_height * 3) - 1 + $num_benchmarks;
    my $width  = 798;
    my $graph_x = 0;               # Fixed up later
    my $graph_width = $width - $border;      # Also fixed up later

    # Map of what we want to the key in the result data that provides the
    # information.
    my %what_key = (
		    'time'  => 'reported_sec',
		    'ratio' => 'ratio',
		   );

    my %font = (
                'tiny' => gdTinyFont,
                'small'=> gdSmallFont,
                'medium' => gdMediumBoldFont,
                'large' => gdLargeFont,
                'giant' => gdGiantFont,
                'norm' => gdSmallFont,                  # Actually used
                'bold' => gdMediumBoldFont,             # Actually used
               );
    if (0) {
      print "\nFont dimensions:\n";
      foreach my $font (keys %font) {
        print "$font: (".$font{$font}->width.', '.$font{$font}->height.")\n";
      }
    }

    # Set up the image object
    my $im = new GD::Image($width+1, $height+1);
    my %color = (
                  'white' => $im->colorAllocate(255,255,255),
                  'black' => $im->colorAllocate(  0,  0,  0),
                  'gray'  => $im->colorAllocate(200,200,200),
                  'blue'  => $im->colorAllocate( 38, 38,179),
                  'red'   => $im->colorAllocate(222,  0,  0),
                );
    $im->transparent($color{'white'});
    $im->interlaced('false');

    # The code below is basically unchanged (except for the drawing
    # mechanism) from the same code in SPEC_graph() in ps.pl.  It should
    # be unified (XXX), but that's activity that can wait for v1.2.
    my @results_list = $r->results_list;

    $smallest = {};
    $largest  = {};
    %max = ( 'ratio' => { 'base' => 0, 'peak' => 0, 'any' => 0 },
	     'time'  => { 'base' => 0, 'peak' => 0, 'any' => 0 },
	   );
    %min = ( 'ratio' => { 'base' => 1<<30, 'peak' => 1<<30, 'any' => 1<<30 },
	     'time'  => { 'base' => 1<<30, 'peak' => 1<<30, 'any' => 1<<30 },
	   );
    for my $res (sort @results_list) {
	my $tune = $res->tune;
	my $tmp;

	my $time  = $res->reported_time;
	my $ratio = $res->ratio;
        if ($::lcsuite eq 'mpi2007') {
            $maxcopies = $res->ranks if $res->ranks > $maxcopies;
        } else {
            $maxcopies = $res->copies if $res->copies > $maxcopies;
        }
	# Fill in mins and maxes for ALL results
        if ($time ne '--') {
            $min{'time'}->{$tune}  = $time  if $min{'time'}->{$tune}  > $time;
            $min{'time'}->{'any'}  = $time  if $min{'time'}->{'any'}  > $time;
            $max{'time'}->{$tune}  = $time  if $max{'time'}->{$tune}  < $time;
            $max{'time'}->{'any'}  = $time  if $max{'time'}->{'any'}  < $time;
            if ($res->selected) {
                $smallest->{$tune}{'time'}  = $time  if $smallest->{$tune}{'time'}  < $time;
                $largest->{$tune}{'time'}   = $time  if $largest->{$tune}{'time'}   > $time;
            }
        }
        if ($ratio ne '--') {
            $min{'ratio'}->{$tune} = $ratio if $min{'ratio'}->{$tune} > $ratio;
            $min{'ratio'}->{'any'} = $ratio if $min{'ratio'}->{'any'} > $ratio;
            $max{'ratio'}->{$tune} = $ratio if $max{'ratio'}->{$tune} < $ratio;
            $max{'ratio'}->{'any'} = $ratio if $max{'ratio'}->{'any'} < $ratio;
            if ($res->selected) {
                $smallest->{$tune}{'ratio'} = $ratio if $smallest->{$tune}{'ratio'} < $ratio;
                $largest->{$tune}{'ratio'}  = $ratio if $largest->{$tune}{'ratio'}  > $ratio;
            }
        }
    }

    my %results = (
                   'time' => {
                              ( map {
                                $_->benchmark.$_->iteration.$_->tune => [ $_->tune, $_->iteration, significant($_->reported_time, undef, undef, 1) ]
                              } @results_list ),
                              'peakmean' => [ 'overall', 0, significant($r->peakmean, undef, undef, 0) ],
                              'basemean' => [ 'overall', 0, significant($r->basemean, undef, undef, 0) ]
                             },
                   'ratio' => {
                               ( map {
                                $_->benchmark.$_->iteration.$_->tune => [ $_->tune, $_->iteration, significant($_->ratio) ]
                               } @results_list ),
                              'peakmean' => [ 'overall', 0, significant($r->peakmean, undef, undef, 0) ],
                              'basemean' => [ 'overall', 0, significant($r->basemean, undef, undef, 0) ]
                              },
                  );
    # The range of the graph SHOULD BE from the min{what} to the
    # maximum{what}.  That's all.  But people don't like that, so it's
    # mungable.
    # On 20 Dec 2005, the CPU subcommittee voted that the default scale
    # minimum should be zero.
    my $graph_min = 0; # $min{$what}->{'any'};
    my $graph_max = $max{$what}->{'any'};
    my $int_ticks = 0;      # Tick only integers?
    
    my $auto_adj = $r->accessor_nowarn('graph_auto');
    if (defined($auto_adj) && $auto_adj) {
      $graph_min = $min{$what}->{'any'}; 
      $graph_max = $max{$what}->{'any'};
    } else {
      my $user_min = $r->accessor_nowarn('graph_min');
      my $user_max = $r->accessor_nowarn('graph_max');
      if (defined($user_min) && $user_min >= 0) {
        if ($auto_adj && $user_min > $min{$what}->{'any'}) {
          ::Log(0, "\nERROR: The specified graph minimum is too large; it will be adjusted.\n");              $graph_min = $min{$what}->{'any'};
        } else {
          $graph_min = $user_min + 0;
        } 
      } 
      if (defined($user_max) && $user_max > 0) {
        if ($auto_adj && $user_max < $max{$what}->{'any'}) {
          ::Log(0, "\nERROR: The specified graph maximum is too small; it will be adjusted.\n");              $graph_max = $max{$what}->{'any'};
        } else {
          $graph_max = $user_max + 0;
        }
      }
    }
    $graph_min = $graph_max if ($graph_min > $graph_max);
    my ($num, $tmpsize);

    # Make sure that there's enough "extra" space at the bottom of the
    # graph to accomodate a label near the original graph_min without
    # spilling over into the benchmark label area.
    $num = significant($graph_min, undef, undef, 0);

    # This is overly generous.  Probably.
    my $needed = $font{'norm'}->width * length($num);
    # The scale is not yet finalized, so it's been worked in to the
    # following calculation, which was originally
    # graph_min = ((graph_width + needed) / -scale) + graph_max
    $needed = ($needed * ($graph_max - $graph_min)) / -$graph_width;

    $graph_min += $needed if ($graph_min > $min{$what}->{'any'} + $needed);
    $graph_min = 0 if $graph_min < 0; # Hopefully this won't happen

    my $range = $graph_max - $graph_min;
    $range++ unless $range;   # This shouldn't ever happen...

    # Now that the scale is known, adjust the range so that it fits nicely
    # into a human-friendly fixed interval.
    # For now, let's shoot for around 40 ticks
    my $interval = $range / 100;

    # Figure out the order of magnitude of the graph_max, so we know
    # which place needs to be a multiple of 5 (and thus, what order of
    # magnitude the interval needs to be).
    # The extra math is to adjust for rounding the last visible place.
    my $interval_oom = significant($graph_max, undef, 1, 0) * -1 + 2;

    # Scale so that the ones place is the one to round
    $interval *= (10 ** $interval_oom);

    # Round up to the nearest integer
    $interval = int($interval + 0.5);

    # Make it a multiple of 5.
    $interval += 5 - ($interval % 5);

    # Scale it back down
    $interval /= (10 ** $interval_oom);

    # Make the graph minimums and maximums integers if the range is
    # sufficient (i.e. enough to make at least two ticks).  This ensures
    # that the endpoints are nice integer ticks as well.
    # A nice side-effect is that it'll automagically conform to the
    # interval.
    if ($graph_max - $graph_min > 2) {
      $graph_min = int($graph_min);
      $graph_max = int($graph_max + 1) unless $graph_max == int($graph_max);
      $int_ticks = 1;
    }

    # Now make sure the graph_min and graph_max are multiples of the
    # interval, and the range will take care of itself
    if (int($graph_min / $interval) != $graph_min / $interval) {
        my $r = $graph_min - (int($graph_min/$interval) * $interval);
        $graph_min -= $r;
        $graph_min = 0 if $graph_min < 0;
    }
    if (int($graph_max / $interval) != $graph_max / $interval) {
        $graph_max = (int($graph_max/$interval + 1) * $interval);
    }

    # In some cases (graph scale!) it may be necessary to show more than
    # three "significant" figures.
    my $min_log = 2;
    while(significant($graph_min, $min_log, undef, 0) == significant($graph_min + $interval, $min_log, undef, 0)) {
        $min_log++;
    }

    # Figure out the minimum width for the label
    for my $bench (sort keys %{$r->benchmarks}) {
        my $w = $font{'bold'}->width * length($bench) + ($border * 3);
        $label_width = $w if ($w > $label_width);
    }
    $graph_x += $label_width;
    $graph_width -= $label_width;

    my $rate_x = 0;
    if (::istrue($r->{'rate'}) || $::lcsuite eq 'mpi2007') {
        # Make space for the copies label
        my $copychars = length('Copies'); 
        $copychars = length ($maxcopies) if length ($maxcopies) > $copychars;
        $rate_x = $font{'bold'}->width * $copychars + ($border * 3);
        $graph_x += $rate_x;
        $graph_width -= $rate_x;
    }

    # Finally, re-figure the scale and range based on the adjustments
    # made earlier.
    $range = $graph_max - $graph_min;
    $range++ unless $range;   # This shouldn't ever happen...
    my $scale = $graph_width / $range;

    # Set up the benchmark graphs
    # This is done before the ticks so that it won't destroy the mean
    # lines.  Likewise, we want to have the benchmark graph
    # data overlay the mean lines; thus the split between setup and
    # graphing.
    my $bar_y = $bar_height;
    foreach my $bench (sort keys %benchmarks) {
        $benchmarks{$bench}->{'y'} = $bar_y;  # For making the graph later

        if (!$rate_x) {
            # Cordon off the label area
            $im->line($label_width, $bar_y + ($bar_height * 0.4),
                      $label_width, $bar_y + ($bar_height * 1.6), $color{'black'});
        } else {
             # Cordon off the rate label area
            $im->line($label_width + $rate_x, $bar_y + ($bar_height * 0.4),
                      $label_width + $rate_x, $bar_y + ($bar_height * 1.6),
                      $color{'black'});
        }

        # Figure out the bounding box for the label
        my $string_base = int($bar_y + $bar_height - ($font{'bold'}->height / 2) - 1);

        # Now write in the right-justified name of the benchmark
        my $rjust_bench = ' 'x(($label_width/$font{'bold'}->width) - length $bench) . $bench;
        $im->string($font{'bold'}, 0 + ($border * 1.5), $string_base, $rjust_bench, $color{'black'});

        $bar_y += $bar_height * 2 + 1;
    }

    # Make the next-to-scale labels (the font size should be okay)
    if ($rate_x) {
        my $label = ($::lcsuite eq 'mpi2007') ? 'Ranks' : 'Copies';
        my $string_base = (($bar_height - $border) / 2) -
                           ($font{'bold'}->height / 2) + 0.5;
        $im->line($label_width + $rate_x, 0, $label_width + $rate_x, $bar_height, $color{'black'});
        $im->string($font{'bold'}, $label_width + ($border * 1.5), $string_base, $label, $color{'black'});
    }

    if ($r->size_class eq 'ref' && !$nc) {

        # Do the ticks and numbers for everything
        my $is_black = 1;
        my $mean = 0;
        my @spots = ();	# List of x values where ticks are
        my %ticked = (); # To keep track of which ticks have been printed
        my @avoid = ();	# Like @spots, but for number_tick
        my @tmpavoid = ();

        # Pre-mark the right edge of the graph to keep labels from spilling
        # over.
        my $xval = $graph_x + ($graph_max - $graph_min) * $scale;
        push @tmpavoid, [ $xval, $xval, 'graph_max' ];

        # Same for the left side (should be obvious, but it's not)
        push @tmpavoid, [ 0, 0, 'left_edge' ];

        # Draw the mean lines.  These are done first to keep the graph_min
        # (in @avoid) from constraining the mean labels.
        my $tmpstr = $r->baseunits.' = '.significant($r->basemean, undef, undef, 0);
        my @avoid_dirs = qw(left right);

        my $basemean = $r->basemean;
        undef $basemean unless $basemean+0 > 0;
        my $basedir = 0;
        my $peakmean = $r->peakmean;
        undef $peakmean unless $peakmean+0 > 0;
        my $peakdir = 1;

        my $min_mean = ::min($basemean, $peakmean);

        if (defined($basemean) && defined($peakmean) &&
            $basemean > $peakmean) {
          # If base > peak, switch the default dodge sides
          $basedir = 1 - $basedir;
          $peakdir = 1 - $peakdir;
        }

        if (defined($peakmean)) {
          $tmpstr = $r->peakunits.' = '.significant($peakmean, undef, undef, 0);
          my $tmpwidth = $font{'bold'}->width * length($tmpstr) / 2;
          $xval = $graph_x + ($peakmean - $graph_min) * $scale;
          if (($peakdir && ($xval + $tmpwidth > $graph_x + $graph_width)) ||
              (!$peakdir && ($xval - $tmpwidth < $graph_x))) {
            # Make it dodge to the other side if it won't fit inside
            # the border.
            $peakdir = 1 - $peakdir;
          }
          number_tick($im, $font{'bold'}, 
                      [ $color{'blue'},
                        $color{'blue'}, $color{'blue'}, gdTransparent, gdTransparent ],
                      $xval, $bar_height,
                      $height - ($bar_height * 2) + ($bar_height * 0.10), 'below',
                      $tmpstr, \@tmpavoid, $avoid_dirs[$peakdir],
                      $bar_height * 0.10, [ 'x', 1 ]);
          push @tmpavoid, [ $xval, $xval, 'peak_mean' ];
        }

        if (defined($basemean)) {
          $tmpstr = $r->baseunits.' = '.significant($basemean, undef, undef, 0);
          my $tmpwidth = $font{'bold'}->width * length($tmpstr) / 2;
          $xval = $graph_x + ($basemean - $graph_min) * $scale;
          if (($basedir && ($xval + $tmpwidth > $graph_x + $graph_width)) ||
              (!$basedir && ($xval - $tmpwidth < $graph_x))) {
            # Make it dodge to the other side if it won't fit inside
            # the border.
            $basedir = 1 - $basedir;
          }
          number_tick($im, $font{'bold'}, $color{'black'},
                      $xval, $bar_height,
                      $height - ($bar_height * 3) + ($bar_height * 0.10), 'below',
                      $tmpstr, \@tmpavoid, $avoid_dirs[$basedir],
                      $bar_height * 0.10, [ 'x', 1 ]);
          push @tmpavoid, [ $xval, $xval, 'base_mean' ];
        }

        # Generate the list of ticks
        my @ticks = ();
        foreach my $num (1 .. ($range / $interval) - 1) {
            push @ticks, $graph_min + ($interval * $num);
        }

        # Tick the graph_min; make it shift to the right
        $xval = $graph_x;
        push @avoid, [ $xval, $xval, 'graph_min' ];
        my $print_num = significant($graph_min, $min_log, undef, 0);
        $ticked{$print_num}++;
        $print_num = '0' if ($graph_min <= 0.0001);
        push @spots, number_tick($im, $font{'norm'}, $color{'black'},
                                 $xval, $bar_height,
                                 $tick_height, 'above',
                                 $print_num, \@avoid, 'right',
                                 $tick_height * 0.2);

        # Tick the graph_max; make it shift to the left.  $border is added
        # to the xval to mark the _actual_ edge of the graph for avoidance.
        $xval = $graph_x + ($graph_max - $graph_min) * $scale;
        push @avoid, [ $xval + $border, $xval, 'graph_max' ];
        $print_num = significant($graph_max, $min_log, undef, 0);
        $ticked{$print_num}++;
        push @spots, number_tick($im, $font{'norm'}, $color{'black'},
                                 $xval, $bar_height,
                                 $tick_height, 'above',
                                 $print_num, \@avoid, 'left',
                                 $tick_height * 0.2);

        # Now that the various non-interfering top and bottom labels are done,
        # future ticks should avoid all of the marked points.
        push @avoid, @tmpavoid;

        for my $num (@ticks) {
            next if (!defined($num) || $num eq '--');
            $print_num = significant($num, $min_log, undef, 0);
            next if $ticked{$print_num};
            next if ($num > $graph_max);

            my $this_tick = $tick_height;
            $ticked{$print_num}++;
            $xval = $graph_x + ($num - $graph_min) * $scale;
            if (($int_ticks && ($num - int($num) > 0.0001)) ||
                !label_ok($xval, $font{'norm'}->width * length($print_num) * 1.2, \@spots)) {
              undef $print_num;
            }

            if (!defined($print_num)) {
                # Ticks without numbers will be smaller by half
                $this_tick /= 2;
            }

            my $interval = number_tick($im, $font{'norm'}, $color{'black'},
                                       $xval, $bar_height,
                                       $this_tick, 'above',
                                       $print_num, undef, undef,
                                       $tick_height * 0.2);
            push @spots, $interval if defined($interval);
        }

        # Pull the right edge of rate text labels a little left if we have room.
        my $rtext_pad = 0;
        if ($rate_x) {
            my $maxcopieswid = length($maxcopies) * $font{'bold'}->width;
            if ($rate_x > $maxcopieswid + ($border * 3)) {
	        if (length($maxcopies) == 1) {
		    $rtext_pad = ($rate_x - $maxcopieswid) * .2;
                } else {
		    $rtext_pad = ($rate_x - $maxcopieswid) * .33;
                }
            }
        }

        # Now do the benchmark graphs
        %results = %{$r->results};
        my $geom = {
            'tune_pad'   => $inch / 32, # Space between bars for each tuning level
            'bar_height' => $bar_height,
            'bar_x'      => $graph_x,
            'graph_min'  => $graph_min,
            'graph_max'  => $graph_max,
            'scale'      => $scale,
            'rate_x'     => $rate_x,
            'rtext_pad'  => $rtext_pad,
            'border'     => $border,
            'tick_height'=> $tick_height,
        };

        # Finally, really draw the graphs
        foreach my $bench (sort keys %results) {
            if (!exists $benchmarks{$bench}) {
              ::Log(0, "\nERROR: Results from $bench, which is not in the current benchset!\n");
              return (undef);
            }
            $bar_y = $benchmarks{$bench}->{'y'};
            do_bm_graph($im, \%font, \%color, $bar_y, $geom,
                        $what_key{$what}, $results{$bench}, \@avoid);
        }

    }
    # Border for entire graph - painted last
    $im->rectangle(0, 0, $width, $height, $color{'black'});

    my $fname = ::jp($path, $name);
    my $fh = new IO::File ">$fname";
    if (defined($fh)) {
	binmode $$fh, ':raw';
	$fh->print( $im->gif );
	$fh->close();
    } else {
	::Log(0, "\nCouldn't open $fname for writing: $!\n");
        return (undef);
    }

    push @output, ' <div class="graph">';
    # Fix up the height and width
    $height++;
    $width++;
    push @output, qq|  <img src="$name" alt="Benchmark results graph" height="$height" width="$width" />|;
    push @output, ' </div>';

    return ($fname, @output);
}

sub HTML_errors {
    # Print all the errors
    my ($r, $invalid, $nc) = @_;
    my @output = ();

    if (isa($r->{'errors'}, 'ARRAY') && @{$r->{'errors'}}) {
	push @output, ' <div class="errors notes">';
	push @output, '  <h2>'.linkto('Errors').'</h2>';
	push @output, '  <pre>';
	push @output, map { auto_link($_, 0) } @{$r->{'errors'}};
	push @output, '  </pre>';
	push @output, ' </div>';
    }

    return @output;
}

sub HTML_flags {
  # Print flag report
  my ($r, $invalid, $nc, $flagfile, $flagurl) = @_;
  my @output = ();

  if (defined($::website_formatter) && $::website_formatter) {
    $flagfile = $flagurl;
  }

  # These will be handy for the flags section
  my $rf = $r->{'reduced_flags'};
  return () unless isa($rf, 'HASH');
  my @benches = sort keys %{$rf->{'benchlist'}};
  my @tunes = sort keys %{$rf->{'tunelist'}};
  my @classes = sort keys %{$rf->{'classlist'}};

  # Do the unknown and forbidden flags.
  foreach my $class (qw(forbidden unknown)) {
    next unless ::check_elem(undef, $rf, 'flaglist', $class);
    # Flags of the class exist for at least one benchmark, so
    # output them.
    my $classref = $rf->{'flaglist'}->{$class};
    for my $tune (sort @tunes) {
      my $printed_title = 0;
      for my $bench (sort keys %{$classref}) {
          next unless ::check_elem('ARRAY', $classref, $bench, $tune);
          my @flags = @{$classref->{$bench}->{$tune}};
          next unless @flags;
          if (!$printed_title) {
            my $title = ucfirst($tune).' '.ucfirst($class).' Flags';
            my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
            push @output, qq| <div class="flags notes" id="$class">|;
            push @output, qq|  <h2><a href="$url">|.::escape_HTML($title).'</a></h2>';
            push @output, qq|  <table class="$class">|;
            $printed_title = 1;
          }
          push @output, '   <tr>';
          push @output, dump_flags($r, { 'unknown' => $class eq 'unknown',
                                         'title'   => $bench.':',
                                         'bench'   => $bench,
                                         'tune'    => $tune,
                                         'url'     => $flagfile,
                                       },
                                   @flags);
          push @output, '   </tr>';
      }
      if ($printed_title) {
        push @output, '  </table>';
        push @output, ' </div>';
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
  foreach my $tune(@tunes) {
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
              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
              push @output, qq| <div class="flags notes" id="${tune}_$class">|;
              push @output, qq|  <h2><a href="$url">|.::escape_HTML($title).'</a></h2>';
              push @output,   '  <p>Same as '.ucfirst($onetune).' '.$class2title{$class}.'</p>';
              push @output,   ' </div>';
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
                      ::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $onetune)) {
                      if (exists($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang}) &&
                          $rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} &&
                          # There might _not_ be an entry for a particular language if, for
                          # the same flag (like -DSPEC_CPU_WINDOWS) one benchmark calls
                          # it portability and another calls it mandatory.  This is
                          # incorrect, but it's no fault of the user.
                          ::check_elem('ARRAY', $rf, 'bylang', 'flaglist', $class, $onetune, $lang) &&
                          @{$rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}}) {
                          my @flags = ();
                          for(my $i = 0; $i < @{$rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}}; $i++) { 
                              my $flag = $rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}->[$i];
                              next unless (::istrue($flag->[2]->{'display'}) || $r->{'review'});
                              my $markup = $rf->{'bylang'}->{'markup'}->{$class}->{$onetune}->{$lang}->[$i];
                              # In order to get the pre-formatted string to be used, make a
                              # new copy of the flag, with just the second element replaced.
                              my @newflag = @{$flag};
                              $newflag[1] = [ $flag->[1], $markup ];
                              push @flags, \@newflag;
                          }
                          if (!$printed_title) {
                              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($class2title{$class});
                              push @output, qq| <div class="flags notes" id="$class">|;
                              push @output, qq|  <h2><a href="$url">|.::escape_HTML($class2title{$class}).'</a></h2>';
                              $printed_title = 1;
                          }
                          my $langname = $lang;
                          $langname =~ s/ /_/g;
                          my $langtitle = $rf->{'var2desc'}->{$lang};
                          if ($rf->{'langmatch'}->{$class}->{$onetune}->{$lang} == 2) {
                              $langtitle .= ' (except as noted below)';
                          }
                          my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($langtitle);
                          push @output, qq|  <h3><a href="$url">|.::escape_HTML($langtitle).'</a>:</h3>';
                          push @output,   qq|  <table class="$class">|;
                          $printed_lang = 1;
                          push @output, '   <tr>';
                          push @output, dump_flags($r, { 'title' => '',
                                                         'bench' => $langname,
                                                         'tune'  => 'ALL'.$lang,
                                                         'noaddbench' => 1,
                                                         'url' => $flagfile,
                                                       }, @flags);
                          push @output, '   </tr>';
                          delete $langstodo{$lang};
                          if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $onetune, $lang)) {
                              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$onetune}->{$lang};
                          }
                      }
                  }

                  # Do the benchmarks that matched across tuning levels
                  if ($rf->{'allmatch'}->{$class} == 1 &&
                      ::check_elem('HASH', $rf, 'stringlist', $class)) {
                      my $classref = $rf->{'flaglist'}->{$class};
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
                          my @flags = ();
                          my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$onetune};
                          for(my $i = 0; $i < @{$flags}; $i++) {
                              next unless (::istrue($flags->[$i]->[2]->{'display'}) || $r->{'review'});
                              my $markup = $rf->{'markup'}->{$class}->{$bench}->{$onetune}->[$i];
                              # In order to get the pre-formatted string to be used, make a
                              # new copy of the flag, with just the second element replaced.
                              my @newflag = @{$flags->[$i]};
                              $newflag[1] = [ $flags->[$i]->[1], $markup ];
                              push @flags, \@newflag;
                          }
                          if (!$printed_title) {
                              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($class2title{$class});
                              push @output, qq| <div class="flags notes" id="$class">|;
                              push @output, qq|  <h2><a href="$url">|.::escape_HTML($class2title{$class}).'</a></h2>';
                              $printed_title = 1;
                          }
                          if (!$printed_lang) {
                              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($rf->{'var2desc'}->{$lang});
                              push @output, qq|  <h3><a href="$url">|.::escape_HTML($rf->{'var2desc'}->{$lang}).'</a>:</h3>';
                              push @output,   qq|  <table class="$class">|;
                              $printed_lang = 1;
                          }
                          push @output, '   <tr>';
                          push @output, dump_flags($r, { 'title' => "$bench:",
                                                         'bench' => $bench,
                                                         'tune'  => 'ALL',
                                                         'noaddbench' => 1,
                                                         'url' => $flagfile
                                                       }, @flags);
                          push @output, '   </tr>';
                          if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $onetune)) {
                              $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$onetune};
                          }
                          $donebench{$bench}++;
                      }
                  }
                  if ($printed_lang) {
                      push @output, '  </table>';
                  }
              }
          }
          if ($printed_title) {
              # Some non-tune-specific flags are mentioned, so output them.
              if ($mismatch) {
                  if ($class eq 'optimization') {
                      push @output, '  <p>(*) Indicates an optimization flag that was found in a portability variable.</p>';
                  } elsif ($class eq 'portability') {
                      push @output, '  <p>(*) Indicates a portability flag that was found in a non-portability variable.</p>';
                  } elsif ($class eq 'compiler') {
                      push @output, '  <p>(*) Indicates a compiler flag that was found in a non-compiler variable.</p>';
                  }
              }
              push @output, ' </div>';
              $mismatch = 0;
          }

          # Next dump class flags by tuning level, with the common per-language
          # set at the top, followed by benchmark-specific settings
          my $classref = undef;
          $printed_title = 0;
          if (::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $tune)) {
              $classref = $rf->{'bylang'}->{'flaglist'}->{$class}->{$tune};
          }
          foreach my $lang (sort ::bylang keys %langstodo) {
              last if $class eq 'portability'; # Portability is by benchmark
              my $printed_lang = 0;
              if (defined($classref) &&
                  ::check_elem('ARRAY', $classref, $lang) &&
                  @{$classref->{$lang}}) {
                  my @flags = ();
                  for(my $i = 0; $i < @{$classref->{$lang}}; $i++) {
                      my $flag = $classref->{$lang}->[$i];
                      next unless (::istrue($flag->[2]->{'display'}) || $r->{'review'});
                      my $markup = $rf->{'bylang'}->{'markup'}->{$class}->{$tune}->{$lang}->[$i];
                      my @newflag = @{$flag};
                      $newflag[1] = [ $flag->[1], $markup ];
                      push @flags, \@newflag;
                  }
                  if (!$printed_title) {
                      my $title = ucfirst($tune).' '.$class2title{$class};
                      my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
                      push @output, qq| <div class="flags notes" id="${tune}_$class">|;
                      push @output, qq|  <h2><a href="$url">|.::escape_HTML($title).'</a></h2>';
                      $printed_title = 1;
                  }
                  my $langname = $lang;
                  $langname =~ s/ /_/g;
                  my $langtitle = $rf->{'var2desc'}->{$lang};
                  if ($rf->{'langmatch'}->{$class}->{$tune}->{$lang} == 2) {
                      $langtitle .= ' (except as noted below)';
                  }
                  if (!$printed_lang) {
                      my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($langtitle);
                      push @output, qq|  <h3><a href="$url">|.::escape_HTML($langtitle).'</a>:</h3>';
                      push @output,   qq|  <table class="$class">|;
                      $printed_lang = 1;
                  }
                  push @output, '   <tr>';
                  push @output, dump_flags($r, { 'title' => '',
                                                 'bench' => '',
                                                 'tune'  => $langname.$tune,
                                                 'url' => $flagfile,
                                               }, @flags);
                  push @output, '   </tr>';
                  $printed_lang = 1;
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
                      my @flags = ();
                      for(my $i = 0; $i < @{$classref->{$bench}->{$tune}}; $i++) {
                          my $flag = $rf->{'flaglist'}->{$class}->{$bench}->{$tune}->[$i];
                          next unless (::istrue($flag->[2]->{'display'}) || $r->{'review'});
                          my $markup = $rf->{'markup'}->{$class}->{$bench}->{$tune}->[$i];
                          my @newflag = @{$flag};
                          $newflag[1] = [ $flag->[1], $markup ];
                          push @flags, \@newflag;
#'from_bench' => $tune.join('', @{$flag->[0]}),
#$opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
                      }
                      if (!$printed_title) {
                          my $title = ucfirst($tune).' '.$class2title{$class};
                          my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
                          push @output, qq| <div class="flags notes" id="${tune}_$class">|;
                          push @output, qq|  <h2><a href="$url">|.::escape_HTML($title).'</a></h2>';
                          $printed_title = 1;
                      }
                      if (!$printed_lang) {
                          my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($rf->{'var2desc'}->{$lang});
                          push @output, qq|  <h3><a href="$url">|.::escape_HTML($rf->{'var2desc'}->{$lang}).'</a>:</h3>';
                          push @output,   qq|  <table class="$class">|;
                          $printed_lang = 1;
                      }
                      push @output, '   <tr>';
                      push @output, dump_flags($r, { 'title' => $bench.':',
                                                     'bench' => $bench,
                                                     'tune'  => $tune,
                                                     'addvar' => 1,
                                                     'url' => $flagfile
                                                   }, @flags);
                      push @output, '   </tr>';
                      if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                          $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
                      }
                  }
              }
              if ($printed_lang) {
                  push @output, '  </table>';
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
                  $printed_title = 0;
                  if (::check_elem('HASH', $rf, 'flaglist', $class)) {
                      my $classref = $rf->{'flaglist'}->{$class};
                      foreach my $bench (sort keys %{$classref}) {
                          next if $donebench{$bench};
                          next unless ::check_elem('ARRAY', $classref, $bench, $port_tune);
                          next unless @{$classref->{$bench}->{$port_tune}};
                          my @flags = ();
                          for(my $i = 0; $i < @{$classref->{$bench}->{$port_tune}}; $i++) {
                              my $flag = $rf->{'flaglist'}->{$class}->{$bench}->{$port_tune}->[$i];
                              next unless (::istrue($flag->[2]->{'display'}) || $r->{'review'});
                              my $markup = $rf->{'markup'}->{$class}->{$bench}->{$port_tune}->[$i];
                              my @newflag = @{$flag};
                              $newflag[1] = [ $flag->[1], $markup ];
                              push @flags, \@newflag;
#'from_bench' => $port_tune.join('', @{$flag->[0]}),
#$opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
                          }
                          if (!$printed_title) {
                              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
                              push @output, qq| <div class="flags notes" id="${port_tune}_$class">|;
                              push @output, qq|  <h2><a href="$url">|.::escape_HTML($title).'</a></h2>';
                              push @output,   qq|  <table class="$class">|;
                              $printed_title = 1;
                          }
                          push @output, '   <tr>';
                          push @output, dump_flags($r, { 'title' => $bench.':',
                                                         'bench' => $bench,
                                                         'tune'  => $port_tune,
                                                         'addvar' => 1,
                                                         'url' => $flagfile
                                                       }, @flags);
                          push @output, '   </tr>';
                          if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $port_tune)) {
                              $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$port_tune};
                          }
                      }
                  }
                  if ($printed_title) {
                      push @output, '  </table>';
                      if ($mismatch) {
                          push @output, '  <p>(*) Indicates a portability flag that was found in a non-portability variable.</p>';
                          $mismatch = 0;
                      }
                      push @output, ' </div>';
                  }
              }
          } else {
              # Portability flags are taken care of above...

              if ($printed_title) {
                  if ($mismatch) {
                      if ($class eq 'optimization') {
                          push @output, '  <p>(*) Indicates an optimization flag that was found in a portability variable.</p>';
                      } elsif ($class eq 'portability') {
                          push @output, '  <p>(*) Indicates a portability flag that was found in a non-portability variable.</p>';
                      } elsif ($class eq 'compiler') {
                          push @output, '  <p>(*) Indicates a compiler flag that was found in a non-compiler variable.</p>';
                      }
                  }
                  push @output, ' </div>';
                  $mismatch = 0;
              }
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

    push @output, ' <div class="notes flagfooter">';
    push @output, '  <hr />';
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
    push @output, ' </div>';
  }

  return @output;
}

sub dump_flags {
  my ($r, $opts, @flags) = @_;
  my $title = $opts->{'title'};
  my $link_title = $opts->{'link_title'};
  my @output = ();

  if ($opts->{'title_alone'}) {
    my $url = '';
    if ($link_title) {
      $url = qq|<a href="http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#|.::makeanchor($title).'>';
    }
    push @output, qq|   <tr><th colspan="2">$url|.::escape_HTML($title).($url ne '' ? '</a>' : '').'</th></tr>';
    $title = '';
    $link_title = 0;
  }

  if ($opts->{'unknown'}) {
    push @output, '   <td>'.::escape_HTML($title).'</td>';
    push @output, '   <td class="al">';
    foreach my $flagref (@flags) {
      push @output, '    "<span class="tt">'.::escape_HTML($flagref->[1]).'</span>" (in <span class="tt">'.::escape_HTML(join(', ', @{$flagref->[0]})).'</span>)';
    }
    push @output, '   </td>';
  } else {
    push @output, '   <td>'.::escape_HTML($title).'</td>';
    push @output, '   <td class="al">';
    foreach my $flag (@flags) {
      my ($doul, $string, $url, $markup);
      if (isa($flag->[2], 'HASH')) {
        # It's a real flag
        $doul = $flag->[2]->{'origin'}->[1] eq 'user' ? 'userflag' : 'flagtext';
        my $from = $opts->{'tune'};
        if ($opts->{'addvar'}) {
          $from .= join('', @{$flag->[0]});
        }
        if (!$opts->{'noaddbench'}) {
          $from .= $opts->{'bench'} if $flag->[2]->{'origin'}->[1] =~ /^(suite|user)$/;
        }
        if (isa($flag->[1], 'ARRAY')) {
          ($string, $markup) = @{$flag->[1]};
        } else {
          $string = $flag->[1];
          $markup = '';
        }
        $url = $opts->{'url'}.'#'.::make_flag_id($flag->[2], $from, $string, 1);
        if (exists $flag->[2]->{'nomap'}) {
          # This is a pseudo-flag added by the tools, so just
          # display the name.  (This should never happen here.)
          $url = '';
          $doul = 'flagtext';
        }
        if ($url ne '') {
          # Link up the flag text (only)
          $string = qq|<span class="$doul"><a href="$url">|.::escape_HTML($string).'</a></span>'.::escape_HTML($markup);
        } else {
          $string = qq|<span class="$doul">|.::escape_HTML($string).'</span>'.::escape_HTML($markup);
        }
        push @output, "    &nbsp;$string&nbsp;";
      } else {
print "XXX Not a 'real' flag.  What do I do?\n";
      }
    }
    push @output, '   </td>';
  }

  return @output;
}

sub HTML_head {
    my ($r, $invalid, $nc, $path) = @_;
    my @output = ();

    my %html_style;
    my @common_style;
    if (!defined $::website_formatter || !$::website_formatter) {
        @common_style = map { tr/\012\015//d; $_ } ::read_file(::jp($ENV{'SPEC'}, 'Docs', 'css', "${main::lcsuite}result.css"));
    } else {
        @common_style = ( '  @import url(http://www.spec.org/includes/css/'.${main::lcsuite}.'result.css);' );
    }
    foreach my $media (qw(screen print)) {
        @{$html_style{$media}} = ('<style type="text/css" id="internal'.ucfirst($media)."\" media=\"$media\">", '<!--', @common_style);
        my $done = 0;
        if (!defined $::website_formatter || !$::website_formatter) {
            # Try to read it in from a file
            my $path = ::jp($ENV{'SPEC'}, 'Docs', 'css', "${main::lcsuite}${media}.css");
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
    push @output, '<!DOCTYPE html',
                  '      PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"',
                  '      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">';
    push @output, '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">';
    push @output, '<head>';
    push @output, qq|<meta name="generator" content="SPEC ${main::suite} Tools $::version (HTML v$html_version)" />|;
    # Ensure that IE8 renders us in standards mode
    push @output, qq|<meta http-equiv="X-UA-Compatible" content="IE=8;FF=2" />|;
    push @output, qq|<meta http-equiv="Content-type" content="text/html;charset=utf-8" />|;
    my %things = ::get_tester_info($r);
    foreach my $thing (qw( tester test_sponsor hw_vendor hw_model )) {
        $things{$thing} = ::escape_HTML(join(' ', @{$things{$thing}}));
    }

    my @keywords = ($things{'hw_vendor'}, $things{'hw_model'});
    push @keywords, $things{'test_sponsor'} if ($things{'test_sponsor'} ne $things{'hw_vendor'});
    if ($things{'test_sponsor'} ne $things{'tester'}) {
      push @output, qq|<meta name="author" content="$things{'tester'} (test sponsored by $things{'test_sponsor'})" />|;
      push @keywords, $things{'tester'} if ($things{'tester'} ne $things{'hw_vendor'});
    } else {
      push @output, qq|<meta name="author" content="$things{'tester'}" />|;
    }
    my $what = '';
    if ($nc == 2) {
      $what = 'Non-available ';
    } elsif ($nc == 1) {
      $what = 'Non-compliant ';
    } elsif ($invalid) {
      $what = 'Invalid ';
    }
    my ($peak, $base) = get_means($r, $invalid, $nc, 0);
    push @keywords, "Base $base" if $base ne 'Not Run';
    push @keywords, "Peak $peak" if $peak ne 'Not Run';

    push @output, qq|<meta name="description" content="${what}$r->{'metric'} result for $things{'hw_model'}; base: $base; peak: $peak" />|;
    push @output, qq|<meta name="keywords" content="${what}$r->{'metric'} SPEC www.spec.org |.join(' ', @keywords).'" />';

    push @output, @{$html_style{'screen'}};
    push @output, @{$html_style{'print'}};
    if ($invalid || $nc) {
      push @output, '<style type="text/css">', '<!--';
      if ($nc == 3) {
        push @output, "body { background-image: url(http://www.spec.org/auto/$::lcsuite/images/cd.jpg) }";
      } elsif ($nc == 2) {
        push @output, "body { background-image: url(http://www.spec.org/auto/$::lcsuite/images/na.jpg) }";
      } elsif ($nc) {
        push @output, "body { background-image: url(http://www.spec.org/auto/$::lcsuite/images/nc.jpg) }";
      } elsif ($invalid) {
        my $fname = ::jp($path, 'invalid.gif');
        if ($image_url eq '' && ( !-f $fname || -s _ < 5000 ) ) {
          # Write out the image that is about to be referenced
          my $invalid_file = write_invalid_image($fname);
          if (!defined($invalid_file)) {
            # An error writing our own, eh?  Never fear, SPEC is here.
            push @output, "body { background-image: url(http://www.spec.org/auto/$::lcsuite/images/invalid.gif) }";
          } else {
            push @{$written}, $invalid_file;
            push @output, "body { background-image: url(invalid.gif) }";
          }
        } else {
          push @output, "body { background-image: url(${image_url}invalid.gif) }";
        }
      }
      push @output, '-->', '</style>';
    }

    my $title = $things{'hw_vendor'}.' '.$things{'hw_model'};
    if ($things{'test_sponsor'} ne $things{'hw_vendor'}) {
	$title .= ' (test sponsored by '.$things{'test_sponsor'};
        if ($things{'tester'} ne $things{'test_sponsor'}) {
            $title .= '; tested by '.$things{'tester'};
        }
        $title .= ')';
    }

    push @output, '',
                   "<title>${what}$r->{'metric'} Result: $title</title>";
    push @output, '</head>', '';

    return @output;
}

sub HTML_title {
    my ($r, $invalid) = @_;
    my @output = ();
    my $tmpstr = '';

    # Make the top title bar thing
    push @output, ' <div class="titlebarcontainer">';
    push @output, '  <div class="logo">';
    if (!$invalid &&		# Invalid results should never get the SPEC
        defined($::website_formatter) && # Seal of Reviewal
        $::website_formatter && 
        defined($::format_for_publication) &&
        $::format_for_publication) {
        # This must be a real result.  Put the cute little picture in
      push @output, qq|   <img src="${image_url}${logo_location}" alt="SPEC Seal" />|;
    }
    push @output, '  </div>';
    push @output, '  <div class="titlebar">';
    $tmpstr = '   <h1>';
    if ($invalid) {
	$tmpstr .= '<span style="color: red">Invalid</span> ';
    }
    $tmpstr .= fixup_trademarks('SPEC '.$r->{'metric'}.' Result').'</h1>';
    push @output, $tmpstr;
    push @output, '   <p style="font-size: smaller">Copyright '.::copyright_dates().' Standard Performance Evaluation Corporation</p>';
    push @output, '  </div>';
    push @output, ' </div>';

    return @output;
}


sub HTML_result {
    my ($r, $invalid, $nc) = @_;
    my @output = ();

    # Make the result bar
    push @output, ' <div class="titlebarcontainer">';
    push @output, '  <div class="systembar">';
    my %things = ::get_tester_info($r);

    if (join(' ', @{$things{'test_sponsor'}}) ne join(' ', @{$things{'hw_vendor'}})) {
        push @output, break_lines('    <p>', $things{'hw_vendor'}, '', 0);
        $things{'test_sponsor'}->[0] = '(Test Sponsor: '.$things{'test_sponsor'}->[0];
        $things{'test_sponsor'}->[$#{$things{'test_sponsor'}}] .= ')';
        push @output, break_lines(' <span style="font-size: 75%">', $things{'test_sponsor'}, '</span>', 0);
        push @output, '</p>';
    } else {
        push @output, break_lines('    <p>', $things{'hw_vendor'}, '</p>', 0);
    }
    push @output, break_lines('    <p>', $things{'hw_model'}, '</p>', 0);
    push @output, '  </div>';

    my ($peakmean, $basemean) = get_means($r, $invalid, $nc, 1);

    push @output, '  <div class="metricbar" id="peak">';
    push @output, '    <p><span class="item">'.linkto($r->peakunits).' = </span>';
    push @output, '       <span class="value">'.$peakmean.'</span></p>';
    push @output, '  </div>';
    push @output, '  <div class="metricbar" id="base">';
    push @output, '    <p><span class="item">'.linkto($r->baseunits).' = </span>';
    push @output, '       <span class="value">'.$basemean.'</span></p>';
    push @output, '  </div>';
    push @output, ' </div>';

    return @output;
}


sub HTML_date {
    my ($r) = @_;
    my @output = ();
    my @tester;

    # Gather up the things that we'll be printing
    my %things = ();
    foreach my $thing (qw( license_num test_sponsor test_date hw_avail sw_avail
                           tester )) {
        $things{$thing} = [ ::allof($r->accessor_nowarn($thing)) ];
    }

    push @output, ' <table class="datebar">';
    push @output, '  <tbody>';
    for my $lineref (
                      [
                          [ $::suite.' license:'     , 'license_num'  ],
                          [ 'Test date:'             , 'test_date'    ],
                      ],
                      [
                          [ 'Test sponsor:'          , 'test_sponsor' ],
                          [ 'Hardware Availability:' , 'hw_avail'     ],
                      ],
                      [
                          [ 'Tested by:'             , 'tester'       ],
                          [ 'Software Availability:' , 'sw_avail'     ],
                      ],
                    ) {
        push @output, '   <tr>';
        for my $info (@{$lineref}) {
            my ($name, $key) = @$info;
            push @output, qq|    <th id="$key">|.linkto($name).'</th>';
            push @output, qq|    <td id="${key}_val">|.join("<br />\n", map { auto_link($_, 0) } @{$things{$key}}).'</td>';
        }
        push @output, '   </tr>';
    }
    push @output, '  </tbody>';
    push @output, ' </table>';

    return @output;
}

# significant() and floor() gleefully stolen wholesale from ps.pl
sub significant {
    my ($value, $min_log, $returnlog, $hack) = @_;
    print "significant(value=\"$value\", min_log=\"$min_log\", returnlog=\"$returnlog\", hack=\"$hack\")\n" if ($debug & 4);
    my ($log);
    $min_log = 2 if !defined $min_log;
    if ($value == 0) {
	if ($value eq '0' || $value !~ /[\d.]/) {
	    print "  not a number; returning \"0.00\"\n" if ($debug & 4);
	    return '0.00';
	}
	$log = 0;
    } elsif ($value > 0) {
	$log = &floor(log($value)/log(10)); 
    } else {
	::Log(0, "ERROR: Can't take log of negative number ($value); called from ".join(':', caller())."\n");
	return $value;
    }
    #$min_log--;

    if ($returnlog == 1) {
	print "  returnlog set to 1; returning \"$log\"\n" if ($debug & 4);
	return $log;
    }

    if (!$hack || $value < 1000) {
        # Return of the STUPID STUPID HACK, but only if $hack is set.
        $value = int($value / (10**($log-$min_log))+.5) * (10**($log-$min_log));
        print "  value rounded to \"$value\"\n" if ($debug & 4);
    } else {
        # Round up; there won't be any decimal places
        $value = int($value + 0.5);
        print "  value without dp; rounded to \"$value\"\n" if ($debug & 4);
    }

    # The number has been changed; re-figure its log (it'll change if, for
    # example, a number less than 1 gets rounded up to 1)
    if ($value > 0) {
	$log = &floor(log($value)/log(10));
	print "  new log = $log\n" if ($debug & 4);
    }
    if ($log < $min_log) {
	$value = sprintf ("%.*f", $min_log-$log, $value) if ($log < $min_log);
	print "  reformatted value = \"$value\"\n" if ($debug & 4);
    }
    print "  returning \"$value\"\n" if ($debug & 4);
    if ($returnlog == 2) {
        # Just get the number of places after the decimal
        return 0 unless $value =~ s/^[^\.]*\.//;
        return length($value);
    }

    return $value;
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

sub pad_rear {
    my ($val, $dp, $class, $hack) = @_;
    $val = significant($val, undef, undef, $hack);
    my $my_dp = significant($val, undef, 2, $hack);
    if (defined($class) && $class ne '') {
        $val = qq|<span class="$class">$val</span>|;
    }
    return $val unless defined($dp);
    my $s = '&nbsp;' x ($dp - $my_dp);
    $s .= '&nbsp;' if ($dp && !$my_dp); # Space for the decimal point
    return "$val$s";
}

sub linkto {
    my ($name, $isbench, $section) = @_;
    if ($isbench) {
      return qq|<a href="http://www.spec.org/auto/$::lcsuite/Docs/${name}.html">|.fixup_trademarks(::escape_HTML($name)).'</a>';
    } else {
      my $anchor = ::makeanchor($section.$name);
      $anchor =~ s/(\S)continued$/$1/i; # Always goes to the same place anyway

      return qq|<a href="http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#$anchor">|.fixup_trademarks(::escape_HTML($name)).'</a>';
    }
}

# From ps.pl
sub label_ok {
  my ($xpos, $size, $spots) = @_;

  # Check to see if xpos occupies a position within +/- size of any position
  # in spots
  foreach my $spot (@{$spots}) {
      if (ref($spot) eq 'ARRAY') {
	  next if $xpos > ($spot->[1] + $size / 2);
	  next if $xpos < ($spot->[0] - $size / 2);
      } else {
	  next if $xpos > ($spot + $size / 2);
	  next if $xpos < ($spot - $size / 2);
      }
      return 0;
  }
  return 1;
}

# From ps.pl (mostly)
sub number_tick {
    my ($im, $font, $color, $tick_x, $tick_y, $tick_height, $side, $label,
        $avoid, $dir, $addspace, $linewidth) = @_;

    # This makes a vertical tick mark (and optionally a label).  The label
    # text is always centered.
    # tick_y is the start of the tick; so if the label is "above", the
    # tick goes up.  Otherwise it goes down.  Either way, the text is at
    # the endpoint farthest from tick_y.

    my $textcolor = $color;
    my @style = ();

#    print "number_tick(im=\$im, font=\$font, color=\$color, tick_x=$tick_x, tick_y=$tick_y, tick_height=$tick_height, side=$side, label=$label, avoid=$avoid, dir=$dir, addspace=$addspace)\n";
 
    if (isa($color, 'ARRAY')) {
      $textcolor = shift @{$color};
      @style = @{$color};
      $color = gdStyled;
    }

   my $text_y = $tick_y;

    if ($side eq 'above') {
        $addspace *= -1;
	$tick_height *= -1;
	$text_y -= $font->height;
    } elsif ($side eq 'below') {
        # This is what happens by default
    } elsif ($side eq 'center') {
        $addspace *= -1;
	$text_y -= ($font->height + $tick_height);
    }

    $text_y += $tick_height;
    $addspace = 0 unless defined($addspace);

    # Go ahead and draw the tick
    draw_line($im, $tick_x, $tick_y, $tick_x, $tick_y + $tick_height - $addspace, $color, $linewidth, \@style);

    # Now the label
    if (defined($label) && $label ne '') {

	# This business is to slide labels to one side or the other if they'll
	# cross over one of the x values in the 'avoid' array.
	my $clash = 0;
	my %shift;
	my $shift = 0;
	my $sw = ($font->width * length($label.' ')) / 2;
	my @sort_try;
	if (defined($avoid) && ref($avoid) eq 'ARRAY') {
	    if (defined($dir) && ($dir eq 'left' || $dir eq 'right')) {
		@sort_try = (0);
	    } else {
		@sort_try = (0, 1);
	    }
	    foreach my $sort_direction (@sort_try) {
		$sortdir = $sort_direction;
		%shift = ('left' => 0, 'right' => 0);
		$clash = 1;
		$shift = 0;
		while ($clash) {
		    $clash = 0;
		    foreach my $x (sort byspot @{$avoid}) {
			my $usedspot = $x;
			my $fromlabel = '';
			my @fromcaller = ();
			if (ref($x) eq 'ARRAY') {
			    $usedspot = ($x->[0] + $x->[1]) / 2;
			    $fromlabel = $x->[2];
			    @fromcaller = @{$x}[3..$#{$x}];
			}
			if (abs($tick_x - $usedspot + $shift{'left'}) < $sw) {
			    $clash = 1;
			    if ($tick_x < $usedspot) {
				# The tick is to the left of the obstruction
				$shift{'left'} -= $sw - abs($tick_x - $usedspot + $shift{'left'});
			    } else {
				# The tick is to the right of the obstruction.
				# So it's stupid to shift more than $sw, but
				# that's what this means.
				$shift{'left'} -= $sw + abs($tick_x - $usedspot + $shift{'left'});
			    }
			}
			if (abs($tick_x - $usedspot + $shift{'right'}) < $sw) {
			    $clash = 1;
			    if ($tick_x < $usedspot) {
				# The tick is to the left of the obstruction.
				# So it's stupid to shift more than $sw, but
				# that's what this means.
				$shift{'right'} += $sw + abs($tick_x - $usedspot + $shift{'right'});
			    } else {
				# The tick is to the right of the obstruction.
				$shift{'right'} += $sw - abs($tick_x - $usedspot + $shift{'right'});
			    }
			}
		    }

		    if ($clash) {
			# Was a direction specified?
			if (defined($dir) && ($dir eq 'left' || $dir eq 'right')) {
			    $shift = $shift{$dir};
			} else {
			    # Try to figure out what the best direction is
			    if (abs($shift{'left'}) < abs($shift{'right'})) {
				$shift = $shift{'left'};
			    } else {
				$shift = $shift{'right'};
			    }
			}
		    }
		}
		$sort_try[$sortdir] = $shift;
	    }
	}
	if (!defined($dir) || ($dir ne 'left' && $dir ne 'right')) {
	    # Pick the direction that shifts the label the least
	    $shift = (sort { abs($a) <=> abs($b) } @sort_try)[0];
	}
	$tick_x += $shift;

	# After all that, show the label
	$im->string($font,
                    $tick_x - (($font->width * length($label)) / 2), $text_y,
                    $label, $textcolor);
	return [ $tick_x - $sw, $tick_x + $sw, $label, caller() ];
    }
    return undef;
}

# Another one from ps.pl
sub do_bm_graph {
    # Actually display the graph of the results data.
    # NOTE: This will need to be changed if more than two tuning levels
    #       are to be plotted at once.
    my ($im, $font, $color, $y, $geom, $what, $results, $avoid) = @_;

    #print "\ndo_bm_graph(im=$im, font=$font, color=$color, y=$y, geom=$geom, what=$what, results=$results, avoid=$avoid)\n";

    my ($bar_x, $bar_height, $graph_min, $graph_max, $border, $tick_height) = 
	  @{$geom}{qw(bar_x bar_height graph_min graph_max border tick_height)};
    my $mid = $bar_height;
    my $min = 1<<31;
    my $basepeak = undef;	# Unless the results say otherwise...
    my %copies = ();

    # Scan the results to find copies and whether or not
    # basepeak is going on...
    foreach my $tune (sort keys %{$results}) {
        next unless ref($results->{$tune});
	foreach my $run (@{$results->{$tune}->{'data'}}) {
            # Once it gets set, don't let it get unset (except when the
            # first run says it WAS, and the second run says it WASN'T).
            # It shouldn't happen.
            $basepeak = $run->{'basepeak'} unless defined($basepeak) && ($basepeak == 0);
	    my $data = $run->{$what};
	    $min     = $data if ($min     > $data);
	    if ($run->{'selected'}) {
                my $sel = ($::lcsuite eq 'mpi2007') ? 'ranks' : 'copies';
                # Similar to copies() or ranks() from benchset_common, except
                # that here we're not working on a result object
                if (defined($run->{"cl$sel"}) && $run->{"cl$sel"} ne '') {
                    $copies{$tune} = $run->{"cl$sel"};
                } else {
                    $copies{$tune} = $run->{$sel};
                }
	    }
	}
    }
    # Even if basepeak is set, don't do the basepeak bar unless peak was run
    $basepeak = undef unless exists($results->{'peak'});

    # Now get down to plotting the data
    my $dir = 'center';
    my $black = 1;
    my $currcolor = $color->{'black'};
    foreach my $tune (sort keys %{$results}) {
        next unless ref($results->{$tune});
        next if (defined($basepeak) && $basepeak && $tune eq 'base');
        my $tunemax = 0;
        my $base_y = $y + $mid;
        my $this_tick = $tick_height;
        my @avoid = @{$avoid};  # Make a copy

        if (defined($basepeak) && $basepeak) {
            $currcolor = $color->{'black'} unless $black;
            $black = 1;
            $dir = 'center';
            $base_y -= $bar_height / 4;
            $this_tick = $bar_height / 2;
        } elsif ($tune eq 'peak') {
            $currcolor = $color->{'blue'} if $black;
            $black = 0;
            $dir = 'above';
            $base_y -= $geom->{'tune_pad'};
        } else {
            $currcolor = $color->{'black'} unless $black;
            $black = 1;
            if (!exists($results->{'peak'})) {
                # for results with only base, center the bar
                $dir = 'center';
                $base_y -= $bar_height / 4;
                $this_tick = $bar_height / 2;
            } else {   
                $dir = 'below';
                $base_y += $geom->{'tune_pad'};
            }
        }

        if ($geom->{'rate_x'} && exists($copies{$tune})) {
          my $string_base = $y + $mid - ($font->{'bold'}->height / 2);
          my $text_x = $bar_x - $border - $geom->{'rtext_pad'} - ($font->{'bold'}->width * length($copies{$tune})); 
          my $text_y = $string_base;
          if ($dir eq 'center') {
            # The default
          } elsif ($dir eq 'above') {
            $text_y = $string_base - ($bar_height * .4);
          } elsif ($dir eq 'below') {
            $text_y = $string_base + ($bar_height * .4 );
          }
          $im->string($font->{'bold'}, $text_x, $text_y, $copies{$tune}, $currcolor);
        }

        my @spots = ();
        my $plotted = 0;
        foreach my $run (sort {
            $b->{'selected'} <=> $a->{'selected'} ||
            $a->{'iteration'} <=> $b->{'iteration'}
        } @{$results->{$tune}->{'data'}}) {
            my $data = $run->{$what};
            next if ($data < $graph_min);
            if ($data > $graph_max) {
              # Arrange for a line to appear at least
              $tunemax = $graph_max;
              $plotted++;
              next;
            }
            my $text = significant($data, undef, undef, 0);
            my $sw = $font->{'norm'}->width * length($text);
            $tunemax = $data if ($tunemax < $data);
            my $x = $bar_x + ($data - $graph_min) * $geom->{'scale'};
            # Print the data value associated with the tick if possible.
            # (The committee has decided that it is only possible to
            #  print the median.  This is pretty close to reality most
            #  of the time.)
            if (!$run->{'selected'} || !label_ok($x, $sw, \@spots)) {
              $text = undef;
              $sw = 0;
            }
            # This is why it's important to do the selected value first.
            my $tx = number_tick($im, $font->{'norm'}, $currcolor,
                                 $x, $base_y,
                                 $this_tick, $dir,
                                 $text,
                                 \@avoid, undef, $tick_height * 0.2, [ ]);
            if (defined($tx) && ref($tx) eq 'ARRAY') {
                push @avoid, $tx;
                push @spots, $tx if $sw;
            }
            $plotted++;
        }
        if ($plotted) {
            # Connect the ticks
            my $tick_center = $base_y;
            if ((defined($basepeak) && $basepeak ) || (!exists($results->{'peak'}))) {
                $tick_center = $y + $mid;
            } elsif ($dir eq 'above' || $dir eq 'center') {
                $tick_center -= $this_tick / 2;
            } else {
                $tick_center += $this_tick / 2;
            }
            
            my $line_x = $bar_x + (($tunemax - $graph_min) * $geom->{'scale'});
            if (defined($basepeak) && $basepeak) {
              draw_line($im, $bar_x, $tick_center, $line_x, $tick_center, $currcolor, [ 'y', -2, -1, 0, 1 ]);
            } elsif ($tune eq 'peak') {
              draw_line($im, $bar_x, $tick_center, $line_x, $tick_center, $currcolor, [ 'y', 0, 1 ]);
            } else {
              draw_line($im, $bar_x, $tick_center, $line_x, $tick_center, $currcolor, [ 'y', -1, 0 ]);
            }
        }
    }
}


# Also from ps.pl
sub byspot {
    my ($ax, $bx);
    if (ref($a) eq 'ARRAY') {
	$ax = ($a->[0] + $a->[1]) / 2;
    } else {
	$ax = $a;
    }
    if (ref($b) eq 'ARRAY') {
	$bx = ($b->[0] + $b->[1]) / 2;
    } else {
	$bx = $b;
    }
    if ($sortdir) {
	return $bx <=> $ax;
    } else {
	return $ax <=> $bx;
    }
}

sub draw_line {
  my ($im, $x1, $y1, $x2, $y2, $color, $linewidth, $style) = @_;
  $style = [] unless isa($style, 'ARRAY');
  $linewidth = [] unless isa($linewidth, 'ARRAY');
  my $whichdir = $linewidth->[0] =~ /^(?:x|y)$/ ? shift(@{$linewidth}) : 'x';
  unshift @{$linewidth}, 0 unless grep { /^0$/ } @{$linewidth};

  foreach my $delta (@{$linewidth}) {
    if (@{$style}) {
      $im->setStyle(@{$style});   # Otherwise it'll get out of sync
    }
    if ($whichdir eq 'x') {
      $im->line($x1 + $delta, $y1, $x2 + $delta, $y2, $color);
    } else {
      $im->line($x1, $y1 + $delta, $x2, $y2 + $delta, $color);
    }
  }
}

sub write_invalid_image {
  my ($fn) = @_;

  my $ofh = new IO::File '>'.$fn;
  return undef unless defined($ofh);
  my $image_data = decode_base64(join('', @image_data));
  binmode $$ofh, ':raw';
  $ofh->print($image_data);
  $ofh->close();
  return $fn;
}

sub auto_link {
    # Look for URLs in strings, and make them links.  Escape everything.
    my ($str, $basename) = @_;
    my $out = '';

    # Skip everything if the text already contains a link
    return $str if $str =~ /<a href/;

    # Only auto-link HTTP, HTTPS, FTP, and mailto URLs
    while ($str =~ s/^(.*?)((?:https?|ftp|mailto):[^[:space:]<>()]+)//i) {
      my ($pre, $url) = ($1, $2);
      if ($pre =~ /(?:ATTACH|LINK )$/) {
          $out .= $pre.$url;    # munge_links will handle this one
          next;
      } 
      my $uri = URI->new($url);
      $out .= ::escape_HTML($pre);
      $out .= qq|<a href="|.$uri->canonical->as_string.qq|">|;
      if ($basename) {
          my $fn = '';
          eval { $fn = (reverse $uri->path_segments)[0] };
          if ($@ ne '' || $fn eq '') {
              # This returns an escaped string
              $fn = $uri->as_string;
          } else {
              # The path segment is unescaped, so escape it
              $fn = ::escape_HTML($fn);
          }
          $fn =~ s/^mailto://;     # It's special
          $out .= $fn;
      } else {
          $url =~ s/^mailto://;     # It's special
          $out .= ::escape_HTML($url);
      }
      $out .= '</a>';
    }
    $out .= ::escape_HTML($str);

    return $out;
}

sub break_lines {
    my ($pre, $lines, $post, $basename) = @_;
    my @output = ();
    if (@{$lines} > 1) {
        push @output, $pre.auto_link(shift(@{$lines}), $basename).'<br />';
        push @output, map { auto_link($_, $basename).'<br />' } @{$lines};
        $output[$#output] .= $post;
        return @output;
    } else {
        return ($pre.auto_link($lines->[0], $basename).$post);
    }
}

sub munge_links {
    my (@lines) = @_;
    my @newlines = ();

    # These lines should've already been seen by auto_link, so all of the text
    # parts should already be escaped.
    foreach my $line (@lines) {
        my $count = 0;
        my $temp = $line;
        while ($count < 40 &&
               $line =~ m#((ATTACH|LINK)\s+(\S+)\s+AS\s+(?:\[([^]]+)\]|(\S+)))#g) {
            my ($section, $what, $url, $text) = ($1, $2, $3, $4.$5);
            $count++;

            if ($what eq 'LINK') {
                # Make sure the URL has a scheme.  No direct file linking
                # allowed here!  (Results files move around.)
                if ($url !~ /^[a-zA-Z]+:/) {
                    ::Log(0, "\nERROR: Relative links and bare file paths may not be used as the URL part\n");
                    ::Log(0, "     of a notes LINK statement.\n");
                    next;
                }
                my $uri = new URI($url);
                $url = $uri->canonical->as_string;
            }

            # Do the replacement
            $temp =~ s#\Q$section\E#<a href="$url">$text</a>#g;
        }
        push @newlines, $temp;
    }
    return @newlines;
}

sub get_means {
    my ($r, $invalid, $nc, $do_pad) = @_;
    my $nc_str = '';
    if ($nc == 3) {
        $nc_str = 'CD';
    } elsif ($nc == 2) {
        $nc_str = 'NA';
    } elsif ($nc) {
        $nc_str = 'NC';
    }

    my $peakmean = $r->peakmean;
    my $basemean = $r->basemean;
    my $meandp = ::max(significant($basemean, undef, 2, 0), significant($peakmean, undef, 2, 0));
    if ($nc) {  
      $peakmean = $nc_str if $peakmean =~ /\d/;
      $basemean = $nc_str if $basemean =~ /\d/;
    } else {
      if ($peakmean =~ /\d/) {
        if ($do_pad) {
          $peakmean = pad_rear($peakmean, $meandp, undef, 0);
        } else {
          $peakmean = significant($peakmean, undef, undef, 0);
        }
      }
      if ($basemean =~ /\d/) {
        if ($do_pad) {
          $basemean = pad_rear($basemean, $meandp, undef, 0);
        } else {
          $basemean = significant($basemean, undef, undef, 0);
        }
      }
    }

    return ($peakmean, $basemean);
}

sub footer {
  return ('<p>', ::trademark_lines('  ', %trademarks_done), '</p>');
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

BEGIN {
  # Prepare for FUN!
  @image_data = qw(
    R0lGODlhSgH3AOepAPAAAPIAAPIACPMACfQACu4QBu8TB+8TEvEWFPAXHPIZFfIZHfMbHvMcJfQe
    H/QeJfUgJvApJfIqJvEqLPMsLfQuLvQuNPUvNfYxNvE3NfM4NvI4O/Q6NvM6PPU7PfY8PvJCPvNE
    RPRFRfZGRvdHR/ZITfNNTfROTvVPTvZQT/dSVvNWVvVXV/ZYWPdZWfZZXvhbX/NdXvRfX/VgYPZh
    YfdiYvljY/RnaPVoafZpavhqa/lrbPRta/VubPZwbfZxc/hydPV2dfZ3dvd4d/l5ePp6efl7f/V+
    f/Z/gPeAgfiBgvmCg/aGg/iHhPmIhfiIi/qJhvmJjPqKjfaNjfeOjviPj/mQkPuRkfeVkvmWk/qX
    lPiXmvmYm/uZnPacnPednfmenvqfn/ugoPyhofijofmkovqlo/umpPmnqvuoq/yprPerrPisrfmt
    rvuur/yvsPizsfm0svu1s/y2tP23tf64tve6vPi8vfq9vvu+v/y/wP3AwfnDwvrEw/zFxf3Gxv7H
    x/rLyPvMyfzNyv3Oy/vO0fzP0v3Q0/nT1PrU1fzW1v3X1/7Y2frb2vvc2/zd3P3e3f7f3v/g3/vj
    4P/i4Pzl4f3m4vzn6v3o6/7p7P/q7fvt7vzu7/3w8P7x8f/y8v/z9Pz29f339v749//5+P/6+f/8
    +vz/+/7//P//////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////yH5BAEKAP8ALAAAAABKAfcA
    AAj+AFEJHEiwoMGDCBMqXMiwocOHECNKnEixosWLGDNq3Mixo8ePIEOKHEmypMmTKFOqXMmypcuX
    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
    xVZ0xVecrIAAADs=
  );
}

1;
