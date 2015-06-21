#
# util.pl
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: util.pl 6417 2011-04-07 04:36:26Z cloyce $

use strict;

require 'listfile.pm';
require 'util_common.pl';

my $version = '$LastChangedRevision: 6417 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'formatter_util.pl'} = $version;

sub center {
    my ($text, $width) = @_;
    my $len = length $text;
    $width = 78 if !defined $width;
    $width = $len if $width < $len;
    ' ' x int(($width - length $text) / 2) . $text;
}

sub write_and_format_flags {
    my ($listfile, $entry, $basename, $flagtext, $flags) = @_;

    my $fh = new IO::File '>'.$basename.'.xml';
    if (!defined($fh)) {
        ::Log(0, "\nERROR: Could not open ${basename}.xml for writing: $!\n");
        my $md5 = $entry->{'md5'};
        $entry->delete();
        $listfile->remove_entry('md5' => $md5);
        $listfile->update();
        $listfile->close();
        do_exit(1);
    }
    $fh->print($flagtext);
    $fh->close();

    # Now dump it.  Because of the advent of multiple flags files, it MAY
    # be the case that the structure in $flags is a superset of what's
    # described in $flagtext.  So just to be sure, re-parse $flagtext and
    # dump _that_.  (It's faster than just calling flag_dump.)
    if ($fh->open('>'.$basename.'.html')) {
      # It's okay if this fails, since it can be re-done by hand
      my $tmpflags = parse_flags($flagtext, $basename, 'user', 1, 'forbid' => [ 'mandatory' ], 'suiteversion' => 'select');
      if (defined($tmpflags) && exists($tmpflags->{'flagmap'})) {
          Spec::Format::flags::flags_to_html($tmpflags, $fh,
                                             !$::format_for_publication);
          $fh->close();
      } else {
          ::Log(0, "\nWARNING: Could not parse flags for ${basename}.html\n");
          $fh->close();
          unlink $basename.'.html';
      }
    } else {
      ::Log(0, "\nWARNING: Could not open ${basename}.html for writing: $!\n");
    }
}

sub store_flags_file {
  # Given some flags text, look in the flags storage area for an pre-existing
  # copy and point to it if it exists.  Otherwise write it there and return
  # a URL pointing to it.
  my ($flagtext, $flags, $rawpath) = @_;
  my @filelist = ();

  my @files = grep { $_ ne '' } split(/<\/flagsdescription>[\s\r\n]*/, $flagtext, -1);   
  for(my $i = 0; $i < @files; $i++) {
      my $tmpflags = $files[$i];
      $tmpflags .= "</flagsdescription>\n";  # Eliminated by split earlier
      my $name = $flags->{'files_by_section'}->{'filelist'}->[$i];

      # We'll need the MD5 no matter what...
      my $md5 = new Digest::MD5;
      $md5->add($tmpflags);
      $md5 = $md5->hexdigest();
      ::Log(15, "Storing or referencing a flags file with sum $md5\n");

      # ...and the flag file's name.
      if (!defined($name) || $name eq '') {
        $name = $::suite.'_flags';
      }
      ::Log(15, "  User-requested name is \"$name\"\n");
      # Clean it up, if necessary
      $name = ::flag_filename_fixup($name);
      ::Log(15, "  Sanitized name is \"$name\"\n");

      # Make a list file
      my $listfile = Spec::Listfile->new($::flag_report_base, 'flagidx');
      # Change the permissions back to something reasonable; this isn't the
      # happy world of a running benchmark, after all.
      chmod 0664, ::jp($::flag_report_base, 'flagidx');

      # Look for a match
      my $entry = $listfile->find_entry($::flag_report_base, 'md5' => $md5);
      my $flagperms = 0000; # No read, no write, nothing...
      if (defined($entry)) {
          # Get the file's permissions
          $flagperms = (stat($entry->path.'.xml'))[2];
      } else {
          $entry = $listfile->new_entry($name,
                                        'md5' => $md5,
                                        'num_width' => 2,
                                        'num_optional' => 1,
                                        'num_is_date' => 1);
      }

      # At this point, there's an entry that points to where our flags file
      # should go.  If the size doesn't match (or it doesn't exist at all),
      # write it out.
      my $filename = $entry->path.'.xml';
      if ( -s $filename != length($tmpflags) ) {
        # Before rewriting, attempt to unlink.  This is because the file that's
        # present _may_ be mode 000, which means no writing to it.  But we can
        # probably remove it and get around the problem that way...
        unlink $filename;

        write_and_format_flags($listfile, $entry, $entry->path, $tmpflags, $flags);
      }
      $listfile->update();

      if (!defined($::private_flag_dir) ||
          (defined($::private_flag_dir) && $rawpath !~ /$::private_flag_dir/)) {
          $flagperms |= 0664;
      }
      my $tries = 0;
      my $done = 0;
      while($done == 0 && $tries < 2) {
          my $current_perms = (stat($entry->path.'.xml'))[2];
          if ($flagperms == $current_perms) {
              # Nothing to do
              $done = 1;
              last;
          }
          my $changed = chmod $flagperms, $entry->path.'.xml', $entry->path.'.html';
          if ($changed == 2) {
              $done = 1;
              last;
          } elsif ($tries == 0) {
              # *sigh* chmod failed, probably because we're not the owner of one
              # or more of the files.  On the (good) assumption that we can write
              # into the directory (hooray for group perms), try writing a new copy
              # of the file, remove the existing one, and rename the new copy.
              # The locking on the list file should ensure that we don't have too
              # much of this fun going on at the same time.

              my ($fn, $dir) = File::Basename::fileparse($entry->path);
              my $path = ::jp($dir, ".$$.".$fn);
              write_and_format_flags($listfile, $entry, $path, $tmpflags, $flags);
              foreach my $ext (qw(xml html)) {
                  unlink $entry->path.".$ext";
                  rename $path.".$ext", $entry->path.".$ext";
              }
          }
          $tries++;
      }
      $listfile->close();

      ::Log(15, "  Final flags URL is \"".$::flag_report_url_base.$entry->name.".xml\"\n");
      push @filelist, $::flag_report_url_base.$entry->name.'.xml';
  }
  return @filelist;
}

sub get_format {
    # Return a format object (from formatlist) based on the name (which must
    # be one of the synonyms that the format lists for itself).
    my ($formatlist, $name) = @_;

    return { } unless isa($formatlist, 'HASH');

    foreach my $format (keys %{$formatlist}) {
        return $formatlist->{$format} if exists($formatlist->{$format}->{'synonyms'}->{lc($name)});
    }
    return { };
}

sub trademark_lines {
  my ($indent, %trademarks_done) = @_;

  my @tms_used = sort { length($a) <=> length($b) } keys %trademarks_done;
  my $line = $indent;
  if (@tms_used) {
      my @types = ();
      my %seen = ();
      my $types = '';
      foreach my $tm (@tms_used) {
          my $type = $::trademarks{$tm};
          $type = $type->[0] if (isa($type, 'ARRAY'));
          next if $seen{$type};
          my $what = undef;
          if ($type eq 'r') {
              $what = 'registered trademark';
          } elsif ($type eq 't') {
              $what = 'trademark';
          } elsif ($type eq 's') {
              $what = 'service mark';
          }
          $what .= "s" if (defined($what) && @tms_used > 1);
          push @types, $what;
          $seen{$type}++;
      }
      if (@types) {
          my $last_type = pop @types;
          if (@types) {
              $types = join(', ', @types);
              $types .= ',' if @types > 1;
              $types .= ' or '.$last_type;
          } else {
              $types = $last_type;
          }
      }
      if (@tms_used > 1) {
          my $last_tm = pop @tms_used;
          $line .= join(', ', @tms_used);
          $line .= ',' if @tms_used > 1;
          $line .= " and $last_tm are $types";
      } else {
          $line .= $tms_used[0].' is a '.$types;
      }
      $line .= " of the Standard Performance Evaluation Corporation.  All other";
  } else {
    $line .= "All";
  }
  $line .= " brand and product names appearing in this result are trademarks or registered trademarks of their respective holders.\n";

  return ::wrap_lines([$line], 75, $indent);
}

sub copyright_dates {
  my $this_year = localtime->year() + 1900;
  if ($this_year < 2011) {
    ::Log(0, "\nERROR: The current year returned by the system is ${this_year}, which is in the past.\nPlease correct the system clock and re-run the formatter.\n");
    ::do_exit(1);
  }

  if ($this_year == $::release_year) {
    return $this_year;
  } elsif ($this_year > $::release_year) {
    return "${main::release_year}-${this_year}";
  } elsif ($this_year < $::release_year) {
    # Whoa, this should NEVER happen after the suite is released
    if ($::suite_version < 5) {
      ::Log(0, "\nWARNING: Current year ($this_year) is earlier than suite release year ($::release_year).  Please check the clock on this system.\n");
    }
    return "${this_year}-${main::release_year}";
  }
}

# Fill a hash with tester info, with keys corresponding to the CPU names
sub get_tester_info {
    my ($r) = @_;
    my %things = ();
    foreach my $thing (qw( tester test_sponsor hw_vendor hw_model )) {
        next unless defined($r->accessor_nowarn($thing));
        $things{$thing} = [ ::allof($r->accessor_nowarn($thing)) ];
    }
    
    # MPI2007 cheat sheet:
    # hw_vendor => system_vendor
    # hw_model  => system_name
    if ($::lcsuite eq 'mpi2007') {
        if (defined($r->accessor_nowarn('system_vendor'))) {
            $things{'hw_vendor'} = [ ::allof($r->accessor_nowarn('system_vendor')) ];
        }
        if (defined($r->accessor_nowarn('system_name'))) {
            $things{'hw_model'} = [ ::allof($r->accessor_nowarn('system_name')) ];
        }
    }

    return %things;
}

sub byformat {
    # Sort formats lexically, except for raw, which always goes first,
    # and screen, which always goes last.  Oh yeah... mail and check go before
    # screen and after everything else.  Finally, flags goes second (and
    # isn't optional).

    # Make sure that we won't have a run-time error below
    return -1 if (!isa($a, 'HASH') &&  isa($b, 'HASH'));
    return  1 if ( isa($a, 'HASH') && !isa($b, 'HASH'));
    return  0 if (!isa($a, 'HASH') && !isa($b, 'HASH'));

    my ($an, $bn) = (lc($a->{'name'}), lc($b->{'name'}));

    foreach my $format (qw(raw flags)) {
      return -1 if ($an eq $format);
      return  1 if ($bn eq $format);
    }
    return  1 if ($an eq 'screen'          && $bn =~ /(?:mail|check)/i);
    return -1 if ($an =~ /(?:mail|check)/i && $bn eq 'screen'         );
    foreach my $format (qw(screen mail check)) {
      return  1 if ($an eq $format);
      return -1 if ($bn eq $format);
    }
    return $an cmp $bn;
}

sub pdf_ok {
    # Here's where we decide if it's okay to have PDF output

# XXX Can't do this because PS hasn't been loaded yet when this is
# evaluated.  Instead, move all the code to the PDF module and put this
# check in ps_ok().
#    return 0 unless ($Spec::Format::ps::name eq 'PostScript');
    return 1 if ::istrue($ENV{'SPEC_TRY_PDF'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_PDF'});

    $::pdf_ok = 1 unless defined($::pdf_ok);
    return $::pdf_ok;
}

sub csv_ok {
    # Here's where we decide if it's okay to have CSV output

    return 1 if ::istrue($ENV{'SPEC_TRY_CSV'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_CSV'});

    eval 'use Text::CSV_XS;';

    return $@ eq '';
}

sub ps_ok {
    # Here's where we decide if it's okay to have PostScript output

    return 1 if ::istrue($ENV{'SPEC_TRY_PS'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_PS'});

    return 1;
}

sub html_ok {
    # Here's where we decide if it's okay to have HTML output

    return 1 if ::istrue($ENV{'SPEC_TRY_HTML'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_HTML'});

    return 1;
}

sub asc_ok {
    # Here's where we decide if it's okay to have ASCII output

    return 1 if ::istrue($ENV{'SPEC_TRY_TXT'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_TXT'});

    return 1;
}

sub screen_ok {
    # Here's where we decide if it's okay to have ASCII output to the screen

    return 0 unless ($Spec::Format::asc::name eq 'ASCII');
    return 1 if ::istrue($ENV{'SPEC_TRY_SCREEN'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_SCREEN'});

    return 1;
}

sub config_ok {
    # Here's where we decide if it's okay to have config file output

    return 1 if ::istrue($ENV{'SPEC_TRY_CONFIG'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_CONFIG'});

    return 1;
}

sub mail_ok {
    # Here's where we decide if it's okay to mail results

    return 1 if ::istrue($ENV{'SPEC_TRY_MAIL'});
    return 0 if ::istrue($ENV{'SPEC_NEVER_TRY_MAIL'});

    return 1;
}

sub raw_ok {
    # raw output is *always* okay
    return 1;
}

1;
