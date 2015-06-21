#
# util_common.pl - Utility functions used by both runspec and rawformat
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: util_common.pl 6582 2011-06-30 22:07:28Z CloyceS $

my $version = '$LastChangedRevision: 6582 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'util_common.pl'} = $version;

use strict;
use Digest::MD5;
use IO::Dir;
use IO::File;
use IO::Compress::Bzip2 qw(:all);
use IO::Uncompress::Bunzip2 qw(:all);
use Compress::Zlib;
use MIME::Base64;
use Text::Wrap;
use Text::Tabs;
use LWP::UserAgent;
#use LWP::Debug qw(+);
use File::Spec;
use File::stat;
use File::Path;
use Fcntl qw(:flock);
use POSIX qw(:sys_wait_h);
use Storable qw(dclone);
use Time::HiRes qw(time sleep);
use UNIVERSAL qw(isa);

our ($exited, $timeout, $ua);

# Make an RE to match either 'notes' or 'NOTES' with all the high bits set
my $isnotes = 'notes';
$isnotes =~ s/(.)/chr(ord(uc($1)) | 0x80)/ge;
$isnotes = qr/(?:notes|$isnotes)/;

sub md5fhdigest {
    my ($fh) = @_;
    my $md5 = new Digest::MD5;
    my $tmp = '';
    $fh->read($tmp, stat($fh)->size);
    $md5->add($tmp);
    return $md5->hexdigest();
}

sub md5refdigest {
    my ($ref) = @_;
    my $md5 = new Digest::MD5;
    $md5->add($$ref);
    return $md5->hexdigest();
}

sub md5diffdigest {
    # Like md5filedigest, but with guaranteed consistent line endings
    # (as will be produced by applying the diff)
    my ($file) = @_;
    my $md5 = new Digest::MD5;

    my $fh  = new IO::File $file, O_RDONLY|O_BINARY;
    if (!defined($fh)) {
        die "$0: can't open '$file' for reading: $!\n";
    } else {
	local $/ = undef;
	my $contents = <$fh>;
	my @file = map { tr{\012\015}{\012\012}s; $_ } split(/(?:\n|\r\n)/, $contents);
        $md5->add(join("\012", @file)."\012");
        $fh->close();
    }
    return $md5->hexdigest();
}

sub md5scalardigest {
    my ($string) = @_;
    my $md5 = new Digest::MD5;
    $md5->add($string);
    return $md5->hexdigest();
}

# List files in a directory.
# Note that the directory handle is closed upon exit from the subroutine
sub list_files {
    my $dir = new IO::Dir "$_[0]";
    return sort grep { !/^\.\.?$/ && ($_[0] eq '.' || s%^%$_[0]/%) } $dir->read() if (defined $dir);
    return undef;
}

# List all normal files in a directory and its subdirectories
sub list_all_files {
    my ($dir) = @_;
    my @rc = ();

    foreach my $file (list_files($dir)) {
        next unless defined($file);
	if (-d $file) {
	    push @rc, ::list_all_files($file);
	} elsif (-f $file) {
	    push @rc, $file;
	}
    }

    return @rc;
}

sub istrue {
    my $val = shift @_;
    return 0 unless defined($val);
    $val = lc($val);
    return ($val eq 'y' || $val eq 'yes' || $val eq 't' || $val eq 'true' || 
	    $val eq 'o' || $val eq 'on'  || $val+0 != 0) ? 1 : 0;
}

sub choose_string {
    my($string, @choices) = @_;
    my($match);
    # This could be done with a map, generating a hash, and then an existence
    # check, but the loop is faster for the found case and not any slower
    # for the not-found case.
    foreach my $choice (@choices) {
        return $choice if lc($choice) eq lc($string);
    }
    for (@choices) {
	if (m/^$string/i) {
	    return undef if defined $match;
	    $match = $_;
	}
    }
    return $match if defined $match;
    undef;
}

sub choose_strings {
    my ($name, $string, $default_choices, $all_choices) = @_;
    my %seen;
    my @results = ();
    my @temp = split(/\s*[,:\s]\s*/, $string);

    for (@temp) {
	my $selection = choose_string($_, @{$default_choices}, @{$all_choices}, "all");
	if (!defined $selection) {
	    Log(0, "$name does not support '$_'\n");
	} elsif ($selection eq "all") {
	    for (@{$default_choices}) {
		if (!$seen{$_}++) {
		    push (@results, $_);
		}
	    }
	} else {
	    if (!$seen{$_}++) {
		push (@results, $_);
	    }
	}
    }
    @results;
}

sub my_system {
  # Run a command, taking care of teeing and logging and everything
  # Log messages and errors at 180; 0 if teeout is on (to make sure it goes
  # to the screen too)
  my ($tee, $basename, $outfile, $errfile, @cmd) = @_;

  my $loglevel = 0;
  if ($tee == 0) {
      # Log only
      $loglevel = 180;
  } elsif ($tee < 0) {
      # NO output
      $loglevel = -1;
  }
  $exited = 0;
  $timeout = undef;

  local $SIG{'CHLD'} = sub { $exited = 1; $timeout = 1; };

  my ($rin, $rout, $ein, $eout) = ((undef) x 4);

  my ($stdout, $stderr) = (undef, undef);

  if ($basename ne '') {
    $stdout = new IO::File ">$outfile";
    if (!defined($stdout)) {
      Log(0, "\nERROR: Couldn't open $outfile for writing: $!\n");
      return -1;
    }
    $stderr = new IO::File ">$errfile";
    if (!defined($stderr)) {
      Log(0, "\nERROR: Couldn't open $errfile for writing: $!\n");
      return -1;
    }
  }

  pipe(CHILD_STDOUT_IN, CHILD_STDOUT_OUT);
  pipe(CHILD_STDERR_IN, CHILD_STDERR_OUT);

  my $pid = undef;
  if ($pid = fork()) {
    # Parent
    close(CHILD_STDOUT_OUT);
    close(CHILD_STDERR_OUT);
    $rin = $ein = '';
    vec($rin, fileno(CHILD_STDOUT_IN), 1) = 1;
    vec($rin, fileno(CHILD_STDERR_IN), 1) = 1;
    $ein = $rin;
    # Wait for I/O
    my $nfound;
    my $exitcode = undef;
    my $backoff = -3;
    while (($nfound = select($rout = $rin, undef, $eout = $ein, $timeout)) ||
           !$exited) {
      my $read_something = 0;
      if (vec($rout, fileno(CHILD_STDOUT_IN), 1)) {
        my $linebuf = undef;
        my $readlen = sysread(CHILD_STDOUT_IN, $linebuf, 131072);
        while ($readlen == 131072) {
          $read_something++;
          Log($loglevel, $linebuf) if ($loglevel >= 0);
          $stdout->print($linebuf) if defined($stdout);
          # Just in case
          $linebuf = '';
	  $readlen = sysread(CHILD_STDOUT_IN, $linebuf, 131072);
        }
        $read_something++ if $linebuf ne '';
        Log($loglevel, $linebuf) if ($loglevel >= 0);
        $stdout->print($linebuf) if defined($stdout);
      }
      if (vec($rout, fileno(CHILD_STDERR_IN), 1)) {
        my $linebuf = undef;
        my $readlen = sysread(CHILD_STDERR_IN, $linebuf, 131072);
        while ($readlen == 131072) {
          $read_something++;
          Log($loglevel, $linebuf) if ($loglevel >= 0);
          $stderr->print($linebuf) if defined($stderr);
          # Just in case
          $linebuf = '';
	  $readlen = sysread(CHILD_STDERR_IN, $linebuf, 131072);
        }
        $read_something++ if $linebuf ne '';
        Log($loglevel, $linebuf) if ($loglevel >= 0);
        $stderr->print($linebuf) if defined($stderr);
      }
      my $rc = waitpid($pid, WNOHANG);
      if ($rc == $pid) {
        # The child was harvested at this point
        $exitcode = $?;
        $exited = 1;
        $timeout = 1;   # Arrange for select to not block forever
      }
      last if $exited > 1;        # Go through one extra time
      $exited++ if $exited;
      if ($read_something == 0 && $nfound != 0) {
        # select() may be broken; slow things down for a bit.  This won't affect
        # the timing of benchmarks, which is done by specinvoke.
        sleep(2**$backoff);
        $backoff++ if $backoff < 3;    # Never sleep more than 4 seconds at a time
      } else {
        $backoff = -3;
      }
    }
    if (!defined($exitcode)) {
      waitpid($pid, 0);
      $exitcode = $?;
    }
    Log(99, "Tee child $pid exited with code $exitcode (exit code=".WEXITSTATUS($exitcode).', signal='.WTERMSIG($exitcode).")\n");
    return $exitcode;
  } else {
    # Child
    close(CHILD_STDOUT_IN);
    close(CHILD_STDERR_IN);
    open(STDOUT, '>&CHILD_STDOUT_OUT') or die "Can't dup STDOUT: $!\nStopped";
    open(STDERR, '>&CHILD_STDERR_OUT') or die "Can't dup STDERR: $!\nStopped";
    autoflush STDOUT 1;
    autoflush STDERR 1;
    autoflush CHILD_STDOUT_OUT 1;
    autoflush CHILD_STDERR_OUT 1;
    if (!exec @cmd) {
      # Command didn't exist _and_ no shell was involved
      print STDERR "exec(".join(' ', @cmd).") failed: $!\n";
      exit 1;
    }
  }
}

## takes a list of values and returns the least of them (numeric only)
sub min {
    my ($val) = @_;
    for (@_) {
        $val = $_ if $_ < $val;
    }
    return $val;
}

## takes a list of values and returns the greatest of them
sub max {
    my ($val) = @_;
    for (@_) {
        $val = $_ if $_ > $val;
    }
    return $val;
}

sub squeeze_undef {
    # Squeeze undefined values from an array
    my ($ary) = @_;

    return [] unless @{$ary};

    for(my $i = 0; $i < @{$ary}; $i++) {
        next if defined($ary->[$i]);
        # undef => remove it
        splice @{$ary}, $i, 1;
        redo if $i < @{$ary};   # $ary->[$i] is now the next element
    }

    return $ary;
}

sub wrap_lines {
    my ($lines, $columns, $indent) = @_;

    my @orig = @$lines;
    $lines = [];
    foreach my $line (@orig) {
	if (length($line) <= $columns) {
	    push @$lines, $line;
	} else {
	    my ($baseindent) = $line =~ /^(\s*)/o;
	    $Text::Wrap::columns = $columns;
	    $Text::Wrap::huge = 'die';
	    $Text::Wrap::unexpand = 0;
	    my $newline = '';
	    eval '$newline = Text::Wrap::wrap($baseindent, $baseindent.$indent, $line);';
	    if ($@) {
		# There was an element that was _so_ large that it was
		# larger than the maximum number of columns.  So log a
		# warning and leave the line unchanged.
		Log(0, "WARNING: the following line:\n-----\n$line\n-----\ncontains an element longer than $columns columns and could not be split.\n");
		push @$lines, $line;
	    } else {
		my @newlines = split(/\n+/, $newline);
		push @$lines, @newlines;
	    }
	}
    }

    return @{$lines};
}

sub wrap_join {
    # Take a list of things and return a list of strings with as many items
    # as can fit in $maxlen columns, indented with $ident and terminated by
    # $eol, except for the last string, which will be unterminated.
    my ($maxlen, $sep, $indent, $eol, @items) = @_;

    my @lines = ();
    my $currstring = shift @items;
    foreach my $item (@items) {
	if ($currstring ne '' &&
	    length($currstring) + length($sep) + length($item) >= $maxlen ||
	    length($currstring) + length($eol)                 >= $maxlen) {
	    push @lines, $currstring.$eol;
	    $currstring = $indent.$item;
	} else {
	    $currstring .= $sep.$item;
	}
    }

    push @lines, $currstring if ($currstring ne '');
    return @lines;
}

sub new_user_agent {
    my $newua = LWP::UserAgent->new;
    $newua->agent("${main::suite}/${main::suite_version} ");
    $newua->protocols_allowed([qw( file http ftp )]);
    $newua->env_proxy;

    return $newua;
}

sub update_manifest {
    my (@files) = @_;
    my %files = map { $_ => 1 } @files;
    my @lines;

    # First read in the contents of the MANIFEST
    my $fh = new IO::File '<'.jp($ENV{'SPEC'}, 'MANIFEST');
    if (!defined($fh)) {
        Log(0, "\nERROR: Could not open MANIFEST for reading: $!\n\n");
        do_exit(1);
    }
    while (my $line = <$fh>) {
        my ($stuff, $file) = ($line =~ m/^(.* [0-9A-F]{8}) (.*)$/);
        next if exists $files{$file};
        push @lines, $line;
    }
    $fh->close();

    # Now rewrite it with the just the lines that aren't the files we want
    $fh = new IO::File '>'.jp($ENV{'SPEC'}, 'MANIFEST');
    if (!defined($fh)) {
        Log(0, "\nERROR: Could not open MANIFEST for writing: $!\n\n");
        do_exit(1);
    }
    $fh->print(@lines);

    # Now run specmd5sum to generate sums for the new files
    if (!open(MD5, '-|', 'specmd5sum', '-e', @files)) {
        $fh->close();   # Flush what we got
        do_exit(1);
    }
    $fh->print(<MD5>);
    $fh->close();
    close MD5;
}

sub shift_indices {
    my ($obj, $index, $delta) = @_;

    return if $delta == 0;

    my @idxkeys = grep { /^cfidx_/ } keys %{$obj};
    foreach my $idxkey (@idxkeys) {
	if ($obj->{$idxkey} >= $index) {
	    $obj->{$idxkey} += $delta;
	}
    }

}

sub renumber_notes {
    my ($result, $format, $backup) = @_;

    foreach my $section (grep { /^$::mpi_desc_re_id?notes/ } keys %{$result}) {
        next unless ref($result->{$section}) eq 'HASH';
        foreach my $key (sort keys %{$result->{$section}}) {
            next unless ref($result->{$section}->{$key}) eq 'ARRAY';
            my $notesref = $result->{$section}->{$key};
            my @newnotes = ();

            # Do the notes renumbering in two passes to avoid trashing
            # unrenumbered lines with newly renumbered lines.
            my $cfline = undef;
            for(my $i = 0; $i < @{$notesref}; $i++) {
                next unless ref($notesref->[$i]) eq 'ARRAY';
                my ($updateline, $oldtag, $note);
                if ($format == 3) {
                    ($updateline, $oldtag, $note) = @{$notesref->[$i]};
                } else {
                    $updateline = 1;
                    ($oldtag, $note) = @{$notesref->[$i]};
                }
                my $newtag = sprintf("%s%s_%03d", $section,
                                                  ($key ne '') ? "_$key" : '',
                                                  ($i * 5));
                # The rename tag needs to be guaranteed unique, but it also
                # needs to be the same length as $newtag, so just set the
                # high bits on all the characters.  It's easy to undo, and
                # because of the regexp in the config file reader, it's
                # guaranteed to not clash with any "natural" name.
                my $renametag = $newtag;
                $renametag =~ s/(.)/chr(ord(uc($1)) | 0x80)/goe;

                if (defined($oldtag)) {
                    $cfline = $result->{'cfidx_'.$oldtag};
                    $cfline = undef if ($cfline eq '');
                } else {
                    $renametag .= ":$cfline" if (defined($cfline));
                }

                # Fix up the tag (and maybe the line itself) in the stored
                # config file, and do _not_ create a backup.
                if (::update_stored_config($result, $oldtag, $renametag, $note, $updateline, 1)) {
                    push @newnotes, [ $newtag, $note ];
                    $cfline++ if (defined($cfline));
                } else {
                    ::Log(0, "ERROR: Edit of stored config file failed;\n        Could not change '$oldtag' to '$newtag' (1st pass).\n");
                }
            }

            # The final rename pass.
            @{$notesref} = ();
            foreach my $noteref (@newnotes) {
                my ($tag, $note) = @$noteref;
                my $renametag = $tag;
                $renametag =~ s/(.)/chr(ord(uc($1)) | 0x80)/goe;
                if (::update_stored_config($result, $renametag, $tag, $note, 0, !$backup)) {
                    push @{$notesref}, [ $tag, $note ];
                } else {
                    ::Log(0, "ERROR: Edit of stored config file failed;\n        Could not change temporary tag to '$tag' (2nd pass).\n");
                }
            }
        }
    }

}

sub compress_encode {
    # Compress and Base-64 encode the input string.  Returns the original
    # string, the compressed version, and the encoded version.  An undef in
    # a return slot indicates a failure of the operation.
    my ($input) = @_;
    my @tries = (['*', $input]);

    my ($compressed, $encoded) = (undef, undef);
    eval '$compressed = Compress::Zlib::compress($input, 9)';
    if (!$@ && defined($compressed)) {
	push @tries, [ '@', $compressed ];
    }
    $compressed = memBzip($input);
    if (defined($compressed)) {
	push @tries, [ '&', $compressed ];
    }

    # Sort them in order of length, so as to use the shortest one
    @tries = sort { length($a->[1]) <=> length($b->[1]) } @tries;

    $encoded = $tries[0]->[0].encode_base64($tries[0]->[1]);

    return wantarray ? ($input, $tries[0]->[1], $encoded) : $encoded;

}

sub decode_decompress {
    # Base-64 decode and uncompress the input string.  Returns the original
    # string, the decoded version, and the decompressed version.  An undef in
    # a return slot indicates a failure of the operation.
    my ($input, $gzip) = @_;
    my ($uncompressed, $decoded) = (undef, undef);
    my $copy = undef;
    my $type = '';
    if ($input =~ /^\s*([\*\&\@])?([A-Za-z0-9+\/=\012\015]+)$/os) {
        $copy = $decoded = decode_base64($2);
	$type = $1 || '#';
    } else {
        $decoded = undef;
        $copy = $input;
    }

    if ($type eq '*') {
	$uncompressed = $decoded;
    } elsif ($type eq '@') {
	eval '$uncompressed = Compress::Zlib::uncompress($copy)';
    } elsif ($type eq '#') {
	eval '$uncompressed = Compress::Zlib::memGunzip($copy)';
    } elsif ($type eq '&') {
        $uncompressed = memBunzip($copy);
    }
    $uncompressed = undef if ($@);

    return wantarray ? ($input, $decoded, $uncompressed) : $uncompressed;
}

sub memBzip {
    my ($data) = @_;

    # In order to maintain compatibility with toolsets that use
    # Compress::Bzip2, it's necessary to take the output from
    # IO::Compress::Bzip2 and add some more crap to the front of the output.

    # First, compress the data:
    my $compressed;
    my $status = bzip2 \$data => \$compressed;
    if (!$status) {
        Log(71, "Compression failed: $Bzip2Error\n");
        return undef;
    }

    # Hooray, the compression was successful.  Now put the "magic" and the
    # encoded length of the input at the front of the string:
    return pack 'CNa*', 0xF0, length($data), $compressed;
}

sub memBunzip {
    my ($input) = @_;

    # In order to maintain compatibility with toolsets that use
    # Compress::Bzip2, it's necessary to strip the first 5 bytes before
    # feeding the data to IO::Uncompress::Bunzip2.  Compress::Bzip2 prepends
    # a magic value of 0xF0 and the length of the input data.

    # Break it down:
    my ($magic, $len, $compressed) = unpack 'CNa*', $input;

    # Decompress the data:
    my $data;
    my $status = bunzip2 \$compressed => \$data, 'Transparent' => 0;
    if (!$status) {
        Log(71, "Decompression failed: $Bunzip2Error\n");
        return undef;
    }

    return $data;
}

sub basepeak_munge {
    my ($result, $bp_only, @benchmarks) = @_;

    return unless $result->{'basepeak'} == 1 || $result->{'basepeak'} == 2;

    # It's stupid to do base->peak substitution if there's no base to
    # substitute in!
    my $hasbase = 1 if grep { /^base$/ } @{$result->{'tunelist'}};

    if ($result->{'basepeak'} == 1) {
	$bp_only = 1;
	@benchmarks = sort keys %{$result->benchmarks};
    }

    if ($bp_only || $::lcsuite =~ /cpu(?:2006|v6)/) {
        if ($bp_only) {
          Log(10, "Doing base -> peak substitution for all benchmarks\n");
        } else {
          Log(10, "Doing base -> peak substitution for some benchmarks (per-benchmark basepeak)\n");
        }
        # Per run rules section 3.5, the base result is used for all
        # per-benchmark basepeak munging.
	for my $bench (@benchmarks) {
	    my $benchref = $result->{'results'}->{$bench};
	    # Make sure the data structures exist
	    foreach my $tmptune ('base', 'peak') {
		if (!isa($benchref->{$tmptune}, 'HASH')) {
		    $benchref->{$tmptune} = {'data' => [] };
		}
                foreach my $iterref (@{$benchref->{$tmptune}{'data'}}) {
                    $iterref->{'basepeak'} = 1;
                }
	    }
            if ($hasbase) {
              @{$benchref->{'peak'}{'data'}} = @{$benchref->{'base'}{'data'}};
            }
	}
    } elsif (@benchmarks) {
        Log(10, "Doing lowest median substitution for some benchmarks (per-benchmark basepeak)\n");
        for my $bench (@benchmarks) {
	    next unless exists($result->{'results'}->{$bench});
	    Log(10, "  basepeak lowest median sub for $bench\n");
	    my %tmpmedian = ();
	    my $benchref = $result->{'results'}->{$bench};
	    for my $tune (keys %{$benchref}) {
		my @tmpdata = ();
		for my $obj (@{$benchref->{$tune}{'data'}}) {
		    $obj->{'basepeak'} = 1;
		    next unless ($obj->valid eq 'S');
		    push (@tmpdata, $obj);
		}
		$tmpmedian{$tune} = median_ratio(0, @tmpdata);
	    }
	    my @sortres = sort { $tmpmedian{$a} <=> $tmpmedian{$b} } keys %tmpmedian;
	    for (my $i = 1; $i < @sortres; $i++) {
		Log(89, "Setting $sortres[$i] to $sortres[0] for $bench\n");
		dupe_results($benchref->{$sortres[$i]}->{'data'},
			     $benchref->{$sortres[0]}->{'data'});
	    }
        }
    }
}

sub dupe_results {
    my ($dest, $src) = @_;

    return unless (ref($dest) eq 'ARRAY' && ref($src) eq 'ARRAY');

    # Do a deep copy, except for the 'tune', 'ref', and 'refs' members
    for (my $i = 0; $i < @{$src}; $i++) {
	my $href = $src->[$i];
	for my $key (keys %{$href}) {
	    next if ($key =~ /^(tune|ref|refs)$/o);
	    $dest->[$i]->{$key} = $src->[$i]->{$key};
	}
    }
}
    
# SIDE EFFECT:  Sets the selected bit for the result if select is nonzero
sub median_ratio {
    my ($select, @objs) = @_;
    my $numobjs = @objs+0;
    return undef if $numobjs <= 0;
    Log(70, "median_ratio(select=$select) for ".$objs[0]->benchmark.'='.$objs[0]->tune.'='.$objs[0]->ext.'='.$objs[0]->mach."\n");
    Log(70, "         ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");

    # Ascending sort by runtime and not ratio; for non 'ref' runs, ratio is
    # always '--'!
    @objs = sort { $a->reported_time <=> $b->reported_time } @objs;

    Log(70, "  sorted ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");

    # Select the middle (truncated) run.  For even # of runs, this will always
    # be the middle+1, which will be the run with the next highest runtime,
    # which will be the lower median ratio.
    # See chapter 9 of Cormen, Thomas, et al. _Introduction to Algorithms,
    #   2nd Edtion_. Cambridge: MIT Press, 2001
    my $sel = int($numobjs / 2);
    Log(70, "  Selected run ".($sel + 1)." of $numobjs\n");

    # Set and unset the selected bits if necessary
    if ($select) {
        for(my $i = 0; $i < @objs; $i++) {
            $objs[$i]->selected(($i == $sel) ? 1 : 0);
        }
    }

    Log(70, "  Returned ratio = ".$objs[$sel]->ratio."\n");
    return wantarray ? ($objs[$sel]->ratio, $objs[$sel]->dp) : $objs[$sel]->ratio;
}

sub round {
  my ($num, $dp) = @_;

  return $num unless defined($dp) && $dp > 0;

  return int(($num * (10 ** $dp)) + 0.5) / (10 ** $dp);
}

sub pluralize {
    my ($count, $word, @verb) = @_;

    my $rc = $count.' '.$word;
    $rc .= 's' if ($count != 1);
    $rc .= ' '.$verb[($count == 1) ? 0 : 1] if (@verb);
    return $rc;
}

sub assemble_cpu_description {
    my ($config) = @_;
    my $hw_ncpu = '';

    # Given these four fields:
    #  hw_ncpu  (n chips)
    #  hw_ncores (n cores)
    #  hw_coresperchip (n cores/chip)
    #  hw_nthreadspercore (n threads/core)
    # This routine assembles a nice text field which is VERY similar to what
    # we have in CPU2000.

    my $hw_nchips = firstof($config->accessor_nowarn('hw_nchips'));
    $hw_nchips = -1 if (!defined($hw_nchips) || $hw_nchips eq '--');

    my $hw_ncores = firstof($config->accessor_nowarn('hw_ncores'));
    $hw_ncores = -1 if (!defined($hw_ncores) || $hw_ncores eq '--');

    my $hw_ncoreschip = firstof($config->accessor_nowarn('hw_ncoresperchip'));
    $hw_ncoreschip = -1 if (!defined($hw_ncoreschip) || $hw_ncoreschip eq '--');

    my $hw_nthreadspercore = firstof($config->accessor_nowarn('hw_nthreadspercore'));
    $hw_nthreadspercore = -1 if (!defined($hw_nthreadspercore) || $hw_nthreadspercore eq '--');

    $hw_ncpu  = ::pluralize($hw_ncores, 'core').', ';
    $hw_ncpu .= ::pluralize($hw_nchips, 'chip').', ';
    $hw_ncpu .= ::pluralize($hw_ncoreschip, 'core').'/chip';
    if ($hw_nthreadspercore != 1) {
        $hw_ncpu .= ', '.::pluralize($hw_nthreadspercore, 'thread').'/core';
    }

    return $hw_ncpu;
}

sub generate_mpi_totals {
    my ($result, $config) = @_;
    foreach my $thing (qw( hw_total_nodes hw_total_chips hw_total_cores
                           hw_total_threads hw_total_memory ) ) {
        $result->{$thing} = 0;
    }
    $result->{'system_class'} = 'Imaginary' unless defined($result->{'system_class'}) && $result->{'system_class'} ne '';

    # Generate node totals for everything
    if (exists($result->{'node'}) && ::isa($result->{'node'}, 'HASH')) {
        my @nodes = grep { $result->{'node'}->{$_}->{'purpose'} =~ /compute/i } keys %{$result->{'node'}};

        # Figure out what class of system is being benchmarked, if it hasn't
        # been set to something other than one of the defaults by the
        # benchmarker.
        if (!exists($result->{'system_class'}) ||
            $result->{'system_class'} eq 'Imaginary' ||
            $result->{'system_class'} eq '--') {
            if (@nodes == 1) {
                # One kind of node -- homogenous cluster or SMP?
                if ($result->{'node'}->{$nodes[0]}->{'count'} > 1) {
                    $result->{'system_class'} = 'Homogenous Cluster';
                } else {
                    $result->{'system_class'} = 'SMP';
                }
            } else {
                $result->{'system_class'} = 'Heterogenous Cluster';
            }
        }

        # ...and now on to the totals
        foreach my $system (@nodes) {
            if (!::isa($result->{'node'}->{$system}, 'HASH')) {
                ::Log(0, "\nERROR: Node \"$system\" does not have the proper kind of data structure\n");
                next;
            }
            my $nodeptr = $result->{'node'}->{$system};
            my $nodecnt = $nodeptr->{'count'};
            $result->{'hw_total_nodes'} += $nodecnt;
            $result->{'hw_total_chips'} += $nodecnt * $nodeptr->{'hw_nchips'};
            my $node_cores = 0;
            if (!exists($nodeptr->{'hw_ncoresperchip'}) &&
                exists($nodeptr->{'hw_nchips'}) && $nodeptr->{'hw_nchips'} > 0  &&
                exists($nodeptr->{'hw_ncores'}) && $nodeptr->{'hw_ncores'} > 0) {
                $nodeptr->{'hw_ncoresperchip'} = $nodeptr->{'hw_ncores'} / $nodeptr->{'hw_nchips'};
                $node_cores = $nodeptr->{'hw_ncores'};
            } elsif (!exists($nodeptr->{'hw_ncores'}) &&
                exists($nodeptr->{'hw_nchips'}) && $nodeptr->{'hw_nchips'} > 0  &&
                exists($nodeptr->{'hw_ncoresperchip'}) && $nodeptr->{'hw_ncoresperchip'} > 0) {
                $node_cores = $nodeptr->{'hw_nchips'} * $nodeptr->{'hw_ncoresperchip'};
                $nodeptr->{'hw_ncores'} = $node_cores;
            } else {
                $node_cores = $nodeptr->{'hw_ncores'};
            }
            $result->{'hw_total_cores'} += $nodecnt * $node_cores;
            $result->{'hw_total_threads'} += $nodecnt * $node_cores * $nodeptr->{'hw_nthreadspercore'};
            my $mem = join(' ', ::allof($nodeptr->{'hw_memory'}));
            $result->{'hw_total_memory'} += $nodecnt * ::decompose_memory($mem);
        }

        # Re-compose the memory size
        $result->{'hw_total_memory'} = ::recompose_memory($result->{'hw_total_memory'});
    }

    # Un-do some laziness
    $result->{'system_class'} =~ s/^Homo$/Homogenous/i;
    $result->{'system_class'} =~ s/^Hetero$/Heterogenous/i;

    # Make it look nice(?)
    $result->{'system_class'} = ucfirst($result->{'system_class'});

}

sub mpi_min_max_ranks {
    my ($result) = @_;

    return undef unless defined($result) && ::isa($result, 'HASH');
    return undef unless exists($result->{'results'}) && ::isa ($result->{'results'}, 'HASH');

    my @tunes = @{$::global_config->{'valid_tunes'}};
    my %min_ranks = map { $_ => '--' } @tunes;
    my %max_ranks = map { $_ => '--' } @tunes;

    foreach my $tune (@tunes) {
        foreach my $bench (sort keys %{$result->{'results'}}) {
            my $benchref = $result->{'results'}->{$bench};
            next unless ::isa($benchref, 'HASH') && exists($benchref->{$tune});
            my $tuneref = $result->{'results'}->{$bench}->{$tune};
            next unless ::isa($tuneref, 'HASH') && exists($tuneref->{'data'});
            for(my $i = 0; $i < @{$tuneref->{'data'}}; $i++) {
                next unless $tuneref->{'data'}->[$i]->{'selected'};
                next unless exists($tuneref->{'data'}->[$i]->{'ranks'});
                $max_ranks{$tune} = $tuneref->{'data'}->[$i]->{'ranks'} if ($tuneref->{'data'}->[$i]->{'ranks'} > $max_ranks{$tune} || $max_ranks{$tune} eq '--');
                $min_ranks{$tune} = $tuneref->{'data'}->[$i]->{'ranks'} if ($tuneref->{'data'}->[$i]->{'ranks'} < $min_ranks{$tune} || $min_ranks{$tune} eq '--');
            }
        }
    }

    if ($min_ranks{'base'} ne '--' && $max_ranks{'base'} ne '--' &&
        $min_ranks{'base'} != $max_ranks{'base'}) {
        # What?  How can this be?
        Log(0, "ERROR: Minimum and maximum base ranks do not match!\n");
        do_exit(1);
    }
    $result->{'base_ranks'} = $min_ranks{'base'};
    $result->{'max_peak_ranks'} = $max_ranks{'peak'};
    $result->{'min_peak_ranks'} = $min_ranks{'peak'};
    $result->{'max_ranks'} = ($max_ranks{'base'} > $max_ranks{'peak'}) ? $max_ranks{'base'} : $max_ranks{'peak'};
}

sub decompose_memory {
    my ($str) = @_;

    # Take a string that _should_ contain a memory size, normalize it to MB,
    # and return it as a number.
    my %mult = ( 'K'=> 1/1024, 'M'=> 1, 'G'=> 1024, 'T'=> 1024 * 1024 );
    if ($str =~ /([\d.]+)\s*(K|M|G|T|P)B/i) {
        my ($size, $unit) = ($1, $2);
        return ($size * $mult{$unit});
    } else {
        return 0;
    }
}

sub recompose_memory {
    my ($size) = @_;

    # Take a memory size (in MB), reduce it to a nice reasonable size, and
    # return it with units.
    my $units = 'M';
    my %nextunit = ( 'K'=>'M', 'M'=>'G', 'G'=>'T', 'T'=>'P', 'P'=>'E', 'E'=>'Z', 'Z'=>'Y' );
    my %prevunit = ( 'M'=>'K', 'G'=>'M', 'T'=>'G', 'P'=>'T', 'E'=>'P', 'Z'=>'E', 'Y'=>'Z' );
    while (($size > 1023) && (($size / 1024) == int($size / 1024))
           && exists($nextunit{$units})) {
        $units = $nextunit{$units};
        $size /= 1024;
    }
    while ($size > 0 && $size < 1024 && ($size < 1 || int($size) != $size)
           && exists($prevunit{$units})) {
        $size = int($size * 1024);
        $units = $prevunit{$units};
    }

    return "$size ${units}B";
}

sub make_path_re {
    my ($path) = @_;

    # Given a path, turn it into a regular expression where either kind of
    # slash matches, and where the match is not case-sensitive on Windows.
    $path =~ s#[\\/]#\[\\/\]#g;
    if ($^O =~ /MSWin/) {
        return qr/$path/i;	# Not case-sensitive
    } else {
        return qr/$path/;
    }
}

sub unrel_path {
  my ($path) = @_;

  # Given a path, strip out explicit references to parent directories:
  # /a/b/c/../../d/e => /a/d/e
  # Always returns an absolute path.
  if ($path !~ m#^[\\/]#) {
    $path = File::Spec->rel2abs($path);
  }
  $path = File::Spec->canonpath($path);
  my @components = split(/[\/\\]+/, $path);
  for(my $i = 1; $i < @components; $i++) {
    if ($components[$i] eq '..' && $i > 0) {
      # Get rid of it and the directory it would back up into
      splice @components, $i-1, 2;
      $i -= 2;
    }
  }

  return File::Spec->catdir(@components);
}

sub firstof {
    my ($thing) = @_;

    if (isa($thing, 'ARRAY')) {
        return $thing->[0];
    } else {
        return $thing;
    }
}


sub allof {
    my ($thing) = @_;

    if (isa($thing, 'ARRAY')) {
        return @{$thing};
    } else {
        return ($thing);
    }
}

sub bytag ($$) {
    my ($a, $b) = @_;		# This is slower but works around the package
    				# variable problem.
    # Sort multi-valued things (like notes) properly, even in the face of
    # notes1 = foo
    # notes002 = bar
    # etc.
    if ($a !~ /\d$/ || $b !~ /\d$/) {
	# Both need trailing numbers to get special attention
	return $a cmp $b;
    }
    my ($atxt, $anum) = ($a =~ /^(.*?)0*(\d+)$/);
    my ($btxt, $bnum) = ($b =~ /^(.*?)0*(\d+)$/);
    return $atxt cmp $btxt || ($anum+0) <=> ($bnum+0);
}

sub bytrailingnum {
    my ($aname, $anum) = $a =~ m/^(\S+?)([\d.]*)$/;
    my ($bname, $bnum) = $b =~ m/^(\S+?)([\d.]*)$/;
    return $aname cmp $bname || ($anum+0) <=> ($bnum+0);
}

sub bytune {
    # Sort a list of tunings into the prescribed order.
    # Currently, we just ensure that base comes first and peak comes last.
    # Otherwise, it's lexical.
    return -1 if $a eq 'base';
    return 1 if $a eq 'peak';
    return $a cmp $b;
}

sub bylang($$) {
  # Sort a list of "language strings" (as seen in the reduced flags structure).
  # The idea is that the "mixed" languages some come last, or at least after
  # the "simple" language lists.
  my ($a, $b) = @_;
  my ($la, $lb) = (length($a), length($b));
  return $a cmp $b if ($la < 5 && $lb < 5);     # Single-language
  return $la <=> $lb;
}

sub lock_file {
    my ($fh, $filename) = @_;
    my $rc = undef;
    my $tries = 0;
    my $max_tries = 10;

    return (undef, 'badfh') unless defined($fh);

    my $cond = [ 'ok' ];
    while($tries < $max_tries && !(defined($rc) && $rc)) {
        eval '$rc = flock($fh, LOCK_EX);';
        $tries++;
        if ($@) {
            $cond = [ 'unimplemented', $@ ];
            last;
        }
        if ($! =~ /(?:not |un)implement/i) {
            $cond = [ 'unimplemented', $! ];
            last;
        }
        if ($rc) {
            return($rc, 'ok');
        } else {
            if ($tries < $max_tries) {
                my $tmptries = ($max_tries - $tries);
                $tmptries .= ($tmptries > 1) ? ' tries' : ' try';
                Log(170, "RETRYABLE ERROR: flock($filename, LOCK_EX) returned \"$!\"; $tmptries left\n");
            }
            sleep int(rand(5)) + 1;
        }
    }

    if ($cond->[0] eq 'unimplemented') {
        # Sigh.  Either the OS does not provide locking (unlikely), or the
        # filesystem being used doesn't support it (likely).
        # So try File::NFSLock. :/
        Log(170, "LOCK NOTICE: flock($fh, LOCK_EX) is unavailable; file-based locking will\n              be attempted\n");
        eval 'use File::NFSLock';
        if ($@) {
            if ($cond->[1] ne '') {
                Log(170, "LOCK ERROR: The File::NFSLock module could not be loaded:\n");
                Log(170, "-------------------------------\n$@\n-------------------------------\n");
                return (undef, @$cond);
            } else {
                return (undef, 'unimplemented', $@);
            }
        } else {
            my $lock = new File::NFSLock({
                                'file'               => $filename,
                                'lock_type'          => LOCK_EX,
                                'blocking_timeout'   => undef,
                                'stale_lock_timeout' => 300,
                                         });
            if (!defined($lock)) {
                return (undef, 'error', $File::NFSLock::errstr);
            }
            # Store it so that it can be unlocked later
            push @{$::locks{$fh}}, $lock;
            return(1, 'ok');
        }
    }

    my ($file, $line, $sub) = (caller(1))[1,2,3];
    $file = basename($file);
    Log(170, "LOCK ERROR: flock($filename, LOCK_EX) failed repeatedly; called from $sub ($file line $line)\n");

    return(undef, 'error', $!);
}

sub unlock_file {
    my ($fh) = @_;
    my $rc = undef;
    my $tries = 0;
    my $max_tries = 10;

    return (undef, 'badfh') unless defined($fh);

    $fh->flush();
    if (exists($::locks{$fh}) && isa($::locks{$fh}, 'ARRAY')) {
        # This was locked with File::NFSLock; no need to try flock
        foreach my $lock (@{$::locks{$fh}}) {
            $lock->unlock();
        }
        return(1, 'ok');
    }

    while($tries < $max_tries && !(defined($rc) && $rc)) {
        eval '$rc = flock($fh, LOCK_UN);';
        $tries++;
        return (undef, 'unimplemented', $@) if $@;
        return (undef, 'unimplemented', $!) if $! =~ /(?:not |un)implement/i;
        if ($rc) {
            return ($rc,   'ok');
        } else {
            if ($tries < $max_tries) {
                my $tmptries = ($max_tries - $tries);
                $tmptries .= ($tmptries > 1) ? ' tries' : ' try';
                Log(170, "RETRYABLE ERROR: flock(LOCK_UN) returned \"$!\"; $tmptries left\n");
            }
            sleep int(rand(5)) + 1;
        }
    }

    my ($file, $line, $sub) = (caller(1))[1,2,3];
    $file = basename($file);
    Log(170, "LOCK ERROR: flock(LOCK_UN) failed repeatedly; called from $sub ($file line $line)\n");

    return (undef, 'error', $!);
}

sub update_stored_config {
    my ($obj, $oldtag, $newtag, $text, $updateline, $no_backup) = @_;

    # Make versions of the tags suitable for printing in error messages
    my $printold = $oldtag;
    $printold =~ s/(.)/chr(ord($1) & 0x7f)/goe;
    $printold .= '(+high bits)' if $printold ne $oldtag;

    my $printnew = $newtag;
    $printnew =~ s/(.)/chr(ord($1) & 0x7f)/goe;
    $printnew .= '(+high bits)' if $printnew ne $newtag;

    ::Log(96, "update_stored_config($obj, oldtag='$printold', newtag='$printnew', text='$text', updateline=$updateline, no_backup=$no_backup) from ".join(':', caller)."\n");

    # Fix up the $tag item in the array $obj->{'txtconfig'} (which should
    # be the text of a config file).  The index into the array is expected
    # to be found in cfidx_$oldtag, unless $oldtag is undef, in which case
    # it'll be added to the end of the config file, in a 'default:' section
    # which is automatically added.

    if (!exists $obj->{'txtconfig'}) {
	Log(0, "ERROR(update_stored_config): $obj does not contain a txtconfig member!\n");
	return 0;
    }
    if (ref($obj->{'txtconfig'}) ne 'ARRAY') {
	Log(0, "ERROR(update_stored_config): The txtconfig member in $obj is not an array!\n");
	return 0;
    }
    if (!defined($newtag) || $newtag eq '') {
	if (!defined($oldtag) || $oldtag eq '') {
	    Log(0, "ERROR(update_stored_config): Both new and old tags are empty!\n");
	    return 0;
	} else {
	    $newtag = $oldtag;
            $printnew = $printold;
	}
    }
    if (!defined($text)) {
	# The line pointed to by $oldtag must be removed
	if (!defined($oldtag) || $oldtag eq '' ||
	    !exists($obj->{'cfidx_'.$oldtag})) {
	    Log(0, "ERROR(update_stored_config): Can't delete line '$printold'; index not found.\n");
	    return 0;
	}

        # Do a rudimentary check to ensure that the line being deleted
        # actually contains the text of $oldtag.  This protects against
        # corruption of the cfidx list, which should never happen.
        if ($obj->{'txtconfig'}->[$obj->{'cfidx_'.$oldtag}] !~ /$oldtag/) {
	    Log(0, "ERROR(update_stored_config): Won't delete line ".$obj->{'cfidx_'.$oldtag}."; line does not contain '$printold'.\n");
            # Get rid of the faulty index
            delete $obj->{'cfidx_'.$oldtag};
	    return 0;
        }

	# Save a copy of the old config (maybe)
        if (!$no_backup) {
            @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
            $obj->{'orig_raw_good'} = 1;
        }

	# Delete the offending line
	splice(@{$obj->{'txtconfig'}}, $obj->{'cfidx_'.$oldtag}, 1);

	# Adjust the indices of lines that used to follow
	::shift_indices($obj, $obj->{'cfidx_'.$oldtag}, -1);

	# Get rid of the old index
	delete $obj->{'cfidx_'.$oldtag};

	return 1;

    }

    if (!defined($oldtag)) {
	# This must be an insertion!
	my $idx = undef;
	if ($newtag =~ s/:(\d+)//) {
	    $idx = $1;
	}
	if (exists($obj->{'cfidx_'.$newtag})) {
	    Log(0, "ERROR(update_config_file): Cannot insert new line with same tag\n                            ($printnew) as existing line!\n");
	    return 0;
	}

	# Save a copy of the old config (maybe)
        if (!$no_backup) {
            @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
            $obj->{'orig_raw_good'} = 1;
        }

	if (defined($idx)) {
	    # There's a specific place to put it
            my $newtext = "$newtag =";
            if ($newtag =~ /$isnotes/) {
                # Leading whitespace for notes is preserved, so don't add any
                $newtext .= $text;
            } else {
                $newtext .= " $text";
            }
	    splice @{$obj->{'txtconfig'}}, $idx, 0, $newtext;
	    # Fix up all the indices that have just been bumped
	    ::shift_indices($obj, $idx, 1);

	    $obj->{'cfidx_'.$newtag} = $idx;
	} else {
	    # Just put it in at the end
	    if (!exists($obj->{'cfidx_tools_added_default_section'}) ||
		$obj->{'cfidx_tools_added_default_section'} <= 0) {
		push @{$obj->{'txtconfig'}},
                '',
                '# The following section was added automatically, and contains settings that',
                '# did not appear in the original configuration file, but were added to the',
                '# raw file after the run.',
                'default:';
		$obj->{'cfidx_tools_added_default_section'} = $#{$obj->{'txtconfig'}};
	    }
            my $newtext = "$newtag =";
            if ($newtag =~ /$isnotes/o) {
                # Leading whitespace for notes is preserved, so don't add any
                $newtext .= $text;
            } else {
                $newtext .= " $text";
            }
	    push @{$obj->{'txtconfig'}}, $newtext;
	    $obj->{'cfidx_'.$newtag} = $#{$obj->{'txtconfig'}};
	}

	return 1;
    }

    # If it's not an insertion and not a deletion, it must be a change.  The
    # fun begins... now!
    if (!exists($obj->{'cfidx_'.$oldtag})) {
	Log(0, "ERROR(update_config_file): No index found for '$printold'; cannot change line!\n");
	return 0;
    }
    my $idx = $obj->{'cfidx_'.$oldtag};
    if (!defined($obj->{'txtconfig'}->[$idx])) {
	Log(0, "ERROR(update_config_file): No config file line found at index $idx!\n");
	return 0;
    }
    if (exists($obj->{'cfidx_'.$newtag}) &&
	$obj->{'cfidx_'.$newtag} != $obj->{'cfidx_'.$oldtag}) {
	Log(0, "ERROR(update_config_file): Cannot change \"$printold\" \@ $idx;\n                             \"$printnew\" already exists \@ ".$obj->{'cfidx_'.$newtag}."!\n");
	return 0;
    }

    # Save a copy of the old config (maybe)
    if (!$no_backup) {
        @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
        $obj->{'orig_raw_good'} = 1;
    }

    # Make things easier by expanding tabs
    $tabstop = 8;
    my $tmpline = expand($obj->{'txtconfig'}->[$idx]);
    my ($initial_whitespace, $found_tag, $post_tag_whitespace, $pre_text_whitespace, $found_text);
    if ($tmpline =~ /^(\s*)(\S+)(\s*)=(\s*)(.*)$/) {
	$initial_whitespace  = $1;
	$found_tag           = $2;
	$post_tag_whitespace = $3;
	$pre_text_whitespace = $4;
	$found_text          = $5;
    } else {
	Log(0, "ERROR(update_config_file): Config file line at $idx does not look like\n                            a variable setting!\n");
	return 0;
    }
    if ($found_tag ne $oldtag) {
        my $printfound = $found_tag;
        $printfound =~ s/(.)/chr(ord($1) & 0x7f)/goe;
        $printfound .= '(+high bits)' if $printfound ne $found_tag;
	Log(0, "ERROR(update_config_file): Config file line at $idx ($printfound) does not match $printold!\n");
	return 0;
    }
    # Adjust the length of the variable section, preserving indentation where
    # possible.
    my $origlen = length($found_tag);
    my $newlen = length($newtag);
    my $newline;
    if ($newlen == $origlen) {
	# Easy!  Just replace $found_tag with $newtag
	$newline = $initial_whitespace.$newtag.$post_tag_whitespace.'=';
    } elsif ($origlen > $newlen) {
	# Also easy! Just pad out $post_tag_whitespace
	$newline = $initial_whitespace.$newtag.$post_tag_whitespace.
	    sprintf '%*s=', ($origlen - $newlen), ' ';
    } elsif ($origlen + length($post_tag_whitespace) > $newlen) {
	# Easy easy! Just remove some from $post_tag_whitespace
	$newline = $initial_whitespace.$newtag.
	    sprintf '%*s=', (($origlen + length($post_tag_whitespace)) - $newlen), ' ';
    } else {
	# Okay, also not too hard; just make the total length the same (if
	# possible), and have no whitespace before the equals
	$newline = sprintf '%*s=', (length($initial_whitespace) + $origlen + length($post_tag_whitespace)), $newtag;
    }

    if ($updateline) {
	# Replace the text, preserving whitespace around the tag, the equals,
	# and (if the tag does not contain "notes") the beginning of the text.
	$pre_text_whitespace = '' if ($oldtag =~ /$isnotes/);
	$newline .= $pre_text_whitespace.$text;
    } else {
	$newline .= $pre_text_whitespace.$found_text;
    }
    $obj->{'txtconfig'}->[$idx] = $newline;
    delete $obj->{'cfidx_'.$oldtag};
    $obj->{'cfidx_'.$newtag} = $idx;
    return 1;
}

sub check_elem {
    my ($endtype, $start, @keys) = @_;
    my $curr = $start;
    my $lastkey = pop @keys;

    return 0 unless isa($start, 'HASH');

    # For a chained hash, check to see that each element exists and is a
    # HASH ref, except for the end, which must be of $endtype type.
    foreach my $key (@keys) {
	return 0 unless exists($start->{$key}) && isa($start->{$key}, 'HASH');
	$start = $start->{$key};
    }
    return 0 unless exists($start->{$lastkey});

    if (defined($endtype) && $endtype ne '') {
	return 0 unless isa($start->{$lastkey}, $endtype);
    }
    return 1;
}

sub deep_copy {
    my ($objref) = @_;

    # Well, I had written one, but this is better:
    return $objref if (ref($objref) eq '');
    my $tmpref;
    eval { local($SIG{__WARN__}) = sub { 1; }; $tmpref = dclone($objref) };
    if ($@ && ($@ !~ /Can.t store item CODE/ || !$Storable::forgive_me)) {
	Log(0, "Error during deep_copy:\n  $@\n");
    }
    return $tmpref;
}

sub read_file {
    my($name) = @_;
    my (@temp);
    my $fh = new IO::File "<$name";
    # IO::File will close the file when $fh goes out of scope
    return () if !defined $fh;
    return <$fh>;
}

sub expand_ranges {
    my (@data) = @_;
    my (@rc, $start, $stop, $step, $i);

    for (@data) {
	if (($start, $stop, $step) = m/^(\d+)-(\d+)(?:x(\d+))?$/) {
	    $step = 1 if $step eq '';
	    if ($start < $stop) {
		for ($i = $start; $i <= $stop; $i += $step) {
		    push (@rc, $i);
		}
	    } else {
		for ($i = $start; $i >= $stop; $i -= $step) {
		    push (@rc, $i);
		}
	    }
	} else {
	    push (@rc, $_);
	}
    }
    return wantarray ? @rc : $rc[0];
}

sub escape_HTML {
  my ($str) = @_;

  $str =~ s/&/&amp;/gso;
  $str =~ s/</&lt;/gso;
  $str =~ s/>/&gt;/gso;

  return $str;
}

sub find_benchmark {
    my ($config, $name) = @_;
    my @objs = map { $config->benchmarks->{$_} } keys %{$config->benchmarks};
    if ($name =~ /^(\d+)$/ || $name =~ /^(\d{1,3})\./) {
        my $benchnum = $1;
	for (@objs) {
	    return $_ if ($_->num == $benchnum);
	}
    } else {
	my $match;
	for (@objs) {
	    return $_ if ($_->name eq $name || $_->benchmark eq $name);

	    if ($_->name =~ m/^$name/) {
		return undef if defined $match;
		$match = $_;
	    }
	}

	return $match if defined $match;
    }
    return undef;
}

sub fixup_subparts {
    my ($result, $syntax, $system) = @_;
    # Given a result object, a syntax table, and an item (node/interconnect
    # name), discover the number of sub-items and return a fixed-up syntax
    # table that will cause format_info to print them all.

    # Figure out where the groups of variable items are
    my @special_idx = ();
    my %special_items = ();
    my ($current_item, $current_idx) = (undef, undef);
    my @starting_indices = ();
    my $found_some = 0;
    for(my $i = 0; $i < @{$syntax}; $i++) {
        if ($syntax->[$i]->[0] =~ /^${system}([^!]+)!(\S*)$/)  {
            my ($item, $subitem) = ($1, $2);
            if (defined($current_item) && defined($current_idx)
                && $current_item ne $item) {
                push @{$special_idx[$current_idx]}, $i - $current_idx;
                $current_item = $item;
                $current_idx = $i;
                push @starting_indices, $i;
            } elsif (!(defined($current_item) || defined($current_idx))) {
                $current_item = $item;
                $current_idx = $i;
                push @starting_indices, $i;
            }
            $special_idx[$i] = [ $subitem, $item, $i ];
            $special_items{$item}++;
            $found_some++;
        } elsif (defined($current_item) && defined($current_idx)) {
            push @{$special_idx[$current_idx]}, $i - $current_idx;
            ($current_item, $current_idx) = (undef, undef);
        }
    }
    if (defined($current_item) && defined($current_idx)) {
        push @{$special_idx[$current_idx]}, @{$syntax} - $current_idx;
    }
    return $syntax unless $found_some;

    # List the keys in $result just once
    my @keys = $result->list_keys;

    # Now get the list of special items that actually appear
    my %actual_items = ();
    foreach my $item (keys %special_items) {
        foreach my $instance (grep { /^${system}${item}/ } @keys) {
            next unless $instance =~ /^${system}${item}([a-zA-Z0-9]+)/;
            $actual_items{${item}.$1}++;
        }
    }
    # These will be processed in reverse order, so sort them appropriately
    my @actual_items = reverse sort keys %actual_items;

    if (@actual_items == 0) {
        # No actual items!  So they're missing... fake up accordingly
        @actual_items = reverse sort map { $_.'MISSING' } keys %special_items;
    }

    # Sort the list of block starts in reverse order; by inserting the
    # last items into the list first, the other indices won't get
    # screwed up.
    @starting_indices = reverse sort { $a <=> $b } @starting_indices;

    # Now make parallel copies of the syntax list, one for each actual item,
    # with '!' replaced by the item's name
    my %syntax_lists = ();
    foreach my $actual (@actual_items) {
        $syntax_lists{$actual} = [ ];
        for(my $i = 0; $i < @{$syntax}; $i++) {
            next unless defined($special_idx[$i]);
            my ($subitem, $item) = @{$special_idx[$i]};
            # Copy the syntax item somewhat deeply
            $syntax_lists{$actual}->[$i] = [ @{$syntax->[$i]} ];
            $syntax_lists{$actual}->[$i]->[0] = "$system$actual$subitem";
        }
    }

    # Make a mungeable copy of the syntax list
    my @syntax = @{$syntax};

    # Now go through all the variable blocks and splice them in to the final
    # syntax list.
    foreach my $idx (@starting_indices) {
        my ($subitem, $item, $start, $num) = @{$special_idx[$idx]};
        foreach my $actual (@actual_items) {
            splice @syntax, $start+$num, 0, @{$syntax_lists{$actual}}[$start..($start+$num-1)];
        }
        # Get rid of the marker lines
        splice @syntax, $start, $num;
    }

    return \@syntax;

}

sub mpi_info_munge {
    my ($r, $item, $system) = @_;

    # In order to avoid doing a LOT of hacking on info_format()
    # we'll make up a hardware/software list that has field names
    # fixed up for the current node or interconnect
    my (@hw_info, @sw_info);
    foreach my $inforef (@{$::mpi_info{$item}}) {
        if ($inforef->[2] =~ /hw/) {
            push @hw_info, [ "${item}_${system}_$inforef->[0]", @{$inforef}[1,2] ];
        }
        if ($inforef->[2] =~ /sw/) {
            push @sw_info, [ "${item}_${system}_$inforef->[0]", @{$inforef}[1,2] ];
        }
    }

    my $hw_info = ::fixup_subparts($r, \@hw_info, "${item}_${system}_");
    my $sw_info = ::fixup_subparts($r, \@sw_info, "${item}_${system}_");

    return ($hw_info, $sw_info);
}

sub protect_notes_links {
  return munge_notes_links(1, @_);
}

sub unprotect_notes_links {
  return munge_notes_links(0, @_);
}

# Given a notes line, look for links like
# ATTACH <url> AS text
# ATTACH <url> AS [text text]
# LINK <url> AS text
# LINK <url> AS [text text]
# and prepare it for handling by line wrappers, etc.
sub munge_notes_links {
    my ($prot, $line, $replacements) = @_;

    $replacements = [] unless ::isa($replacements, 'ARRAY');
    if ($prot == 1) {
        # Protect it from the wrappers, etc.
        my $count = 0;
        while ($count < 100 &&
               $line =~ /((LINK|ATTACH)\s+(\S+)\s+AS\s+(\[[^]]+\]|\S+))/) {
            my ($section, $what, $url, $text) = ($1, $2, $3, $4);
            # Strip enclosing square brackets from $text
            $text =~ s/^\[(.*)\]$/$1/;
            my $repl = gen_notes_link_sym($section, $url, $text);
            if ($line !~ s/\Q$section\E/$repl/) {
                Log(0, "ERROR: Protection of \"$section\" in notes line failed\n");
                last;
            }
            $count++;
            push @{$replacements}, $repl;
        }
        return ($line, $replacements);
    } elsif ($prot == 0) {
        # Put the sections back
        foreach my $repl (@{$replacements}) {
            my $section = get_notes_link_section($repl);
            if (!defined($section)) {
                $repl =~ s/(.)/chr(ord($1) & 0x7f)/goe;
                Log(0, "ERROR: Replacement tag \"$repl\" not found when deprotecting notes line \"$line\"\n");

                next;
            }
            $line =~ s/\Q$repl\E/$section/; # Okay if it fails; might not be
                                            # in this line anyway.
        }
        return $line;
    }
}

sub gen_notes_link_sym {
    # Given the bits of a notes link (the full text, the URL, and the link text)
    # produce a token that can be subtituted into the original line which
    # will produce its presentation length (for the purposes of wrapping),
    # or which can be used to decompose the line for presentation.
    my ($section, $url, $text) = @_;

    if (exists($::sec_to_repl{$section})) {
        # Nothing to do
        return $::sec_to_repl{$section};
    }

    # Keep it relatively simple, but don't make it order n for new insertions
    # either...
    my $repl = length($text) x (length($text) / length(length($text)));
    $repl .= '_' x (length($text) % length($repl));
    my $testrepl;
    ($testrepl = $repl) =~ s/(.)/chr(ord($1) | 0x80)/goe;
    while (exists($::repl_to_sec{$testrepl})) {
        my $oldrepl = $repl;
        $repl++;
        $repl .= '_' x (length($text) % length($repl));
        if ($repl eq $oldrepl) {
            # Overflow!  Start from the beginning
            $repl = '0' x length($text);
        }
        ($testrepl = $repl) =~ s/(.)/chr(ord($1) | 0x80)/goe;
    }
    $repl =~ s/(.)/chr(ord($1) | 0x80)/goe;

    $::sec_to_repl{$section} = $repl;
    $::repl_to_sec{$repl} = {
                              'url'     => $url,
                              'text'    => $text,
                              'section' => $section
                            };
    return $repl;
}

sub get_notes_link_section {
    my ($repl) = @_;

    return undef unless exists($::repl_to_sec{$repl});
    return $::repl_to_sec{$repl}->{'section'};
}

# Look for "ATTACH <url> AS <text>" bits in notes, gather the files, rename
# them, fix up the URL in the notes line, and return it.
sub handle_ATTACH {
    my ($text, $basename, $written) = @_;
    $written = [] unless ::isa($written, 'ARRAY');
    my $rc = $text;
    my ($base, $dir, undef) = fileparse($basename);

    my $count = 0;
    while ($count < 40 && $text =~ /(ATTACH\s+(\S+)\s+AS\s+(\[[^]]+\]|\S+))/g) {
        my ($section, $url, $linktext) = ($1, $2, $3);
        $count++;

        # Check to see if the URL has our basename.  If so, check to see if
        # it's present and skip the fetch/rename cycle.  This can happen
        # on the initial run, when runspec gathers the attachments and then
        # rawformat runs through them again.
        if ($url =~ /^$base/) {
            next if (-f ::jp($dir, $url));
            # Otherwise, the file doesn't exist, so we'll let the code go
            # on and complain.
        }

        # Make sure it has an entry in sec_to_repl and repl_to_sec
        my $tag = ::gen_notes_link_sym($section, $url, $linktext);
        
        # Have we already seen and retrieved this URL?  If so, just point to
        # it.
        my $fname;
        if (exists($::url_to_file{$url})) {
            $fname = $::url_to_file{$url};
        } else {
            # Retrieve the URL and copy it to the supplied basename with a
            # proper extension.
            ::Log(1, "Retrieving attached file from $url\n");

            # If the URL is just a filename, and it doesn't exist in the
            # current directory, prepend the dirname of the current file
            # being formatted, if the file exists there.
            if ($url !~ m#/# && ! -r $url && -r jp($dir, $url)) {
                $url = jp($dir, $url);
            }

            my $res = ::get_url(\$ua, $url);
            if (!$res->is_success) {
                $::repl_to_sec{$tag}->{'url'} = undef;
                ::Log(0, "\nERROR: Could not retrieve file attachment from \"$url\".\n");
                ::Log(0, "       Status returned was\n         ".$res->status_line."\n\n");
                next;
            }

            # What's in it?
            my $content = $res->content();

            # Figure out what extension to give a file based on its MIME
            # content type.
            my $mime_type = $res->header('Content-Type');
            $mime_type =~ s/(\S+?)(?:,|\s+).*/$1/;
            my $ext;

            if (exists($::mime2ext{lc($mime_type)})) {
                $ext = '.'.$::mime2ext{lc($mime_type)};
            } elsif ( $mime_type =~ m#image/(.*)# ) {
                $ext = '.'.lc($1);
                $ext =~ s/jpeg/jpg/;
            } else {
                # What could it be?  Just leave it as-is, I guess...
                my $uri = URI->new($url);
                my @path_segs = $uri->path_segments;
                $ext = $path_segs[-1];
                $ext =~ s/^.*?(\.[^.]+)$/$1/;
            }
            # If the extension chosen is one that _might_ appear on any of our
            # other result outputs, prepend 'attached' to make it clear that
            # this is _not_ part of the result.
            my @formats = sort keys %{$::global_config->{'formats'}};
            my %extensions;
            if (@formats > 1) {
                # Rawformat always has the full list
                %extensions = map { '.'.$::global_config->{'formats'}->{$_}->{'extension'} => 1 } @formats;
            } else {
                # Runspec doesn't.  Not hardcoding this would require making
                # runspec locate all the formats, which would require lots
                # more (technically) useless file duplication, so I decline.
                %extensions = map { '.'.$_ => 1 } qw(txt cfg csv flags.html
                                                    html pdf ps rsf);
            }
            # And the HTML formatter will generate GIF files:
            $extensions{'.gif'} = 1;
            $ext = '.attached'.$ext if exists($extensions{$ext});

            # Choose a unique filename
            $fname = $basename.$ext;
            my $count = 0;
            while (-f $fname) {
                $count++;
                $fname = $basename.".$count".$ext;
            }

            # Write it out (finally!)
            my $ofh = new IO::File '>'.$fname;
            if (!defined($ofh)) {
                Log(0, "\nERROR: Could not open \"$fname\" for writing: $!\n");
                $::repl_to_sec{$tag}->{'url'} = undef;
                next;
            }
            binmode $$ofh, ':raw';
            $ofh->print($content);
            $ofh->close();
            push @{$written}, $fname;

            # Now strip off the directory part -- all these files should be in
            # the same place.
            # XXX This code will need some fixup for use at SPEC.
            $fname = basename($fname);
            $::repl_to_sec{$tag}->{'url'} = $fname;
            $::url_to_file{$url} = $fname;
        }

        # Now with filename in hand, rewrite the section, replacing the URL
        # with the filename
        if ($rc !~ s/\Q$section\E/ATTACH $fname AS $linktext/g) {
            ::Log(0, "\nERROR: Couldn't rewrite \"$url\" to \"$fname\" in notes line\n       \"$text\"\n");
        }
    }

    return $rc;
}

# Given a URL, file, or whatever, get the content and return an HTTP::Response
# object.
# The agent passed should be a scalar reference.
sub get_url {
    my ($agent, $url, $timeout, $proxy) = @_;
    if (!defined($timeout) && defined($::global_config)) {
        $timeout = $::global_config->http_timeout;
    }
    if (!defined($proxy) && defined($::global_config)) {
        $proxy = $::global_config->http_proxy;
    }
    $$agent = ::new_user_agent() unless defined($$agent);
    $$agent->timeout($timeout);
    $$agent->proxy(['http', 'ftp'], $proxy) if (defined($proxy) && $proxy ne '');
    my $res = new HTTP::Response( 503, 'Unknown Error' );
    my $content;

    if ($url !~ m|^[^:]+://|) {
	# Bare path; LWP not needed
	my $ifh = new IO::File '<'.$url;
        if (!defined($ifh)) {
            my $err = $!;
            ::Log(0, "\nERROR: The specified file ($url) could not\n       be opened for reading: $err\n");
            $res->code( 403 );
            $res->message( $err );
        } else {
            my $oldeol = $/;
            undef $/;
            $content = <$ifh>;
            $/ = $oldeol;
            $res->code( 200 );
            $res->message( "OK; filename is $url" );
            $res->content( $content );
        }
    } elsif (defined($$agent)) {
	if ($url !~ /^(http|ftp|file):/) {
	    Log(0, "\nERROR: Unsupported URL scheme; please use file:, http:, or ftp:.\n");
	    Log(0, "The URL specified was $url\n");
            $res->code( 405 );
            $res->message( 'Method Not Allowed' );
	} else {
	    $res = $$agent->get($url);
	    if ($res->is_success) {
		$content = $res->decoded_content('raise_error' => 0);
                if (!defined($content)) {
                    Log(0, "\nNOTICE: Could not decode content from $url; error was\n          $@\n      Proceeding using raw contents.\n");
                    $content = $res->content;
                }
                $res->content( $content );
                $res->code( 200 );
                $res->message( 'OK; filename is '.URI->new($url)->path );
	    }
	}
    } else {
	# A URL was specified, but LWP is not available.
        # This should never happen.
	Log(0, "\nERROR: A URL was specified, but LWP is not available to retrieve it.\n");
	Log(0, "       Without LWP, only normal files can be specified for URLs\n");
        $res->code( 503 );
        $res->message( 'LWP module not available' );
    }

    return $res;
}

sub expand_all {
    my (@selection) = @_;
    my @replacements;

    # Convert 'all' to whatever it's supposed to be
    if ($::lcsuite eq 'cpu2006') {
      # 'all' is really ('int', 'fp') as decided by the committee.
      @replacements = (qw(int fp));
    } elsif ($::lcsuite eq 'cpuv6') {
      # 'all' is really 'all', until the committee decides otherwise.
      @replacements = (qw(cpu));
    } elsif ($::lcsuite eq 'mpi2007') {
      @replacements = (qw(medium large));
    } elsif ($::lcsuite eq 'omp2001') {
      @replacements = (qw(medium large largepower));
    } elsif ($::lcsuite eq 'omp2012') {
      @replacements = (qw(gross));
    }
    if (@replacements) {  
        # This makes things nicer in several ways, but less general
        # Position is important, so we search for the index of 'all' so we can
        # use splice.
        for (my $i = 0; $i < @selection; $i++) {
            if ($selection[$i] =~ /^(\^)*(all|${main::lcsuite})\b/io) {
                my $not = $1;
                my $what = $2;
                my @tmp_replace = map { "${not}$_" } @replacements;
                splice @selection, $i, 1, @tmp_replace;
                Log(24, "'${not}${what}' replaced by ('".join("', '", @tmp_replace)."') at position $i of selection list\n");
            }
        }
    }

    return @selection;
}

sub get_tmp_directory {
    my ($config, $create, @extra) = @_;
    my $top = $config->top;
    if ($config->output_root ne '') {
      $top = $config->output_root;
    }

    my @subdirs = ();

    my $subdir = $config->expid;
    push @subdirs, $subdir if $subdir ne '';

    $subdir = $config->accessor_nowarn('lognum');
    if ($subdir ne '') {
        $subdir = $::suite.'.'.$subdir;
        push @subdirs, $subdir;
    }
    push @subdirs, @extra;

    # Protect against blocker files
    my $count = undef;
    my $madeit = 0;
    my $dir = '';
    my @trydirs = ('tmp', 'tmp.'.$::lcsuite, 'tmp.'.$::lcsuite.'.');
DIR: foreach my $try (@trydirs) {
        if ($try eq 'tmp.'.$::lcsuite.'.') {
            $count++;
            $try .= $count;
            $count++;   # Get ready for the next one
            push @trydirs, 'tmp.'.$::lcsuite.'.'.$count;
        } elsif (defined($count) && $count < 10) {
            $count++;   # Get ready for the next one
            push @trydirs, 'tmp.'.$::lcsuite.'.'.$count;
        }
        $dir = $top;
        foreach my $tmpdir ($try, @subdirs) {
            my $trydir = jp($dir, $tmpdir);
            next DIR if (-e $trydir && ! -d $trydir);
            $dir = jp($dir, $tmpdir);
        }
        # At this point, $dir should point to a place that _might_ be a
        # directory, or might not exist.  Either is probably okay.
        if ($create) {
            my @created = ();
            eval { @created = mkpath($dir); };
            if ($@) {
                Log(0, "ERROR: Could not create temp directory \"$dir\": $@\n");
                next DIR;
            }
        }
        $madeit = 1;
        last;
    }
    if ($madeit) {
        $::tmpdirs_seen{$dir}++;
        return $dir;
    } else {
        my $what = ($create) ? 'create ANY' : 'find ANY usable';
        Log(0, "ERROR: Could not $what temporary directory\n");
        my $from = (caller(1))[3];
        if ($from =~ /do_exit/) {
            # Avoid infinite loop
            return undef;
        } else {
            do_exit(1);
        }
    }
}

1;
