#
# listfile.pm
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: listfile.pm 6671 2011-07-20 14:29:40Z CloyceS $

package Spec::Listfile::entry;

use strict;

my $version = '$LastChangedRevision: 6671 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'listfile.pm'} = $version;

sub new {
    my ($name, $ref) = @_;
    return bless $ref, $name;
}

sub lock {
    my $me = shift;

    $me->{'lock'} = 1;
    return $me;
}

sub unlock {
    my $me = shift;

    $me->{'lock'} = 0;
    return $me;
}

sub remove {
    my $me = shift;
    for (keys %$me) {
	delete $me->{$_};
    }
    return $me;
}

sub delete {
    my $me = shift;

    $me->{'lock'} = 0;
    return $me;
}

sub name {
    my $me = shift;
    return $me->{'name'};
}

sub path {
    my $me = shift;
    return $me->{'dir'};
}

package Spec::Listfile;

use strict;
use IO::File;
use Fcntl qw(O_CREAT O_RDWR O_EXCL LOCK_EX LOCK_UN);
use File::Path;

sub jp { main::joinpaths(@_); }

sub new {
    my ($class, $dir, $fname) = @_;
    my $me = bless {}, $class;

    $me->{'dir'}      = $dir;
    $me->{'filename'} = jp($dir,$fname);

    $me->open();

    return $me;
}

sub open {
    my ($me, $fname) = @_;
    my $config = $main::global_config;

    if (defined($fname) && ($fname ne '')) {
	$me->{'filename'} = $fname;
    }
    $fname = $me->{'filename'};

    my $fh = new IO::File;

    # If the listfile doesn't exist, create it.  Force IO::File to use sysopen
    # with exclusive create.  This means that there will be only one lock file
    # created.  Of course for NFS all bets are off, and multi-user is at your
    # own risk.
    if ( ! -f "$fname") {
	my @dir = (::dirname($fname));
	if (! -d $dir[0]) {
	    while (! -d $dir[0]) {
		unshift @dir, ::dirname($dir[0]);
	    }
	    foreach my $dir (@dir) {
		eval { File::Path::mkpath($dir, 0, 0777) };
                if ($@) {
                    Log(0, "ERROR: Could not create listfile directory: $@\n");
                    do_exit(1);
                }
	    }
	}
	# Jeff Reilly wants list files to always be at least mode 666, so
	# we'll do some umask fiddling to accomodate.  We'll use eval, just
        # to be safe.
        my $oldumask;
	eval { $oldumask = umask 00000; };	# We don't care if it fails
	my $rc = $fh->open($fname, O_RDWR|O_CREAT|O_EXCL, 00666);
	eval { umask $oldumask; }; 		# as long as we don't die too
	$fh->close() if ($rc);
    }

    # Grab and lock file
    if (! $fh->open("+<$fname")) {
	Log(0, "Can't open list file '$fname': $!\n");
        # This can't be ignored
        do_exit(1); 
    }
    if (istrue($main::global_config->locking) &&
        !istrue($main::global_config->absolutely_no_locking)) {
        my ($rc, $what) = lock_file($fh, $fname);
        if (!defined($rc) || $what ne 'ok') {
            $me->{'filelocked'} = 0;
            if ($what eq 'unimplemented') {
                Log(0, "\n\nLOCK ERROR: The list file ($fname) could not be locked;\n        this could cause problems for concurrent runs of this benchmark.\n\n");
            } elsif ($what eq 'error') {
                Log(0, "\n\nLOCK ERROR: There was an error locking the list file ($fname).\n");
                Log(0, "   This would only be a problem for concurrent runs, but because there is\n");
                Log(0, "   no way to know whether other copies are running, the run will now\n");
                Log(0, "   be aborted.\n");
                Log(0, "   To forego list file locking in the future, set\n");
                Log(0, "      locking=0\n");
                Log(0, "   in the global section of your config file.\n\n");
                ::do_exit(1);
            }
        } else {
            $me->{'filelocked'} = 1;
        }
    } else {
        Log(0, "WARNING: file locking is disabled in your config file.  This could cause\n         problems for concurrent runs of this benchmark.\n");
    }
    $me->{'filehandle'} = $fh;

    # Parse the file
    $me->parse();

    return $me;
}


# Read the file data into the object
sub parse {
    my $me = shift;
    my $fh = $me->{'filehandle'};

    # Get rid of old data
    $me->{'data'} = {}; 

    seek ($fh, 0, 0); # Read file from beginning

    while (my $line = <$fh>) {
	next if $line =~ m/^\s*\#/ || $line =~ m/^\s*$/;
	last if $line =~ m/^__END__/;
        chomp($line);
        $line =~ s/([^ ]*)//;    # Strip off the name
	my ($name, @data) = ($1, split(/ ?(\S+=)/, $line));
        shift(@data) if (!defined($data[0]) || $data[0] eq '');
	while (@data) {
            my $tag = shift(@data);
            if ($tag =~ s/=$//) {
                my $item = shift(@data);
                if (defined($item)) {
                    $me->{'data'}{$name}{$tag}=$item;
                } else {
                    ::Log(0, "Notice: Ignoring tag \"$tag\" with undefined data\n");
                }
            } else {
                ::Log(0, "Notice: Ignoring strange non-tag element \"$tag\"\n");
            }
	}
	$me->{'data'}{$name}{'name'} = $name;
	$me->{'data'}{$name}{'lock'} = 0 if !defined $me->{'data'}{$name}{'lock'};
    }
}

sub update {
    my $me = shift;
    my $fh = $me->{'filehandle'};

    seek($fh, 0, 0);
    for my $name (sort keys %{$me->{'data'}}) {
	next if keys %{$me->{'data'}{$name}} == 0;
	next if exists($me->{'data'}) && exists($me->{'data'}->{$name}) &&
	    ($me->{'data'}->{$name}->{'delete'} ne '');
	print $fh "$name";
	for (sort keys %{$me->{'data'}{$name}}) {
	    next if $_ eq 'name';
	    print $fh " $_=",$me->{'data'}{$name}{$_};
	}
	print $fh "\n";
    }
    print $fh "__END__\n";
    # Attempt to be nice and truncate the file, this isn't too important
    # as the __END__ tag will caues the rest of the file to be ignored.
    # eval to keep operating systems without truncate happy.
    eval '$fh->truncate(tell($fh))';
}

sub find_entry {
    my ($me, $topdir, %criteria) = @_;
    my $seen_warning = 0;
    if ($topdir !~ /\/$/) {
	$topdir .= '/';
    }
    loop:
    for my $name (sort keys %{$me->{'data'}}) {
        my $tmpdir = $me->{'data'}{$name}{'dir'};
        next if $tmpdir eq '';  # Happens when previous dirs are removed
        my $tmptop = $topdir;
        $tmpdir =~ s#\\#/#g;
        $tmptop =~ s#\\#/#g;
	if (
            # case-insensitive comparison for non-case-sensitive filesystems
            # (like HFS+ and NTFS)
            ($^O =~ /(?:MSWin|darwin)/i && $tmpdir !~ /^$tmptop/i)
            ||
            # case-sensitive comparison for more sensible filesystems
	    ($^O !~ /(?:MSWin|darwin)/i && $tmpdir !~ /^$tmptop/)
          ) {
            if (!$seen_warning) {
              $seen_warning = 1;
              my $type = $me->{'data'}->{$name}->{'type'};
              $type = 'run' unless $type ne '';
              Log(0, "\nNotice: Unusable path detected in $type directory list file.
  $me->{'filename'}
references one or more paths which will be ignored, because they are
not subdirectories of this $type directory.  This condition may be a
result of having moved your SPEC benchmark tree.  If that's what happened,
and if you don't need the old $type directories, you can just remove
them, along with the list file.  (Usually it's safe to delete old
$type directories, as they are automatically re-created when needed.)
\n\n");
            }
	    next;
	}
	for my $crit (keys %criteria) {
	    next loop if $me->{'data'}{$name}{$crit} ne $criteria{$crit};
	}
	my $entry = new Spec::Listfile::entry($me->{'data'}{$name});
	return $entry;
    }
    return undef;
}

# Fast way to find an entry by its name
sub find_entry_name {
    my ($me, $name) = @_;

    if (defined $me->{'data'}{$name}) {
	return new Spec::Listfile::entry($me->{'data'}{$name});
    }
    return undef;
}

sub remove_entry {
    my $me = shift;
    my $entry = $me->find_entry(@_);
    $entry->remove() if ($entry);
}

sub new_entry {
    my ($me, $name, %criteria) = @_;
    my $config = $main::global_config;
    my $no_num = $criteria{'num_optional'};
    delete $criteria{'num_optional'};
    my $num_is_date = $criteria{'num_is_date'};
    delete $criteria{'num_is_date'};
    my $num_width = $criteria{'num_width'} || 4;
    delete $criteria{'num_width'};

    if ($num_is_date && exists $me->{'data'}->{$name}) {
      $name .= '.'.::timeformat(time, 2);
    }
    my $num = 0;
    foreach my $currext (map { if (/^\Q$name\E\.(\d+)$/) { $1 } else { undef } } keys %{$me->{'data'}}) {
        next unless defined($currext);
	$num = $currext + 1 if ($num+0 <= $currext+0); 
    }
    if ($num+0 >= 9999999) {
	Log(0, "ERROR: Too many run directories!\n");
        do_exit(1);
    }
    if (!$no_num || exists $me->{'data'}->{$name}) {
      $name = sprintf("%s.%0${num_width}d", $name, $num);
    }

    $me->{'data'}{$name}={};  # Make sure there is a hash

    $me->{'data'}{$name}{'dir'}=jp($me->{'dir'},$name);
    for (keys %criteria) {
	$me->{'data'}{$name}{$_}=$criteria{$_};
    }
    $me->{'data'}{$name}{'name'} = $name;
    my $entry = new Spec::Listfile::entry($me->{'data'}{$name});
    return $entry;
}

sub close {
    my $me = shift;
    if (defined $me->{'filehandle'}) {
	unlock_file($me->{'filehandle'}) if ($me->{'filelocked'} && istrue($main::global_config->locking) && !istrue($main::global_config->absolutely_no_locking));
	close($me->{'filehandle'});
	delete $me->{'filehandle'};
    }
}

sub DESTROY {
    my $me = shift;
    $me->close();
}

sub istrue  { main::istrue(@_);  }
sub do_exit { main::do_exit(@_); }
sub Log     { main::Log(@_);     }
sub lock_file   { main::lock_file(@_);   }
sub unlock_file { main::unlock_file(@_); }

1;
