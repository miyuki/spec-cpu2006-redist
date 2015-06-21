#
#  setup.pl - early-access routines for runspec/rawformat startup
#  Copyright 2006-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: setup_common.pl 6710 2011-08-05 21:53:46Z CloyceS $

my $tmpversion = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$tmpversion =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'setup.pl'} = $tmpversion;
$::suite_version = 0;

use strict;
use IO::File;			# Because we want to read a file early
use Digest::MD5;		# To check the integrity of the tools
use File::Basename;		# Ditto
use Time::HiRes;                # For early internal timings
use IO::Uncompress::Bunzip2 qw(:all); # For md5filedigest's decompress mode
use UNIVERSAL qw(isa);

sub get_suite_version {
    my $fh = new IO::File "<$ENV{'SPEC'}/version.txt";  # DOS is still dumb
    if (defined($fh)) {
	my $suite_ver = <$fh>;
	$suite_ver =~ tr/\015\012//d;
        # CVT2DEV: $suite_ver .= 'dev';
	return $suite_ver;
    } else {
        if (!exists ($ENV{'SPEC'}) || !defined($ENV{'SPEC'}) ||
            $ENV{'SPEC'} eq '') {
            # One of those impossible things
            print STDERR "\nThe SPEC environment variable is not set; please source the shrc before\n  invoking runspec.\n\n";
            exit 1;
        }
        print STDERR "\nThe ${main::suite} suite version could not be read.  Your distribution is corrupt\n  or incomplete.\n\n";
        exit 1;
    }
}

sub read_toolset_name {
    my $fh = new IO::File "<$ENV{'SPEC'}/bin/packagename";
    my $packagename = defined($fh) ? <$fh> : 'unknown';
    $packagename =~ tr/\015\012//d;
    return $packagename;
}

sub timeformat {
    my ($time, $format) = @_;

    # Doing this is easier than adding Date;:Format to the tools...
    # Some of this is stolen from ctime.pl, which comes with perl
    my @DoW = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    my @MoY = ('Jan','Feb','Mar','Apr','May','Jun',
	       'Jul','Aug','Sep','Oct','Nov','Dec');

    my ($sec, $min, $hour, $mday, $mon, $year, $wday);
    if (::isa($time, 'ARRAY')) {
      ($sec, $min, $hour, $mday, $mon, $year, $wday) = @{$time};
    } else {
      ($sec, $min, $hour, $mday, $mon, $year, $wday) = gmtime($time);
    }
    $year += 1900;
    if ($format == 2) { # YYYYMMDD
      return sprintf "%04d%02d%02d", $year, $mon+1, $mday;
    } elsif ($format == 3) { # MMM-YYYY
      return sprintf("%3s-%4d", $MoY[$mon], $year);
    } else {    # Human readable
      return sprintf("%02d-%3s-%4d %02d:%02d:%02d", $mday, $MoY[$mon], $year,
                     $hour, $min, $sec);
    }
}

sub joinpaths {
    my @dirs;
    for my $tmp (@_) {
	next unless defined($tmp) && $tmp ne '';
	# Replace all backslashes with forward slashes (for NT)
	my $a = $tmp;
	$a =~ s|\\|/|go;
	next if $a eq '';
	# If this is the start of an absolute path, remove what's already there
	@dirs = () if ($a=~m/^([^:\[]*):?\[(\S*)\]/o || $a =~ m|^/|o || $a =~ m|^[a-zA-Z]:|o);

	if ($a=~m/^([^:\[]*):?\[(\S*)\]/o) { # VMS path - make it a UNIX-alike
	    push (@dirs, $1, split('.', $2));
	} else { # Unix PATH
            $a =~ s#/+$##;      # Strip trailing /
	    push (@dirs, $a);
	}
    }
    my $result = join('/',@dirs);
    return $result;
}
sub jp { joinpaths(@_); }

sub read_manifest {
    my ($path, $manifest, $re) = @_;
    my %md5 = ();
    my %sizes = ();

    if (!-e $manifest) {
        # This should never happen.  But just in case...
        print STDERR "\nThe manifest file ($manifest) is missing or unreadable;\n  the benchmark suite is incomplete or corrupted!\n\n";
        exit 1;
    } elsif (!-r $manifest) {
        print STDERR "\nThe manifest file ($manifest) is present but cannot be read; please check the permissions!\n\n";
        exit 1;
    }

    my $manifestreadstart = Time::HiRes::time;

    # Read in the manifest and store it in a hash
    my @lines = ();
    my $fh = new IO::File '<'.$manifest;
    if (!defined($fh)) {
        print STDERR "There was a problem opening $manifest: $!\n";
        exit 1;
    }

    while(defined(my $line = <$fh>)) {
        next unless $line =~ /$re/;
        my ($md5, $size, $fn) = $line =~ m/^([A-Fa-f0-9]{32}) (?:\*| ) ([A-F0-9]{8}) (.*)$/o;
        next if ($fn eq '') || ($md5 eq '');
        my $fullpath = jp($path, $fn);
        # Normalize the path separators
        $fullpath =~ tr|\\|/|;
        $md5{$fullpath} = $md5;
        if (defined($size)) {
            $sizes{$fullpath} = hex($size);
        } else {
            # Stat it
            $sizes{$fullpath} = -s $fullpath;
        }
    }
    $fh->close();
    printf("read_manifest completed in %8.7fs\n", Time::HiRes::time - $manifestreadstart) if ($::debug >= 99);

    return ((keys %md5)+0, \%sizes, \%md5);
}

sub check_files {
    my ($sums, @files) = @_;
    my $top = $ENV{'SPEC'};

    # Check a list of files against the MD5 sums in %file_md5
    foreach my $file (@files) {
	# Add the path to the top level directory, if it's not already there:
	$file = jp($top, $file) unless $file =~ /^$top/oi;
	if (!exists($sums->{$file})) {
#	    print "No MD5 for $file!\n";
	    return wantarray ? (0, $file) : 0;
	} else {
	    my $genmd5 = md5filedigest($file);
	    if ($sums->{$file} ne $genmd5) {
#		print "MD5 mismatch! ($file_md5{$file} ne $genmd5)\n";
		return wantarray ? (0, $file) : 0;
	    }
	}
    }

    return wantarray ? (1, undef) : 1;
}

sub md5filedigest {
    my ($file, $decompress) = @_;
    my $md5 = new Digest::MD5;

    if ($decompress && $file =~ /\.bz2$/) {
        # For automatic decompression, the file _must_ have a .bz2 extension
        my $bz = new IO::Uncompress::Bunzip2 $file, 'Buffer' => 262144;
        my $tmp;
        my $status;

        while(($status = $bz->read($tmp)) > 0) {
            $md5->add($tmp);
        }
        $tmp = "Error reading from '$file': $Bunzip2Error\n";
        $bz->close();
        if ($status < 0) {
            # Error
            if (exists($INC{'log.pl'})) {
                Log(0, $tmp);
            } else {
                print STDERR $tmp;
            }
            return undef;
        }
    } else {
        my $fh  = new IO::File $file, O_RDONLY|O_BINARY;
        if (!defined($fh)) {
            if ($file !~ s/\.bz2$//) {
                # Try it _with_ the bz2 extension
                $fh = new IO::File "${file}.bz2", O_RDONLY|O_BINARY;
            } else {
                # Try it _without_ the bz2 extension (because the subst
                # above will have stripped it)
                $fh = new IO::File $file, O_RDONLY|O_BINARY;
            }
        }
        if (!defined($fh)) {
            my $msg = "md5filedigest: can't open '$file' for reading.\n  The error message was '$!'\n";
            if (exists($INC{'log.pl'})) {
                Log(0, $msg);
            } else {
                print STDERR $msg;
            }
            return '';
        } else {
            $md5->addfile($fh);
            $fh->close();
        }
    }
    return $md5->hexdigest();
}

sub load_module {
    my ($module, $quiet) = @_;

    if ($::check_integrity && !check_files(\%::file_md5, jp('bin', $module))) {
	die "\n\nPart of the tools ($module) is corrupt!  Aborting...\n\n";
    }
    eval "require \"$module\";";
    print '.' unless ($quiet);
    if ($@) {
	print "\nError loading $module!  Your tools are incomplete or corrupted.\n";
	die "eval said '$@'\nStopped";
    }
}

sub read_manifests {
    my (@files) = @_;

    # CVT2DEV: return ({}, {});
    return ({}, {}) if ($::tools_versions{'formatter_vars.pl'} &&
                        defined($::website_formatter) && $::website_formatter);

    $::check_integrity = 0;
    $::suite_version = get_suite_version();
    print "Reading MANIFEST... " unless $::from_runspec;
    my ($files, $file_size, $file_md5) = (0, {}, {});
    foreach my $file (@files) {
        my ($re, $fn) = (isa($file, 'ARRAY') ? @{$file} : (qr/./, $file));
        my $path = jp($ENV{'SPEC'}, $fn);
        if (!-e $path) {
            if ($fn eq 'SUMS.tools') {
                print "\n\nThe checksums for the binary tools could not be found.  If you have just\n";
                print "built a new set of tools, please run packagetools to create the checksums.\n";
                print "Additionally, packagetools will create a tar file of the newly built tools\n";
                print "which you can use for other installations on the same architecture/OS.\n";
                print "\n";
                exit 1;
            } elsif ($fn eq 'SUMS.data') {
                # It's okay -- just ignore it
                next;
            }
        }
        my ($tmpcnt, $tmpsize, $tmpmd5) = read_manifest($ENV{'SPEC'}, $path, $re);
        foreach my $key (keys %{$tmpsize}) {
            $file_size->{$key} = $tmpsize->{$key};
        }
        foreach my $key (keys %{$tmpmd5}) {
            $file_md5->{$key} = $tmpmd5->{$key};
        }
        $files += $tmpcnt;
    }
    print "$files files\n" unless $::from_runspec;

    return ($file_size, $file_md5);
}

sub check_important_files {
    my ($re) = @_;

    # CVT2DEV: $::check_integrity = 0; return;
    return if (   $::suite_version > 4 # YYY - version
               && !$ENV{'SPEC_CHECK'});
    $::check_integrity = 1;
    foreach my $important_file (jp('bin', basename($0)),
                                grep { m/$re/ } keys %::file_md5) {
        if (!check_files(\%::file_md5, $important_file)) {
            print STDERR "\n\nPart of the tools ($important_file) is corrupt!\nAborting...\n\n";
            exit 1;
        }
    }
}

1;
