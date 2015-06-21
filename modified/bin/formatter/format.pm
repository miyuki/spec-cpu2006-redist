#
# format.pm
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: format.pm 6710 2011-08-05 21:53:46Z CloyceS $
#
package Spec::Format;

# Keep -w quiet
{ my $trash = $DB::signal=$DB::single; }

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'format.pm'} = $version;

sub new {
    my ($class) = @_;
    my $me       = bless {}, $class;

    $me->{'name'}      = ${"${class}::name"};
    $me->{'extension'} = ${"${class}::extension"};
    $me->{'synonyms'}  = ${"${class}::synonyms"} || { lc($me->{'name'}) };
    $me->{'binary'}    = defined(${"${class}::binary"}) ? ${"${class}::binary"} : 0;

    return $me;
}

sub name {
    my $me = shift;
    return $me->{'name'};
}

# Search through the list of references for the data, can assign
# data too if an entry for the data exists.
sub accessor {
    my ($me, $what, @rest) = @_;
    if (! exists $me->{$what}) {
	my ($pkg, $file, $ln, $subroutine, $hasargs,
	    $wantarray, $evaltext, $is_require, $hints, $bitmask);
	my $lvl = 2;
	while (!defined($file) && $lvl >= 0) {
	    ($pkg, $file, $ln, $subroutine, $hasargs,
	     $wantarray, $evaltext, $is_require, $hints, $bitmask) = caller($lvl);
#	     printf("pkg=%s\tfile=%s\nln=%d\tsubroutine=%s\thasargs=%s\twantarray=%s\nevaltext=%s\nis_require=%d\thints=%s\tbitmask=%08x\n----------\n",
#		    $pkg, $file, $ln, $subroutine, $hasargs,
#		    $wantarray, $evaltext, $is_require, $hints, $bitmask);
	    $lvl--;
	}
	$file = File::Basename::basename($file) if (defined($file) && $file ne '');
	Log(0, "WARNING: accessor '$what' not found; called from $pkg::$subroutine (call at $file line $ln) for object ".ref($me)."\n");
	$DB::signal = $DB::single = 1;
	return undef;
    }
    my $old = $me->{$what};
    if (@rest) {
	if (ref($old) eq 'ARRAY') {
	    $me->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $me->{$what} = { @rest };
	} else {
	    $me->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
    }
    return $old;
}

AUTOLOAD {
    use vars qw($AUTOLOAD);
    my $name;
    my ($pkg,$func) = $AUTOLOAD =~ /(.*)::([^:]+)$/;
    if ($func eq 'DESTROY') {
	eval "package $pkg; sub $func {}";
    } else {
	eval "package $pkg; sub $func { shift->accessor('$func', \@_); }";
    }
    # Uncomment the following line to help track down
    # Can't call method "accessor" on unblessed reference at (eval 533) line 1.
    # errors.
    #print "pkg=$pkg\nfunc=$func\n";
    goto &$AUTOLOAD;
}

1;
