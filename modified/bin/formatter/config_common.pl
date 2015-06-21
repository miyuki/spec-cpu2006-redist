#
# config_common.pl
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: config_common.pl 6364 2011-03-05 00:41:51Z cloyce $
#

use strict;
use UNIVERSAL qw(isa);

my $version = '$LastChangedRevision: 6364 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'config_common.pl'} = $version;

# Create a new object
sub new {
    my ($class) = @_;
    my $me = bless {}, $class;

    # These are used for accessor lookups.  Initialize to be the base level
    $me->{'refs'} = [ $me ];

    return $me;
}

sub default_lookup {
    my ($config, $key, $ext, $mach, @sets) = @_;

    # Look up $key in $config's default=default=default=default section (and
    # others, if any) or in $config itself.
    return undef unless ::isa($config, 'Spec::Config');


    my $tmp = { 'refs' => [ reverse($config,
                                    $config->ref_tree('tmp',
						      ['default', @sets],
						      ['default', 'base'],
						      ['default', $ext],
						      ['default', $mach]))
			  ] };
    bless $tmp, ref($config);

    return  $tmp->accessor_nowarn($key);
}

sub bytrailingnum {
    my ($anum) = $a =~ m/([0-9.]+)\s*$/;
    my ($bnum) = $b =~ m/([0-9.]+)\s*$/;
    my $rc =  $anum <=> $bnum;
    return $rc if $rc;
    return $a cmp $b;
}

sub list_keys {
    my ($me) = @_;
    my %seen;
    my @rc;
    my @refs = ();

    if (ref($me) eq 'HASH') {
	if (exists($me->{'refs'}) && ref($me->{'refs'}) eq 'ARRAY') {
	    @refs = @{$me->{'refs'}};
	} else {
	    @refs = ($me);
	}
    } else {
	@refs = (@{$me->refs});
    }

    for my $hash (@refs) {
	for my $key (keys %$hash) {
            if (!$seen{$key}) {
	        push (@rc, $key);
                $seen{$key}++;
            }
	}
    }
    return sort @rc;
}

sub match_keys {
    my ($me, @pats) = @_;
    my @rc;
    for my $val (list_keys($me)) {
	for my $pat (@pats) {
	    if ($val =~ m/^${pat}[0-9.]*$/) {
		push (@rc, $val);
		next;
	    }
	}
    }
    return sort bytrailingnum @rc;
}

sub accessor_backend {
    my ($me, $warn, $what, @rest) = @_;
    my @old = ();
    my @refs;
    my %seen = ();
    my $stringify = 0;

    return undef unless exists ($me->{'refs'}) && ref($me->{'refs'}) eq 'ARRAY';

    @refs = @{$me->{'refs'}};
    while (defined(my $ref = shift @refs)) {
  	if (exists $ref->{$what}) {
	    if (isa($ref->{$what}, 'HASH') && 
		defined($ref->{$what}->{op}) &&
		$ref->{$what}->{op}=='+=') {
		push @old, $ref->{$what}->{value};
		$stringify++;
	    } else {
		if ($ref->{$what} eq '%undef%') {
		    @old = ( '%undef%' );
		} else {
		    push @old, $ref->{$what};
		}
		last;
	   }
  	} elsif (exists ($ref->{'inherit_from'}) &&
  		 ref($ref->{'inherit_from'}) eq 'ARRAY') {
  	    # Look in places from which we're supposed to inherit settings, but
	    # _don't_ look in the same place twice; that could cause loops
  	    unshift @refs, grep { !exists($seen{$_}) } @{$ref->{'inherit_from'}};
  	}
  	$seen{$ref}++;
    }

    if (!@old) {
	if ($warn) {
            # Believe it or not, but all of the code down to the Log(...) is
            # there just to find the most recent non-eval caller.
	    my ($pkg, $file, $ln, $subroutine, $hasargs,
		$wantarray, $evaltext, $is_require, $hints, $bitmask);
	    my $lvl = 3;
	    while (!defined($file) && $lvl >= 0) {
		($pkg, $file, $ln, $subroutine, $hasargs,
		 $wantarray, $evaltext, $is_require, $hints, $bitmask) = caller($lvl);
#		printf("pkg=%s\tfile=%s\nln=%d\tsubroutine=%s\thasargs=%s\twantarray=%s\nevaltext=%s\nis_require=%d\thints=%s\tbitmask=%08x\n----------\n",
#		       $pkg, $file, $ln, $subroutine, $hasargs,
#		       $wantarray, $evaltext, $is_require, $hints, $bitmask);
		$lvl--;
	    }
	    $file = File::Basename::basename($file) if (defined($file) && $file ne '');
	    Log(0, "WARNING: accessor '$what' not found; called from $pkg::$subroutine (call at $file line $ln) for object ".ref($me)."\n");
	    $DB::single=$DB::signal=1 if $warn;
	}
	return undef if !@rest;
    }

    # Return nothing if the key is not supposed to exist
    if (@old == 1 && $old[0] eq '%undef%') {
         return undef;
    }

    if (@old==1 && @rest) {
	my $old = shift @old;
	my $firstref = undef;
	if (ref($me->{'refs'}) eq 'ARRAY') {
	    $firstref = $me->{'refs'}->[0];
	}
	if (!defined($firstref)) {
	    $firstref = $me;
	}
	if (ref($old) eq 'ARRAY') {
	    $firstref->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $firstref->{$what} = { @rest };
	} else {
	    $firstref->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
     }

    return ($stringify) ? join(' ', reverse @old) : $old[0];
}

sub accessor {
    my $me = shift;
    $me->accessor_backend(1, @_);
}
sub accessor_nowarn {
    my $me = shift;
    $me->accessor_backend(0, @_);
}

# Automagically create new accessor functions for the class
AUTOLOAD {
    use vars qw($AUTOLOAD);
    my $name;
    my ($pkg,$func) = $AUTOLOAD =~ /(.*)::([^:]+)$/;
    if ($func eq 'DESTROY') {
	eval "package $pkg; sub $func {}";
    } else {
	eval "package $pkg; sub $func { shift->accessor('$func', \@_); }";
    }
    goto &$AUTOLOAD;
}

# Alias some main:: package functions into our namespace so we don't have to
# keep calling out the package
sub jp        { main::joinpaths(@_); }
sub istrue    { main::istrue(@_); }
sub Log       { main::Log(@_); }
sub uniq      { main::uniq(@_); }
sub deep_copy { main::deep_copy(@_); }
sub do_exit   { main::do_exit(@_); }
sub pluralize { main::pluralize(@_); }

1;
