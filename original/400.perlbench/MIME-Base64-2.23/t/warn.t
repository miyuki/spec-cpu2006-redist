#!perl -w

BEGIN {
    if ($ENV{'PERL_CORE'}){
        chdir 't' if -d 't';
        @INC = '../lib';
    }
}

BEGIN {
    eval {
	require warnings;
    };
    if ($@) {
	print "1..0\n";
	print $@;
	exit;
    }
}

use strict;
use MIME::Base64 qw(decode_base64);

if ($MIME::Base64::OLD_CODE) {
    print "1..0 # skipped: only XS version generates warnings\n";
    exit;
}

print "1..1\n";

use warnings;

my @warn;
$SIG{__WARN__} = sub { push(@warn, @_) };

warn;
my $a;
$a = decode_base64("aa");
$a = decode_base64("a===");
warn;
$a = do {
    no warnings;
    decode_base64("aa");
};
$a = do {
    no warnings;
    decode_base64("a===");
};
warn;
$a = do {
    local $^W;
    decode_base64("aa");
};
$a = do {
    local $^W;
    decode_base64("a===");
};
warn;

if ($^O eq 'MSWin32') {
    for (@warn) {
	s|\\|/|g;
    }
}

for (@warn) {
    print "# $_";
}

print "not " unless join("", @warn) eq <<'EOT'; print "ok 1\n";
Warning: something's wrong at t/warn.t line 36.
Premature end of base64 data at t/warn.t line 38.
Premature padding of base64 data at t/warn.t line 39.
Warning: something's wrong at t/warn.t line 40.
Premature end of base64 data at t/warn.t line 43.
Premature padding of base64 data at t/warn.t line 47.
Warning: something's wrong at t/warn.t line 49.
Warning: something's wrong at t/warn.t line 58.
EOT
