#!./perl

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    $ENV{SPECPERLLIB} = '../lib';
}

our $pragma_name = "warnings";
our $UTF8 = (${^OPEN} || "") =~ /:utf8/;
require "../t/lib/common.pl";
