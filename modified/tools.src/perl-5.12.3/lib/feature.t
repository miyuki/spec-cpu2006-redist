#!./perl

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    $ENV{SPECPERLLIB} = '../lib';
}

our $pragma_name = "feature";
require "../t/lib/common.pl";
