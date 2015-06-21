#!/usr/bin/perl -w

use strict;
use warnings;
use Test::More;

plan tests => 2;

main: {
    my ($fh, $mail_text, $entity, $parser);

    #-- Check whether Digest::MD5 is available
    my $has_md5 = eval "require Digest::MD5";

    #-- Load MIME::Parser
    use_ok("MIME::Parser");

    #-- Prepare parser
    $parser = MIME::Parser->new();
    $parser->output_to_core(1);
    $parser->decode_bodies(0);

    #-- Parse quoted-printable encoded file
    $entity = parse_empty_preamble_file($parser);

    #-- Check if MD5 resp. length match as expected
    $mail_text = $entity->as_string;

    if ( $has_md5 ) {
        my $md5 = Digest::MD5::md5_hex($mail_text);
        ok($md5 eq "7e32c04607e2bc47774b0f9f025e0d39", "Encoded MD5 match");
    } else {
        my $len = length($mail_text);
        ok($len == 930, "Encoded length match");
    }
}

#-- Parse quoted printable file and return MIME::Entity
sub parse_empty_preamble_file {
    my ($parser) = @_;
    open (my $fh, "testmsgs/empty-preamble.msg")
        or die "can't open testmsgs/empty-preamble.msg: $!";
    my $entity = $parser->parse($fh);
    close $fh;
    return $entity;
}

1;
