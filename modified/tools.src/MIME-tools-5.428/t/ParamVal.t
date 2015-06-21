use strict;
use warnings;
use Test::More tests => 5;

use MIME::Field::ContType;

# Trivial test
{
	my $field = Mail::Field->new('Content-type');

	isa_ok( $field, 'MIME::Field::ParamVal');
	isa_ok( $field, 'Mail::Field');

	$field->param('_', 'stuff');
	$field->param('answer', 42);

	is( $field->stringify, 'stuff; answer="42"', 'Object stringified to expected value');
}

# Test for CPAN RT #34451
{
	my $header = 'stuff; answer*=%FE%20%FF';

	my $field = Mail::Field->new('Content-type');
	$field->parse( $header );
	is( $field->param('_'), 'stuff', 'Got body of header');

	my $expected = pack('C', 0xfe) . ' ' . pack('C', 0xff);
	is( $field->param('answer'), $expected, 'answer param was unpacked correctly');
}
