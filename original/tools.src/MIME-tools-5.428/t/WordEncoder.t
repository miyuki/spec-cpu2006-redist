use strict;
use warnings;
use Test::More tests => 8;
use MIME::Words qw(:all);

is(encode_mimeword('wookie', 'Q', 'ISO-8859-1'),
    '=?ISO-8859-1?Q?wookie?=');
is(encode_mimeword('François', 'Q', 'ISO-8859-1'),
    '=?ISO-8859-1?Q?Fran=E7ois?=');
is(encode_mimewords('Me and François'), '=?ISO-8859-1?Q?Me=20and=20Fran=E7ois?=');
is(decode_mimewords('=?ISO-8859-1?Q?Me=20and=20Fran=E7ois?='),
   'Me and François');

is(encode_mimewords('Me and François and François    and François       and François               and François                      and François'),
   '=?ISO-8859-1?Q?Me=20and=20Fran=E7ois=20an?= =?ISO-8859-1?Q?d=20Fran=E7ois=20=20=20=20and=20?= =?ISO-8859-1?Q?Fran=E7ois=20=20=20=20=20=20=20and?= =?ISO-8859-1?Q?=20Fran=E7ois=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20=20=20and=20Fran=E7ois?= =?ISO-8859-1?Q?=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20and=20Fran=E7ois?=');


is(decode_mimewords('=?ISO-8859-1?Q?Me=20and=20Fran=E7ois=20an?= =?ISO-8859-1?Q?d=20Fran=E7ois=20=20=20=20and=20?= =?ISO-8859-1?Q?Fran=E7ois=20=20=20=20=20=20=20and?= =?ISO-8859-1?Q?=20Fran=E7ois=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20=20=20and=20Fran=E7ois?= =?ISO-8859-1?Q?=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20and=20Fran=E7ois?='),
   'Me and François and François    and François       and François               and François                      and François');


is(encode_mimewords('Me and François and François    and François       and François               and François                      and François and wookie and wookie and wookie and wookie and wookie and wookie'),
   '=?ISO-8859-1?Q?Me=20and=20Fran=E7ois=20an?= =?ISO-8859-1?Q?d=20Fran=E7ois=20=20=20=20and=20?= =?ISO-8859-1?Q?Fran=E7ois=20=20=20=20=20=20=20and?= =?ISO-8859-1?Q?=20Fran=E7ois=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20=20=20and=20Fran=E7ois?= =?ISO-8859-1?Q?=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20and=20Fran=E7ois=20a?=nd wookie and wookie and wookie and wookie and wookie and wookie');

is(decode_mimewords('=?ISO-8859-1?Q?Me=20and=20Fran=E7ois=20an?= =?ISO-8859-1?Q?d=20Fran=E7ois=20=20=20=20and=20?= =?ISO-8859-1?Q?Fran=E7ois=20=20=20=20=20=20=20and?= =?ISO-8859-1?Q?=20Fran=E7ois=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20=20=20and=20Fran=E7ois?= =?ISO-8859-1?Q?=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20=20?= =?ISO-8859-1?Q?=20=20=20=20and=20Fran=E7ois=20a?=nd wookie and wookie and wookie and wookie and wookie and wookie'),
   'Me and François and François    and François       and François               and François                      and François and wookie and wookie and wookie and wookie and wookie and wookie');

