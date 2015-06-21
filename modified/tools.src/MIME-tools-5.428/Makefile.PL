use strict;
use warnings;
use inc::Module::Install;

name            'MIME-tools';
version_from    'lib/MIME/Tools.pm';
author          q{Eryq <eryq@zeegee.com>, David F. Skoll <dfs@roaringpenguin.com>, Dave O'Neill <dmo@roaringpenguin.com>};
abstract        'Tools to manipulate MIME messages';
license         'perl';

# We require 5.8.x, for PerlIO support of "in memory" filehandles opened on scalars
perl_version    '5.008';

# Runtime deps
requires        'Mail::Header'   => 1.01;
requires        'Mail::Internet' => 1.0203;
requires        'Mail::Field'    => 1.05;

# We prefer 3.03, but can work with 2.20
requires        'MIME::Base64'   => 2.20;
recommends      'MIME::Base64'   => 3.03;

requires        'IO::File'       => 1.13;
requires        'IO::Handle';
requires        'IO::Stringy'    => 2.110;
requires        'File::Spec'     => 0.6;
requires        'File::Path'     => 1;
requires        'File::Temp'     => 0.18;

# Build deps
build_requires	'Test::More';

auto_install;
WriteAll;
