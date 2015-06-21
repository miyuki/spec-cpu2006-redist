#
# os.pm
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: os.pl 6710 2011-08-05 21:53:46Z CloyceS $

use strict;

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'os.pl'} = $version;

# It's really _all_ common
require 'os_common.pl';

1;
