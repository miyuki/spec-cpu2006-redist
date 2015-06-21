########################################################################
#
# this file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################
#
#  Project      :  File Preprocessor - toupper module
#  Filename     :  $RCSfile: test.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.3 $
#  Last changed :  $Date: 2001/08/02 21:48:27 $
#  Description  :  This module converts all letters to upper case.
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m toupper.pm <files>
########################################################################
package Test;

use strict;

require "function.pm";

sub ToUpper
{
    my $string = shift;
    return uc($string);
}

sub ToLower
{
    my $string = shift;
    return lc($string);
}
Function::AddFunction("tolower", "Test::ToLower");

sub MultiInputs
{
    my $arg;
    my $output = "Multiple inputs: ";
    foreach $arg (@_) {
	$output = $output."<$arg> ";
    }
    return $output;
}

return 1;
########################################################################
# End of file
########################################################################

