########################################################################
#
# filedb is free software; you can redistribute it and/or modify
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
#  Project      :  File Preprocessor - filedb database module
#  Filename     :  $RCSfile: filedb.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.5 $
#  Last changed :  $Date: 2002/11/03 22:18:37 $
#  Description  :  Implements a simple database as a filepp module
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m filedb.pm <files>
########################################################################

package Filedb;

use strict;

# version number of module
my $VERSION = '0.1.0';

Filepp::Error("The filedb module is not fully implmented yet!");

########################################################################
# main filedb data structures
########################################################################

# hash of all tables, has form:
# hash index = table name
# hash entry = (@Fields)
my %Tables = ();

# hash of all table separators, has form:
# hash index = table name
# hash entry = $separator
my %Separators = ();

# hash of all table entries, has form:
# hash index = table name
# hash entry = (%Entries)
#   %Entries is a hash of all the entries in the table and has form:
#   hash index = first column of table
#   hash entry = (@Entries) (includes first entry)
my %Entries = ();

########################################################################
# Table keyword, creates a new table in the data base,
# takes inputs (as one space separated string) :
# tablename   - name of table
# separator   - separator used when entering fields in table
#               note: the separator can not appear in any fields in the table!
# list of space separated fields - each field is the name of a column in
#                                  the table
########################################################################
sub Table
{
    my $input = shift;
    my ($name, $separator, @Fields) = split(/\s+/, $input);
    
    print("Table name: \"$name\"\n");
    print("Table separator: \"$separator\"\n");
    my $field;
    foreach $field (@Fields) {
	print("Field: \"$field\"\n");
    }

    # enter new table into tables
    $Tables{$name} = \@Fields;
    # enter separator
    $Separators{$name} = $separator;
}

########################################################################
# Add the table keyword
########################################################################
Filepp::AddKeyword("table", \&Table);


########################################################################
# Insert keyword, adds an entry to a table,
# takes inputs (as one space separated string) :
# tablename   - name of table
# separator   - separator used when entering fields in table
#               note: the separator can not appear in any fields in the table!
# list of space separated fields - each field is the name of a column in
#                                  the table
########################################################################
sub Insert
{
    my $input = shift;
    my ($table, $entry) = split(/\s+/, $input, 2);

    # check the table exists
    if(!exists($Tables{$table})) {
      Filepp::Warning("Table $table not found");
	return;
    }
    
    # split up entry and add it to table
    my $numfields = @{$Tables{$table}};
    my @Entry = split(/$Separators{$table}/, $entry, $numfields);

    # pad with blanks if needed
    while($#Entry < $numfields-1) { push(@Entry, ""); }

    # add to hash
    $ {$Entries{$table}} {$Entry[0]} = \@Entry;
}

########################################################################
# Add the insert keyword
########################################################################
Filepp::AddKeyword("insert", \&Insert);


########################################################################
# Select keyword, prints a table,
# takes inputs (as one space separated string) :
# tablename   - name of table
########################################################################
sub Select
{
    my ($items, $tables, $conditions);
    my $input = shift;
    ($items, $input) = split(/\s+FROM\s+/, $input, 2);
    ($tables, $conditions) = split(/\s+WHERE\s+/, $input, 2);
    print("items <$items>, tables <$tables>, conditions <$conditions>\n");

    my @Items = split(/\s*,\s*/, $items);
    foreach $items (@Items) {
	print("item: <$items>\n");
    }    
    my @Tables = split(/\s*,\s*/, $tables);
    foreach $tables (@Tables) {
	print("table: <$tables>\n");
    }
    my @Conditions = split(/\s*,\s*/, $conditions);
    foreach $conditions (@Conditions) {
	print("condition: <$conditions>\n");
    }    
}

########################################################################
# Add the select keyword
########################################################################
Filepp::AddKeyword("select", \&Select);

########################################################################
# List a whole table
########################################################################
sub List
{
    my $table = shift;
    my $field;
    print("Table:\t");
    foreach $field (@{$Tables{$table}}) { print("$field\t"); }
    print("\n");
    my @Fields = sort( keys( %{$Entries{$table}} ) );
    foreach $field (@Fields) {
	my @Entry = @{  ${ $Entries{$table}}{$field}  };
        my $entry;
        print("\n");
        foreach $entry (@Entry) { print("Entry: $entry\n"); }
    }
}

########################################################################
# Add the list keyword
########################################################################
Filepp::AddKeyword("list", \&List);


return 1;

########################################################################
# End of file
########################################################################
