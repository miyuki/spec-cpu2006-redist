########################################################################
#
# remover is free software; you can redistribute it and/or modify
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
#  Project      :  File Preprocessor - remover module
#  Filename     :  $RCSfile: remover.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.2 $
#  Last changed :  $Date: 2005/09/01 17:37:20 $
#  Description  :  This module removes all code between #ifdef and #if
#                  statements for a predifined set of macros.  Useful
#                  for permanently removing uneeded bits of code.
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: edit the list of macros below then do:
# filepp -m remover.pm <files>
########################################################################

package Remover;

use strict;

# version number of module
my $VERSION = '0.1.0';

# list of macros to be removed
# enter macro as '1' if it should be initially defined
# enter macro as '0' if it should be initially undefined
my %Macros = ();

# "if", "else", "endif" keywords - should match set you are using,
# defaults to standard filepp/cpp list
my %Ifwords = ('if',     '',
	       'ifdef',  '',
	       'ifndef', '');
my %Elsewords = ('else', '',
		 'elif', '');
my %Endifwords = ('endif', '');

# turn off keyword processing - will be enabled only when needed
Filepp::RemoveProcessor("Filepp::ParseKeywords");
# turn off all macro processing
Filepp::RemoveProcessor("Filepp::ReplaceDefines");

my $keywordchar = Filepp::GetKeywordchar();


# keywords which may or may not be removed
my %Keywords = (
		'define'  => \&Define,
		'elif'    => \&Elif,
		'else'    => \&Else,
		'endif'   => \&Endif,
		'if'      => \&If,
		'ifdef'   => \&Ifdef,
		'ifndef'  => \&Ifndef,
		'remove'  => \&Remove,
		'undef'   => \&Undef,
		);


# counter for number of #if[n][def] loops currently in
my $iflevel = 0;
# flag to control when to write output
my @Writing = (1); # initialise default to 'writing'
# flag to show if current 'if' block has passed a 'true if'
my @Ifdone = (0); # initialise first to 'not passed true if'

# flag to say if keyword with removable macros found 
my @RemoveLine = (0);
my $remove_line = 0;

# function to set macros for removal
sub Remove
{
    my $macrodefn = shift;
    my $macro;
    my $defn;
    my $i;

    # check there is an argument
    if($macrodefn !~ /\S/o) {
	Filepp::Error("remove keyword used without arguments");
    }
    
    # find end of macroword - assume separated by space or tab
    $i = Filepp::GetNextWordEnd($macrodefn);
    
    # separate macro and defn (can't use split, doesn't work with '0')
    $macro = substr($macrodefn, 0, $i);
    $defn  = substr($macrodefn, $i);
    
    # strip leading whitespace from $defn
    if($defn) {
	$defn =~ s/^[ \t]*//;
    }
    else {
	$defn = "";
    }
    $remove_line = 1;
    $Macros{$macro} = $defn;
    if($defn) { 
	Filepp::Debug("Remove: $macro added for removal defined=$defn");
	Filepp::Define($macrodefn);
    }
    else {
	Filepp::Debug("Remove: $macro added for removal undefined");	  
	Filepp::Undef($macro);
    }
}


# remove #define for all defines of unwanted macro
sub Define
{
    my $macrodefn = shift;
    $macrodefn =~ /^\s*\w+\b/;
    my $macro = $&;
    if(exists($Macros{$macro})) {
        Filepp::Debug("Remove: removing define <$macro>");
	$remove_line = 1;
    }
    else { Filepp::Define($macrodefn); }
}

# remove #undef for all undefs of unwanted macro
sub Undef
{
    my $macro = shift;
    if(exists($Macros{$macro})) {
        Filepp::Debug("Remove: removing undef <$macro>");
	$remove_line = 1;
    }
    else { Filepp::Undef($macro); }
}

# remove if statements with unwanted conditional in, ignore rest
sub If
{
    my $expr = shift;
    my $macro;
    my $found = 0;
    foreach $macro (keys(%Macros)) {
	if($expr=~ /\b$macro\b/) { $found = 1; }
    }
    if($found) {
	Filepp::Debug("Remove: removing if line <$expr>");
	$RemoveLine[$iflevel] = 1;
	$remove_line = 1;
	return Filepp::If($expr);
    }
    else { $RemoveLine[$iflevel] = 0; }    
    return 1;
}

# elif is not fully supported, prints warning out to for by hand removal
sub Elif
{
    my $expr = shift;
    my $macro;
    foreach $macro (keys(%Macros)) {
	if($expr=~ /\b$macro\b/) { 
	    Filepp::Warning("Remove: #elif removal not fully supported, remove by hand");
	}
    }
    if($RemoveLine[$iflevel]) {
	$remove_line = 1;
	return Filepp::Elif($expr);
    }
    return 1;
}

# remove ifdef statements with unwanted conditional in, ignore rest
sub Ifdef
{
    my $macro = shift;
    # separate macro from any trailing garbage
    $macro = substr($macro, 0, Filepp::GetNextWordEnd($macro));    
    if(exists($Macros{$macro})) {
	Filepp::Debug("Remove: removing Ifdef line <$macro>");
	$RemoveLine[$iflevel] = 1;
	$remove_line = 1;
	return Filepp::Ifdef($macro);
    }
    else { $RemoveLine[$iflevel] = 0; }
    return 1;
}


# remove ifndef statements with unwanted conditional in, ignore rest
sub Ifndef
{
    my $macro = shift;
    # separate macro from any trailing garbage
    $macro = substr($macro, 0, Filepp::GetNextWordEnd($macro));    
    if(exists($Macros{$macro})) {
	Filepp::Debug("Remove: removing Ifndef line <$macro>");
	$RemoveLine[$iflevel] = 1;
        $remove_line = 1;
        return Filepp::Ifndef($macro);
    }
    else { $RemoveLine[$iflevel] = 0; }
    return 1;
}

sub Else
{
    $remove_line = $RemoveLine[$iflevel];
    return 1;
}

sub Endif
{
    $remove_line = $RemoveLine[$iflevel];
    return 1;
}

##############################################################################
# Keyword parsing routine
##############################################################################
sub ParseKeywords
{
    # input is next line in file
    my $inline = shift;
    my $outline = "";

    my $thisline = $inline;	
    my $keyword;

    # remove whitespace from start of line
    $thisline = Filepp::CleanStart($thisline);
    # check if first char on line is a #
    if($thisline && $thisline =~ /^$keywordchar/) {
	# remove "#" and any following whitespace
	$thisline =~ s/^$keywordchar\s*//g;
	# check for keyword
	if($thisline && $thisline =~ /^\w+\b/ && exists($Keywords{$&})) {
	    $keyword = $&;
	    # remove newline from line
	    chomp($thisline);
	    # remove leading whitespace and keyword from line
	    my $inline = Filepp::CleanStart(substr($thisline,
						   length($keyword)));
	    
	    # check for 'if' style keyword
	    if(exists($Ifwords{$keyword})) {
		# increment ifblock level and set ifdone to same
		# value as previous block
		$iflevel++;
		$Ifdone[$iflevel] = 0;
		$Writing[$iflevel] = $Writing[$iflevel - 1];
		$RemoveLine[$iflevel] = $RemoveLine[$iflevel - 1];
		if(!$Writing[$iflevel]) { $Ifdone[$iflevel] = 1; }
	    }
	    # check for out of place 'else' or 'endif' style keyword
	    elsif($iflevel <= 0 && (exists($Elsewords{$keyword}) ||
				    exists($Endifwords{$keyword}) )) {
		Filepp::Warning($keywordchar.$keyword.
				" found without preceding ".$keywordchar.
				"[else]ifword");
	    }
	    
	    # decide if to run 'if' or 'else' keyword
	    if(exists($Ifwords{$keyword}) || exists($Elsewords{$keyword})){
		# run keyword to set RemoveLine
		my $value = $Keywords{$keyword}->($inline);
		if($RemoveLine[$iflevel] && !$Ifdone[$iflevel]) {
		    # check return value of 'if'
		    if($value) {
			$Ifdone[$iflevel] = 1;
			$Writing[$iflevel] = 1;
		    }
		    else { $Writing[$iflevel] = 0; }
		}
		elsif($RemoveLine[$iflevel] || $Ifdone[$iflevel]) {
		    $Writing[$iflevel] = 0;
		}
		else { $Writing[$iflevel] = 1; }
	    }
	    # check for 'endif' style keyword
	    elsif(exists($Endifwords{$keyword})) {
		# run endif keyword and decrement iflevel if true
		if($Keywords{$keyword}->($inline)) { $iflevel--; }
	    }
	    # run all other keywords
	    elsif($Writing[$iflevel] || $RemoveLine[$iflevel] == 0) {
		$Keywords{$keyword}->($inline);
	    }

	} # keyword if statement
    }
    # no keywords in line - write line to file if not #ifdef'ed out
    if($remove_line || !$Writing[$iflevel]) {
	# do nothing
    }
    else {
	$outline = $outline.$inline;
    }
    
    $remove_line = 0;
    return $outline;
}
Filepp::AddProcessor("Remover::ParseKeywords");


return 1;

########################################################################
# End of file
########################################################################
