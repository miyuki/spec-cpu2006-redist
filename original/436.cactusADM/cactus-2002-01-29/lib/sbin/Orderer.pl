#! /usr/bin/perl -s
#/*@@
#  @file      Orderer.pl
#  @date      Wed Feb 10 08:55:51 1999
#  @author    Tom Goodale
#  @desc 
#  Routines to order a set of things which have specified before and after.
#  @enddesc 
#@@*/

#/*@@
#  @routine    TestOrderList
#  @date       Sun Feb 21 08:22:42 1999
#  @author     Tom Goodale
#  @desc 
#  Routine to test the OrderList function
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub TestOrderList
{
  my(%data);
  my($sorted_things);

  # Create a list of data and rules to sort them.
  %data = ("thorns", "c b a d e f",
	   "A BEFORE", "f",
	   "B BEFORE", "c",
	   "B AFTER",  "a",
	   "C BEFORE", "d",
	   "D BEFORE",  "e",
	   "E BEFORE", "f",
	   "E AFTER",  "a",
	   "F AFTER",  "b",
	  );

  # Find the sorted list
  @sorted_things = &OrderList("Error", "thorns", %data);

  # Report
  print join(",", @sorted_things);
  print "\n";

}


#/*@@
#  @routine    OrderList
#  @date       Sun Feb 21 07:52:43 1999
#  @author     Tom Goodale
#  @desc 
#  Orders a set of strings given info in a database
#  There should be records with names like \U<thing> BEFORE and
#  \U<thing> AFTER specifying any ordering between the strings.
#  @enddesc 
#  @calls     
#  @calledby   
#  @var     error_string
#  @vdesc   The string to be printed before any error messages.
#  @vtype   string
#  @vcomment 
#
#  @endvar
#  @var     fieldname
#  @vdesc   The name of the field containing the names of the strings to be sorted.
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     database
#  @vdesc   The database.
#  @vtype   hash table
#  @vcomment 
#
#  @endvar 

#  @history 
#
#  @endhistory 
#@@*/

sub OrderList
{
  my($error_string, $fieldname, %database) = @_;
  my(@things);
  my($thing, $other_thing);
  my($nerrors);
  my(@thing_list); 

  $nerrors = 0;
  @things = split(" ", $database{$fieldname});

  # Make complete first level lists of before and after.
  foreach $thing (@things)
  {
    if($database{"\U$thing BEFORE"})
    {
      foreach $other_thing (split(" ", $database{"\U$thing BEFORE"}))
      {
	$database{"\U$other_thing ALLAFTER"} .= " $thing";
	$database{"\U$thing ALLBEFORE"}      .= " $other_thing";
      }
    }
    if($database{"\U$thing AFTER"})
    {
      foreach $other_thing (split(" ", $database{"\U$thing AFTER"}))
      {
	$database{"\U$other_thing ALLBEFORE"} .= " $thing";
	$database{"\U$thing ALLAFTER"}        .= " $other_thing";
      }
    }
  }

  # Now go through the list and find the complete before and after lists.
  foreach $thing (@things)
  {
    %complete = &RecurseThings($thing, "ALLBEFORE", 0, %database);

    $database{"\U$thing ALLBEFORE"} = join(" ", keys %complete);

    %complete = &RecurseThings($thing, "ALLAFTER", 0, %database);

    $database{"\U$thing ALLAFTER"} = join(" ", keys %complete);

  }

  # Check that something doesn't appear on its own lists !
  foreach $thing (@things)
  {
    $nerrors += &CheckThings($error_string, $thing, "ALLBEFORE", %database);

    $nerrors += &CheckThings($error_string, $thing, "ALLAFTER", %database);
  }

  # Stop if there have been any errors.
#  if($nerrors)
#  {
#    print "$error_string: $nerrors errors detected\n";
#    exit;
#  }

#  foreach $field ( sort keys %database )
#  {
#    print "$field has value $database{$field}\n";
#  }

  # Finally, sort the strings.
  @thing_list = &SortThings("ALLBEFORE", "ALLAFTER", scalar(@things), @things, %database);

#  foreach $thing (@thing_list)
#  {
#    print "thing is $thing\n";
#  }

  return @thing_list;
}


#/*@@
#  @routine    RecurseThings
#  @date       Sun Feb 21 08:01:55 1999
#  @author     Tom Goodale
#  @desc 
#  Recurses through a database, constructing the full list of before
#  and after properties for a particular string.
#  @enddesc 
#  @calls     
#  @calledby 
#  @var     thing
#  @vdesc   The string to construct data for
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     keyword
#  @vdesc   The keyword in the database used to find the relationship
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     nthings
#  @vdesc   The number of things which have been found so far.
#  @vtype   integer
#  @vcomment 
#
#  @endvar 
#  @var     indata
#  @vdesc   The rest of the arguments
#  @vtype   array
#  @vcomment 
#  This consists of two hash tables - one containing the things found so
#  far, and one containing the database with the relations between the 
#  strings.
#  @endvar 

#  @history 
#
#  @endhistory 
#
#@@*/
sub RecurseThings
{
  my($thing, $keyword, $nthings, @indata) = @_;
  my(%things);
  my(%database);

  # Extract the hash tables
  if($nthings > 0)
  {
    %things = @indata[0..2*$nthings-1];
    %database = @indata[2*$nthings..$#indata];
  }
  else
  {
    %things = ();
    %database = @indata;
  }

  # Recurse
  if($database{"\U$thing $keyword"})
  {
    foreach $other_thing (split(" ", $database{"\U$thing $keyword"}))
    {
      if(! $things{"\U$other_thing\E"})
      {
	$things{"\U$other_thing\E"} = 1;
	%things = &RecurseThings($other_thing, $keyword, scalar(keys %things), %things,%database);
      }
    }
  }

  return %things;

}


#/*@@
#  @routine    CheckThings
#  @date       Sun Feb 21 08:08:28 1999
#  @author     Tom Goodale
#  @desc 
#  Checks that something doesn't appear on its own ordering list.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#  @var     error_string
#  @vdesc   The string to be printed before any error messages.
#  @vtype   string
#  @vcomment 
#
#  @endvar
#  @var     thing
#  @vdesc   The string to check the data for
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     keyword
#  @vdesc   The keyword in the database to be checked
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     database
#  @vdesc   The database of relation data
#  @vtype   hash table
#  @vcomment 
#
#  @endvar 
#
#@@*/
sub CheckThings
{
  my($error_string, $thing, $keyword, %database) = @_;
  my($other_thing);
  my($nerrors);

  if($database{"\U$thing $keyword"})
  {
    foreach $other_thing (split(" ", $database{"\U$thing $keyword"}))
    {
      
      if( $thing =~ m:^$other_thing$:i)
      {
	$message = "$error_string:  $thing appears in its own $keyword list";
	&CST_error(0,$message,__LINE__,__FILE__);
	$nerrors++;
      }
    }
  }

  return $nerrors;
}

#/*@@
#  @routine    SortThings
#  @date       Sun Feb 21 08:11:49 1999
#  @author     Tom Goodale
#  @desc 
#  Sorts a set of strings given data in a database
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#  @var     before
#  @vdesc   The keyword for finding strings before which a particular string should appear.
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     before
#  @vdesc   The keyword for finding strings after which a particular string should appear.
#  @vtype   string
#  @vcomment 
#
#  @endvar 
#  @var     n_things
#  @vdesc   The number of things in the list to be sorted.
#  @vtype   integer
#  @vcomment 
#
#  @endvar 
#  @var     rest
#  @vdesc   The rest of the arguments.
#  @vtype   hash
#  @vcomment 
#  This consists of an array containing the things to be sorted and
#  a hash table containing the relation data.
#  @endvar 
#
#@@*/
sub SortThings
{
  my($before, $after, $n_things, @rest) = @_;
  my(@things);
  my(%database);
  my(@sorted_things);

  # Extract the list of things and the database
  if($n_things)
  {
    @things   = @rest[0..$n_things-1];
    %database = @rest[$n_things..$#rest];
  }
  else
  {
    return;
  }

  # Remove anything which doesn't need sorting
  $sortcount=0;
  $returncount=0;
  for ($i=0;$i<scalar(@things);$i++)
  {
      if ($database{"\U$things[$i] AFTER"} || $database{"\U$things[$i] ALLAFTER"} || 
	  $database{"\U$things[$i] BEFORE"} || $database{"\U$things[$i] ALLBEFORE"})
      {
	  $sortthings[$sortcount] = $things[$i];
	  $sortcount++;
      }
      else
      {
	  $returnthings[$returncount] = $things[$i];
	  $returncount++
      }
  }


  # Sort the things
  @sorted_things = sort ThingSorter @sortthings;
  return (@returnthings,@sorted_things);

}


#/*@@
#  @routine    ThingSorter
#  @date       Sun Feb 21 08:17:08 1999
#  @author     Tom Goodale
#  @desc 
#  A customised sort routine to sort strings based upon details
#  stored in a database.
#
#  The database should be called %database, and should contain
#  entries of the form 
#     \U<string> $before   and
#     \U<string> $after
#  which are lists of strings before or after which <string> should
#  appear.
#  $before, $after and %database need to be provided, and be in scope.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ThingSorter
{
  my($retval);
  if($database{"\U$a $before"} =~ m:\b$b\b:i)
  {
#    print "$b in $a $before list - " . $database{"\U$a $before"} . "\n";
    $retval = -1;
  }
  elsif($database{"\U$b $after"} =~ m:\b$a\b:i)
  {
#    print "$a in $b $after list - " . $database{"\U$b $after"} . "\n";
    $retval = 1;
  }
  elsif($database{"\U$a $after"} =~ m:\b$b\b:i)
  {
#    print "$b in $a $after list - " . $database{"\U$a $after"} . "\n";
    $retval = 1;
  }
  elsif($database{"\U$b $before"} =~ m:\b$a\b:i)
  {
#    print "$a in $b $before list - `" . $database{"\U$b $before"} . "'\n";
    $retval = -1;
  }
  else
  {
    $retval = 0;
  }

#  print "Sorting $a and $b, return val is $retval\n";
#  print "cmp would give " . ($a cmp $b) . "\n";

  return $retval;
}

1;

