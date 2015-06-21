#! /usr/bin/perl -w

#/*@@
#  @file      interface_parser.pl
#  @date      Wed Sep 16 15:07:11 1998
#  @author    Tom Goodale
#  @desc 
#  Parses interface.ccl files
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/sbin/interface_parser.pl,v 1.50 2001/11/02 16:21:06 goodale Exp $
#@@*/

#/*@@
#  @routine    create_interface_database
#  @date       Wed Sep 16 15:07:11 1998
#  @author     Tom Goodale
#  @desc 
#  Creates a database of all the interfaces
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub create_interface_database
{
  my($n_system,@inargs) = @_;
  my(%thorns);
  my(%system_database);
  my($thorn, @indata);
  my(@new_interface_data);
  my(@interface_data);
  
  %system_database = @inargs[0..2*$n_system-1];
  %thorns = @inargs[2*$n_system..$#inargs];
  
  #  Loop through each  thorn's interface file.
  foreach $thorn (keys %thorns)
  {
    print "   $thorn\n";
    #       Get the arrangement name for the thorn
    $thorns{$thorn} =~ m:.*/arrangements/([^/]*)/[^/]*:;
    $arrangement = $1;

    #       Read the data
    @indata = &read_file("$thorns{$thorn}/interface.ccl");

    #       Get the interface data from it
    @new_interface_data = &parse_interface_ccl($arrangement,$thorn, @indata);

    &PrintInterfaceStatistics($thorn, @new_interface_data);
    
    #       Add the interface to the master interface database
    push (@interface_data, @new_interface_data);
    
  }

  @interface_data = &cross_index_interface_data(scalar(keys %thorns), scalar(keys %system_database), (keys %thorns), %system_database, @interface_data);

  return @interface_data;
}



sub cross_index_interface_data
{
  my($n_thorns, $n_system, @indata) = @_;
  my(@thorns);
  my(%interface_data);
  my(%implementations);
  my(%system_database);
  my($implementation);
  my(%ancestors);
  my(%friends);

  @thorns = @indata[0..$n_thorns-1];
  %system_database = @indata[$n_thorns..$n_thorns+2*$n_system-1];
  %interface_data = @indata[$n_thorns+2*$n_system..$#indata];

  foreach $thorn (@thorns)
  {
    $implementation = $interface_data{"\U$thorn\E IMPLEMENTS"};
    if($implementation =~ m:^\s*$:)
    {
      $message = "Thorn $thorn doesn't specify an implementation";
      $hint = "All compiled thorns must specify an implementation in their interface.ccl file with the format IMPLEMENTS: <implementation>";
      &CST_error(0,$message,$hint,__LINE__,__FILE__);
      next;
    }

    # Put if statement around this to prevent perl -w from complaining.
    if($interface_data{"IMPLEMENTATION \U$implementation\E THORNS"})
    {
      $interface_data{"IMPLEMENTATION \U$implementation\E THORNS"} .= "$thorn ";
    }
    else
    {
      $interface_data{"IMPLEMENTATION \U$implementation\E THORNS"} = "$thorn ";
    }

    $implementations{"\U$implementation\E"} = "$implementation";
  }

  $interface_data{"THORNS"} = join(" ", @thorns);

  foreach $implementation (keys %implementations)
  {

    # Put if statement around this to prevent perl -w from complaining.
    if($interface_data{"IMPLEMENTATIONS"})
    {
      $interface_data{"IMPLEMENTATIONS"} .= $implementations{"\U$implementation\E"} . " ";
    }
    else
    {
      $interface_data{"IMPLEMENTATIONS"} = $implementations{"\U$implementation\E"} . " ";
    }

    &check_implementation_consistency($implementation, %interface_data);

    %ancestors = &get_implementation_ancestors($implementation, 0,  scalar(keys %system_database), %system_database, %interface_data);

    $interface_data{"IMPLEMENTATION \U$implementation\E ANCESTORS"} = join(" ",( keys %ancestors));

    $interface_data{"IMPLEMENTATION \U$implementation\E FRIENDS"} = &get_friends_of_me($implementation, scalar(keys %implementations), (keys %implementations),%interface_data);

  }

  foreach $thorn (@thorns)
  {
    &check_interface_consistency($thorn, %interface_data);
  }

  foreach $implementation (keys %implementations)
  {
    %friends = &get_implementation_friends($implementation, 0, %interface_data);
    $interface_data{"IMPLEMENTATION \U$implementation\E FRIENDS"} = join(" ",( keys %friends));
  }

  return %interface_data;
}


sub get_friends_of_me
{
  my($implementation, $n_implementations,@indata) = @_;
  my(@implementations);
  my(%interface_data);
  my($other_implementation);
  my($thorn);
  my($friend,$friends);

  @implementations = @indata[0..$n_implementations-1];
  %interface_data = @indata[$n_implementations..$#indata];

  # Initialise to stop perl -w from complaining.
  $friends = "";

  foreach $other_implementation (@implementations)
  {

    $interface_data{"IMPLEMENTATION \U$other_implementation\E THORNS"} =~ m:(\w+):;

    $thorn = $1;

    foreach $friend (split(" ", $interface_data{"\U$thorn\E FRIEND"}))
    {
      if($friend =~ m:$implementation:i)
      {
          $friends .= "$other_implementation "; 
      }
    }
  }

  return $friends;
}
  

sub get_implementation_friends
{
  my($implementation, $n_friends, @indata) = @_;
  my(%friends);
  my(%interface_data);
  my($thorn);
  my($friend, $friends);
  my($friends_of_me);
  my($other_implementation);

  if($n_friends > 0)
  {
    %friends = @indata[0..2*$n_friends-1];
    %interface_data = @indata[2*$n_friends..$#indata];
  }
  else
  {
    %friends = ();
    %interface_data = @indata;
  }

  $interface_data{"IMPLEMENTATION \U$implementation\E THORNS"} =~ m:(\w+):;

  $thorn = $1;

  # Recurse
  foreach $friend (split(" ", $interface_data{"\U$thorn\E FRIEND"}), 
                   split(" ", $interface_data{"IMPLEMENTATION \U$implementation\E FRIENDS"}))
  {
    if(! $friends{"\U$friend\E"})
    {
      $friends{"\U$friend\E"} = 1;
      if(! $interface_data{"IMPLEMENTATION \U$friend\E THORNS"})
      {
        $message = "$implementation is friends with $friend - non-existent implementation";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        next;
      }
      %friends = &get_implementation_friends($friend, scalar(keys %friends), %friends,%interface_data);
    }
  }
  
  return %friends;

}

sub get_implementation_ancestors
{
  my($implementation, $n_ancestors, $n_system, @indata) = @_;
  my(%ancestors);
  my(%interface_data);
  my($thorn);
  my($ancestor, $ancestors);

  if($n_ancestors > 0)
  {
    %ancestors = @indata[0..2*$n_ancestors-1];
    %system_database = @indata[2*$n_ancestors..2*($n_ancestors+$n_system)-1];
    %interface_data = @indata[2*($n_ancestors+$n_system)..$#indata];
  }
  else
  {
    %ancestors = ();
    %system_database = @indata[0..2*$n_system-1];
    %interface_data = @indata[2*$n_system..$#indata];
  }

  $interface_data{"IMPLEMENTATION \U$implementation\E THORNS"} =~ m:(\w+):;

  $thorn = $1;

  # Recurse.
  foreach $ancestor (split(" ", $interface_data{"\U$thorn\E INHERITS"}))
  {
    if(! $ancestors{"\U$ancestor\E"})
    {
      $ancestors{"\U$ancestor\E"} = 1;
      if(! $interface_data{"IMPLEMENTATION \U$ancestor\E THORNS"})
      {
        # Implementation not found give extensive information
        %info = &buildthorns("$cctk_home/arrangements","thorns");
        $suggest_thorns = "";
        foreach $thorninfo (keys %info)
        {
         $info{"$thorninfo"} =~ /^([^\s]+)/;
         $testimp = $1;
         if ($testimp =~ m:^$ancestor$:i)
         {
           $suggest_thorns .= "\n        $thorninfo";
         }
        }
        $message = "$implementation (thorn $thorn) inherits from $ancestor\n";
        $message .= "     No thorn in your current ThornList implements $ancestor\n";
        $message .= "     Either remove $thorn, or add a thorn to your\n";
        $message .= "      ThornList implementing $ancestor\n";
        if ($suggest_thorns !~ m:^$:)
        {
          $message .= "     Available thorns in arrangements directory implementing $ancestor:";
          $message .= "$suggest_thorns";
        }
        else
        {
          $message .= "     No thorns in arrangements directory implement $ancestor";
        }
        &CST_error(0,$message,"",__LINE__,__FILE__);

        next;
      }

      %ancestors = &get_implementation_ancestors($ancestor, scalar(keys %ancestors), scalar(keys %system_database), %ancestors,%system_database, %interface_data);
    }
  }
  
  return %ancestors;
}

sub check_implementation_consistency
{
  my($implementation, %interface_data) = @_;
  my(@thorns);
  my($thorn);
  my($thing);
  my(%inherits);
  my(%friend);
  my(%public_groups);
  my(%private_groups);
  my(%variables);
  my($n_errors);
  my($group);
  my(%attributes);
 
  # Find out which thorns provide this implementation.
  @thorns = split(" ", $interface_data{"IMPLEMENTATION \U$implementation\E THORNS"});

  if(scalar(@thorns) > 1)
  {
    foreach $thorn (@thorns)
    {
      # Record the inheritance
      foreach $thing (split(" ", $interface_data{"\U$thorn\E INHERITS"}))
      {
        if($thing =~ m:\w:)
        {
          # Put if statement around this to prevent perl -w from complaining.
          if($inherits{"\U$thing\E"})
          {
            $inherits{"\U$thing\E"} .= "$thorn ";
          }
          else
          {
            $inherits{"\U$thing\E"} = "$thorn ";
          }
        }
      }

      # Record the friends
      foreach $thing (split(" ", $interface_data{"\U$thorn\E FRIEND"}))
      {
        if($thing =~ m:\w:)
        {
          # Put if statement around this to prevent perl -w from complaining.
          if($friend{"\U$thing\E"})
          {
            $friend{"\U$thing\E"} .= "$thorn ";
          }
          else
          {
            $friend{"\U$thing\E"} = "$thorn ";
          }         
        }
      }
  
      # Record the public groups
      foreach $thing (split(" ", $interface_data{"\U$thorn\E PUBLIC GROUPS"}))
      {
        if($thing =~ m:\w:)
        {
          # Put if statement around this to prevent perl -w from complaining.
          if($public_groups{"\U$thing\E"})
          {
            $public_groups{"\U$thing\E"} .= "$thorn ";
          }
          else
          {
            $public_groups{"\U$thing\E"} = "$thorn ";
          }
        }
      }

      # Record the protected groups
      foreach $thing (split(" ", $interface_data{"\U$thorn\E PROTECTED GROUPS"}))
      {
        if($thing =~ m:\w:)
        {
          # Put if statement around this to prevent perl -w from complaining.
          if($protected_groups{"\U$thing\E"})
          {
            $protected_groups{"\U$thing\E"} .= "$thorn ";
          }
          else
          {
            $protected_groups{"\U$thing\E"} = "$thorn ";
          }
        }
      }
    }

    $n_thorns = @thorns;

    # Check the consistency of the inheritance
    foreach $thing (keys %inherits)
    {
      if(split(" ", $inherits{$thing}) != $n_thorns)
      {
        $message  = "Inconsistent implementations of $implementation. ";
        $message .= "Implemented by thorns " . join(" ", @thorns);
        $message .= "Not all inherit: $thing";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        $n_errors++;
      }
    }

    # Check the consistency of the friendships
    foreach $thing (keys %friend)
    {
      if(split(" ", $friend{$thing}) != $n_thorns)
      {
        $message  = "Inconsistent implementations of $implementation\n";
        $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
        $message .= "Not all are friends of: $thing";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        $n_errors++;
      }
    }

    # Check the consistency of the public groups
    foreach $thing (keys %public_groups)
    {
      if(split(" ", $public_groups{$thing}) != $n_thorns)
      {
          $message  = "Inconsistent implementations of $implementation\n";
          $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
          $message .= "Not all declare public group: $thing";
          &CST_error(0,$message,"",__LINE__,__FILE__);
          $n_errors++;
      }
    }

    # Check the consistency of the protected groups
    foreach $thing (keys %protected_groups)
    {
      if(split(" ", $protected_groups{$thing}) != $n_thorns)
      {
        $message  = "Inconsistent implementations of $implementation\n";
        $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
        $message .= "Not all declare protected group: $thing";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        $n_errors++;
      }
    }

    # Check consistancy of group definitions
    foreach $group ((keys %public_groups), (keys %protected_groups))
    {
      %variables = ();
      %attributes = ();

      foreach $thorn (@thorns)
      {
        # Remember which variables are defined in this group.
        foreach $thing (split(" ",$interface_data{"\U$thorn GROUP $group\E"}))
        {
          # Put if statement around this to prevent perl -w from complaining.
          if($variables{"\U$thing\E"})
          {
            $variables{"\U$thing\E"} .= "$thorn ";
          }
          else
          {
            $variables{"\U$thing\E"} = "$thorn ";
          }
        }

        # Check variable type definition.
        if($attributes{"VTYPE"})
        {
          if($attributes{"VTYPE"} ne $interface_data{"\U$thorn GROUP $group\E VTYPE"})
          {
            $message  = "Inconsistent implementations of $implementation";
            $message .= " in thorns " . join(" ", @thorns) . ". ";
            $message .= "Group $group has inconsistent variable type ($attributes{\"VTYPE\"} and $interface_data{\"\\U$thorn GROUP $group\\E VTYPE\"}). ";
            $hint = "All public and protected groups implementing $implementation must have groups with consistent properties";
            &CST_error(0,$message,$hint,__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"VTYPE"} = $interface_data{"\U$thorn GROUP $group\E VTYPE"};
        }

        # Check group type definition.
        if($attributes{"GTYPE"})
        {
          if($attributes{"GTYPE"} ne $interface_data{"\U$thorn GROUP $group\E GTYPE"})
          {
            $message  = "Inconsistent implementations of $implementation";
            $message .= " in thorns " . join(" ", @thorns) . ". ";
            $message .= "Group $group has inconsistent group type ($attributes{\"GTYPE\"} and $interface_data{\"\U$thorn GROUP $group\E GTYPE\"}). ";
            $hint = "All public and protected groups implementing $implementation must have groups with consistent properties";
            &CST_error(0,$message,$hint,__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"GTYPE"} = $interface_data{"\U$thorn GROUP $group\E GTYPE"};
        }

        # Check the number of time levels is consistent.
        if($attributes{"TIMELEVELS"})
        {
          if($attributes{"TIMELEVELS"} ne $interface_data{"\U$thorn GROUP $group\E TIMELEVELS"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "Group $group has inconsistent time levels";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"TIMELEVELS"} = $interface_data{"\U$thorn GROUP $group\E TIMELEVELS"};
        }

        # Check the size array sizes are consistent.
        if($attributes{"SIZE"})
        {
          if($attributes{"SIZE"} ne $interface_data{"\U$thorn GROUP $group\E SIZE"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "Group $group has inconsistent size";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"SIZE"} = $interface_data{"\U$thorn GROUP $group\E SIZE"};
        }

        # Check the ghostsize array sizes are consistent.
        if($attributes{"GHOSTSIZE"})
        {
          if($attributes{"GHOSTSIZE"} ne $interface_data{"\U$thorn GROUP $group\E GHOSTSIZE"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "Group $group has inconsistent ghostsize";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"GHOSTSIZE"} = $interface_data{"\U$thorn GROUP $group\E GHOSTSIZE"};
        }

        # Check the distribution of arrays are consistent.
        if($attributes{"DISTRIB"})
        {
          if($attributes{"DISTRIB"} ne $interface_data{"\U$thorn GROUP $group\E DISTRIB"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "      Group $group has inconsistent distribution";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }
        else
        {
          $attributes{"GHOSTSIZE"} = $interface_data{"\U$thorn GROUP $group\E GHOSTSIZE"};
        }

        # Check the dimensions are consistant
        if($attributes{"DIM"} && $attributes{"GTYPE"} ne "SCALAR")
        {
          if($attributes{"DIM"} ne $interface_data{"\U$thorn GROUP $group\E DIM"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "Group $group has inconsistent dimension";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }         
        else
        {
          $attributes{"DIM"} = $interface_data{"\U$thorn GROUP $group\E DIM"};
        }

        # Check the staggering are consistant
        if($attributes{"STYPE"})
        {
          if($attributes{"STYPE"} ne $interface_data{"\U$thorn GROUP $group\E STYPE"})
          {
            $message  = "Inconsistent implementations of $implementation\n";
            $message .= "Implemented by thorns " . join(" ", @thorns) . "\n";
            $message .= "Group $group has inconsistent staggering type";
            &CST_error(0,$message,"",__LINE__,__FILE__);
            $n_errors++;
          }
        }         
        else
        {
          $attributes{"STYPE"} = $interface_data{"\U$thorn GROUP $group\E STYPE"};
        }
      }
    }
  }
  else
  {
    # No need to do a consistency check if only one thorn 
    # provides this implementation.

  }

}

  
#/*@@
#  @routine    check_interface_consistency
#  @date       Sun Jun 3 2001
#  @author     Gabrielle Allen
#  @desc 
#  Check consistency of the interfaces files
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub check_interface_consistency
{
  my($thorn, %interface_data) = @_;
  my($implementation);
  my($private_group);
  my($ancestor_imp,$ancestor_thorn);
  my($message);

  # Find implementation 
  $implementation =  $interface_data{"\U$thorn\E IMPLEMENTS"};

  # Loop over ancestors 
  foreach $ancestor_imp (split " ",$interface_data{"IMPLEMENTATION \U$implementation\E ANCESTORS"})
  {
    # Need one thorn which implements this ancestor (we already have checked consistency)
    $ancestor_thorn = $interface_data{"IMPLEMENTATION \U$ancestor_imp\E THORNS"};
    if ($ancestor_thorn =~ m:(\w+)[^\w]*:)
    {
      $ancestor_thorn = $1;
    }
    foreach $private_group (split " ",$interface_data{"\U$thorn\E PRIVATE GROUPS"})
    {
      if ($interface_data{"\U$ancestor_thorn\E PUBLIC GROUPS"} =~ $private_group)
      {
        $message = "Private group $private_group in thorn $thorn has same name as \n     public group in ancestor implementation $ancestor_imp (e.g. thorn $ancestor_thorn)";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
    }    
  }
}

  

#/*@@
#  @routine    parse_interface_ccl
#  @date       Wed Sep 16 15:07:11 1998
#  @author     Tom Goodale
#  @desc 
#  Parses an interface.ccl file and generates a database of the values.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub parse_interface_ccl
{
  my($arrangement, $thorn, @data) = @_;
  my($line_number, $line, $block, $type, $variable, $description);
  my($data, %interface_db);
  my($implementation);
  my($option,%options);
  my(%known_groups);
  my(%known_variables);
      

  # Initialise some stuff to prevent perl -w from complaining.

  $interface_db{"\U$thorn INHERITS\E"} = "";
  $interface_db{"\U$thorn FRIEND\E"} = "";
  $interface_db{"\U$thorn PUBLIC GROUPS\E"} = "";
  $interface_db{"\U$thorn PROTECTED GROUPS\E"} = "";
  $interface_db{"\U$thorn PRIVATE GROUPS\E"} = "";
  $interface_db{"\U$thorn USES HEADER\E"} = "";
  $interface_db{"\U$thorn FUNCTIONS\E"} = "";
  $interface_db{"\U$thorn PROVIDES FUNCTION\E"} = ""; 
  $interface_db{"\U$thorn USES FUNCTION\E"} = "";
  $interface_db{"\U$thorn ARRANGEMENT\E"} = "$arrangement";
  
  #   The default block is private.
  $block = "PRIVATE";
  
  for($line_number = 0; $line_number < @data; $line_number++)
  {
    $line = $data[$line_number];
    
    #       Parse the line
    if($line =~ m/^\s*(PUBLIC|PROTECTED|PRIVATE)\s*$/i)
    {
      #           It's a new block.
      $block = "\U$1\E";
    } 
    elsif ($line =~ m/^\s*IMPLEMENTS\s*:/i)
    {
      if ($line =~ m/^\s*IMPLEMENTS\s*:\s*([a-z]+[a-z_0-9]*)\s*$/i)
      {
        if(!$implementation)
        {
          $implementation = $1;
          $interface_db{"\U$thorn\E IMPLEMENTS"} = $implementation;
        }
        else
        {
          $message = "Multiple implementations specified in $thorn";
          $hint = "A thorn can only specify one implementation in it's interface.ccl file, with the format implements:<implementation>";
          &CST_error(0,$message,$hint,__LINE__,__FILE__);
        }
      }
      else
      {
        $message = "Implementation line has wrong format in $thorn";
        $hint = "A thorn must specify one implementation in it's interface.ccl file with the format IMPLEMENTS: <implementation>";
        &CST_error(0,$message,$hint,__LINE__,__FILE__);
      }
    }
    # implementation names can be sepeated by ,\s, where , are stripped out below
    elsif ($line =~ m/^\s*(INHERITS|FRIEND)\s*:(([,\s]*[a-zA-Z]+[a-zA-Z_0-9]*)*[,\s]*)$/i)
    {
      $interface_db{"\U$thorn $1\E"} .= $2;
      $interface_db{"\U$thorn $1\E"}=~s/,/ /g;
    }
    elsif ($line =~ m/^\s*(PUBLIC|PROTECTED|PRIVATE)\s*:\s*$/i)
    {
      $block = "\U$1\E";
    }
    elsif ($line =~ m/^\s*PROVIDES\s*FUNCTION\s*([a-zA-Z_0-9]+)\s*WITH\s*(.+)\s*$/i)
    {
      $funcname = $1;
      $provided_by = $2;

      if($provided_by =~ m/(.*)\s*LANGUAGE\s*(.+)/i)
      {
        $provided_by          = $1;
        $provided_by_language = $2;
      }
      else
      {
        $provided_by_language = "Fortran";
      }

      $interface_db{"\U$thorn PROVIDES FUNCTION\E"} .= "$funcname ";
      $interface_db{"\U$thorn PROVIDES FUNCTION\E $funcname WITH"} .= "$provided_by ";
      $interface_db{"\U$thorn PROVIDES FUNCTION\E $funcname LANG"} .= "$provided_by_language ";

    }
    elsif ($line =~ m/^\s*USES\s*FUNCTION\s*([a-zA-Z_0-9]+)\s*$/i)
    {
      $funcname = $1;
      $interface_db{"\U$thorn USES FUNCTION\E"} .= "$funcname ";
    }
    elsif ($line =~ m/^\s*([a-zA-Z_0-9]+)\s*FUNCTION\s*([a-zA-Z_0-9]+)\s*(.*)\s*$/i)
    {
      $rettype  = $1;
      $funcname = $2;
      $rest     = $3;
      if($rest =~ m/(.*)\s*PROVIDED-BY\s*(.+)/i)
      {
        $funcargs = $1;
        $provided_by = $2;

        if($provided_by =~ m/(.*)\s*LANGUAGE\s*(.+)/i)
        {
          $provided_by          = $1;
          $provided_by_language = $2;
        }
        else
        {
          $provided_by_language = "Fortran";
        }
      }
      else
      {
        $funcargs = $rest;
        $provided_by = "";
      }

      $interface_db{"\U$thorn FUNCTIONS\E"} .= "$funcname ";
      $interface_db{"\U$thorn FUNCTION\E $funcname ARGS"} .= "$funcargs";
      $interface_db{"\U$thorn FUNCTION\E $funcname RET"} .= "$rettype";
      
      if($provided_by ne "")
      {
        $interface_db{"\U$thorn PROVIDES FUNCTION\E"} .= "$funcname";
        $interface_db{"\U$thorn PROVIDES FUNCTION\E $funcname WITH"} .= "$provided_by";
        $interface_db{"\U$thorn PROVIDES FUNCTION\E $funcname LANG"} .= "$provided_by_language";
      }
    }
    elsif ($line =~ m/^\s*(CCTK_)?(CHAR|BYTE|INT|INT2|INT4|INT8|REAL|REAL4|REAL8|REAL16|COMPLEX|COMPLEX8|COMPLEX16|COMPLEX32)\s*(([a-zA-Z]+[a-zA-Z_0-9]*)(\[([^]]+)\])?)\s*(.*)\s*$/i)
    {

#      for($i = 1; $i < 10; $i++)
#      {
#        print "$i is ${$i}\n";
#      }

      my $vtype = $2;
      my $current_group = "$4";
      my $isgrouparray = $5;
      my $grouparray_size = $6;
      my $options_list = $7;

      if($known_groups{"\U$current_group\E"})
      {
        $message = "Duplicate group $current_group in thorn $thorn";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        if($data[$line_number+1] =~ m:\{:)
        {
            $message = "Skipping interface block";
            &CST_error(1,$message,"",__LINE__,__FILE__);
            $line_number++ until ($data[$line_number] =~ m:\}:);
        }
        next;
      }
      else
      {
        $known_groups{"\U$current_group\E"} = 1;

        # Initialise some stuff to prevent perl -w from complaining.
        $interface_db{"\U$thorn GROUP $current_group\E"} = "";
      }
      
      $interface_db{"\U$thorn $block GROUPS\E"} .= " $current_group";
      $interface_db{"\U$thorn GROUP $current_group\E VTYPE"} = "\U$vtype\E";
      
      %options = split(/\s*=\s*|\s+/, $options_list);
      
      # Parse the options
      foreach $option (keys %options)
      {
        if($option =~ m:DIM|DIMENSION:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E DIM"} = $options{$option};
        }
        elsif($option =~ m:STAGGER:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E STYPE"} = "\U$options{$option}\E";
        }
        elsif($option =~ m:TYPE:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E GTYPE"} = "\U$options{$option}\E";
        }
        elsif($option =~ m:TIMELEVELS:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E TIMELEVELS"} = "\U$options{$option}\E";
        }
        elsif($option =~ m:GHOSTSIZE:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E GHOSTSIZE"} = "\U$options{$option}\E";
        }
        elsif($option =~ m:DISTRIB:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E DISTRIB"} = "\U$options{$option}\E";
        }
        elsif($option =~ m:SIZE:i)
        {
          $interface_db{"\U$thorn GROUP $current_group\E SIZE"} = "\U$options{$option}\E";
        }
        else
        {
          $message = "Unknown option $option in group $current_group of thorn $thorn";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
      }

      # Put in defaults      
      if(! $interface_db{"\U$thorn GROUP $current_group\E GTYPE"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E GTYPE"} = "SCALAR";
      }

      if($interface_db{"\U$thorn GROUP $current_group\E GTYPE"} eq "SCALAR")
      {
        $interface_db{"\U$thorn GROUP $current_group\E DIM"} = 1;
      }

      if(! $interface_db{"\U$thorn GROUP $current_group\E DIM"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E DIM"} = 3;
      }

      if(! $interface_db{"\U$thorn GROUP $current_group\E TIMELEVELS"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E TIMELEVELS"} = 1;
      }

      if(! $interface_db{"\U$thorn GROUP $current_group\E STYPE"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E STYPE"} = "NONE";
      }
      
      if(! $interface_db{"\U$thorn GROUP $current_group\E DISTRIB"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E DISTRIB"} = "DEFAULT";
      }

      if(! $interface_db{"\U$thorn GROUP $current_group\E COMPACT"})
      {
        $interface_db{"\U$thorn GROUP $current_group\E COMPACT"} = 0;
      }

      # Check that it is a known group type
      if($interface_db{"\U$thorn GROUP $current_group\E GTYPE"} !~ m:^\s*(SCALAR|GF|ARRAY)\s*$:)
      {
          $message =  "Unknown GROUP TYPE " .
          $interface_db{"\U$thorn GROUP $current_group\E GTYPE"} .
            " for group $current_group of thorn $thorn";
          $hint = "Allowed group types are SCALAR, GF or ARRAY";
          &CST_error(0,$message,$hint,__LINE__,__FILE__);
          if($data[$line_number+1] =~ m:\{:)
          {
              $message = "Skipping interface block in $thorn";
              &CST_error(1,$message,"",__LINE__,__FILE__);
              $line_number++ until ($data[$line_number] =~ m:\}:);
          }
        next;
      }       

      # Check that it is a known distribution type
      if($interface_db{"\U$thorn GROUP $current_group\E DISTRIB"} !~ m:DEFAULT|CONSTANT:)
      {
          $message =  "Unknown DISTRIB TYPE " .
          $interface_db{"\U$thorn GROUP $current_group\E DISTRIB"} .
            " for group $current_group of thorn $thorn";
          $hint = "Allowed distribution types are DEFAULT or CONSTANT";
          &CST_error(0,$message,"",__LINE__,__FILE__);
          if($data[$line_number+1] =~ m:\{:)
          {
              $message = "Skipping interface block in $thorn";
              &CST_error(1,$message,"",__LINE__,__FILE__);
              $line_number++ until ($data[$line_number] =~ m:\}:);
          }
        next;
      }       

      # Is it is a vararray ?

      if($isgrouparray)
      {
        # Create a variable with the same name as the group
        $function = $current_group;

        if(! $known_variables{"\U$function\E"})
        {
          $known_variables{"\U$function\E"} = 1;
          
          $interface_db{"\U$thorn GROUP $current_group\E"} .= " $function";
        }
        else
        {
          $message = "Duplicate variable $function in thorn $thorn";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }

        # get its size

        $interface_db{"\U$thorn GROUP $current_group\E VARARRAY_SIZE"} = $grouparray_size;

        if($data[$line_number+1] =~ m/^\s*\{\s*$/)
        {
          &CST_error(1,"Can't give explicit list of array names with an array group - ignoring list","",__LINE__,__FILE__);
          $line_number++ until ($data[$line_number] =~ m:\}:);
        }
      }
      else
      {
        # Fill in data for the scalars/arrays/functions
        $line_number++;
        if($data[$line_number] =~ m/^\s*\{\s*$/)
        {
          $line_number++;
          while($data[$line_number] !~ m:\}:i)
          {
            @functions = split(/[^a-zA-Z_0-9]+/, $data[$line_number]);
            foreach $function (@functions)
            {
              $function =~ s:\s*::g;
              
              if($function =~ m:[^\s]+:)
              {
                if(! $known_variables{"\U$function\E"})
                {
                  $known_variables{"\U$function\E"} = 1;
                  
                  $interface_db{"\U$thorn GROUP $current_group\E"} .= " $function";
                }           
                else
                {
                  $message = "Duplicate variable $function in thorn $thorn";
                  &CST_error(0,$message,"",__LINE__,__FILE__);
                }
              }
            }
            $line_number++;
          }
        }
        else
        {
          # If no block, create a variable with the same name as group.
          $function = $current_group;
          if(! $known_variables{"\U$function\E"})
          {
            $known_variables{"\U$function\E"} = 1;
          
            $interface_db{"\U$thorn GROUP $current_group\E"} .= " $function";
          }
          else
          {
            $message = "Duplicate variable $function in thorn $thorn";
            &CST_error(0,$message,"",__LINE__,__FILE__);
          }
        
          # Decrement the line number, since the line is the first line of the next CCL statement.
          $line_number--;
        }
      }
    }
    elsif ($line =~ m/^\s*(USES\s*INCLUDE)S?\s*(SOURCE)S?\s*:\s*(.*)\s*$/i)
    {
      $interface_db{"\U$thorn USES SOURCE\E"} .= " $3";      
    }
    elsif ($line =~ m/^\s*(USES\s*INCLUDE)S?\s*(HEADER)?S?\s*:\s*(.*)\s*$/i)
    {
      $interface_db{"\U$thorn USES HEADER\E"} .= " $3";      
    }
    elsif ($line =~ m/^\s*(INCLUDE)S?\s*(SOURCE)S?\s*:\s*(.*)\s+IN\s+(.*)\s*$/i)
    {
      $header = $3;
      $header =~ s/ //g;
      $interface_db{"\U$thorn ADD SOURCE\E"} .= " $header";      
#      print "Adding $header to $4\n";
      $interface_db{"\U$thorn ADD SOURCE $header TO\E"} = $4;      
    }
    elsif ($line =~ m/^\s*(INCLUDE)S?\s*(HEADER)?S?\s*:\s*(.*)\s+IN\s+(.*)\s*$/i)
    {
      $header = $3;
      $header =~ s/ //g;
      $interface_db{"\U$thorn ADD HEADER\E"} .= " $header";      
#      print "Adding $header to $4\n";
      $interface_db{"\U$thorn ADD HEADER $header TO\E"} = $4;      
    }
    else
    {
      if($line =~ m:\{:)
      {
        $message = "...Skipping interface block with missing keyword....";
        &CST_error(0,$message,"",__LINE__,__FILE__);

        $line_number++ until ($data[$line_number] =~ m:\}:);
      }
      else
      {
        $message = "Unknown line in thorn $arrangement/$thorn\n$line";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
    }
  }
  
  return %interface_db;
}


sub print_interface_database
{
  my(%database) = @_;
  my($field);
  
  foreach $field ( sort keys %database )
  {
    print "$field has value $database{$field}\n";
  }
}

#/*@@
#  @routine    PrintInterfaceStatistics
#  @date       Sun Sep 19 13:03:23 1999
#  @author     Tom Goodale
#  @desc 
#  Prints out some statistics about a thorn's interface.ccl  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintInterfaceStatistics
{
  my($thorn, %interface_database) = @_;
  my($block);
  my($sep);

  print "           Implements: " . $interface_database{"\U$thorn IMPLEMENTS"} . "\n";

  if($interface_database{"\U$thorn INHERITS"} ne "")
  {
    print "           Inherits:  " . $interface_database{"\U$thorn INHERITS"} . "";
  }

  if($interface_database{"\U$thorn FRIEND"} ne "")
  {
    print "           Friend of: " . $interface_database{"\U$thorn FRIEND"} . "";
  }

  $sep = "           ";
  foreach $block ("Public", "Protected", "Private")
  {
    print $sep . scalar(split(" ", $interface_database{"\U$thorn $block\E GROUPS"})) . " $block";
    $sep = ", ";
  }

  print " variable groups\n";

  return;
}

1;
