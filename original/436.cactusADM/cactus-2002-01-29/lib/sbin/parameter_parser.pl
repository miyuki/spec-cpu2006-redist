#! /usr/bin/perl

#%implementations = ("flesh", "flesh", "test1", "test1", "test2", "test2");
 
#%parameter_database = create_parameter_database(%implementations);

#&print_parameter_database(%parameter_database);

#/*@@
#  @routine    create_parameter_database
#  @date       Wed Sep 16 11:45:18 1998
#  @author     Tom Goodale
#  @desc 
#  Creates a database of all the parameters
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub create_parameter_database
{
  my(%thorns) = @_;
  my($thorn, @indata);
  my(@new_parameter_data);
  my(@parameter_data);
  
  #  Loop through each implementation's parameter file.
  foreach $thorn (keys %thorns)
  {
    print "   $thorn\n";
    #       Read the data
    @indata = &read_file("$thorns{$thorn}/param.ccl");
    
    #       Get the parameters from it
    @new_parameter_data = &parse_param_ccl($thorn, @indata);

    &PrintParameterStatistics($thorn, @new_parameter_data);

    #       Add the parameters to the master parameter database
    push (@parameter_data, @new_parameter_data);
    
  }
  
  @parameter_data = &cross_index_parameters(scalar(keys %thorns), (keys %thorns), @parameter_data);
  
  return @parameter_data;
}

sub cross_index_parameters
{
  my($n_thorns, @indata) = @_;
  my(@thorns);
  my(%parameter_database);
  my(@module_file);
  my($line);
  my(@data);
  my($thorn);
  
  @thorns = @indata[0..$n_thorns-1];
  %parameter_database = @indata[$n_thorns..$#indata];
  
  $parameter_database{"GLOBAL PARAMETERS"} = "";
  
  foreach $thorn (@thorns)
  {
    foreach $parameter (split(/ /, $parameter_database{"\U$thorn\E GLOBAL variables"}))
    {
      if($public_parameters{"\U$parameter\E"})
      {
          $message = "Duplicate public parameter $parameter, defined in $imp and ".$public_parameters{"\Uparameter\E"};
          &CST_error(0,$message,"",__LINE__,__FILE__);
      }
      else
      {
        $public_parameters{"\Uparameter\E"} = "$thorn";
        
        $parameter_database{"GLOBAL PARAMETERS"} .= "$thorn\::$parameter ";
      }
    }
  }
  
  return %parameter_database;
}



#/*@@
#  @routine    parse_param_ccl
#  @date       Wed Sep 16 11:55:33 1998
#  @author     Tom Goodale
#  @desc 
#  Parses a param.ccl file and generates a database of the values.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/

sub parse_param_ccl
{
  my($thorn, @data) = @_;
  my($line_number, $line, $block, $type, $variable, $description);
  my($current_friend, $new_ranges, $new_desc);
  my($data, %parameter_db);
  my(%friends);
  my(%defined_parameters);
  my($use_or_extend, $use_clause, $skip_range_block);
  my($message);
  
  #   The default block is private.
  $block = "PRIVATE";
  
  # Initialise, to prevent perl -w from complaining.
  $parameter_db{"\U$thorn PRIVATE\E variables"} = "";

  for($line_number = 0; $line_number < @data; $line_number++)
  {
    $line = $data[$line_number];
    
    #       Parse the line
    if($line =~ m/(GLOBAL|RESTRICTED|PRIVATE|SHARES)\s*:(.*)/i)
    {
      #           It's a new block.
      $block = "\U$1\E";
      
      if($block eq "SHARES")
      {
        $current_friend = $2;
        $current_friend =~ s:\s::;
        
        #               It's a friend block.
        $block .= " \U$current_friend\E";
        #               Remember this friend, but make the memory unique.
        $friends{"\U$current_friend\E"} = 1;
      }
      
      # Do some initialisation to prevent perl -w from complaining.
      if(!$parameter_db{"\U$thorn $block\E variables"})
      {
        $parameter_db{"\U$thorn $block\E variables"} = "";
      }
    }
    elsif($line =~ m:(EXTENDS |USES )?\s*(?\:CCTK_)?(INT|REAL|BOOLEAN|KEYWORD|STRING)\s+([a-zA-Z]+[a-zA-Z0-9_]*)\s+(\"[^\"]*\")?\s*(.*)$:i)
    {
      # This is a parameter definition.

      $use_or_extend = $1;
      $type = "\U$2\E";

      $variable = $3;
      $description = $4;
      $options = $5;

      if($use_or_extend =~ m:USES:i)
      {
        $use_clause = 1;
      }
      else
      {
        $use_clause = 0;
      }

      if($description !~ m:\":)
      {
        if($use_or_extend)
        {
          $description = "";
        }
        else
        {
          $message = "Missing description for $variable in thorn $thorn.";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
      }

      if($defined_parameters{"\U$variable\E"})
      {

        $message = "Duplicate parameter $variable in thorn $thorn. Ignoring second definition";
        &CST_error(1,$message,"",__LINE__,__FILE__);

        $line_number++ until ($data[$line_number] =~ m:\}:);
      }
      elsif($use_or_extend && $use_or_extend =~ m:(EXTENDS|USES):i && $block !~ m:SHARES\s*\S:)
      {
        # Can only extend a friend variable.
        $message =  "Parse error in $thorn/param.ccl";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        $line_number++ until ($data[$line_number] =~ m:\}:);
      }
      elsif($data[$line_number+1] !~ m:^\s*\{\s*$: && $use_clause == 0)
      {
        # Since the data should have no blank lines, the next
        # line should have { on it.
        $message = "Parse error in $thorn/param.ccl - missing \"{\" in definition of parameter \"$variable\"";
        &CST_error(0,$message,"",__LINE__,__FILE__);
        # Move past the end of this block.
        $line_number++ until ($data[$line_number] =~ m:\}:);
      }
      else
      {
        $skip_range_block = 0;
        # Move past {
        if($data[$line_number+1] !~ m:\s*\{\s*:)
        {
          if ($use_clause) 
          {
            $skip_range_block = 1;
          }
          else
          {
#           This message is already given above.
#           message = "Missing { at start of range block for parameter $variable pf thorn $thorn";
#           &CST_error(0,$message,"",__LINE__,__FILE__);
            die "Internal error in parser: this line should never be reached."
          }
        }
        else
        {
          $skip_range_block = 0;
          $line_number++;
          $line_number++;
        }

        # Parse the options
        %options = split(/\s*=\s*|\s+/, $options);
      
        foreach $option (keys %options)
        {
          if($option =~ m:STEERABLE:i)
          {
            $parameter_db{"\U$thorn $variable\E steerable"} = $options{$option};
          }
          else
          {
            $message = "Unknown option $option for parameter $variable of thorn $thorn";
            &CST_error(0,$message,"",__LINE__,__FILE__);
          }
        }

        # Store data about this variable.
        $defined_parameters{"\U$variable\E"} = 1;
        
        $parameter_db{"\U$thorn $block\E variables"} .= $variable." ";
        $parameter_db{"\U$thorn $variable\E type"} = $type;
        $parameter_db{"\U$thorn $variable\E description"} = $description;
        $parameter_db{"\U$thorn $variable\E ranges"} = 0;
        
        if(! $skip_range_block)
        {
          # Parse the allowed values and their descriptions.
          # The (optional) description is seperated by ::
          while($data[$line_number] !~ m:^\s*\}:)
          {
            if($data[$line_number] =~ m/::/)
            {
              ($new_ranges, $delim, $new_desc) = $data[$line_number] =~ m/(.*)(::)(.*)/;
            }
            else
            {
              ($new_ranges, $delim, $new_desc) = ($data[$line_number],"","");
            }       
            # Increment the number of ranges found (ranges)
            $parameter_db{"\U$thorn $variable\E ranges"}++;
            # Strip out any spaces in the range for a numeric parameter.
            if($type =~ m:INT|REAL:)
            {
              $new_ranges =~ s/\s//g;
            }
            
            $parameter_db{"\U$thorn $variable\E range $parameter_db{\"\U$thorn $variable\E ranges\"} range"} = $new_ranges;
            
            # Check description
            if($delim eq "" || ($delim =~ /::/ && $new_desc =~ /^\s*$/))
            {
              $message = "Missing description of range '$new_ranges' for parameter $thorn\::$variable";
              &CST_error(1,$message,"",__LINE__,__FILE__);
            }
            elsif ($new_desc =~ /^\s*\".*[^\s\"]\s*$|^\s*[^\s\"].*\"\s*$/)
            {
              $message = "Description of range for $thorn\::$variable has misplaced quotes ($new_desc)";
              &CST_error(0,$message,"",__LINE__,__FILE__);
            }
            $parameter_db{"\U$thorn $variable\E range $parameter_db{\"\U$thorn $variable\E ranges\"} description"} = $new_desc;
            $line_number++;
          }
        }

        # Give a warning if no range was given and it was needed
        if (($use_clause == 0)  && ($parameter_db{"\U$thorn $variable\E ranges"}==0 && $type =~ m:INT|REAL:))
        {
            $message = "No range given for $variable in $thorn";
            &CST_error(0,$message,"",__LINE__,__FILE__);
        }
        if($block !~ m:SHARES:)
        {
          if($data[$line_number] =~ m:\s*\}\s*([^\s].*):)
          {
              $default = $1;
              if ($type =~ m:INT|REAL: && $default =~ m:":)
              {
                  $message = "String default given for $type $variable in $thorn";
                  &CST_error(0,$message,"",__LINE__,__FILE__);
              }
              elsif ($type =~ m:STRING|KEYWORD: && $default !~ m:".*":)
              {
                  $message = "Default given for $type $variable in $thorn is not a string";
                  &CST_error(0,$message,"",__LINE__,__FILE__);
              }
              elsif ($type =~ m:BOOLEAN: && $default =~ m:": && $default !~ m:".*":)
              {
                  $message = "Default given for $type $variable in $thorn is missing a quote";
                  &CST_error(0,$message,"",__LINE__,__FILE__);
              }
              
              $default = $1 if ($default =~ m:\"(((\\\")|[^\"])*)\":);
           
              &CheckParameterDefault($thorn,$variable,$default,%parameter_db);

              $parameter_db{"\U$thorn $variable\E default"} = $default;
          }
          else
          {
            $message =  "Unable to find default value for parameter $variable";
	    $hint = "Last line of parameter block should be } <default value>";
            &CST_error(0,$message,$hint,__LINE__,__FILE__);
          }             
        }
      }
    }
    else
    {
      if($line =~ m:\{:)
      {
        $message = "Skipping parameter block in $thorn with missing keyword";
        &CST_error(1,$message,"",__LINE__,__FILE__);
        $line_number++ until ($data[$line_number] =~ m:\}:);
      }
      else
      {
        $message = "Unknown line \"$line\" in $thorn/param.ccl";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
    }
  }
  
  $parameter_db{"\U$thorn\E SHARES implementations"} = join(" ", keys %friends);
  
  return %parameter_db;
}

#/*@@
#  @routine    print_parameter_database
#  @date       Wed Sep 16 14:58:52 1998
#  @author     Tom Goodale
#  @desc 
#  Prints out a parameter database.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/
sub print_parameter_database
{
  my(%parameter_database) = @_;
  my($field);
  
  foreach $field ( sort keys %parameter_database )
  {
    print "$field has value $parameter_database{$field}\n";
  }
}


#/*@@
#  @routine    PrintParameterStatistics
#  @date       Sun Sep 19 13:04:18 1999
#  @author     Tom Goodale
#  @desc 
#  Prints out some statistics about a thorn's param.ccl
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintParameterStatistics
{
  my($thorn, %parameter_database) = @_;
  my($block);
  my($sep);

  $sep = "          ";
  foreach $block ("Global", "Restricted", "Private")
  {
    print $sep . scalar(split(" ", $parameter_database{"\U$thorn $block\E variables"})) . " $block";
    $sep = ", ";
  }

  print " parameters\n";

  return;
}


#/*@@
#  @routine    CheckParameterDefault
#  @date       Sun Dec 17 18.20
#  @author     Gabrielle Allen
#  @desc 
#  Check default in allowed range
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/

sub CheckParameterDefault
{
  my($thorn,$variable,$default,%parameter_db) = @_;
  my($foundit,$message,$i,$range,$minok,$maxok);

  # Check that boolean default is correct
  if ($parameter_db{"\U$thorn $variable\E type"} =~ /BOOLEAN/)
  {
    if ($default !~ m:^yes|no|y|n|1|0|t|f|true|false$:i)
      {
        $message = "Default ($default) for boolean incorrect for $variable in $thorn";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
  }

  # Check that keyword default is correct
  if ($parameter_db{"\U$thorn $variable\E type"} =~ /KEYWORD/)
  {
    $foundit = 0;
    $nranges=$parameter_db{"\U$thorn $variable\E ranges"};
    for ($i=1; $i<=$nranges; $i++)
    {
      $range = $parameter_db{"\U$thorn $variable\E range $i range"};
      $range =~ s/^\s*//;
      $range =~ s/\s*$//;
      $range =~ s/^"(.*)"$/$1/;
      # Key words don't use pattern matching
      $range = quotemeta $range;
      if ($default =~ m:$range:i)
      {
        $foundit = 1;
      }
    }
    if ($foundit == 0)
    {
      $message = "Default ($default) for keyword incorrect for $variable in $thorn";
      &CST_error(0,$message,"",__LINE__,__FILE__);
    }
  }
  
  # Check that string default is correct 
  if ($parameter_db{"\U$thorn $variable\E type"} =~ /STRING/)
  {
    $foundit = 0;
    $nranges=$parameter_db{"\U$thorn $variable\E ranges"};
    for ($i=1; $i<=$nranges; $i++)
    {
      $range = $parameter_db{"\U$thorn $variable\E range $i range"};
      $range =~ s/^\s*//;
      $range =~ s/\s*$//;
      $range =~ s/^"(.*)"$/$1/;

      if ($default =~ m:$range:i)
      {
        $foundit = 1;
      }
    }
    if ($foundit == 0)
    {
      $message = "Default ($default) for string incorrect for $variable in $thorn";
      &CST_error(0,$message,"",__LINE__,__FILE__);
    }
  }
  
  # Check that integer default is correct 
  if ($parameter_db{"\U$thorn $variable\E type"} =~ /INT/)
  {
    $nranges=$parameter_db{"\U$thorn $variable\E ranges"};
    for ($i=1; $i<=$nranges; $i++)
    {
      $minok=0;
      $maxok=0;
      $range = $parameter_db{"\U$thorn $variable\E range $i range"};
      $range =~ s/^\s*//;
      $range =~ s/\s*$//;
      $range =~ s/^"(.*)"$/$1/;
      $range =~ /^([\s\*0-9]*):([\s\*0-9]*)/;
      $min = $1;
      $max = $2;
      if ($min =~ /^\s*[\*\s]*\s*$/)
      {
        $minok=1;
      }
      elsif ($default >= $min)
      {
        $minok=1;
      }
      if ($max =~ /^\s*[\*\s]*\s*$/)
      {
        $maxok=1;
      }
      elsif ($default <= $max)
      {
        $maxok=1;
      }
      if ($minok == 1 && $maxok == 1)
      {
        $foundit = 1;
      }
    }
    if ($foundit == 0)
    {
      $message = "Default ($default) for integer incorrect for $variable in $thorn";
      &CST_error(0,$message,"",__LINE__,__FILE__);
    }
  }
  
  # Check that real default is correct 
  if ($parameter_db{"\U$thorn $variable\E type"} =~ /REAL/)
  {
    $nranges=$parameter_db{"\U$thorn $variable\E ranges"};
    for ($i=1; $i<=$nranges; $i++)
    {
      $minok=0;
      $maxok=0;
      $range = $parameter_db{"\U$thorn $variable\E range $i range"};
      $range =~ s/^\s*//;
      $range =~ s/\s*$//;
      $range =~ s/^"(.*)"$/$1/;
      $range =~ /^([\s\*0-9\.]*):([\s\*0-9\.]*)/;
      $min = $1;
      $max = $2;
      if ($min =~ /^\s*[\*\s]*\s*$/)
      {
        $minok=1;
      }
      elsif ($default >= $min)
      {
        $minok=1;
      }
      if ($max =~ /^\s*[\*\s]*\s*$/)
      {
        $maxok=1;
      }
      elsif ($default <= $max)
      {
        $maxok=1;
      }
      if ($minok == 1 && $maxok == 1)
      {
        $foundit = 1;
      }
    }
    if ($foundit == 0)
    {
      $message = "Default ($default) for real incorrect for $variable in $thorn";
      &CST_error(0,$message,"",__LINE__,__FILE__);
    }
  }
  
  return;
}

1;


