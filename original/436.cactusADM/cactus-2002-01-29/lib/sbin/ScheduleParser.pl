#! /usr/bin/perl 
#/*@@
#  @file      ScheduleParser.pl
#  @date      Thu Sep 16 19:13:05 1999
#  @author    Tom Goodale
#  @desc 
#  New schedule parser
#  @enddesc 
#  @version $Header: /cactus/Cactus/lib/sbin/ScheduleParser.pl,v 1.18 2001/12/29 00:39:40 tradke Exp $
#@@*/

#/*@@
#  @routine    create_schedule_database
#  @date       Thu Sep 16 23:31:00 1999
#  @author     Tom Goodale
#  @desc 
#  Parses the schedule files for all thorns.  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub create_schedule_database
{
  my(%thorns) = @_;
  my($thorn, @indata);
  my(@new_schedule_data);
  my(@schedule_data);
  
  #  Loop through each implementation's schedule file.
  foreach $thorn (keys %thorns)
  {
    print "   $thorn\n";
    #       Read the data
    @indata = &read_file("$thorns{$thorn}/schedule.ccl");
    
    #       Get the schedule stuff from it
    @new_schedule_data = &parse_schedule_ccl($thorn, @indata);

    &PrintScheduleStatistics($thorn, @new_schedule_data);
    
    #       Add the schedule stuff to the master schedule database
    push (@schedule_data, @new_schedule_data);
    
  }
  
#  @schedule_data = &cross_index_schedule_data(scalar(keys %thorns), (keys %thorns), @schedule_data);
  
  return @schedule_data;
}

#/*@@
#  @routine    parse_schedule_ccl
#  @date       Thu Sep 16 23:23:07 1999
#  @author     Tom Goodale
#  @desc 
#  Parses a schedule ccl file  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub parse_schedule_ccl
{
  my($thorn, @data) = @_;
  my($line_number);
  my(%schedule_db);
  my($buffer);
  my($n_blocks);
  my($n_statements);
  my($name, $as, $type, $description, $where, $language, 
       $mem_groups, $comm_groups, $trigger_groups, $sync_groups,
       $options, $before_list, $after_list, $while_list);
  my($groups);

  $buffer       = "";
  $n_blocks     = 0;
  $n_statements = 0;

  for($line_number = 0; $line_number < scalar(@data); $line_number++)
  {
    if($data[$line_number] =~ m:^\s*schedule\s*:i)
    {
      ($line_number, 
       $name, $as, $type, $description, $where, $language, 
       $mem_groups, $comm_groups, $trigger_groups, $sync_groups,
       $options,$before_list, $after_list, $while_list) = &ParseScheduleBlock($thorn,$line_number, @data);

      $schedule_db{"\U$thorn\E BLOCK_$n_blocks NAME"}        = $name;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks AS"}          = $as;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks TYPE"}        = $type;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks DESCRIPTION"} = $description;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks WHERE"}       = $where;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks LANG"}        = $language;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks STOR"}        = $mem_groups;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks COMM"}        = $comm_groups;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks TRIG"}        = $trigger_groups;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks SYNC"}        = $sync_groups;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks OPTIONS"}     = $options;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks BEFORE"}      = $before_list;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks AFTER"}       = $after_list;
      $schedule_db{"\U$thorn\E BLOCK_$n_blocks WHILE"}       = $while_list;
 
      $buffer .= "\@BLOCK\@$n_blocks\n";
      $n_blocks++;
    }
    elsif($data[$line_number] =~ m/^\s*(STOR|COMM)[^:]*:\s*/i)
    {
      ($line_number, $type, $groups) = &ParseScheduleStatement($line_number, @data);
      $schedule_db{"\U$thorn\E STATEMENT_$n_statements TYPE"}        = $type;
      $schedule_db{"\U$thorn\E STATEMENT_$n_statements GROUPS"}      = $groups;
      $buffer .= "\@STATEMENT\@$n_statements\n";
      $n_statements++;      
    }
    else
    {
      $buffer .= "$data[$line_number]\n";
    }
  }

  $schedule_db{"\U$thorn\E FILE"}         = $buffer;
  $schedule_db{"\U$thorn\E N_BLOCKS"}     = $n_blocks;
  $schedule_db{"\U$thorn\E N_STATEMENTS"} = $n_statements;

  return %schedule_db;
}

#/*@@
#  @routine    ParseScheduleBlock
#  @date       Thu Sep 16 23:34:55 1999
#  @author     Tom Goodale
#  @desc 
#  Parses a schedule block and extracts all the info.  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseScheduleBlock
{
  my($thorn,$line_number, @data) = @_;
  my($name, $as, $type, $description, $where, $language, 
     $mem_groups, $comm_groups, $trigger_groups, $sync_groups,
     $options, $before_list, $after_list, $while_list);
  my(@fields);
  my($field);
  my(@before_list)    = ();
  my(@after_list)     = ();
  my(@while_list)     = ();
  my(@mem_groups)     = ();
  my(@comm_groups)    = ();
  my(@trigger_groups) = ();
  my(@sync_groups)    = ();
  my(@options)        = ();
  my($keyword) = "";
  my(@current_sched_list) = ();

  $where = "";
  $as    = "";

  #Parse the first line of the schedule block

  $data[$line_number] =~ m:^\s*(.*)\s*$:;

  @fields = split(/([\s,\(\)]+)/, $1);

  # Find the type of the block,
  if($fields[2] =~ m:^group$:i)
  {
    $type = "GROUP";
    $field = 4;
  }
  elsif($fields[1] =~ m:^function$:i)
  {
    $type = "FUNCTION";
    $field = 4;
  }
  else
  {
    $type = "FUNCTION";
    $field = 2;
  }    

  $name = $fields[$field];
  $field ++;

  while($field <= $#fields)
  {
    if($fields[$field] =~ m:^\s*$:)
    {
      $field++;
      next;
    }

    if($fields[$field] =~ m:^AT$:i)
    {
      $field+=2;
      if($where ne "")
      {
        print STDERR "Error parsing schedule block line '$data[$line_number]'\n";
        print STDERR "Attempt to schedule same block at/in two places.\n";
      }
      else
      {
        if($fields[$field] =~ m:CCTK_:i)
        {
          $where = "\U$fields[$field]\E";
        }
        else
        {
          $where = "CCTK_\U$fields[$field]\E";
        }
      }
      $field+=2;
    }
    elsif($fields[$field] =~ m:^IN$:i)
    {
      $field+=2;
      if($where ne "")
      {
        print STDERR "Error parsing schedule block line '$data[$line_number]'\n";
        print STDERR "Attempt to schedule same block at/in two places.\n";
      }
      else
      {
        $where = "$fields[$field]";
      }
      $field+=2;
    }
    elsif($fields[$field] =~ m:^AS$:i)
    {
      $field+=2;
      if($as ne "")
      {
        print STDERR "Error parsing schedule block line '$data[$line_number]'\n";
        print STDERR "Attempt to schedule same block with two names.\n";
      }
      else
      {
        $as = "$fields[$field]";
      }
      $field+=2;
    }
    elsif($fields[$field] =~ m:^BEFORE$:i)
    {
      if($keyword ne "")
      {
        $message = "Error parsing schedule block line '$data[$line_number]'\n";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
      $keyword = "BEFORE";
      $field++;
    }
    elsif($fields[$field] =~ m:^AFTER$:i)
    {
      if($keyword ne "")
      {
        $message="Error parsing schedule block line '$data[$line_number]'\n";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
      $keyword = "AFTER";
      $field++;
    }
    elsif($fields[$field] =~ m:^WHILE$:i)
    {
      if($keyword ne "")
      {
        $message="Error parsing schedule block line '$data[$line_number]'\n";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
      $keyword = "WHILE";
      $field++;
    }
    elsif($keyword ne "" && $fields[$field] =~ m:\s*\(\s*:)
    {
      # Parse a clause of the form BEFORE(a,b,c)
      @current_sched_list = ();

      $field++;

      while($fields[$field] !~ m:\s*\)\s*: && $field <= $#fields)
      {
        if($fields[$field] =~ m:\s*,\s*:)
        {
          $field++;
          next;
        }

        push(@current_sched_list, $fields[$field]);
        $field++;
      }
      
      $field++;
      
      if($keyword eq "BEFORE")
      {
        push(@before_list, @current_sched_list);
      }
      elsif($keyword eq "AFTER")
      {
        push(@after_list, @current_sched_list);
      }
      elsif($keyword eq "WHILE")
      {
        push(@while_list, @current_sched_list);
      }
      
      # Reset keyword to empty for next time.
      $keyword = "";
    }
    elsif($keyword ne "" && $fields[$field] =~ m:\w:)
    {
      if($keyword eq "BEFORE")
      {
        push(@before_list, $fields[$field]);
      }
      elsif($keyword eq "AFTER")
      {
        push(@after_list, $fields[$field]);
      }
      elsif($keyword eq "WHILE")
      {
        push(@while_list, $fields[$field]);
      }
      $field++;
      $keyword = "";
    }
    elsif(($keyword eq "") && ($field == $#fields) && ($fields[$field] =~ m:\s*\{\s*:))
    {
      # This bit matches a { at the end of a line
      # I don't like it, but it seems to be already in use 8-(
      $line_number--;
      $keyword = "";
      last;
    }
    else
    {
      $message="Error parsing schedule block line '$data[$line_number]'\n";
      &CST_error(0,$message,"",__LINE__,__FILE__);
      $keyword = "";
      $field++;
    }      
  }
  $line_number++;

  # If no alias is set, just use the name.
  if($as eq "")
  {
    $as = $name;
  }

  # Parse the rest of the block

  if($data[$line_number] !~ m:\s*\{\s*:)
  {
    $message="Error parsing schedule block line '$data[$line_number]'\nMissing { at start of block\n";
    &CST_error(0,$message,"",__LINE__,__FILE__);
    $line_number++ while($data[$line_number] !~ m:\s*\}\s*:);
  }
  else
  {
    while($data[$line_number] !~ m:\s*\}\s*:)
    {
      $line_number++;
      if($data[$line_number] =~ m/^\s*STOR[^:]*:\s*(.*)$/i)
      {
        push(@mem_groups, split(/\s,/, $1));
      }
      elsif($data[$line_number] =~ m/^\s*COMM[^:]*:\s*(.*)$/i)
      {
        push(@comm_groups, split(/\s,/, $1));
      }
      elsif($data[$line_number] =~ m/^\s*TRIG[^:]*:\s*(.*)$/i)
      {
        push(@trigger_groups, split(/\s,/, $1));
      }
      elsif($data[$line_number] =~ m/^\s*SYNC[^:]*:\s*(.*)$/i)
      {
        push(@sync_groups, split(/\s,/, $1));
      }
      elsif($data[$line_number] =~ m/^\s*OPTI[^:]*:\s*(.*)$/i)
      {
        push(@options, split(/\s,/, $1));
      }
      elsif($data[$line_number] =~ m/^\s*LANG[^:]*:\s*(.*)$/i)
      {
        if($language ne "")
        {
          $thisline = $data[$line_number];
          $thisline =~ s/^\s*([^\s])\s$/$1/;
          $message  = "Error parsing schedule block in $thorn\n";
          $message .= "Attempt to specify language more than once\n";
          $message .= "Line: $thisline";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
        else
        {
          $language= $1; 
        }
      }
      elsif($data[$line_number] =~ m:\s*\}\s*:)
      {
        # do nothing.
      }
      else
      {
        $message = "Error parsing schedule block line '$data[$line_number]'\nUnrecognised statement";
        &CST_error(0,$message,"",__LINE__,__FILE__);
      }
    }
  }
  if($data[$line_number] =~ m:\s*\}\s*\"([^\"]*)\":)
  {
    $description = $1;
  }
  else
  {
    $message = "Error: Missing description at end of schedule block\n";
    &CST_error(0,$message,"",__LINE__,__FILE__);
  }

  # Turn the arrays into strings.
  $mem_groups     = join(",", @mem_groups);
  $comm_groups    = join(",", @comm_groups);
  $trigger_groups = join(",", @trigger_groups);
  $sync_groups    = join(",", @sync_groups);
  $options        = join(",", @options);  
  $before_list    = join(",", @before_list);
  $after_list     = join(",", @after_list);
  $while_list     = join(",", @while_list);


  return ($line_number, 
          $name, $as, $type, $description, $where, $language, 
          $mem_groups, $comm_groups, $trigger_groups, $sync_groups,
          $options,$before_list, $after_list, $while_list);

}

#/*@@
#  @routine    ParseScheduleStatement
#  @date       Thu Sep 16 23:36:04 1999
#  @author     Tom Goodale
#  @desc 
#  Extracts info from a simple schedule statement.  
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub ParseScheduleStatement
{
  my($line_number, @data) = @_;
  my($type, $groups);

  $data[$line_number] =~ m/^\s*(STOR|COMM)[^:]*:\s*([\w\s\,]*)/i;

  $type = "\U$1\E";

  $groups = $2;
  
  return ($line_number, $type, $groups);
}

#/*@@
#  @routine    print_schedule_database
#  @date       Thu Sep 16 19:13:05 1999
#  @author     Tom Goodale
#  @desc 
#  Prints out a schedule database.
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#@@*/
sub print_schedule_database
{
  my(%schedule_database) = @_;
  my($field);
  
  foreach $field ( sort keys %schedule_database )
  {
    print "$field has value $schedule_database{$field}\n";
  }
}

#/*@@
#  @routine    PrintScheduleStatistics
#  @date       Sun Sep 19 13:07:08 1999
#  @author     Tom Goodale
#  @desc 
#  Prints out statistics about a thorn's schedule.ccl
#  @enddesc 
#  @calls     
#  @calledby   
#  @history 
#
#  @endhistory 
#
#@@*/
sub PrintScheduleStatistics
{
  my($thorn, %schedule_database) = @_;

  print "          " . $schedule_database{"\U$thorn\E N_BLOCKS"} . " schedule blocks.\n";

  return;
}

1;
