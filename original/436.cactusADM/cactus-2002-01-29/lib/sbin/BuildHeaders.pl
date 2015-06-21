 #! /usr/bin/perl -s

#/*@@
#  @routine BuildHeaders
#  @date Sun 13 Sep 1999
#  @author Gabrielle Allen
#  @desc
#  Creates the dynamic header files requested in interface.ccl files
#  and writes them into the Bindings include directory
#  @enddesc
#  @calls
#  @calledby
#  @history
#  @endhistory
#@@*/

require "$sbin_dir/CSTUtils.pl";

sub BuildHeaders
{
  my($cctk_home,$bindings_dir,%database) = @_;
  my($start_dir,$thorn,$inc_file,$inc_file1,$inc_file2,$tmpline);

  $start_dir = `pwd`;
  chdir $bindings_dir;
  chdir "include";

# First set all data strings 
  foreach $thorn (split(" ",$interface_database{"THORNS"}))
  {
    foreach $inc_file (split(" ",$interface_database{"\U$thorn USES HEADER"}))
    {
      $data{"$inc_file"} = "/* Include header file $inc_file */\n\n";
    }
    foreach $inc_file (split(" ",$interface_database{"\U$thorn USES SOURCE"}))
    {
      $data{"$inc_file"} = "/* Include source file $inc_file */\n\n";
    }
  }

# Add the headers from thorns
  foreach $thorn (split(" ",$interface_database{"THORNS"}))
  {

    $arrangement = $interface_database{"\U$thorn ARRANGEMENT"};

    foreach $inc_file1 (split(" ",$interface_database{"\U$thorn ADD HEADER"}))
    {
      if ($inc_file1 !~ /^\s*$/)
      {
        $inc_file1 =~ s/ //g;
        $inc_file2 = $interface_database{"\U$thorn ADD HEADER $inc_file1 TO"};

        # Write information to the global include file
        $data{"$inc_file2"} .= "/* Including header file $inc_file1 from $thorn */\n";
        
        # Now have to find the include file and copy it
        if (-e "$cctk_home/arrangements/$arrangement/$thorn/src/$inc_file1")
        {
          $data{"$inc_file2"} .= "#include \"$arrangement/$thorn/src/$inc_file1\"\n\n";
        }
        elsif (-e "$cctk_home/arrangements/$arrangement/$thorn/src/include/$inc_file1")
        {
          $data{"$inc_file2"} .= "#include \"$arrangement/$thorn/src/include/$inc_file1\"\n\n";
        }
        else
        {
          $message = "Include file $inc_file1 not found in $arrangement/$thorn\n";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
        $data{"$inc_file2"} .= "/* End of include header file $inc_file1 from $thorn */\n";
      }
    }

    foreach $inc_file1 (split(" ",$interface_database{"\U$thorn ADD SOURCE"}))
    {
      if ($inc_file1 !~ /^\s*$/)
      {
        $inc_file1 =~ s/ //g;
        $inc_file2 = $interface_database{"\U$thorn ADD SOURCE $inc_file1 TO"};

        # Write information to the global include file
        $data{"$inc_file2"} .= "/* Including source file $inc_file1 from $thorn */\n";
        
        # Now have to find the include file and copy it
        if (-e "$cctk_home/arrangements/$arrangement/$thorn/src/$inc_file1")
        {
          $tmpline = "#include \"$arrangement/$thorn/src/$inc_file1\"\n";
        }
        elsif (-e "$cctk_home/arrangements/$arrangement/$thorn/src/include/$inc_file1")
        {
          $tmpline = "#include \"$arrangement/$thorn/src/include/$inc_file1\"\n}\n";
        }
        else
        {
          $message = "Include file $inc_file1 not found in $arrangement/$thorn\n";
          &CST_error(0,$message,"",__LINE__,__FILE__);
        }
         
        $data{"$inc_file2"} .= "#ifdef FCODE\n";
        $data{"$inc_file2"} .= "      if (CCTK_IsThornActive(\"$thorn\").eq.1) then\n";
        $data{"$inc_file2"} .= "#else\n"; 
        $data{"$inc_file2"} .= "if (CCTK_IsThornActive(\"$thorn\")){\n";
        $data{"$inc_file2"} .= "#endif\n";           
        $data{"$inc_file2"} .= "$tmpline\n";
        $data{"$inc_file2"} .= "#ifdef FCODE\n";
        $data{"$inc_file2"} .= "      end if\n";
        $data{"$inc_file2"} .= "#else\n"; 
        $data{"$inc_file2"} .= "\n}\n";
        $data{"$inc_file2"} .= "#endif\n";           

        $data{"$inc_file2"} .= "/* End of include source file $inc_file1 from $thorn */\n";
      }
    }

  }

  foreach $thorn (split(" ",$interface_database{"THORNS"}))
  {
    foreach $inc_file1 (split(" ",$interface_database{"\U$thorn USES HEADER"}))
    {
      &WriteFile($inc_file1,\$data{"$inc_file1"});
    }
    foreach $inc_file1 (split(" ",$interface_database{"\U$thorn USES SOURCE"}))
    {
      &WriteFile($inc_file1,\$data{"$inc_file1"});
    }
  }

  chdir $start_dir;
  return;

}

1;
