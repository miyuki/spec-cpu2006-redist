#!/usr/bin/perl -s

#########################
# ->> ParamLatex.pl <<- #
#########################################################################
#                                                                       #
#                      standard help function                           #
#                                                                       #
#########################################################################
if ($h || $help) {
   print "--> ParamLatex.pl <--\n";
   print "Options:\n";
   print "\t-th=        : (semi opt) thorn to process\n";
   print "\t-arr=       : (semi opt) arrangement to process\n";
   print "\t-processall : (opt) process all arrangements\n";
   print "\t-directory= : (opt) dir. of arrangements (default arrangements/)\n";
   print "\t-thornlist= : (opt) list specific thorns to process\n";
   print "\t-dump       : (opt) dumps output to screen, not in Latex\n";
   print "\t-scope=     : (opt) restricted/global/private/shares/all (default all)\n";
   print "\t-sort=      : (opt) by: scope, type, name (default name)\n";
   print "\t\t-reverse    : (opt) reverse whatever the sorting is\n";
   print "\t-grouping=  : (opt) file ouput grouping scope (bythorn/byarrangement/all)\n";
   print "\t-document   : (opt) creates a TeX document, not just a table\n";
   print "\t-width=     : (opt) fixed width of table (default 160mm)\n";
   print "\t-spacing=   : (opt) vertical spacing between elements (default 6mm)\n";
   print "\t-outdir=    : (opt) directory to dump output files, default is .\n";
   print "\t-section    : (opt) makes this a section of a greater document\n";
   print "\t-h/-help    : (opt) this screen\n";
   print "Example:\n";
   print "\tperl -s /lib/sbin/ParamLatex.pl -outfile=mytable.tex -width=8.5cm\n";
   exit 0;
}

#########################################################################
#                                                                       #
#  This program will take as input a thorn, and output a latex table    #
#  that contains the information in that thorns param.ccl file.         #
#                                                                       #
#########################################################################

##############
# REQUIRE(S) #
##############

$sbin_dir = "lib/sbin";
require "$sbin_dir/parameter_parser.pl";
require "$sbin_dir/CSTUtils.pl";

###########################
# COMMAND LINE VAR. CHECK #
###########################

if (! $width)   {$width="160mm";}
if (! $sort)   {$sort="name";}
if (! $spacing) {$spacing="6mm";}
if (((! $th) && (! $arr)) && (! defined $processall)) {
   die "\nNo -th= or -arr= (or -processall) specified, nothing to process!\n";
}
if (! $directory) {$directory="arrangements/";} 
if (! $grouping)  {$grouping="bythorn";}


##################
# INITIALIZATION #
##################

@valid_types = qw(restricted global private shared);

$start_directory=`pwd`;
chomp($start_directory);

if (! $outdir) {
    $outdir = "./";
} else {
   if ($outdir =~ /^\//) {
      if (! -d "$outdir") {
         mkdir($outdir, 0755);
         print STDERR "\nCreating directory: $outdir" if ($verbose);
      }
   } else {
      if (! -d "$start_directory/$outdir") {
         mkdir("$start_directory/$outdir", 0755);
         print STDERR "\nCreating directory: $start_directory/$outdir" if ($verbose);
      }
   }
   if (! ($outdir =~ /\/$/)) {
      $outdir .= "/";
   }
}

print STDERR "\nOutput directory is: $outdir" if ($verbose);

if (defined $thornlist) {
   &Read_ThornList($thornlist);
 }

#################################################
# FIND THORN(S)/ARRANGEMENT(S) AND CREATE LATEX #
#################################################

@arrangements = &Find_Directories($directory);

&StartDocument("MasterTable") if ((! $dump) && ($grouping eq "all"));

foreach $arrangement (sort @arrangements) {
   @thorns = &Find_Directories($directory . $arrangement);

   &StartDocument($arrangement) if ((! $dump) && ($grouping eq "byarrangement"));
   THORN: foreach $thorn (@thorns) 
   {
   #   print STDERR "\nTHORN: $arrangement $thorn";
      if (($processall) || (($thorn eq $th) || ($arr eq $arrangement))) 
      { 

         ## do not create a file for this thorn if it is not in the THORNLIST (if one is specified)
	 if ((defined $thornlist) && (! defined $thornlist{"$arrangement/$thorn"})) {
            next THORN;
	 }

         if (-e "$directory$arrangement/${thorn}/param.ccl") 
         {
            &StartDocument($thorn, $arrangement) if ((! $dump) && ($grouping eq "bythorn"));

            $$thorn{$thorn} = "${directory}${arrangement}/${thorn}";
            %parameter_database = &create_parameter_database(%$thorn);
            &ReadLatexDatabase(%parameter_database);
            &FormatTable;

            &EndDocument if ((! $dump) && ($grouping eq "bythorn"));

	    ###################################################
            # reset the variables in case they are used again #
            undef %alreadydone;

  	    foreach $group_name (@valid_types) {
#                print STDERR "\n\t\t-->$group_name";
      		foreach $table (@$group_name) {
 #                  print STDERR "[$table]";
		   undef %$table;
		}
	    }
            # END of reset #
            ################
         }       
      }
   }
   &EndDocument if ((! $dump) && ($grouping eq "byarrangement")); 
}

&EndDocument if ((! $dump) && ($grouping eq "all")); 

print "\n" if ($verbose);
#########################################################################
#                 END OF MAIN SECTION OF THE PROGRAM                    # 
#########################################################################

#########################################################################
#                     BEGINNING OF SUB-ROUTINES                         #
#########################################################################

#########################################################################
# ReadLatexDataBase                                                     #
#   Calls parameter_parser.pl, which will read in the param.ccl file    #
#   from a single thorn and return all data as a %hash table, which we  #
#   will then parse to put into our own %hash tables named according to #
#   the variable names.                                                 #
#                                                                       #
#   %(variable_name)  : any number of hashes created with their names   #
#                       being the variable names, they then have $keys  #
#                       (descriptions) with $values (well, values)      #
#                          (e.g.) $name{"default"} = "Cactus";          #
#   @global       \                                                     #
#   @restricted    \  -> arrays containing the names of the relative    #
#   @private       /     hashes that are contained within their scopes  #
#   @shared       /             (masterlist contained in @valid_types)  #
#########################################################################
sub ReadLatexDatabase
{
  my(%parameter_database) = @_;
  my($field);
  my(@temp) = ();
  my($temp) = "";
  my($name) = "";
  
  #######################
  # reinitialize values #
  foreach $typ (@valid_types) {
     while (defined pop(@$typ)) {pop @$typ;}
  }
                                 # where is global type???

  foreach $field (sort keys %parameter_database)
  {
      print STDERR "\n$field --> $parameter_database{$field}" if ($verbose);

      &Clean;
         #################
         # ADD TO SCOPES #
         #################
      if ($field =~ /\sPRIVATE\s/) {
	@private = split/ /, $parameter_database{$field};	
      } elsif ($field =~ /\sRESTRICTED\s/) {
        @restricted = split/ /, $parameter_database{$field};
      } elsif ($field =~ /\sSHARES\s+(\w+)\s+var/) {
        @temp = split/ /, $parameter_database{$field};
 
        foreach $var (@temp) {
           $var=~ tr/A-Z/a-z/;
           $$var{"shared"}=$1;
        }
        push @shared, @temp;
      } else {
        #####################
        # ADD VARIABLE HASH #
        #####################

        if ($field =~ /(.*?)\s(.*?)\s(.*)/) {
           $name = $2; 
           $field_des=$3;
           $temp = $1; 
        } else {
   	   #print STDERR "\n\nTHROWNING AWAY: $field $paramater_database{$field}"; 
        }

        $name =~ tr/A-Z/a-z/;
        $$name{$field_des}=$parameter_database{$field};

        $$name{"name"} = $name;

        $$name{"thorn"} = $temp;
      }
   } #-- foreach
   print "\n";

   ################################
   # ADD ELEMENT $$table{"scope"} #
   ################################

   foreach $group_name (@valid_types) 
   {
      foreach $table (@$group_name) 
      {
       #  print STDERR "\n\t\t!!!!!!!! $table";
         $table =~ tr/A-Z/a-z/;
      # print STDERR "\n$table";
         $$table{"scope"} = $group_name;
         if ($$table{"shared"}) {
            $$table{"scope"} .= " from " . $$table{"shared"};
         } 
      } 

   } 
   return $name;
} ## END :ReadLatexDatabase:

#########################################################################
# FormatTable                                                           #
#    Primary function to be used to format the LaTex output to whatever #
#    sorting or grouping mechanism specified by the user.               #
#########################################################################
sub FormatTable {
        # sort: scope, type, name
   my (@all);
   while (defined pop(@all)) {pop @all;}

   if ($sort eq "scope") {
      if ((defined $scope) && ($scope ne "all")) {  # only one scope to output
           if ($dump) {
               &Dump(sort @$scope);
            } else {
               &CreateLatexTable(sort @$scope);
            }
      } else {
         foreach $type (@valid_types) {
            if ($dump) {
               &Dump(sort @$type);
            } else {
               &CreateLatexTable(sort @$type);
            }
         }
      }
   } else {
      if ((defined $scope) && ($scope ne "all")) {   # only print one scope
         @all = @$scope;
      } else {                                       # print all scopes
         foreach $type (@valid_types) {
             push @all, @$type;
          }   
      }

      if ($sort eq "name") {
         @all = sort @all; 
      } else {
         @all = sort SortByType @all;
      }

      if ($dump) {
        &Dump(@all);
      } else {
        &CreateLatexTable(@all);
      }      
  }

    print "\n" if ($verbose || $dump);
} ## END :FormatTable:

#########################################################################
# CreateLatexTable                                                      #
#    Intermediary function in the process of creating a LaTex table that#
#    loops through an array (of name passed in) and outputs LaTex in    #
#    THE ORDER SPECIFIED IN THAT ARRAY.  So, there you have it, the     #
#    foundation for sorting. :)                                         #
#########################################################################
sub CreateLatexTable {
   my (@cur_group) = @_;

   foreach $table (@cur_group) 
   {
      $table =~ tr/A-Z/a-z/;

      if (lc($$table{"thorn"}) eq lc($thorn)) {
         if (! defined $alreadydone{"$thorn$table"}) { 
            &LatexTableElement;
            $alreadydone{"$thorn$table"} = 1;   # to try to only print each once.
          }
      } 
   }
} ## END :CreateLatexTable:


#########################################################################
# StartDocument                                                         #
#    Opens the file OUT with the name sent in to this function a .tex   #
#    extension in the working directory for output of LaTeX code.       #
#                                                                       #
#    If specified with the -document flag on the command line, this     #
#    function will also print basic LaTeX commands to start a document  #
#########################################################################
sub StartDocument {
   my ($group_name) = shift;
   my ($arrangement) = shift;

   die "Cannot group tables for output, internal error in &StartDocument" if (! defined $group_name);

   chdir ($start_directory);
   open(OUT, ">$outdir${arrangement}_${group_name}_par.tex") || die "cannot open $outdir${group_name}_par.tex for output: $!";
   $oldfilehandle = select OUT;

   if ($document) {
      print "\\documentclass[12pt,a4paper]\{article\} \n";
      print "\\begin\{document\} \n\n";
   } elsif ($section) {
      $group_name =~ s/\_/\\\_/g;
      print "\\section{Parameters} \n\n";
   }


} ## :StartDocument:

#########################################################################
# EndDocument                                                           #
#    Ends a LaTeX document (if -document) specified, and closes the OUT #
#    file, which is used to write the LaTeX table to.                   #
#########################################################################
sub EndDocument {
   if ($document) {
      print "\\end\{document\} \n";
   }
   close OUT;
   select $oldfilehandle;
} ## :EndDocument:

#########################################################################
# LatexTableElement                                                     #
#    Takes whatever table element is currently reffered to by $table    #
#    and prints it out into a LaTeX table.  Only nifty things it curr.  #
#    does is NOT print ranges for BOOLEAN and SHARED elements.          #
#########################################################################
sub LatexTableElement {
   $table =~ tr/A-Z/a-z/;
   my ($name) = $table;
   my ($description) = $$table{"description"};
   my ($default) = $$table{"default"};
   $default =~ s/\_/\\\_/g;
   $default =~ s/%/\\%/g;
   
   $name =~ s/\_/\\\_/g;

   $description =~ s/\_/\\\_/g;
   
   # new addition 4/2001
   $description =~ s/\^/\\\^/g;

   print "$table{\"thorn\"}";
 
   $parawidth = $width;
   $parawidth =~ /(\d+)(.*)/;

   $parawidth = ($1 - 35) . $2;

   if (! (($$table{"description"} !~ /\w/) && (defined $$table{"shared"}))) {
      print "\\begin\{tabular*\}\{$width\}\{|c|c|c|\@\{\\extracolsep\{\\fill\}\}r|\} \\hline \n";
      print "\{\\bf Name\} & \{\\bf Default\} & \{\\bf Scope\} & \{\\bf Type\} \\\\ \n";
      print "\\hline \n";
      print "$name & $default & $$table{\"scope\"} & $$table{\"type\"} \\\\ \n";
      print "\\hline\\hline\n";
      print "\\multicolumn\{4\}\{|l|\}\{\\rule[-2mm]\{-2mm\}\{0.5cm\}\{\\bf Description\}~\\vline~ \n";
      print "\\parbox\{${parawidth}\}\{\\it $description\}\} \\\\ \n";
      print "\\hline \n";
   } else {
      print "\\begin\{tabular*\}\{$width\}\{|c|c|\@\{\\extracolsep\{\\fill\}\}r|\} \\hline \n";
      print "\{\\bf Name\} & \{\\bf Scope\} & \{\\bf Type\} \\\\ \n";
      print "\\hline \n";
      print "$name & $$table{\"scope\"} & $$table{\"type\"} \\\\ \n";
      print "\\hline \n";
   }

   print "\\end\{tabular*\} \n\n";

   if ((($$table{"type"} ne "BOOLEAN") && ! $$table{"shared"}) && ($$table{"ranges"} > 0)) {

      print "\\vspace\{1mm\} \n\n";

      print "\\begin\{tabular*\}\{$width\}\{|c|l@\{\\extracolsep\{\\fill\}\}r|\} \\hline \n";
      print "\{\\bf Range\} & & \\\\ \n";
      print "\\hline\\hline \n";

      for ($i=1; $i <= $$table{"ranges"}; $i++) {
         $tempvar =  $$table{"range $i description"};
         $tempvar =~  s/\_/\\\_/g;

         $tempvar2 =  $$table{"range $i range"};
         $tempvar2 =~  s/\_/\\\_/g;

         # new addition 4/2001
         $tempvar2 =~ s/\^/\\\^/g;
         $tempvar2 =~ s/\$/\\\$/g;

         print "$tempvar2 & $tempvar & \\\\ \\hline \n";
      }
      print "\\end\{tabular*\} \n\n";
   }

   print "\\vspace\{$spacing\} \n\n";
} ## END :LatexTableElement:

#########################################################################
# Find_Directories                                                      #
#    Finds the current directories that are not CVS stuff contained w/in#
#    the directory name that is sent to this function, used to find     #
#    thorns and arrangements in this program.                           #
#########################################################################
sub Find_Directories {
   my(@good_directories);
   chdir ("$start_directory") || die "cannot chdir to $start_directory: $!";

   chdir("$_[0]") || die "cannot change directory to $_[0] : $!";
   open(LS, "ls -p|");
   while(chomp($name = <LS>)) {
      if ((($name =~ /\/$/) && ($name ne "History/")) && ($name ne "CVS/"))  {
         $name =~ s#/##;
         push(@good_directories, $name);
      }
   }   
   close(LS);
   chdir ($start_directory);
   return (@good_directories);
} ## END :Find_Directories:

#########################################################################
# SortByType                                                            #
#    Sorts the ouput by "type" within their respective thorns, as we can#
#    have repetitive variable names within different thorns, currently  #
#    output is restricted to internal (within thorn) sorting.           #
#########################################################################
sub SortByType {
   if (lc($$a{"type"}) cmp lc($$b{"type"}) < 0) {
      return -1;
   } elsif (lc($$a{"type"}) cmp lc($$b{"type"}) > 0) {
      return 1;
   } else {
      return -1;
   }
} ## END :SortByType:

#########################################################################
# SortByName                                                            #
#    Sorts the ouput by "name" within their respective thorns, as we can#
#    have repetitive variable names within different thorns, currently  #
#    output is restricted to internal (within thorn) sorting.           #
#########################################################################
sub SortByName {
   my ($first) = $$a{"name"};
   my ($second) = $$b{"name"};

   if (lc($first) cmp lc($second) < 0) {
      print "\n$first : $second -1";
      return -1;
   } elsif (lc($$a{"name"}) cmp lc($$b{"name"}) > 0) {
      print "\n$$a{\"name\"} : $$b{\"name\"} 0";
      return 1;
   } else {
      print "\n$$a{\"name\"} : $$b{\"name\"} -1 (2)";
      return -1;
   }
} ## END :SortByName:

#########################################################################
# Dump                                                                  #
#    Function to dump output to the screen rather than to a .tex file   #
#    table, this does NOT create latex, simple provides an easy way to  #
#    view the variables from the command line without use of Latex.     #
#########################################################################
sub Dump {
  my (@cur_group) = @_;
 
  print "\n$thorn variables:\n";

  foreach $value (@cur_group) {
    if (lc($$value{"thorn"}) eq lc($thorn)) { 
      print "\t$value:\n";
      foreach $key (keys %$value) {
	  print "\t\t$key -> $$value{$key}\n";
      }
    }
  }
} ## END :Dump:

#########################################################################
# Clean                                                                 #
#    Function to perform any cleaning that may need to be done to       #
#    variables before they are put into a hash of their name.           #
#########################################################################
sub Clean {
} ## END :Clean:


##########################
# Reads in the thornlist #
##########################
sub Read_ThornList 
{
   my ($thornlist) = shift;

   chomp($directory);

   open (TL, "$thornlist") 
      || die "cannot open thornlist ($thornlist) for reading: $!";

   while (<TL>) 
   {
      s/(.*?)#.*/\1/;            # read up to the first "#"
      s/\s+//g;                  # replace any spaces with nothing
      if (/\w+/) {               
         $thornlist{$_} = 1;
      }
   }
   close TL;     
}
