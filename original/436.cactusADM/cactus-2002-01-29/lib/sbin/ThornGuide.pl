#!/usr/local/bin/perl 

##########################################################################
# Create cactus documentation based upon documenation provided by thorns #
# in the current checkout being examined.                                #
#                                                                        #
# It will create a "manual" for this particular checkout, with an index  #
# and everything.  Note that the documentation provided by thorns may    #
# not use macros, etc. (except of course, the ones we provide globally)  #
##########################################################################

#################################
# what file are we looking for? #
$file = "documentation.tex";

$mydir = `pwd`;
chomp($mydir);
$mydir =~ s/\/lib\/sbin\/$//;

########
# HELP #
########
if ($help || $h) {
   print "--> ThornGuide.pl <--\n";
   print "Options:\n";
   print "\t-directory= : (semi opt) directory to process\n";
   print "\t-thornlist= : (semi opt) thornlist to process\n";
   print "\t-outfile=   : (opt) output file (default ThornGuide.tex)\n";
   print "\t-outdir=    : (opt) output directory (default ./)\n";
   print "\t-verbose    : (opt) print verbose output\n";
   print "\t-h/-help    : (opt) this screen\n";

   print "Example:\n";
   print "\tperl -s /lib/sbin/ThornGuide.pl -thornlist=configs/test/ThornList -outfile=cactus.tex -outdir=configs/test/doc -verbose\n";
   exit 0;  
}

############################
# get directory to process #
if (! $directory) 
{ 
   if (! $thornlist) {
      die "Cannot locate directory or thornlist to process.";
   } else {
      $directory = "$mydir/arrangements/";
   }
} elsif (! ($directory =~ /^\//)) {
    $directory = "$mydir/$directory";  
}

if (! ($directory =~ /\/$/) ) {
   $directory .= '/';
}

print STDERR "\nProcessing: $directory" if ($verbose);

# what are we going to process, if we are processing a directory?
if (defined $directory) {
   if ($directory =~ /arrangements\/(.*?)\//) { 
      $arrangement = $1;
      print "\nProcessing one arrangement" if ($verbose);
      $level = "one_arr";
   } elsif ($directory =~ /arrangements\//) {
      print "\nProcessing all arrangements" if ($verbose);
      $level = "all_arr";
   } else {
      print "\nProcessing one thorn" if ($verbose);
      $level = "thorn";
   }
}

$start_directory=`pwd`;
chomp($start_directory);

# determine the output directory
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

###################
# specify outfile #
if (! $outfile) {
   $outfile = "ThornGuide.tex";
}

############################
# open the file for output #
open (OUT, ">$outdir$outfile") || die "\ncannot open $outdir$outfile: $!";

###########
## START ##
###########
&Output_Top;


if (! $thornlist) {
   &Recur($directory);   # if we are processing a directory
} else {
   @foundfiles = &Read_ThornList($thornlist, $directory); # thornlist
}

#############################################################
# Goes through and classifies thorns based upon arrangement #
foreach (@foundfiles) 
{
   s/^$directory//;
   /(.*?)\/(.*?)\//;

   $arr = $1;          # get arrangements and thorn names
   $thorn =$2;

   if (! defined $arrangements{$arr}) {     # hash of arrangements
      $arrangements{$arr} = $1;
   } 
   push @$arr, $thorn;                      # hash of arrangements & thorns
 #  print STDERR "\nFound: $_"  if ($verbose);
}
#                                                            #
##############################################################


##############################################################
# Iterates through our known arrangements, then its thorns,  #
# reading in documentation and creating .tex file            #

#########################################
# if we are processing all arrangements #
if ($level eq "all_arr") {
   $i=1;                                           # $i = "cactuspart" counter
   foreach $arrangement (sort keys %arrangements) 
   {
      &Start_Arr($arrangement,$i);                 # begins a "cactuspart"
      foreach $thorn (@$arrangement) {
         $$thorn = &Read_Thorn_Doc("$directory$arrangement/$thorn/doc");
         &Add_Section($thorn, $$thorn);            # add thorn documentation
      }
      &End_Arr;                                    # ends a "cactuspart"
      $i++;  
   }
#############################################
# if we are just processing one arrangement #
} elsif ($level eq "one_arr") {
   &Start_Arr($arrangement, "1");
   foreach $thorn (keys %arrangements) 
   {
      $$thorn = &Read_Thorn_Doc("$directory$thorn/doc");
      &Add_Section($thorn, $$thorn);
   }
   &End_Arr;
}
#                                                             #
###############################################################

&Output_Bottom;
print STDERR "\nFinished\n" if ($verbose);
#print "\n";
#######################
## END OF MAIN STUFF ##
#######################




####################################################################
#                    BEGINNING OF SUB-ROUTINES                     #
####################################################################

####################################################################
# Reads in the thorn documentation file and returns it as a string #
# 
# This function starts reading when it hits he first section after #
# the \begin{document} section, and continues until the \end{d...} #
# statement.  it also adds the include paramater to include the    #
# output from ParamLatex.pl.                                       #
####################################################################
sub Read_Thorn_Doc 
{
   my ($path) = shift;
   my ($contents) = "";
   my ($pathandfile);

   my ($start) = 0;
   my ($stop)  = 0;
   my ($temp)  = 0;

   $pathandfile .= "$path/$file";

   open (DOC, "$pathandfile");

   while (<DOC>)                            # loop through thorn doc.
   {
      if (/\\end\{document\}/) {            # stop reading
         $stop = 1;
         $contents .= "\n\\include{${arrangement}_${thorn}_par}\n";
      }

      if ($start && ! $stop) {              # add to $contents
         s/(\\includegraphics.*?\{)\s*?(.*\.eps\s*?\})/$1$path\/$2/g;
         $contents .= $_;
      } elsif (/\\begin\{document\}/) {     # don't begin yet.... 1st flag
         $temp = 1;
      }

      if (($temp) && ( /\\section\{/ )) {   # start reading
          $start = 1;
          $contents = $_;
          $temp = 0;
      }
   }

   # if it never started reading, then we print some error message
   if (! $start) {
      $tmp = $thorn;
      $tmp=~ s/\_/\\\_/g;
      if (-e $pathandfile) {
         $contents = "Could not parse latex documentation for $arrangement/$tmp($file)";
      } else {
         $contents = "Could not find latex documentation for $arrangement/$tmp ($file)";
      }
      $contents .= "\n\n\\include{${arrangement}\_${thorn}\_par}\n";
   }
   
   close DOC;

   return $contents;
}

##########################################################################
# Adds a thorn section (chapter), mini table of contents and whatever we #
# have parsed out from ($file) [documentation.tex]                       #
##########################################################################
sub Add_Section 
{
   my ($thorn) = shift;
   my ($contents) = shift;

$thorn =~ s/\_/\\\_/g;
print OUT <<EOC;

\\chapter{$thorn}
\\minitoc

$contents
EOC
}  

############################################################
# Ends a cactuspart, which will normally be an arrangement #
############################################################
sub End_Arr
{
print OUT <<EOC;

\\end{cactuspart}

EOC
}  

##############################################################
# Starts a cactuspart, which will normally be an arrangement #
##############################################################
sub Start_Arr 
{
   my ($arr) = shift;
   my ($partnum) = shift;

print OUT <<EOC;

\\begin{cactuspart}{$partnum}{$arr}{}{}
\\renewcommand{\\thepage}{\\Alph{part}\\arabic{page}}
EOC
}  


###########################   
# Ends the latex document #
###########################
sub Output_Bottom 
{
print OUT <<EOC;

\\end{document}
EOC
}

##########################
# Reads in the thornlist #
##########################
sub Read_ThornList 
{
   my ($thornlist) = shift;
   my ($directory) = shift;
   my (@temp);
   my (@tl);

   chomp($directory);

   open (TL, "$thornlist") 
      || die "cannot open thornlist ($thornlist) for reading: $!";

   while (<TL>) 
   {
      s/(.*?)#.*/\1/;            # read up to the first "#"
      s/\s+//g;                  # replace any spaces with nothing
      if (/\w+/) {               
         push @temp, $_;         # add to array if something is left
      }
   }     

   foreach $tempvar (@temp)      # see if docs exist for these thorns
   {
#      if (-e "$directory$tempvar/doc/$file") {
         push @tl, "$directory$tempvar/doc/$file";
#      } else {
#         print "\nCannot find: $tempvar/doc/$file" if ($verbose);
#      }
   }

    return @tl;
}

##############################################################
# This is the function that finds me the file I want ($file) #
##############################################################
sub Recur 
{
   local ($dir) = shift;
   local (@dirs);

   chdir ($dir) || die "\nFatal Error: cannot chdir to $dir: $!";

   open (LS, "ls -p|");

   while(chomp($name = <LS>)) 
   {
      if ((($name =~ /\/$/) && ($name ne "History/")) && ($name ne "CVS/")) {
         push @dirs, $name; 
      }
   }
   close (LS);

   if ($dir =~ /arrangements\/(.*?)\/(.*?)\/$/) {
      if (-e "${dir}/doc/$file") {                              # we found the file
         push @foundfiles, $dir; 
         print STDERR "\n$dir\t\tFound $file" if ($verbose);
      } elsif (-e "${dir}param.ccl") {                   # we didn't find the file
         print STDERR "\n$dir\t\tNo $file, but param.ccl" if ($verbose); 
         push @foundfiles, "${dir}/doc";
      } else {
        #print STDERR "\n$dir\t\tNo $file, no param.ccl" if ($verbose);
      }
   }
   foreach (@dirs) {                    # look in sub directories
      &Recur("$dir$_");
   }
   return;
}

###########################################################################
# Starts the latex document, using lots of stuff taken from the UserGuide #
###########################################################################
sub Output_Top 
{

print OUT  <<EOC;
\\documentclass{report}
\\usepackage{fancyhdr}

\\usepackage{minitoc}
\\usepackage{latexsym}
\\usepackage{ifthen}
\\usepackage{calc}
\\usepackage{graphicx}

\\setlength{\\mtcindent}{24pt}
\\renewcommand{\\mtcfont}{\\small\\rm}
\\setcounter{minitocdepth}{2}

\\makeatletter
\\\@addtoreset{chapter}{part}
\\makeatother

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\parskip = 2 pt
\\oddsidemargin = 0 cm
\\textwidth = 16 cm
\\topmargin = -1 cm
\\textheight = 24 cm

\\usepackage{tocloft}
\\addtolength{\\cftchapnumwidth}{0.5em}
\\addtolength{\\cftsecnumwidth}{0.5em}
\\addtolength{\\cftsubsecnumwidth}{0.5em}
\\addtolength{\\cftsubsubsecnumwidth}{0.5em}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\def\\q{\\bf QUERY: }
\\def\\t{\\tt \\obeylines }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MANPAGE like description setting for options, use as % \\begin{Lentry}
%\\item[text] text \\end{Lentry} \\usepackage{ifthen,calc}
\\newcommand{\\entrylabel}[1]{\\mbox{\\textsf{#1}}\\hfil}
\\newenvironment{entry}
  {\\begin{list}{}
    {\\renewcommand{\\makelabel}{\\entrylabel}
      \\setlength{\\labelwidth}{90pt}
      \\setlength{\\leftmargin}{\\labelwidth+\\labelsep}
    }
  }
  {\\end{list}} \\newlength{\\Mylen} \\newcommand{\\Lentrylabel}[1]{%
  \\settowidth{\\Mylen}{\\textsf{#1}}%
  \\ifthenelse{\\lengthtest{\\Mylen > \\labelwidth}}%
    {\\parbox[b]{\\labelwidth} % term > labelwidth
      {\\makebox[0pt][l]{\\textsf{#1}}\\\\}} %
    {\\textsf{#1}} %
  \\hfil\\relax} \\newenvironment{Lentry}
  {\\renewcommand{\\entrylabel}{\\Lentrylabel}
   \\begin{entry}}
  {\\end{entry}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Takes three arguments - the name of the document, the revision, and
% the date.
% Additionally ther eis an optional first argument with the version number

\\newcommand{\\cactustitlepage}[4][4.0]
{
\\thispagestyle{empty}
\\setlength{\\parindent}{0mm}
\\setlength{\\parskip}{0mm}
\\vspace*{\\stretch{1}}
\\rule{\\linewidth}{1mm}
\\begin{flushright}
  \\Huge Cactus #1\\\\[5mm]
        #2
\\end{flushright}
\\rule{\\linewidth}{1mm}
\\vspace*{\\stretch{2}}
\\begin{center}
\\includegraphics[angle=0,width=5cm]{bincactus.eps}
\\end{center}
\\vspace*{\\stretch{2}}
\\begin{center}
   \\Large #3 \\\\[3mm]
          #4
\\end{center}
\\newpage
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\newenvironment{cactuspart}[4]
{
  \\clearpage
  \\renewcommand{\\thepage}{\\Alph{part}\\arabic{page}}
  % Redefine the plain style
  \\fancypagestyle{plain}
  {
    \\fancyhf{} % Clear all header and footer fields
    \\lfoot{#3}
    \\cfoot{#4}
    %  \\rfoot{\\thepage/\\pageref{lastpage:\\thepart}}
    \\rfoot{\\thepage}
    \\renewcommand{\\headrulewidth}{0.0pt}  
    \\renewcommand{\\footrulewidth}{0.4pt}  
    \\renewcommand{\\thepage}{\\Alph{part}\\arabic{page}}
  }

  % Make sure it's arabic numbering
  \\pagenumbering{arabic}
  % Start the page counter at 1
  \\setcounter{page}{1}
  % Start a new part
  \\renewcommand{\\thepage}{\\Alph{part}\\arabic{page}}
  \\part{#2}
  \\setcounter{part}{#1}
  % Redefine the page
  % Set up fancy headings.
  \\lfoot{#3}
  \\cfoot{#4}
   % \\rfoot{\\thepage/\\pageref{lastpage:\\thepart}}
  \\rfoot{\\thepage}
  \\renewcommand{\\headrulewidth}{0.4pt}
  \\renewcommand{\\footrulewidth}{0.4pt}
}
{
  % Remember the last page of the part
  \\label{lastpage:\\thepart}
  \\clearpage
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\def\\nhat{{\\hat n}} 
\\def\\vone{{v_{{}_{(1)}}}} 
\\def\\vtwo{{v_{{}_{(2)}}}}
\\def\\vthree{{v_{{}_{(3)}}}} 
\\def\\eref#1{(\\ref{#1})}


\\begin{document}
\\cactustitlepage{Thorn Guide}{Revision: }{Date: 2000/12/07}
\\dominitoc

\\setcounter{page}{1}

% Table of contents
\\pagenumbering{roman}

\\tableofcontents

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\renewcommand{\\thepart}{\\Alph{part}}
\\renewcommand{\\thechapter}{\\Alph{part}\\arabic{chapter}}
\\renewcommand{\\thepage}{\\Alph{part}\\arabic{page}}
\\pagestyle{fancy}

\\newpage
%%%%%%%%%%%%%%%%%%%%%%%
EOC
}

