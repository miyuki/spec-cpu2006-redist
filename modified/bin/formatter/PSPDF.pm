#
# PSPDF.pm
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: PSPDF.pm 6364 2011-03-05 00:41:51Z cloyce $
#

require 'util.pl';
require 'flagutils.pl';

package PSPDF;
use Font::AFM;
use IO::File;
use UNIVERSAL qw(isa);
use strict;

use vars qw($indent %afms);
my $version = '$LastChangedRevision: 6364 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'PSPDF.pm'} = $version;

my $ps_ok = ::ps_ok();

$indent = '';

my %code2mark = ( 'r' => '\322',
                  't' => '\324',
                  's' => 'SM',
                );
sub new {
    my ($class, $mode) = @_;
    if ($mode ne 'PS' && $mode ne 'PDF') {
	main::Log(0, "PSPDF::new: mode of '$mode' not supported, using 'PS'\n");
	$mode = 'PS';
    }
    if ($mode eq 'PDF' && !$::pdf_ok) {
	main::Log(0, "NOTICE: PDF output is disabled\n");
	return bless { 'error' => 1 }, $class;
    }
    if ($mode eq 'PS' && !$ps_ok) {
	main::Log(0, "NOTICE: PostScript output is disabled\n");
	return bless { 'error' => 1 }, $class;
    }
    my $me = bless {
	'mode'    => $mode,   # This should be PS or PDF
	'output'  => '',
	'isopen'  => 0,
	'CreationDate' => '',
	'Creator' => '',
	'Producer'=> '',
	'Author'  => '',
	'Title'   => '',
	'Subject' => '',
	'changedfont' => 0,
	'states'  => [],
	'saves'   => [],
        'possave' => [],
        'tm_done' => {},
        'tm_doc_done' => {},
    }, $class;
    return $me;
}

sub output {
    my ($me) = @_;
    if ($me->{'isopen'}) {
	main::Log(0, "PSPDF::output: called when object still open!\n");
	return '';
    }
    if ($me->{'mode'} eq 'PS') {	# Fix up the number of pages
	my $pages = $me->{'pagenum'} || 0;
	$me->{'output'} =~ s/%%PUTPAGESHERE%%/$pages/;
    }
    return $me->{'output'};
}

sub DESTROY {
    my ($me) = @_;
    unlink ($me->{'outputname'}) if $me->{'outputname'} ne '';
}

sub anon_var {
    my ($me, $name, $val) = @_;
    my $old = $me->{$name};
    $me->{$name} = $val if defined $val;
    return $old;
}
sub CreationDate { my $me = shift; return $me->anon_var('CreationDate', @_); }
sub Creator { my $me = shift; return $me->anon_var('Creator', @_); }
sub Producer{ my $me = shift; return $me->anon_var('Producer', @_); }
sub Author  { my $me = shift; return $me->anon_var('Author' , @_); }
sub Title   { my $me = shift; return $me->anon_var('Title'  , @_); }
sub Subject { my $me = shift; return $me->anon_var('Subject', @_); }

sub open {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'}="%!PS-Adobe-3.0\n";
	$me->{'output'}.="%%Pages: %%PUTPAGESHERE%%\n";
    } elsif ($me->{'mode'} eq 'PDF') {
	$me->{'pdf'} = PDF::API2->new();
        #$me->{'pdf'}->{'forcecompress'} = 0;
	$me->{'ip'} = {};
	for my $foo (qw(CreationDate Creator Author Title Subject Producer)) {
	    $me->{'ip'}->{$foo} = $me->{$foo};
	}
	$me->{'pdf'}->info(%{$me->{'ip'}});
        $me->{'pdf'}->preferences('-onecolumn' => 1);
    }
    $me->{'isopen'}=1;
}

sub close {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "%%EOF\n";
    } elsif ($me->{'mode'} eq 'PDF' && $me->{'isopen'}) {
	$me->{'output'} = $me->{'pdf'}->stringify;
	$me->{'pdf'}->end;
    }
    $me->{'isopen'}=0;
}

use vars qw(@pagenumtxt);
@pagenumtxt = qw(zero one two three four five six seven eight nine ten);

sub begin_page {
    my ($me, $size) = @_;

    if ($size eq 'letter') {
	$me->{'width'}  = 612;
	$me->{'height'} = 792;
    } elsif ($size eq 'a4') {
	$me->{'width'}  = 595;
	$me->{'height'} = 842;
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'page'} = $me->{'pdf'}->page;
        $me->{'page'}->mediabox($me->{'width'}, $me->{'height'});
        $me->{'content'} = $me->{'page'}->gfx;
	$me->{'pagenum'}++;
    } elsif ($me->{'mode'} eq 'PS') {
	my $num = ++$me->{'pagenum'};
	$me->{'output'} .= "%%Page: $pagenumtxt[$num] $num\n";
    }
}

sub end_page {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "showpage\n";
    }
}

sub get_font {
    my ($me) = @_;
    return($me->{'fontname'}, $me->{'fontsize'});
}

sub set_font {
    my ($me, $font, $size, $force) = @_;
    $size = $me->{'fontsize'} unless defined($size);
    if (exists $afms{$font} && ref($afms{$font}) eq 'Font::AFM') {
        $me->{'afm'} = $afms{$font};
    } else {
	$me->{'afm'} = new Font::AFM "$ENV{'SPEC'}/bin/fonts/$font";
	$afms{$font} = $me->{'afm'};
    }
    return if (!(defined($force) && $force) && $me->{'fontsize'} == $size && $me->{'fontname'} eq $font );
    $me->{'fontsize'} = $size;
    $me->{'fontname'} = $font;
    $me->{'changedfont'} = 1;
    if ($me->{'mode'} eq 'PDF') {
	my $encoding = 'winansi';
	$encoding = 'builtin' if ($me->{'fontname'} eq 'Symbol');
	# It's hard to believe that PDF::API2 doesn't do this internally!
	if (exists $me->{'pdffonts'}->{$me->{'fontname'}.$encoding}) {
	    $me->{'currfont'} = $me->{'pdffonts'}->{$me->{'fontname'}.$encoding};
	} else {
	    $me->{'currfont'} = $me->{'pdffonts'}->{$me->{'fontname'}.$encoding} = 
		$me->{'pdf'}->corefont($me->{'fontname'},
				       '-encoding' => $encoding);
	}
    }
}

sub set_font_really {
    my ($me) = @_;
    if ($me->{'mode'} eq 'PDF') {
	$me->set_font($me->{'fontname'}, $me->{'fontsize'});
    } else {
	$me->{'output'} .= "/$me->{'fontname'} findfont $me->{'fontsize'} scalefont setfont\n";
    }
}

sub set_text_pos {
    my ($me, $x, $y) = @_;
    $me->{'text_x'} = $x;
    $me->{'text_y'} = $y;

    if ($me->{'mode'} eq 'PS') {
	$me->{'output'} .= "$x $y moveto\n";
    }
}

sub stringwidth {
    my ($me, $str, $size) = @_;
    my $rc = 0;
    $size = $me->{'fontsize'} if !defined $size;

    # Heck always use the AFM stuff, this prevents us from perhaps
    # inadvertantly growing the PDF file.
    $rc = $me->{'afm'}->stringwidth($str, $size);

    # Here's a hack...
    if ($rc == 0 && $str ne '' && $me->{'fontname'} eq 'Symbol') {
	# It seems unlikely that any symbol would have absolutely zero width...
	$rc = $me->{'afm'}->stringwidth(' ' x (length($str) + 2), $size);
    }
    return $rc;
}

sub string_fit {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext, %opts) = @_;
#print "\nstring_fit($me, \"$str\", $x, $y, $width, $height, $minsize, $errtext) in $me->{'fontname'}\@$me->{'fontsize'} ";
    my $size = $me->string_calcsize($str, $width, $height, $minsize, $errtext);
#    $size = $me->string_calcsize("   .000,    .000", $width, $height, $minsize, $errtext);
    return 0 if ($size <= 0);
    $me->set_font($me->{fontname}, $size);
    $me->set_text_pos($x, $y+($height-$size)/2);
#print "=> ($x, ".($y+($height-$size)/2).") in $me->{'fontname'}\@$size\n";
    $me->show($str, undef, undef, %opts);
#    $me->show(sprintf("%.3f, %.3f", $x, ($y+($height-$size)/2)), undef, %opts);
    return $size;
}

sub string_fit_right {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext) = @_;
    my $size = $me->string_calcsize($str, $width, $height, $minsize, $errtext);
    return 0 if ($size <= 0);
    $me->set_font($me->{fontname}, $size);
    $me->set_text_pos($x, $y+($height-$size)/2);
#print "string_fit_right showing \"$str\" at ($x,".($y+($height - $size)/2).") in ".$me->{fontname}." $size\n";
    $me->show_right($str);
    return $size;
}

sub string_fit_center {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext, %opts) = @_;
    my $size = $me->string_calcsize($str, $width, $height, $minsize, $errtext);
#print "\nstring_fit_center($me, \"$str\", $x, $y, $width, $height, $minsize, $errtext) in $me->{'fontname'}\@$size\n";
    return 0 if ($size <= 0);
    $me->set_font($me->{fontname}, $size);
    $me->set_text_pos($x + ($width/2), $y+($height - $me->string_XHeight())/2);
    $me->show_center($str, undef, undef, %opts);
    return $size;
}

sub strings_fit {
    my ($me, $x, $y, $width, $height, $minsize, $maxsize, $errtext, @lines) = @_;
    # Do the right thing for array refs
    for(my $i = 0; $i < @lines+0; $i++) {
      if (ref($lines[$i]) eq 'ARRAY') {
	splice(@lines, $i, 1, @{$lines[$i]});
      }
    }
    my $numlines = @lines+0;
    return 1 if ($numlines == 0);
    if ($maxsize > $height / ($numlines * 1.1)) {
      $maxsize = $height / ($numlines * 1.1);
    }

    # Get a rough cut at the font size
    my $size = $maxsize;
    foreach my $line (@lines) {
      my $tmpsize = $me->string_calcsize($line, $width, $height / $numlines,
					 $minsize, $errtext);
      return 0 if ($tmpsize <= 0);
      $size = $tmpsize if ($tmpsize < $size);
    }
    $me->set_font($me->{fontname}, $size);

    # Arrange for some space for the descenders, plus a bit
    $y += abs($me->string_Descender()) + 1;
    $height -= ($me->string_Descender()) + 2;

    # Do the size calc over again with the newly revised height
    foreach my $line (@lines) {
      my $tmpsize = $me->string_calcsize($line, $width, $height / $numlines,
					 $minsize, $errtext);
      return 0 if ($tmpsize <= 0);
      $size = $tmpsize if ($tmpsize < $size);
    }
    $me->set_font($me->{fontname}, $size, 1);

    # Actually show the strings
    my $string_base = ($height / $numlines) - ($me->string_CapHeight() / 2) + 1;
    for(my $i = $numlines - 1; $i >= 0; $i--) {
      $me->set_text_pos($x, $y + ($string_base * ($numlines - $i - 1)));
      $me->show($lines[$i]);
    }
    return $size;
}
sub string_calcsize {
    my ($me, $str, $width, $height, $minsize, $errtext) = @_;
#print "\nstring_calcsize($me, \"$str\", $width, $height, $minsize, $errtext)\n";
    my $w = $me->{'afm'}->stringwidth($str, $height);
    return $height if $w == 0;
    my $size = $width / $w * $height;
    $size = $height if $size > $height;
#print "w=$w, size=$size\n";
    return $size if !defined $minsize;
    if ($size < $minsize && $errtext ne '') {
	my $txt = $errtext;
	$txt =~ s/%s/sprintf "%.2f", $size/eg;
	main::Log(0, "\n$txt\n");
    }
    return $size;
}

sub string_XHeight {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->XHeight() * $size / 1000;
}
sub string_CapHeight {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->CapHeight() * $size / 1000;
}
sub string_Descender {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->Descender() * $size / 1000;
}
sub string_Ascender {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    return $me->{'afm'}->Ascender() * $size / 1000;
}
sub string_MaxWidth {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    my $width = 0;
    if (!exists $me->{'maxwidth'}->{$me->{'fontname'}}) {
      $me->get_font_widths();
    }
    $width = $me->{'maxwidth'}->{$me->{'fontname'}} * $size / 1000;
    return $width;
}
sub string_MaxCapWidth {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    my $width = 0;
    if (!exists $me->{'maxcapwidth'}->{$me->{'fontname'}}) {
      $me->get_font_widths();
    }
    $width = $me->{'maxcapwidth'}->{$me->{'fontname'}} * $size / 1000;
    return $width;
}
sub string_MaxLCWidth {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    my $width = 0;
    if (!exists $me->{'maxlcwidth'}->{$me->{'fontname'}}) {
      $me->get_font_widths();
    }
    $width = $me->{'maxlcwidth'}->{$me->{'fontname'}} * $size / 1000;
    return $width;
}
sub string_MaxDigitWidth {
    my ($me, $size) = @_;
    $size = $me->{'fontsize'} if ! defined $size;
    my $width = 0;
    if (!exists $me->{'maxdigitwidth'}->{$me->{'fontname'}}) {
      $me->get_font_widths();
    }
    $width = $me->{'maxdigitwidth'}->{$me->{'fontname'}} * $size / 1000;
    return $width;
}
sub get_font_widths {
    my ($me) = @_;
    my %width = ( 'digit' => 0, 'lc' => 0, 'cap' => 0, '' );
    my $wx = $me->{'afm'}->Wx();
    if (!::isa($wx, 'HASH')) {
      main::Log(0, "ERROR: Found no character width data for $me->{'fontname'}!\n");
      $wx = {};
    }
    # Do the overall
    foreach my $charname (keys %{$wx}) {
      $width{''} = $wx->{$charname} if ($wx->{$charname} > $width{''});
    }
    # Now upper-case
    foreach my $charname ('A' .. 'Z') {
      $width{'cap'} = $wx->{$charname} if ($wx->{$charname} > $width{'cap'});
    }
    # Now lower-case
    foreach my $charname ('a' .. 'z') {
      $width{'lc'} = $wx->{$charname} if ($wx->{$charname} > $width{'lc'});
    }
    # Now digits
    foreach my $charname (qw(
                            zero one two three four five six seven eight nine
                            )
                         ) {
      $width{'digit'} = $wx->{$charname} if ($wx->{$charname} > $width{'digit'});
    }

    foreach my $key (keys %width) {
      $me->{'max'.$key.'width'}->{$me->{'fontname'}} = $width{$key};
    }
}

sub fontsize {
    my ($me) = @_;

    return $me->{'fontsize'};
}

sub fit_link_center {
    my ($me, $str, $x, $y, $width, $height, $minsize, $errtext, %opts) = @_;

    if (isa($str, 'ARRAY')) {
        # Assemble the text of the string
        my $tmpstr = join('', map { isa($_, 'ARRAY') ? $_->[1] : $_ } @{$str});
        # Get the font size to use
        my $size = $me->string_calcsize($tmpstr, $width, $height, $minsize, $errtext);
        return 0 if ($size <= 0);
        $me->set_font($me->{'fontname'}, $size);
        # Get some relevant dimensions
        my $strwidth = $me->stringwidth($tmpstr);
        my $xpos = $x + ( $width / 2) - ( $strwidth / 2);
        my $ypos = $y + ( $height - $me->string_XHeight() ) / 2;
        foreach my $fragment (@{$str}) {
            $me->set_text_pos($xpos, $ypos);
            my ($url, $string) = (undef, undef);
            if (isa($fragment, 'ARRAY')) {
                ($url, $string) = @{$fragment};
            } else {
                $url = undef;
                $string = $fragment;
            }
            $me->show($string, undef, $url, %opts);
            $xpos += $me->stringwidth($string);
        }
        return $size;
    } else {
        return $me->string_fit_center($str, $x, $y, $width, $height, $minsize, $errtext, %opts);
    }
}

sub linkto {
    my ($me, $str, $ul, %opts) = @_;
    if (exists($opts{'isbench'}) && $opts{'isbench'}) {
        return $me->show($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/${str}.html", %opts);
    } else {
        my $anchor = ::makeanchor($opts{'section'}.$str);
        $anchor =~ s/(\S)continued$/$1/i; # Always goes to the same place anyway

        # Relative file links do not "just work".  In fact, they don't work.  So
        # everyone will get to visit SPEC for the answers!
        return $me->show($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#$anchor", %opts);
    }
}

sub show {
    my ($me, $str, $ul, $url, %opts) = @_;
#print "show($me, str=\"$str\", ul=$ul, url=\"$url\",opts=(\n    ".join(",\n    ", map { "$_ => $opts{$_}" } sort keys %opts)."\n   ))\n";

    if ($opts{'tmsearch'}) {
        # Looking at strings that might contain SPEC trademarks, so hand the
        # whole mess off to fixup_trademarks, which will call us back to
        # actually show the parts of the strings
        $me->fixup_trademarks($str, $ul, $url, %opts);
        return;
    }
    if ($me->{'changedfont'}) {
	$me->set_font_really();
	$me->{'changedfont'} = 0;
    }
    if ($me->{'mode'} eq 'PDF') {
        $str =~ s/\\(\d{3})/chr(oct($1))/ego if $opts{'doencode'};
        $me->{'content'}->textlabel($me->{'text_x'}, $me->{'text_y'},
                                    $me->{'currfont'}, $me->{'fontsize'},
                                    $str, $ul ? (-underline => [ 'auto', $ul]) : (), %opts); 
        if (defined($url) && $url ne '') {
            my $ant = $me->{'page'}->annotation;
            if (::isa($url, 'PDF::API2::NamedDestination')) {
                $ant->dest($url);
            } else {
                $ant->url($url);
            }
            # Adding mx and my (current total translation) is necessary
            # because the annotation rectangles are rendered _after_ all the
            # graphics states are un-done.
            my @rect = (
                        $me->{'mx'} + $me->{'text_x'},  # llx
                        $me->{'my'} + $me->{'text_y'} + $me->string_Descender,  # lly
                        $me->{'mx'} + $me->{'text_x'} + $me->stringwidth($str), # urx
                        $me->{'my'} + $me->{'text_y'} + $me->string_CapHeight);# + $me->string_Descender); # ury
            $ant->rect(@rect);
            #$ant->border(0.1, 0.1, 0.5); # YYY Only for debugging!
            $ant->content($str);
        }
	$me->{'text_x'} += $me->stringwidth($str);
    } else {
	$str =~ s/([(\\%)])/\\$1/g unless $opts{'noescape'};
#	$me->{'output'} .= "($str \(".sprintf("%.2f", $me->{'fontsize'})."\)) show\n";
        if (exists($opts{-render}) && $opts{-render} == 1) {
            $me->{'output'} .= "($str) false charpath stroke\n";
        } else {
            $me->{'output'} .= "($str) show\n";
        }
	if ($ul) {
	    # Do an underline
	    $me->gsave();
	        $me->{'output'} .= "$ul setlinewidth\n";
	        $me->{'output'} .= "0 -1 rmoveto\n";
	        $me->{'output'} .= "($str) stringwidth\n";
	        $me->{'output'} .= "exch neg exch rlineto stroke\n";
	    $me->grestore();
	}
    }
}

sub continue_text {
    my ($me, $str, $ul, $url, %opts) = @_;

    # This is probably wrong for PDF
    $me->{'text_y'} -= $me->{'fontsize'};

    if ($me->{'mode'} eq 'PS') {
        $me->set_text_pos($me->{'text_x'}, $me->{'text_y'});
    }
    $me->show($str, $ul, $url, %opts);
}

sub show_center {
    my ($me, $str, $ul, $url, %opts) = @_;

    my $x = $me->{'text_x'};
    my $y = $me->{'text_y'};
    my $oldx = $x;

    $x -= $me->stringwidth($str)/2;

    $me->set_text_pos($x, $y);
    $me->show($str, $ul, $url, %opts);
    $me->{'text_x'} = $oldx;
}

sub linkto_center {
    my ($me, $str, $ul, %opts) = @_;
    if (exists($opts{'isbench'}) && $opts{'isbench'}) {
        return $me->show_center($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/${str}.html", %opts);
    } else {
        my $anchor = ::makeanchor($opts{'section'}.$str);
        $anchor =~ s/(\S)continued$/$1/i; # Always goes to the same place anyway

        # Relative file links do not "just work".  In fact, they don't work.  So
        # everyone will get to visit SPEC for the answers!
        return $me->show_center($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#$anchor", %opts);
    }
}


sub continue_text_center {
    my ($me, $str, $ul, $url, %opts) = @_;
    $me->{'text_y'} -= $me->{'fontsize'};
    $me->show_center($str, $ul, $url, %opts);
}

sub show_right {
    my ($me, $str, $ul, $url, %opts) = @_;

    my $x = $me->{'text_x'};
    my $y = $me->{'text_y'};
    my $oldx = $x;


    $x -= $me->stringwidth($str);

    $me->set_text_pos($x, $y);
    $me->show($str, $ul, $url, %opts);
    $me->{'text_x'} = $oldx;
}

sub linkto_right {
    my ($me, $str, $ul, %opts) = @_;
    if (exists($opts{'isbench'}) && $opts{'isbench'}) {
        return $me->show_right($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/${str}.html", %opts);
    } else {
        my $anchor = ::makeanchor($opts{'section'}.$str);
        $anchor =~ s/(\S)continued$/$1/i; # Always goes to the same place anyway

        # Relative file links do not "just work".  In fact, they don't work.  So
        # everyone will get to visit SPEC for the answers!
        return $me->show_right($str, $ul, "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#$anchor", %opts);
    }
}

sub continue_text_right {
    my ($me, $str, $ul, $url, %opts) = @_;
    $me->{'text_y'} -= $me->{'fontsize'};
    $me->show_right($str, $ul, $url, %opts);
}

sub moveto {
    my ($me, $x, $y) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->move($x, $y);
    } else {
	$me->{'output'} .= "$x $y moveto\n";
    }
}

sub lineto {
    my ($me, $x, $y) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->line($x, $y);
    } else {
	$me->{'output'} .= "$x $y lineto\n";
    }
}

sub rlineto {
    my ($me, $x, $y) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->line($me->{'x'} + $x, $me->{'y'} + $y);
    } else {
	$me->{'output'} .= "$x $y rlineto\n";
    }

    $me->{'x'} += $x;
    $me->{'y'} += $y;
}

sub setdash {
    my ($me, $spacing) = @_;

    if (ref($spacing) ne 'ARRAY') {
	if ($spacing+0) {
	    $spacing = [ $spacing ];
	} else {
	    $spacing = [ ];
	}
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->linedash(@{$spacing});
    } else {
	$me->{'output'} .= "[ ".join(' ', @{$spacing})." ] 0 setdash\n";
    }
}

sub curveto {
    my ($me, $x1, $y1, $x2, $y2, $x3, $y3) = @_;

    $me->{'x'} = $x3;
    $me->{'y'} = $y3;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->curve($x1, $y1, $x2, $y2, $x3, $y3);
    } else {
	$me->{'output'} .= "$x1 $y1 $x2 $y2 $x3 $y3 curveto\n";
    }
}

sub circle {
    my ($me, $x, $y, $r) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    if ($me->{'mode'} eq 'PDF') {
        $me->{'pdf'}->circle($x, $y, $r);
    } else {
        $me->{'output'} .= "newpath $x $y $r 0 360 arc closepath\n";
    }
}

sub circle_fill {
    my ($me, $x, $y, $r, $gray, $width, $do_line) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->circle($x, $y, $r);
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
	    $me->circle($x, $y, $r);
	    $me->stroke();
        $me->grestore();
    }
}

sub translate {
    my ($me, $x, $y) = @_;

    return if ($x == 0 && $y == 0);

#print "${indent}$x $y translate at line ".(caller)[2]."\n";
    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->transform(-translate => [ $x, $y ]);
    } else {
	$me->{'output'} .= "$x $y translate\n";
    }
    $me->{'mx'} += $x;
    $me->{'my'} += $y;
}

sub rect {
    my ($me, $x, $y, $w, $h) = @_;

    $me->{'x'} = $x;
    $me->{'y'} = $y;

    my $nh = -$h;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->rect($x, $y, $w, $h);
    } else {
	$me->{'output'} .= "$x $y moveto 0 $h rlineto $w 0 rlineto 0 $nh rlineto closepath\n";
    }
}

sub rectxy {
    my ($me, $x1, $y1, $x2, $y2) = @_;

    # Draw a rectange with opposite corners at (x1,y1) and (x2,y2)
    $me->rect($x1, $y1, ($x2 - $x1), ($y2 - $y1));
}

sub rect_fill {
    my ($me, $x, $y, $w, $h, $gray, $width, $do_line) = @_;
#print "rect_fill($me, $x, $y, $w, $h, $gray, $width, $do_line)\n";
    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->rect($x, $y, $w, $h);
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
	    $me->rect($x, $y, $w, $h);
	    $me->stroke();
        $me->grestore();
    }
}

sub rectxy_fill {
    my ($me, $x1, $y1, $x2, $y2, $gray, $width, $do_line) = @_;

    # Draw & fill a rectange with opposite corners at (x1,y1) and (x2,y2)
    $me->rect_fill($x1, $y1, ($x2 - $x1), ($y2 - $y1), $gray, $width);
}

sub poly {
    my ($me, @coords) = @_;

    my ($x, $y) = @coords[0,1];
    return unless defined($x) && defined($y);

    # Snip dangling ordinate, if any
    if ($#coords % 2 == 0) {
        splice(@coords,-1);
    }

    my ($lastx, $lasty) = @coords[$#coords-1, $#coords];

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->poly(@coords);
    } else {
	($x, $y) = splice(@coords, 0, 2);
	return unless defined($x) && defined($y);
	$me->moveto($x, $y);
	while (@coords) {
	    ($x, $y) = splice(@coords, 0, 2);
	    last if (!defined($x) || !defined($y));
	    $me->lineto($x, $y);
	} 
    }
    $me->closepath if (($lastx != $me->{'x'}) || ($lasty != $me->{'y'}));
}

sub poly_fill {
    my ($me, $coords, $gray, $width, $do_line) = @_;
    return unless ref($coords) eq 'ARRAY';

    my ($x, $y) = @{$coords}[0,1];
    return unless defined($x) && defined($y);

    $me->{'x'} = $x;
    $me->{'y'} = $y;
    $me->gsave();
	$me->setgray($gray)       if defined $gray;
	$me->poly(@{$coords});
	$me->fill();
    $me->grestore();
    if ((defined($do_line) && $do_line) || defined($width)) {
        $me->gsave();
	    $me->setlinewidth($width) if defined $width;
            $me->poly(@{$coords});
            $me->stroke();
	$me->grestore();
    }
}

sub setgray {
    my ($me, $gray) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->setrgbcolor($gray, $gray, $gray);
    } else {
	$me->{'output'} .= "$gray setgray\n";
    }

    $me->{'color'} = $gray;
}

sub setrgbcolor {
    my ($me, $red, $green, $blue) = @_;

    $me->{'color'} = [ $red, $green, $blue ];

    if ($me->{'mode'} eq 'PDF') {
        my $colorstr = sprintf("#%02x%02x%02x",
                               $red * 255, $green * 255, $blue * 255);
	$me->{'content'}->fillcolor($colorstr);
	$me->{'content'}->strokecolor($colorstr);
    } else {
	$me->{'output'} .= "$red $green $blue setrgbcolor\n";
    }
}

sub setlinewidth {
    my ($me, $w) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->linewidth($w);
    } else {
	$me->{'output'} .= "$w setlinewidth\n";
    }
}

sub rotate {
    my ($me, $w) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->transform(-rotate => $w);
    } else {
	$me->{'output'} .= "$w rotate\n";
    }
}

sub fill {
    my ($me) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->fill;
    } else {
	$me->{'output'} .= "fill\n";
    }
}

sub stroke {
    my ($me) = @_;

    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->stroke;
    } else {
	$me->{'output'} .= "stroke\n";
    }
}

sub closepath {
    my ($me) = @_;

    if ($me->{'mode'} ne 'PDF') {
	$me->{'output'} .= "closepath\n";
    }
}

my @state_vars = qw(color x y text_x text_y fontsize fontname);
sub gsave {
    my ($me) = @_;
#print "${indent}gsave at line ".(caller)[2]."\n"; $indent .= '  ';
    my $state = {};
    for (@state_vars) {
	$state->{$_} = $me->{$_};
    }
    push (@{$me->{'states'}}, $state);
    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->save;
    } else {
	$me->{'output'} .= "gsave\n";
    }
    push @{$me->{'saves'}}, [ caller() ];
    push @{$me->{'possaves'}}, [ $me->{'mx'}, $me->{'my'} ];
}

sub grestore {
    my ($me) = @_;
#substr($indent, 0, 2) = ''; print "${indent}grestore at line ".(caller)[2]."\n";
    my $state = pop(@{$me->{'states'}});
    for (keys %$state) {
	$me->{$_} = $state->{$_};
    }
    if ($me->{'mode'} eq 'PDF') {
	$me->{'content'}->restore;
    } else {
	$me->{'output'} .= "grestore\n";
    }

    # Reset the font, as it looks like PDF doesn't handle this as part of
    # restore, and it doesn't hurt PostScript either...
    $me->set_font($me->{'fontname'}, $me->{'fontsize'}, 1);

    pop @{$me->{'saves'}};
    my $posref = pop @{$me->{'possaves'}};
    $me->{'mx'} = $posref->[0];
    $me->{'my'} = $posref->[1];
}

sub get_saves {
    my ($me) = @_;

    if (@{$me->{'saves'}}) {
	return @{$me->{'saves'}};
    } else {
	return ();
    }
}

sub add_raw {
    my ($me, $filename, $compression, $rawlines) = @_;

    # Add the raw file in.  This must happen before everything is closed out,
    # because when the PDFs are compressed, text after the %%EOF chokes
    # some interpreters.
    my $rawfilestring = "% The following is an encoded version of the raw file that was used to produce\n% this result.  Use the extract_raw script to retrieve it.\n% BEGIN";
    $rawfilestring .= "$compression $filename\n";
    $rawfilestring .= '% '.join("\n% ", split("\n", $rawlines))."\n";
    $rawfilestring .= "% END\n";
    if ($me->{'mode'} eq 'PS') {
        $me->{'output'} .= $rawfilestring;
    } elsif ($me->{'mode'} eq 'PDF') {
        # Fake up an XML container for data that the tools will still be able
        # to decode.
        my $xml = "<rawfile name=\"$filename\">\n";
        $xml .= "<content>\n$rawfilestring\n</content>\n</rawfile>\n";
        $me->{'pdf'}->xmpMetadata($xml);
    } else {
        # Do nothing; this shouldn't happen!
    }
}

sub trademarks_done {
    my ($me) = @_;
    return %{$me->{'tm_doc_done'}};
}

# Look in the input string for trademarks and mark them up as appropriate.
# Also keep track of which ones were used so that they can be mentioned in
# the result page footer.
sub fixup_trademarks {
  my ($me, $str, $ul, $url, %opts) = @_;
#print "fixup_trademarks($me, str=\"$str\", ul=$ul, url=\"$url\",opts=(\n    ".join(",\n    ", map { "$_ => $opts{$_}" } sort keys %opts)."\n   ))\n";

  $opts{'tmsearch'} = 0;

  foreach my $tm (sort { length($b) <=> length($a) } keys %::trademarks) {
    next if exists($me->{'tm_done'}->{$tm});
    my $tmre = qr/^(.*?\b${tm})((?=[^a-zA-Z])|\b)/;
    my $tmcode = $::trademarks{$tm};
    if (isa($::trademarks{$tm}, 'ARRAY')) {
        if ($::trademarks{$tm}->[1]) {
            $tmre = qr/^(.*?\b${tm})/;
            $tmcode = $::trademarks{$tm}->[0];
        }
    }
    if ($str =~ s/$tmre//) {
        my $prestr = $1;
        $me->show($prestr, $ul, $url, (%opts, 'tmsearch' => 0));
        $me->{'text_x'} += $me->stringwidth($prestr) unless $me->{'mode'} eq 'PDF';
        my $tmstring = $code2mark{$tmcode};
        my @curfont = $me->get_font();
        my $y = $me->{'text_y'};
        my $delta = $me->string_XHeight() / 3 * 2;
        if ($tmstring =~ /^\\/) {
            $me->set_font('Symbol', $curfont[1]/2);
        } else {
            $me->set_font($curfont[0], $curfont[1]/2);
        }
        $me->set_text_pos($me->{'text_x'}, $y + $delta);
        $me->show($tmstring, $ul, undef, (%opts, 'noescape' => 1, 'doencode' => 1));
        if ($me->{'mode'} eq 'PS') {
            # In PostScript, the stringwidth() sub is just a guess for the
            # Symbol font, and it's wrong. :)  So we'll let the interpreter
            # handle it...
            $me->{'output'} .= "($tmstring) stringwidth $delta neg rmoveto\n";
        } else {
            $me->set_text_pos($me->{'text_x'}, $y);
        }
        $me->set_font(@curfont);
        $me->{'tm_done'}->{$tm}++;
        $me->{'tm_doc_done'}->{$tm}++;
    }
  }
  if ($str ne '') {
      $me->show($str, $ul, $url, %opts);
  }
}

