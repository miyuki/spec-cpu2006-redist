#
#  mail.pl - mails result files to configurable addresses
#  Copyright 2004-2011 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Cloyce D. Spradling
#
# $Id: mail.pl 6710 2011-08-05 21:53:46Z CloyceS $

use strict;

use Mail::Mailer;
use MIME::Entity;
use IO::Compress::Bzip2 qw(:all);
use IO::File;
use File::Basename;

use vars qw($name $extension $synonyms);

$name      = 'mail';
$extension = undef;
$synonyms  = { map { lc($_) => 1 } ($name, qw(mailto email)) };

$Spec::Format::mail::non_default = 1;       # You must ask for it by name
my $mail_version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$mail_version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'mail.pl'} = $mail_version;

sub format {
    my($me, $r, $fn, $files) = @_;

    my @nc = ::allof($r->{'nc'});

    if (!defined($r->mail->{'mailto'}) || $r->mail->{'mailto'} eq '') {
	return (undef, undef);
    }

    # Squeeze undefined values from the file list
    $files = [ grep { defined($_) } @{$files} ];

    my $m = (@{$files} > 1) ? 's' : '';
    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));

    # Make a summary list of success codes
    my %codes = ();
    for my $bench (sort keys %{$r->{'results'}}) {
      my $benchres = $r->{'results'}{$bench};
      for my $tune (sort keys %{$r->{'results'}{$bench}}) {
        $benchres = $r->{'results'}{$bench}{$tune}{'data'};
        my $tmp;
        for my $res (@{$benchres}) {
          $codes{$res->valid}++;
        }
      }
    }

    # Now construct the subject
    my $subject = '';
    if (keys %codes) {
      # Likely
      $subject = '['.join(', ', map { "$codes{$_} $_" } sort bycode keys %codes).'] ';
    }
    if (::istrue($::global_config->reportable)) {
      # John suggests only shouting if the result was supposed to be reportable
      $subject .= 'INVALID ' if $invalid;
      $subject .= $r->metric;
      $subject .= ' RATE' if ::istrue($r->rate);
      $subject .= ' RESULTS ';
    } else {
      $subject .= $r->metric;
      $subject .= ' rate' if ::istrue($r->rate);
      $subject .= ' results ';
      $subject .= '(invalid for publication) ' if $invalid;
    }
    $subject .= "(${main::lcsuite}test #".$r->mail->{'lognum'}.')';

    # Make the list of recipients
    my @to = split(/[,\s]+/, $r->mail->{'mailto'});

    # Start the top-level entity
    my $top = MIME::Entity->build(From       => $r->mail->{'username'}.'@localhost',
				  To         => [ @to ],
				  Subject    => $subject,
				  Type       => 'multipart/mixed',
				  'X-Mailer' => "SPEC ${main::suite} tools (mailto formatter $mail_version)");

    # Attach a short description
    $top->attach(Data    => [
"This message contains the result file$m from ${main::suite} run #".$r->mail->{'lognum'}.".\n",
"The runs were of ".$r->metric." using the ".$r->size." workload.\n",
"\n",
"runspec: ".$r->mail->{'runspec_argv'}."\n",
"\n",
"A results summary follows:\n",
map { "$_\n" } Spec::Format::asc::screen_format($me, $r, $fn, 0, !!$invalid, \@nc),
"\n",
"--------------------------------------------------------------------------\n",
"This mail was generated on ".scalar(localtime)."\n",
"\n",
			     ],
		 Encoding => '-SUGGEST',
		 );

    # Figure out which files we're going to want.
    my @want_extensions;
    if ($r->mail->{'mail_reports'} =~ /\ball\b/) {
      @want_extensions = ();
    } else {
      foreach my $report (split(/[,\s]+/, $r->mail->{'mail_reports'})) {
        if (lc($report) eq 'log') {
          push @want_extensions, '.log';
        } else {
          my $e = ::get_format($::global_config->formats, $report)->{'extension'};
          push @want_extensions, '.'.$e if (defined($e));
        }
      }
    }

    # Now attach each of the files
    my $count = 0;
    foreach my $file (@{$files}) {
        if (@want_extensions) {
          # Check to see if the current file is one that we're interested in
          next unless grep { $file =~ /\Q$_\E$/ } @want_extensions;
        }
        $count++;
	if (!::istrue($r->mail->{'compress'})) {
	    $top->attach(Encoding => '-SUGGEST',
			 Path     => $file);
	} else {
	    # Compress it and send it along
	    my $ifh = new IO::File '<'.$file;
	    next unless defined($ifh);
	    my $oldeol = $/;
	    undef $/;
            my $compressed;
            my $status = bzip2 $ifh => \$compressed;
            next unless $status;
	    $top->attach(Encoding => 'Base64',
			 Type     => 'application/x-bzip2',
			 Data     => $compressed,
			 Filename => basename($file).'.bz2');
	    $/ = $oldeol;
	}
    }
    return ([], undef) unless $count > 0; # Don't send empty mail

    $top->sync_headers(Length => 'COMPUTE');

#    print "\nFrom cloyce Tue Sep  7 11:28:42 1999\n";
#    $top->print(\*STDOUT);

    # Make sure the method is a sane one
    if ($r->mail->{'mailmethod'} !~ /^(?:mail|sendmail|smtp|test)$/oi) {
	::Log(0, "\nERROR: Mail method must be 'smtp' or 'sendmail'\n");
	return ([], undef);
    }

    my @args = (lc($r->mail->{'mailmethod'}));
    if ($args[0] eq 'smtp') {
	push @args, ('Server' => $r->mail->{'mailserver'},
		     'Port'   => $r->mail->{'mailport'},
		     'Debug'  => $Spec::Format::mail::debug,
		     );
    } elsif ($args[0] eq 'sendmail') {
	if (-x $r->mail->{'sendmail'}) {
	    push @args, $r->mail->{'sendmail'};
	} else {
	    ::Log(0, "\nERROR: The sendmail program specified (".$r->mail->{'sendmail'}.") does not exist or is not executable.");
	    return (undef, undef);
	}
    }

    my $mailer;
    eval '$mailer = new Mail::Mailer @args;';
    if ($@) {
      ::Log(0, "\nERROR: When attempting to instantiate a mailer object, Mail::Mailer returned:\n");
      ::Log(0,"       $@\n");
      return (undef, undef);
    }
    eval '$mailer->open($top->head->header_hashref);';
    if ($@) {
      if (lc($r->mail->{'mailmethod'}) eq 'smtp' && $@ =~ /Invalid argument/i) {
        # It's likely that there's no mail server listening at the specified
        # address
        ::Log(0, "\nERROR: Mailer could not be opened.\n");
        ::Log(0, "      Is the SMTP server at ".$r->mail->{'mailserver'}.':'.$r->mail->{'mailport'}." off or unreachable?\n");
      } else {
        ::Log(0, "\nERROR: When attempting to open the mailer, Mail::Mailer returned:\n");
        ::Log(0,"       $@\n");
      }
      return (undef, undef);
    }
    print $mailer $top->stringify_body;
    $mailer->close();

    return ([], [@to]);
}

sub bycode {
  # Sort return codes with 'S' first
  return -1 if $a eq 'S';
  return 1 if $b eq 'S';
  return $a cmp $b;
}
