#
# log_common.pl
#
# Copyright 1999-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: log_common.pl 6710 2011-08-05 21:53:46Z CloyceS $

use strict;
use Time::HiRes;
use UNIVERSAL qw(isa);

my $version = '$LastChangedRevision: 6710 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'log_common.pl'} = $version;

sub clear_log_state {
    @::saved_logs = ();    # Things logged before the log file was opened
    @::all_log = ();       # Everything logged before the log file was opened
    %::partial_lines = (
                        'screen' => [ undef, 0, 0 ],
                        'log'    => [ undef, 0, 0 ],
                        'all'    => [ undef, 0, 0 ],
                       );

    $::log_opened = 0;
    $::log_to_screen = 1;
}

clear_log_state();

## ############
## SUB                   LOG
## ############

## The first argument is the log level associated with the call.
## The following arguments hold the information that needs to be
## logged. The global $verbose level needs to be set to determine
## if the message is to be logged.

sub Log {
    # The multi-level multi-type log writer.  The log statement will output
    # if the global verbosity level is equal to or greater than the level
    # associated with the call to Log. There are messages that are
    # formatted both for the screen and (separately) for the log file.  All
    # messages with a level greater than 100 are sent to the log file (if
    # any) regardless of the verbosity level.

    my ($level, @data) = @_;
    my ($type, @output);
    my $verbose    = $main::global_config->verbose;
    my $line_width = $main::global_config->line_width;
    my $log_line_width = $main::global_config->log_line_width;
    my $timestamp = $main::global_config->log_timestamp;
    my $log_output    = '';
    my $all_log       = '';
    my $screen_output = '';
    my $logfile = 0;
    foreach my $data (@data) {		## handle each data type reference
	if (isa($data, 'ARRAY')) {## arrays referenced are joined w/o separator
				  ## and then appended to the output
	    $data = join("", @{$data});
	} elsif (isa($data, 'SCALAR')) { ## scalars reference are just appended
	    $data = ${$data};
	}
	if ($level >= 100) {
	    $level   -= 100;
	    $logfile  = 1;
	}
	$all_log       .= $data;
	$log_output    .= $data if (($verbose >= $level) || $logfile);
	$screen_output .= $data if ($verbose >= $level && $::log_to_screen);
    }

    if ($timestamp) {
        $timestamp = sprintf '%.2f', Time::HiRes::time;
    } else {
        $timestamp = undef;
    }

    # Handle debug log output
    if ($all_log ne '') {       # This should always be true
        $all_log = split_line('all', $all_log, $log_line_width, 0, sprintf('%.2f', Time::HiRes::time));
        if ($::log_opened != 0) {
            $::all_log_handle->print($all_log);
        } elsif ($all_log eq "\n" || $all_log !~ /^\s*$/) {
            push @::all_log, $all_log;
        }
    }

    # Handle screen output
    if ($screen_output ne '') {
        print split_line('screen', $screen_output, $line_width, 0, undef);
    }

    # Handle normal log output
    if ($log_output ne '') {
        $log_output = split_line('log', $log_output, $log_line_width, 0, $timestamp);
        if ($::log_opened != 0) {
            $::log_handle->print($log_output);	  ## log file
        } elsif ($log_output eq "\n" || $log_output !~ /^\s*$/) {
            push @::saved_logs, $log_output;
        }
    }

}

sub split_line {
  my ($dest, $line, $width, $addpid, $time) = @_;
  my $duration = 0;

  if (exists($::partial_lines{$dest}) && isa($::partial_lines{$dest}, 'ARRAY')
      && defined($::partial_lines{$dest}->[0])) {
    if (defined($time) && defined($::partial_lines{$dest}->[1])) {
        $duration = $time - $::partial_lines{$dest}->[1];
    }
    $line = $::partial_lines{$dest}->[0].$line;
    $::partial_lines{$dest} = [ undef, 0, 0 ];
  }

  # log line wrapping
  my @output = split ("\n", $line, -1);
  my @log_output = ();

  # Save a trailing newline, if any
  $line =~ s/.*?(\n)?$/$1/s;

  if ($width <= 0) {
    @log_output = @output;
  } else {
    # Split things up
    for (@output) {		
        while ($_) {
            push @log_output, substr($_, 0, $width);
            substr($_, 0, $width) = '';
        }
    }
  }

  # Add the trailing newline (if any) back in
  if (@log_output) {
    if ($line ne '') {
      # There was a newline on the end, so this will be a complete line.
      $log_output[$#log_output] .= $line if ($width > 0);
    } else {
      # No newline means that we need to save the last "line" (actually
      # line fragment) in partial_lines so that its length can be properly
      # accounted for against the width constraints next time around.
      # If there _is_ no "next time around" it'll get kicked out by
      # close_log; it's always guaranteed to be less than width characters
      # anyway.
      $::partial_lines{$dest} = [ pop(@log_output), $time - $duration, $duration ];

      # Ensure that the final line has a newline attached; if there was
      # a part without a newline, it's now in partial_lines.
      $log_output[$#log_output] =~ s/([^\n])$/$1\n/ if (@log_output);
    }
  } elsif ($line ne '') {
    # This just handles the special case of a single newline
    @log_output = ($line);
  }

  # Put it all back together
  @log_output = map { if ($_ ne '') { "$$: $_" } else { $_ } } @log_output if $addpid && @log_output;
  @log_output = map { if ($_ ne '') { "$time: $_" } else { $_ } } @log_output if defined($time) && @log_output;
  if (int($duration * 100) > 0) {
      # Add it to the first line of output, as it's left over from a previous
      # partial line
      my $newtime = sprintf '%.2f(%.2fs)', $time - $duration, $duration;
      $log_output[0] =~ s/^$time: /$newtime: /;
  }
  return join("\n", @log_output);
}

1;
