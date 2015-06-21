#
# vars_common.pl
#
# Copyright 1995-2011 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: vars_common.pl 6713 2011-08-06 02:25:55Z CloyceS $

##############################################################################
# Initialize Variables
##############################################################################

use strict;
use Config;
use Cwd;
use UNIVERSAL;

# Customize the name of the suite and the multipliers here
$::suitebase = 'CPU';
$::year = '2006';
$::release_year = 2006;   # Or just set to $::year
$::suite = $::suitebase.$::year;
$::current_version = 1.2; # YYY - version: Adjust by hand
$::current_runspec = 6674; # YYY - revision: Adjust by hand
$::treeowner = 'cloyce';    # Mostly for cleanuptree
$::treegroup = 'cpudevel';  # Mostly for cleanuptree
$::suite_version = 0;   # Just a placeholder.  MUST BE 0!

my $version = '$LastChangedRevision: 6713 $ '; # Make emacs happier
$version =~ s/^\044LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'vars_common.pl'} = $version;

# lcsuite should match the name of at least one of your benchsets.  If not,
# be sure to edit expand_all in util.pl to do the right thing.
$::lcsuite = lc($::suite);

# Ratio multipliers vs. reference machine
if ($::suite =~ /CPU(?:2006|v6)/) {
  $::speed_multiplier = 1;	# See osgcpu-14317
  $::rate_multiplier = 1;	# See osgcpu-14317
} else {
  $::speed_multiplier = 1;	# A reasonable default
  $::rate_multiplier = 1;	# A reasonable default
}

# Public location of benchmark and suite flag descriptions
$::spec_flags_base = "http://www.spec.org/auto/$::lcsuite/flags/";

# Config items that may not be changed by the user
$::nonvolatile_config = {
    'benchdir'   => 'benchspec',     # benchmark directory
    'resultdir'  => 'result',        # result directory
    'configdir'  => 'config',        # configuration directory
    'log'        => $::suite,        # extra error logging file
    'dirprot'    => 0777,            # directory protection
    'rundir'     => 'run',           # Where to actually do the runs
    'worklist'   => 'list',          # List of runs in rundir
    'exitvals'   => 'spec_exit',     # File containing return codes
    'datadir'    => 'data',          # directory containing input sets
    'inputdir'   => 'input',         # dir under datadir containing input
    'outputdir'  => 'output',        # dir under datadir containing output
    'reftime'    => 'reftime',       # file containing reference for input
    'bindir'     => 'exe',           # directory containing executables
    'srcdir'     => 'src',           # directory containing source
    'specmake'   => 'Makefile.YYYtArGeTYYYspec', # Override file for Makefile
    'specdiff'   => 'specdiff',      # Number of lines of difference
    # Minimum number of runs to be valid
    'min_report_runs' => ($::lcsuite =~ /^(mpi2007|omp2001|omp2012)$/) ? 2 : 3,
    'orig_argv'  => [ @ARGV ],
    'orig_env'   =>  { %ENV },
    'help'       => 0,
    'version'    => 0,
    'output'     => 'asc',
    'valid_actions'    => [ qw(
                               buildsetup runsetup setup
                               build only_run onlyrun
                               run validate
                               configpp
                               clean realclean trash clobber scrub
                               interclean
                               report
                               makebundle usebundle
                              ) ],
    'valid_tunes'      => [ qw(base peak) ],
    'vendor_makefiles' => 0,
    'specrun'          => 'specinvoke',
    'check_integrity'  => 1,
    'flag_url_base'    => $::spec_flags_base,
    'realuser'         => 'your name here',
    'default_size'     => 'ref',              # Default size or class of input
};

$::default_config = {
    'build_in_build_dir'  => 1,               # Whether to build in builddir
    'builddir'            => 'run',           # Where to do the builds
    'compile_error'       => 0,               # had problems compiling?
    'setup_error'         => 0,               # had problems during setup?
    'reportable'          => 0,               # run follows all reporting rules
    'check_md5'           => 1,               # check saved md5 sums
    'prefix'              => '',              # prefix to prepend to files
    'sigint'              =>  undef,          # Signal # of SIGINT
    'ignore_sigint'       =>  0,              # Ignore SIGINT
    'make'                => 'specmake',      # Name of make executable (override in benchmark.pm)
    'vendor'              => 'anon',          # Vendor/Version for filenames
    'action'              => 'validate',      # Default action
    'run'                 => 'all',           # What benchmarks to run
    'config'              => 'default.cfg',   # configuration file
    'mach'                => 'default',       # Vendor/Version for filenames
    'ext'                 => 'none',          # default extension for executable
    'size'                => $::nonvolatile_config->{'default_size'},
    'tune'                => 'base',          # default tuning level
    'output_format'       => 'default',       # output format
    'calctol'             => 0,               # calculate tolerances
    'rawformat_opts'      => [],              # options to rawformat
    'nc'                  => '',              # format as non-compliant
    'nc_is_na'            => 0,               # format as not available
    'nc_is_cd'            => 0,               # format as code defect
    'srcalt'              => '',              # approved source mods
    'fake'                => 0,               # send -n to spec{make,invoke}
    'expand_notes'        => 0,               # do var expansion in notes
    'max_active_compares' => 0,               # Max # of parallel compares
    'difflines'           => 10,              # Number of lines of difference
    'endian'              => $Config{'byteorder'},
    'ignore_errors'       => 0,               # Ignore certain errors
    'mean_anyway'         => 0,               # Calculate mean even if invalid
    'setprocgroup'        => 1,               # Set the process group
    'verbose'             => 5,               # Verbosity level
    'deletework'          => 0,               # Delete existing working dirs
    'deletebinaries'      => 0,               # Delete existing binaries
    'rate'                => 0,     # Throughput run (or convert speed to rate)
    'speed'               => 0,               # Convert 1-copy rate to speed
    'unbuffer'            => 1,               # Unbuffer STDOUT
    'line_width'          => 0,               # line wrap width
    'log_line_width'      => 0,               # line wrap width for logfile
    'log_timestamp'       => 0,               # timestamp log lines?
    'feedback'            => 1,               # Default to allow feedback
    'copies'              => 1,               # Number of copies
    'ranks'               => 1,               # Number of ranks (MPI only)
    'uid'                 => $<,              # User ID of the user
    'rebuild'             => 0,               # Rebuild binaries even if they
                                              # already exist
    'env_vars'            => 0,               # Allow environment to be
                                              # overriden by ENV_*
    'locking'             => 1,               # Try to lock files
    'absolutely_no_locking' => 0,             # Don't try to lock for run #
    'keeptmp'             => 0,               # Keep temporary files?
    'os_exe_ext'          => '',	      # Some OSes (NT) create
                                              # executables with specific
				              # extensions
    'makeflags'           => '',	      # Extra flags for make (like -j)
    'OS'                  => 'unix',	      # OS type
    'teeout'              => 0,               # Run output through 'tee' so
				              # you can see it on the screen
    'minimize_rundirs'    => 0,		      # Try to keep working disk size
				              # down as small as possible
    'minimize_builddirs'  => 0,		      # Try to keep working disk size
				              # down as small as possible
    'backup_config'       => 1,		      # Whether to keep backup config
				              # file left over from updating
    				              # MD5s
    'make_no_clobber'     => 0,               # Don't blow away directory when
				              # building executables
    'basepeak'            => 0,		      # Use base binary for peak
				              # measurements
                                              # Number of iterations to run
    'iterations'          => $::nonvolatile_config->{'min_report_runs'},
    'commandfile'         => 'speccmds.cmd',  # Name of command file
    'commanderrfile'      => 'speccmds.err',  # Name of command error file
    'commandstdoutfile'   => 'speccmds.stdout',# Name of command stdout file
    'commandoutfile'      => 'speccmds.out',  # Name of command output file
    'comparefile'         => 'compare.cmd',   # Name of compare file
    'compareerrfile'      => 'compare.err',   # Name of compare error file
    'comparestdoutfile'   => 'compare.stdout',# Name of compare stdout file
    'compareoutfile'      => 'compare.out',   # Name of compare output file
    'table'               => 1,               # Produce a table of results
    'safe_eval'           => 1,               # Very strict opcode mask for
				              # string expansion
    'section_specifier_fatal' => 1,           # Is a typo in a section specifier fatal?
    'delay'               => 0,	# Sleep a bit before and after each benchmark run?
    'command_add_redirect'=> 0,
    'use_submit_for_speed'=> ($::lcsuite =~ /^(mpi2007|omp2001|omp2012)$/) ? 1 : 0,
    'mailto'              => '',
    'mailserver'          => '127.0.0.1',
    'mailmethod'          => 'smtp',
    'mailport'            => 25,
    'mailcompress'        => 0,
    'sendmail'            => '/usr/sbin/sendmail',
    'mail_reports'        => 'all',
    'no_monitor'          => '',
    'plain_train'         => ($::lcsuite =~ /cpu(?:2006|v6)/) ? 1 : 0,
    'info_wrap_columns'   => 50,# Wrap non-notes at 50 columns by default
    'notes_wrap_columns'  => 0,	# Do not wrap notes lines by default
    'notes_wrap_indent'   => '  ', # Indent continuation 2 spaces
    'shrate'              => 0,	# Do a staggered homogenous rate run?
    'stagger'             => 10, # Stagger delay for the above
    'strict_rundir_verify'=> 1,  # Make sure that the MD5s of file in the
                                 # rundir match the MANIFEST _and_ the original
                                 # source file.
    # CVT2DEV: 'strict_rundir_verify' => 0,
    'sysinfo_program'     => 'specperl $[top]/Docs/sysinfo',
    'no_input_handler'    => 'close',
    'flagsurl'            => '',# Flags file URL
    'check_version'       => 1, # Check suite version at SPEC?
    # CVT2DEV: 'check_version' => 0,
                             # YYY - version #                          vvv
    'version_url'         => ($::current_version && $::current_version < 5) ?
                               "http://www.spec.org/auto/$::lcsuite/current_version" :
                               "http://www.spec.org/auto/$::lcsuite/devel_version",
    'http_timeout'        => 30, # How long to wait for HTTP responses
    'http_proxy'          => '', # HTTP proxy (if any)
    'update-flags'        => 0,
    'review'              => 0, # Format for review?
    'allow_extension_override' => 0,    # Ext with no settings okay?
    'output_root'         => '',
    'expid'               => '',
    'train_with'          => 'train',   # Workload to use for training runs
    'parallel_setup_type' => 'fork',
    'parallel_setup'      => 1,
    'parallel_setup_prefork' => '',
    'parallel_test'       => 0,
    'parallel_test_submit'=> 0,
    'parallel_test_workloads'=>'',
    'preenv'              => 1,
    'note_preenv'         => 0,         # Automatically turned on when needed
    'power'               => 0, # Do power measurement?
};

# This is the list of benchsets and benchmarks that will be used for the
# standalone result formatter at SPEC.  This should be updated whenever the
# *bset files change, and right before release.
%fm::bmarklist = ('int' => { 'name' => 'int',
                             'topdir' => 'CPU'.$::year,
                             'units' => 'SPECint',
                             'metric' => 'CINT'.$::year,
                             'speed_multiplier' => 1,
                             'rate_multiplier' => 1,
                             'ref' => 'ref',
                             'train' => 'train',
                             'test' => 'test',
                             'benchmarklist' => [
                             # CPU2006 INT benchset benchmark list begins here
                                 [ '400.perlbench',	'C'   ],
                                 [ '401.bzip2', 	'C'   ],
                                 [ '403.gcc',   	'C'   ],
                                 [ '429.mcf',   	'C'   ],
                                 [ '445.gobmk', 	'C'   ],
                                 [ '456.hmmer', 	'C'   ],
                                 [ '458.sjeng', 	'C'   ],
                                 [ '462.libquantum',	'C'   ],
                                 [ '464.h264ref',	'C'   ],
                                 [ '471.omnetpp',	'CXX' ],
                                 [ '473.astar', 	'CXX' ],
                                 [ '483.xalancbmk',	'CXX' ],
                                 [ '999.specrand',	'C'   ],
                             ],
                             'no_output' => { '999.specrand' => 1 },
			       },
		     'fp' => { 'name' => 'fp',
                               'topdir' => 'CPU'.$::year,
			       'units' => 'SPECfp',
			       'metric' => 'CFP'.$::year,
                               'speed_multiplier' => 1,
                               'rate_multiplier' => 1,
                               'ref' => 'ref',
                               'train' => 'train',
                               'test' => 'test',
			       'benchmarklist' => [
                               # CPU2006 FP benchset benchmark list begins here
                                   [ '410.bwaves',	'F'     ],
                                   [ '416.gamess',	'F'     ],
                                   [ '433.milc',	'C'     ],
                                   [ '434.zeusmp',	'F'     ],
                                   [ '435.gromacs',	'F,C'   ],
                                   [ '436.cactusADM',	'F,C'   ],
                                   [ '437.leslie3d',	'F'     ],
                                   [ '444.namd',	'CXX'   ],
                                   [ '447.dealII',	'CXX'   ],
                                   [ '450.soplex',	'CXX'   ],
                                   [ '453.povray',	'CXX'   ],
                                   [ '454.calculix',	'F,C'   ],
                                   [ '459.GemsFDTD',	'F'     ],
                                   [ '465.tonto',	'F'     ],
                                   [ '470.lbm', 	'C'     ],
                                   [ '481.wrf', 	'F,C'   ],
                                   [ '482.sphinx3',	'C'     ],
                                   [ '998.specrand',	'C'     ],
			       ],
                               'no_output' => { '998.specrand' => 1 },
			      },
		     );

%::mime2ext = (
                'image/x-coreldraw'        => 'cdr',
                'image/x-xpm'              => 'xpm',
                'image/x-3ds'              => '3ds',
                'image/x-portable-bitmap'  => 'xbm',
                'image/x-portable-greymap' => 'xgm',
                'image/x-portable-pixmap'  => 'xpm',
                'image/x-niff'             => 'niff',
                'image/tiff'               => 'tiff',
                'image/unknown'            => 'unknown',
                'image/gif'                => 'gif',
                'image/jpeg'               => 'jpg',
                'image/bmp'                => 'bmp',
                'image/png'                => 'png',
                'image/x-photoshop'        => 'psd',
                'image/x-djvu'             => 'djvu',
                'image/x.djvu'             => 'djvu',
                'application/excel'             => 'xls',
                'application/ichitaro4'         => 'jsw',
                'application/ichitaro5'         => 'jaw',
                'application/ichitaro6'         => 'jbw',
                'application/java'              => 'java',
                'application/java-archive'      => 'jar',
                'application/jar'               => 'jar',
                'application/mac-binhex40'      => 'hqx',
                'application/ms-tnef'           => 'tnef',
                'application/msaccess'          => 'mdb',
                'application/msword'            => 'doc',
                'application/octet-stream'      => 'unknown',
                'application/ogg'               => 'ogg',
                'application/pdf'               => 'pdf',
                'application/postscript'        => 'ps',
                'application/powerpoint'        => 'ppt',
                'application/vnd.rn-realmedia'  => 'ram',
                'application/x-123'             => '123',
                'application/x-arc'             => 'arc',
                'application/x-archive'         => 'arc',
                'application/x-arj'             => 'arj',
                'application/x-awk'             => 'awk',
                'application/x-bittorrent'      => 'torrent',
                'application/x-bzip2'           => 'bz2',
                'application/bzip2'             => 'bz2',
                'application/x-compress'        => 'Z',
                'application/compress'          => 'Z',
                'application/x-coredump'        => 'core',
                'application/x-cpio'            => 'cpio',
                'application/x-dbf'             => 'dbf',
                'application/x-dbm'             => 'dbm',
                'application/x-dbt'             => 'dbt',
                'application/x-debian-package'  => 'deb',
                'application/x-dosexec'         => 'exe',
                'application/x-dvi'             => 'dvi',
                'application/x-elc'             => 'elc',
                'application/x-executable'      => 'exe',
                'application/x-frame'           => 'fm',
                'application/x-gawk'            => 'awk',
                'application/x-gdbm'            => 'dbm',
                'application/x-gzip'            => 'gz',
                'application/gzip'              => 'gz',
                'application/x-iso9660'         => 'iso',
                'application/x-kdelnk'          => 'link',
                'application/x-lha'             => 'lha',
                'application/x-lharc'           => 'lha',
                'application/x-nawk'            => 'awk',
                'application/x-object'          => 'o',
                'application/x-perl'            => 'pl',
                'application/x-rar'             => 'rar',
                'application/x-realaudio'       => 'ra',
                'application/x-rpm'             => 'rpm',
                'application/x-sc'              => 'sc',
                'application/x-sharedlib'       => 'so',
                'application/x-shellscript'     => 'sh',
                'application/x-shockwave-flash' => 'swf',
                'application/x-stuffit'         => 'sit',
                'application/x-stuffitx'        => 'sit',
                'application/x-svr4-package'    => 'pkg',
                'application/x-tar'             => 'tar',
                'application/tar'               => 'tar',
                'application/x-tex-tfm'         => 'tfm',
                'application/x-zip'             => 'zip',
                'application/x-zip-compressed'  => 'zip',
                'application/zip'               => 'zip',
                'application/x-zoo'             => 'zoo',
                'text/plain'           => 'txt',
                'text/html'            => 'html',
              );

sub initialize_variables {
    my ($config) = @_;

    for (keys %$::default_config) {
	$config->{$_} = $::default_config->{$_};
    }
    for (keys %$::nonvolatile_config) {
	$config->{$_} = $::nonvolatile_config->{$_};
    }

    $config->{'runspec'} = join(' ', ($0, @ARGV));

    # If this is an invocation as a result of preenv usage, strip out those two
    # flags.  It wouldn't be reasonable for an end-user to specify both
    # --nopreenv and --note-preenv, so presence of both of those should be a
    # good indicator.
    if ($config->{'runspec'} =~ s/ --nopreenv --note-preenv//) {
        $config->{'orig_argv'} = [ grep { !m/^--(?:no|note-)preenv$/ } @{$config->{'orig_argv'}} ];
    }

    my $name = '';
    $name = $ENV{'SPECUSER'}     if ($name eq '') && (exists $ENV{'SPECUSER'});
    $name = $ENV{'USER'}         if ($name eq '') && (exists $ENV{'USER'});
    $name = $ENV{'USERNAME'}     if ($name eq '') && (exists $ENV{'USERNAME'});
    $name = $ENV{'LOGNAME'}      if ($name eq '') && (exists $ENV{'LOGNAME'});
    $name = eval q/getlogin || getpwuid $config->{'uid'}/ if ($name eq '');
    $name = $config->{'uid'}     if ($name eq '');
    $name = 'default'            if ($name eq '');
    $config->{'realuser'} = $name;      # Not mungable
    $config->{'username'} = $name;      # May be changed by command line

    # Check to see if OS was specified in environment
    $config->{'OS'} = lc($ENV{'OS'}) if exists($ENV{'OS'}) && ($ENV{'OS'} ne '');
    if ($config->{'OS'} =~ /^windows/) {
	$config->{'os_exe_ext'} = '.exe';
	$config->{'ignore_sigint'} = 1;
    }

    # See where the top of the SPEC tree is
    $config->{'top'} = $ENV{'SPEC'};
    $config->{'top'} = cwd if $ENV{'SPEC'} eq '' || ! -d $ENV{'SPEC'};
    $config->{'specrun'} = jp($config->top, $config->specrun);

    # Check to see if sigint is defined in the Config data
    {
	my @nums = split(" ", $Config{'sig_num'});
	my @names = split(" ", $Config{'sig_name'});
	while (@nums && @names) {
	    my $num = shift @nums;
	    my $name = shift @names;
	    if ($name eq 'INT') {
		$config->{'sigint'} = $num;
		last;
	    }
	}
    }
}

sub finalize_config {
    my ($config, $cl_opts) = @_;
    # Command line options override config file options

    # set rate to 0 if --speed is set on the command line.
    if (defined($cl_opts->{'speed'})) {
	$config->{rate} = 0;
	$config->{copies} = 1;
    }

    for ( keys %$cl_opts) {
	next if $_ eq 'ref' || $_ eq 'refs';
	$config->{$_} = $cl_opts->{$_};
    }

    # Make sure none of the unchangeble constants changed
    for ( keys %$::nonvolatile_config) {
	$config->{$_} = $::nonvolatile_config->{$_};
    }
}

sub suiteinfo {
    # Output suitable for Bourne shell eval
    print "suite='$::suite'\n";
    print "lcsuite='$::lcsuite'\n";
    print "suitebase='$::suitebase'\n";
    print "year='$::year'\n";
    print "release_year='$::release_year'\n";
    print "current_version='$::current_version'\n";
    print "current_runspec='$::current_runspec'\n";
    print "treeowner='$::treeowner'\n";
    print "treegroup='$::treegroup'\n";
}

1;

__END__
