#!/usr/bin/perl
use ExtUtils::MakeMaker;

#------------------------------------------------------------
# Makefile:
#------------------------------------------------------------

# Write the Makefile:
WriteMakefile(
	      NAME         => "IO-stringy",
	      VERSION_FROM => "lib/IO/Stringy.pm",
	      DISTNAME     => "IO-stringy",
	      'dist'       => {
		  PREOP    => "cd docs ; make",
		  COMPRESS => 'gzip',
		  SUFFIX   => 'gz',
	      }
	      );

