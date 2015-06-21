
use ExtUtils::MakeMaker;

WriteMakefile( 
	'NAME'		=> 'Text',
	'DISTNAME'	  => 'Text-Tabs+Wrap',
	'VERSION_FROM' => 'lib/Text/Wrap.pm',
	($] >= 5.005 ?
	    ('ABSTRACT' => 'Expand tabs and do simple line wrapping',
	     'AUTHOR'	=> 'David Muir Sharnoff <muir@idiom.com>') : ()),
	'dist' => { COMPRESS=>"gzip", SUFFIX=>"gz" },
	'INSTALLDIRS'	=> 'perl',
	);

