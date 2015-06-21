
TOP = ..
LIB = ../lib
P2CH   = perl -I${HOME}/dev/perl/podpeople/lib ${HOME}/dev/perl/podpeople/bin/pod2coolhtml

dist: all

all: readme htmls

readme: ../README ../README.system

../README : ${LIB}/IO/Stringy.pm
	pod2readme -t $< > ../README

../README.system :
	perlsetup > ../README.system

htmls:
	${P2CH} -R ../lib -O . -I IO




