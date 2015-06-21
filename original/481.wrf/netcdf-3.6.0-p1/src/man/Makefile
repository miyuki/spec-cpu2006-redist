
# This makefile builds netcdf documentation from texinfo source. The
# four *.texi files are the texinfo files, the *.info, *.txt, etc. are
# all generated from the texi source.

# Users need not generate their own documentation, as the latest
# version is available at the netcdf website
# http://www.unidata.ucar.edu/packages/netcdf

# If you do choose to run this makefile, make sure your makeinfo is
# reasonably up to date. Perl is used to added css link to all html
# pages, but if it fails that's no big deal. htmltidy is also run on
# the html files.

# Below I just check for the info version of each file, and if it's
# out of date, I regenerate all documentation for that manual. I would
# love to fancify this more, but it's Sunday morning and the little
# darlings are about to wake up, so this is no time to get interested
# in make!

# There once was a programmer named Ed,
# who carried build infomation in his head,
# as he aged he forgot,
# and forgot quit a lot,
# so now he uses Makefiles instead!

# $Id: Makefile,v 1.21 2004/12/16 16:31:37 ed Exp $

PACKING_LIST	= Makefile  netcdf.m4 netcdf-install.texi \
		  netcdf.texi netcdf-c.texi netcdf-cxx.texi netcdf-f77.texi netcdf-f90.texi 
include ../macros.make

info_files = netcdf.info netcdf-f77.info netcdf-cxx.info netcdf-f90.info netcdf-c.info netcdf-install.info
txt_files = netcdf.txt netcdf-f77.txt netcdf-cxx.txt netcdf-f90.txt netcdf-c.txt netcdf-install.txt
html_files = netcdf.html netcdf-f77.html netcdf-cxx.html netcdf-f90.html netcdf-c.html netcdf-install.html
ps_files = netcdf.ps netcdf-f77.ps netcdf-cxx.ps netcdf-f90.ps netcdf-c.ps netcdf-install.ps
dvi_files = netcdf.dvi netcdf-f77.dvi netcdf-cxx.dvi netcdf-f90.dvi netcdf-c.dvi netcdf-install.dvi
pdf_files = netcdf.pdf netcdf-f77.pdf netcdf-cxx.pdf netcdf-f90.pdf netcdf-c.pdf netcdf-install.pdf

all: $(info_files) $(txt_files) $(html_files) $(ps_files) $(dvi_files) $(pdf_files) netcdf-man-3.html netcdf-man-3f.html ncgen-man-1.html ncdump-man-1.html INSTALL

%.info : %.texi
	makeinfo $<

%.pdf : %.texi
	texi2pdf -q --output=$*.pdf $<

%.dvi: %.texi 
	texi2dvi -c -q $<

%.ps: %.texi %.dvi
	dvips -q -o $*.ps $*.dvi

# I don't know why, but makeinfo swallows the "i" in @import. So I have two.
%.html : %.texi
	echo "@iimport url(http://my.unidata.ucar.edu/themes/Unidata/style/style.css);">tmp.css
	makeinfo --css-include=tmp.css --html $<
	makeinfo --no-split --css-include=tmp.css --html $<
	-cd $*; tidy -meq *.html

%.txt : %.texi
	makeinfo -o $*.txt --no-headers $<

# This isn't to install docs, it's to make the INSTALL doc.
INSTALL: netcdf-install.texi
	makeinfo -o INSTALL --no-headers netcdf-install.texi
	cp INSTALL ..

# The man pages are made in libsrc/Makefile and fortran/Makefile too,
# when the distribution is made. Next version (4.0) they will only be
# made here. For this version (3.6.0), make them here so I can easily
# put them in the web site tarball.
netcdf.3: netcdf.m4
	$(M4) $(M4FLAGS) -DAPI=C $? >$@  || rm $@

netcdf-man-3.html: netcdf.3
	groff -man -Thtml $? > $@

ncgen-man-1.html: ../ncgen/ncgen.1
	groff -man -Thtml $? > $@

ncdump-man-1.html: ../ncdump/ncdump.1
	groff -man -Thtml $? > $@

netcdf.3f: netcdf.m4
	$(M4) $(M4FLAGS) -DAPI=FORTRAN $? >$@ || rm $@

netcdf-man-3f.html: netcdf.3f
	groff -man -Thtml $? > $@

# This isn't made by all, but tars up everything needed for the website
web-tarball: all 
	tar cf netcdf_info.tar *.info*
	gzip -f netcdf_info.tar
	tar cf netcdf_pdf.tar *.pdf
	gzip -f netcdf_pdf.tar
	tar cf netcdf_html.tar netcdf netcdf-c netcdf-f77 netcdf-f90 netcdf-cxx netcdf-install
	gzip -f netcdf_html.tar
	tar cf netcdf_txt.tar *.txt
	gzip -f netcdf_txt.tar
	tar cf netcdf_man.tar netcdf.3 netcdf.3f -C ../ncdump ncdump.1 -C ../ncgen ncgen.1
	gzip -f netcdf_man.tar
	tar cf netcdf_docs.tar netcdf netcdf-c netcdf-f77 netcdf-f90 netcdf-cxx netcdf-install *.pdf *.txt *.html netcdf_info.tar.gz netcdf_pdf.tar.gz netcdf_html.tar.gz netcdf_txt.tar.gz netcdf_man.tar.gz
	gzip -f netcdf_docs.tar

clean_up:
	-rm *.info *.info *.ps *.txt *.html tmp.css *.dvi *.log *.pdf INSTALL netcdf.3 netcdf.3f
	-rm -rf netcdf netcdf-c netcdf-cxx netcdf-f77 netcdf-f90 netcdf-install

include ../rules.make


