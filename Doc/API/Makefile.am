##-*- makefile -*-############################################################
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 22 09:26:25 2022
#  Last Modified : <220524.1551>
#
#  Description	
#
#  Notes
#
#  History
#	
#  $Log$
#
##############################################################################
#
#    Copyright (C) 2022  Robert Heller D/B/A Deepwoods Software
#			51 Locke Hill Road
#			Wendell, MA 01379-9728
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
# 
#
##############################################################################

API = APIManual
API_PDF = $(API).pdf 
APIHBase = API
API_HTML = $(APIHBase)/index.html
API_Sources = 
API_FIGS = 


DOXYFILE = Doxyfile.in
BIBLIO = ../RPG.bib
EXTRA_DIST = $(DOXYFILE)
pkgdatadir = $(datadir)/$(PACKAGE)/Doc
pkgdata_DATA = $(API_PDF)
noinst_DATA  = $(API_HTML)

man3dir = $(mandir)/man3

CLEANFILES = $(API_PDF) $(API)_Version.h

clean-local:
	-rm -rf $(USERMANHBase) latex man
	
BUILT_SOURCES = $(API)_Version.h

$(API)_Version.h: $(top_builddir)/config.status
	echo '/** @page Version Version' >$(API)_Version.h
	echo "  * System version is $(VERSION)." >>$(API)_Version.h
	echo "  */" >>$(API)_Version.h
	
$(API_HTML) $(API_PDF) man/man3rpg : $(API_Sources) $(API_FIGS) $(BIBLIO) \
	$(API)_Version.h ../User/Preface.h ../User/COPYING.h Doxyfile \
	../User/footer.tex
	-rm -rf $(APIHBase)/
	-rm -rf latex
	-rm -rf man
	$(DOXYGEN) Doxyfile 
	-rm -f man/man3rpg/*.h.3rpg man/man3rpg/*.tcl.3rpg man/man3rpg/*.i.3rpg
	-rm -f man/man3rpg/_*trunk*_.3rpg
	cp $(srcdir)/$(BIBLIO) latex/
	make -C latex pdf
	mv latex/refman.pdf ./$(API_PDF)
	

install-data-hook: $(API_HTML)
	$(INSTALL) -d $(DESTDIR)$(pkgdatadir)/$(APIHBase)
	for p in $(APIHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=`echo $$p | sed -e 's|^.*/||'`; \
	  echo " $(INSTALL_DATA) '$$d$$p' '$(DESTDIR)$(pkgdatadir)/$(APIHBase)$$f'"; \
	  $(INSTALL_DATA) "$$d$$p" "$(DESTDIR)$(pkgdatadir)/$(APIHBase)/$$f"; \
	done
	test -z "$(man3dir)" || $(mkdir_p) "$(DESTDIR)$(man3dir)"
	@list=man/man3rpg/* ; \
	for i in $$list; do \
	  file=$$i; \
	  echo " $(INSTALL_DATA) '$$file' '$(DESTDIR)$(man3dir)/'"; \
	  $(INSTALL_DATA) "$$file" "$(DESTDIR)$(man3dir)/; \
	done	  
	    

uninstall-hook: 
	for p in $(DESTDIR)$(pkgdatadir)/$(APIHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=`echo $$p | sed -e 's|^.*/||'`; \
	  echo " rm -f '$(DESTDIR)$(pkgdatadir)/$(APIHBase)$$f'"; \
	  rm -f "$(DESTDIR)$(pkgdatadir)/$(APIHBase)/$$f"; \
	done
	@list=man/man3rpg/* ; \
	for i in $$list; do \
	  file=$$i;
	  f=`echo $$i | sed -e 's|^.*/||'`; \
	  echo " rm -f '$(DESTDIR)$(man3dir)/$$f'"; \
	  rm -f "$(DESTDIR)$(man3dir)/$$f"; \
	done
