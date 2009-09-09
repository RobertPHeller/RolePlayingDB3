USERMAN_TEXS = UserManual.tex Preface.tex Introduction.tex \
	Help.tex Tutorial.tex Reference.tex
USERMAN_FIGS = CharacterEditor.png   CreateOrOpenTemplate.png  \
	       TemplateEditor1.png   CreateOrOpenChar.png  \
	       MainWindow.png        TemplateEditor2.png
USERMAN = UserManual
USERMAN_PDF = $(USERMAN).pdf
USERMANHBase = UserManual
USERMAN_HTML = $(USERMANHBase)/$(USERMAN).html

BIBLIO = ../RPG.bib

EXTRA_DIST = $(USERMAN_TEXS) $(USERMAN_FIGS) COPYING.tex

pkgdatadir = $(datadir)/$(PACKAGE)/Doc

pkgdata_DATA = $(USERMAN_PDF) 
noinst_DATA  = $(USERMAN_HTML)


CLEANFILES = UserManual.pdf *.aux *.bbl *.blg *.idx *.ilg \
	*.ind *.lof *.lot *.lol *.toc *.css UserManual


BUILT_SOURCES = $(USERMAN)_Version.tex

$(USERMAN)_Version.tex : $(top_builddir)/config.status
	echo '\chapter*{Version}'               >$(USERMAN)_Version.tex
	echo '\addcontentsline{toc}{chapter}{Version}' \
						>>$(USERMAN)_Version.tex
	echo "System version is $(VERSION).\\\\" >>$(USERMAN)_Version.tex




$(USERMAN_PDF) : $(USERMAN_TEXS) $(USERMAN_FIGS) $(BIBLIO) \
	$(USERMAN)_Version.tex ../MyTitlepage.sty ../titlepage.tex \
		 COPYING.tex  ../Cover1.png
	for f in $(USERMAN_TEXS) $(USERMAN_FIGS); do \
	    if test -f "./$$f"; then continue; else ln -s $(srcdir)/$$f ./; fi; \
	done
	-ln -s $(srcdir)/COPYING.tex ./
	-ln -s $(srcdir)/../Cover1.png ./
	-ln -s $(srcdir)/$(BIBLIO) ./
	-ln -s $(srcdir)/../MyTitlepage.sty ./
	-ln -s $(srcdir)/../titlepage.tex ./
	$(PDFLATEX) $(USERMAN)
	-$(BIBTEX) $(USERMAN)
	echo '\begin{theindex}' > $(USERMAN).ind
	echo '\item Dummy, 0' >> $(USERMAN).ind
	echo '\end{theindex}' >> $(USERMAN).ind
	$(PDFLATEX) $(USERMAN)
	$(MAKEINDEX) $(USERMAN).idx
	$(PDFLATEX) $(USERMAN)

$(USERMAN_HTML) : $(USERMAN_PDF)
	-rm -rf $(USERMANHBase)
	-mkdir $(USERMANHBase)
	$(HTLATEX) $(USERMAN) "html,2,info" "" "-d$(USERMANHBase)/"
	(cd $(USERMANHBase)&&ln -s ../*.png ./)
	-rm -f zz$(USERMAN).p* $(USERMAN).4ct $(USERMAN).xref \
		 $(USERMAN).4tc tex4ht.fls $(USERMAN).tmp $(USERMAN).lg \
		 $(USERMAN).idv $(USERMAN).dvi $(USERMAN)*.html

install-data-hook: $(USERMAN_HTML)
	$(INSTALL) -d $(DESTDIR)$(pkgdatadir)/$(USERMANHBase)
	for p in $(USERMANHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " $(pkgdataDATA_INSTALL) '$$d$$p' '$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)$$f'"; \
	  $(pkgdataDATA_INSTALL) "$$d$$p" "$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/$$f"; \
	done
	
uninstall-hook: 
	for p in $(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=$(am__strip_dir) \
	  echo " rm -f '$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)$$f'"; \
	  rm -f "$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/$$f"; \
	done
	


