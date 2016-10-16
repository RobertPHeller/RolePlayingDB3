USERMAN_HS = Preface.h Introduction.h \
	Help.h Tutorial.h Reference.h

USERMAN_FIGS = CharacterEditor.png   CreateOrOpenTemplate.png  \
	       TemplateEditor1.png   CreateOrOpenChar.png  \
	       MainWindow.png        TemplateEditor2.png \
	       ConfigurationEditor.png SpaceEditor.png LevelEditor.png \
	       MapEditor.png OptionMenu.png CreateOrEditTemplateDialog.png \
	       EmptyTemplateEditor.png AddNewTemplate.png \
	       AddNewTemplatePlayer.png PlayerTemplateEditor.png \
	       EmptyEditContainerText.png EditContainerTextWithText.png \
	       PlayerTemplateEditorHeading.png AddNewFieldDialog.png \
	       AddNewFieldDialogWithField.png \
	       PlayerTemplateEditorWithField.png \
	       AddNewFieldDialogWithContainer.png \
	       PlayerTemplateEditorWithContainer.png \
	       OpenConfigurationEditor.png SelectTemplateFile.png \
	       EmptyPlayerCharacterSheet.png CreateOrOpenMap.png \
	       EmptyMapEditorWindow.png TestMapEditorWindow1.png \
	       CreateNewLevel.png EmptyLevelEditorWindow.png \
	       ExampleLevelEditorWindow.png CreateNewSpace.png \
	       CreateEntryMeadowSpace.png NewEntryMeadowSpace.png \
	       EntryMeadowSpaceBasicInformation.png StrongholdLevel1.png \
	       EntryRoom.png CreatingHillock.png CreatingStairsDown.png \
	       UpdatedSpaceWithStairsHillockBushesTrees.png


USERMAN = UserManual
USERMAN_PDF = $(USERMAN).pdf
USERMANHBase = UserManual
USERMAN_HTML = $(USERMANHBase)/$(USERMAN).html

DOXYFILE = Doxyfile.in

BIBLIO = ../RPG.bib

EXTRA_DIST = $(USERMAN_HS) $(USERMAN_FIGS) COPYING.h $(DOXYFILE) footer.tex

pkgdatadir = $(datadir)/$(PACKAGE)/Doc

pkgdata_DATA = $(USERMAN_PDF) 
noinst_DATA  = $(USERMAN_HTML)


CLEANFILES = UserManual.pdf $(USERMAN)_Version.h

clean-local:
	-rm -rf $(USERMANHBase) latex

BUILT_SOURCES = $(USERMAN)_Version.h

$(USERMAN)_Version.h : $(top_builddir)/config.status
	echo '/** @page Version Version' >$(USERMAN)_Version.h
	echo "  * System version is $(VERSION)." >>$(USERMAN)_Version.h
	echo "  */" >>$(USERMAN)_Version.h



$(USERMAN_HTML) $(USERMAN_PDF) : $(USERMAN_HS) $(USERMAN_FIGS) $(BIBLIO) \
	$(USERMAN)_Version.h COPYING.h Doxyfile footer.tex
	-rm -rf $(USERMANHBase)/
	-rm -rf latex
	$(DOXYGEN) Doxyfile
	for p in $(USERMAN_FIGS); do \
	   if test ! \( -f $(USERMANHBase)/$$p \); then cp $(srcdir)/$$p $(USERMANHBase)/; fi; \
	done
	-rm -f $(USERMANHBase)/pages.html
	cp $(srcdir)/$(BIBLIO) latex/
	make -C latex pdf
	mv latex/refman.pdf ./$(USERMAN_PDF)

install-data-hook: $(USERMAN_HTML)
	$(INSTALL) -d $(DESTDIR)$(pkgdatadir)/$(USERMANHBase)
	for p in $(USERMANHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=`echo $$p | sed -e 's|^.*/||'`; \
	  echo " $(INSTALL_DATA) '$$d$$p' '$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)$$f'"; \
	  $(INSTALL_DATA) "$$d$$p" "$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/$$f"; \
	done

uninstall-hook: 
	for p in $(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/*; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f=`echo $$p | sed -e 's|^.*/||'`; \
	  echo " rm -f '$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)$$f'"; \
	  rm -f "$(DESTDIR)$(pkgdatadir)/$(USERMANHBase)/$$f"; \
	done

	


