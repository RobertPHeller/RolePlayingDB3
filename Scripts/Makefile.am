Scripts = BWStdMenuBar.tcl fileentry.tcl HTMLHelp.tcl labelcombobox.tcl \
	  labelselectcolor.tcl labelspinbox.tcl ReadConfiguration.tcl \
	  RolePlayingDB3.tcl splash.tcl RPGTemplate.tcl ZipArchive.tcl \
	  RPGSheetEdit.tcl
Images  = DeepwoodsBanner.gif new.gif open.gif save.gif print.gif close.gif \
	  AddNewExitBanner.png AddNewItemBanner.png CharacterBanner.png \
	  CharacterButton.png CharacterRollBanner.png CreateMapBanner.png \
	  CreateSpaceBanner.png DressingBanner.png DressingButton.png \
	  ExitButton.png ExitInfoBanner.png MapBanner.png MapButton.png \
	  MonsterBanner.png MonsterButton.png SpaceBanner.png \
	  TemplateBanner.png ConfigMonster.png SmallTemplateMonster.png \
	  SpellBanner.png SpellButton.png TreasureBanner.png \
	  TreasureButton.png TrickTrapBanner.png TrickTrapButton.png \
	  background.png RPG-Icon.png CharacterDialogIcon.png \
	  DressingDialogIcon.png MonsterDialogIcon.png SpellDialogIcon.png \
	  TreasureDialogIcon.png TrickTrapDialogIcon.png \
	  SheetEditDialogIcon.png
	  
BUILT_SOURCES = Version.tcl
noinst_DATA = Version.tcl

EXTRA_DIST = $(Scripts) $(Images)

TCLKIT = $(top_builddir)/BuildData/tclkit${BEXEEXT}

BUILT_SOURCES += $(TCLKIT)


CLEANFILES = Version.tcl

Version.tcl : $(top_builddir)/config.status 
	$(TCLKIT) $(top_srcdir)/BuildScripts/MakeVersion.kit \
		$(PACKAGE) $(VERSION) \
		RPGDB_VERSION $(RPGDB_VERSION) \
		RPGDB_MAJOR_VERSION $(RPGDB_MAJOR_VERSION) \
		RPGDB_MINOR_VERSION $(RPGDB_MINOR_VERSION) \
		SYSTEMVERSION $(SYSTEMVERSION) \
		RPGDB_PATCH_LEVEL $(RPGDB_PATCH_LEVEL)


$(top_builddir)/BuildData/tclkit${BEXEEXT}: FORCE
	$(MAKE) -C $(top_builddir)/BuildData tclkit${BEXEEXT}

FORCE:

