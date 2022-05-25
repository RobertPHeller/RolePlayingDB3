Scripts = $(wildcard *.tcl)
Images  = $(wildcard *.gif) $(wildcard *.png) $(wildcard *.xbm) \
		$(wildcard *.xpm)
	  
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


