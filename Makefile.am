SUBDIRS = BuildScripts BuildData Doc Scripts SampleData

EXTRA_DIST = RolePlayingDB3.spec RolePlayingDB3.spec.in bootstrap Doxyfile.in Doxyfile

TCLKIT = $(top_builddir)/BuildData/tclkit${BEXEEXT}

BUILT_SOURCES = $(TCLKIT)

bin_SCRIPTS = RolePlayingDB3$(EXEEXT)

man_MANS = man1/RolePlayingDB3.1

AllScripts = $(wildcard $(top_srcdir)/Scripts/*.tcl) $(wildcard Scripts/*.tcl)
LibScripts = $(filter-out %RolePlayingDB3.tcl,$(AllScripts))
AllImages  = $(wildcard $(top_srcdir)/Scripts/*.gif) \
		$(wildcard $(top_srcdir)/Scripts/*.png)
AllHelpDep = Doc/User/UserManual/UserManual.html
AllHelpFiles = $(wildcard Doc/User/UserManual/*)

RolePlayingDB3$(EXEEXT): $(AllScripts) $(AllImages) $(AllHelpDep)
	-rm -rf RolePlayingDB3.vfs
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit qwrap \
					$(srcdir)/Scripts/RolePlayingDB3.tcl
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit unwrap RolePlayingDB3.kit
	-rm -rf RolePlayingDB3.kit
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(BWLIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(SNITLIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit RolePlayingDB3 \
		lib $(URILIB)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/$(IMG_ZIP)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/$(TCLXML_ZIP)
	$(UNZIP) -qq -d RolePlayingDB3.vfs/lib \
		$(top_srcdir)/BuildData/pdf4tcl05.zip
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 lib/Common $(LibScripts)		
	$(TCLKIT) $(top_srcdir)/BuildScripts/MakePkgIndex.kit RolePlayingDB3 \
		Common 
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 Images $(AllImages)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir) \
		RolePlayingDB3 Help $(AllHelpFiles)
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit wrap \
		RolePlayingDB3$(EXEEXT) \
		-runtime $(top_srcdir)/BuildData/$(TCLKITRUN)
	-rm -rf RolePlayingDB3.vfs RolePlayingDB3.kit

$(top_builddir)/BuildData/tclkit${BEXEEXT}: FORCE
	$(MAKE) -C $(top_builddir)/BuildData tclkit${BEXEEXT}

$(top_builddir)/Scripts/Version.tcl: $(top_builddir)/config.status
	$(MAKE) -C $(top_builddir)/Scripts Version.tcl


FORCE:

Doc/User/UserManual/UserManual.html: FORCE
	$(MAKE) -C $(top_builddir)/Doc/User UserManual/UserManual.html

dist-binary-tarbz2: all
	$(MAKE) $(AM_MAKEFLAGS) DESTDIR=/tmp/$(PACKAGE)-$(VERSION)-$(target) install
	tar cjf $(PACKAGE)-$(VERSION)-$(target).tar.bz2 -C /tmp/$(PACKAGE)-$(VERSION)-$(target)${prefix} .
	rm -rf /tmp/$(PACKAGE)-$(VERSION)-$(target)

dist-binary-zip: all
	$(MAKE) $(AM_MAKEFLAGS) DESTDIR=/tmp/$(PACKAGE)-$(VERSION)-$(target) install
	rm -f $(PACKAGE)-$(VERSION)-$(target).zip
	(cd  /tmp/$(PACKAGE)-$(VERSION)-$(target)${prefix}; zip -rq $(BUILD_DIR)/$(PACKAGE)-$(VERSION)-$(target).zip .)

	
man1/RolePlayingDB3.1 : Doxyfile Scripts/RolePlayingDB3.tcl
	-rm -rf man1
	$(DOXYGEN)

clean-local:
	-rm -rf man1


